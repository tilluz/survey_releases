#################################################

# In this file, we calculate the risk-related measures for the three survey types ('original', 'geomasked', 'synthetic').

#################################################

library(tidyverse)
library(reticulate)
library(randomForest)
library(caret)
library(foreach)
library(doParallel)

# Number of processors to use for parallelization
registerDoParallel(7)

# Some dplyr configs
options(dplyr.summarise.inform = FALSE)
select <- dplyr::select

# Helpers to read pickle files
py_install('pandas')
pd <- import('pandas')

# Settings
var_order <- 'mixed' # Options: mixed or identical
model <- 'GC' # Options are: GC
syn_level <- 'strata' # Defines on which level the copula fitting for SDG was performed. Options are: country, strata or pcd
pop_level = 'sample' # Detailing the sampling type for synthetic data. Options are: population or sample
encoding <- 'categorical' # Options are: 'categorical' or 'label_encoding'
field_dist <- 'gaussian' # Options are: 'gaussian' or 'parametric'
nbi_type <- 'syn_NBI' # Options are: 'new_NBI', 'syn_NBI'
n_sim <- 100 # Number of simulation rounds
n_mse <- 1 # Number of anonymization re-runs per simulation

source('helpers.R')

# Load data ---------------------------------------------------------------

# Census
census<-readRDS("./data/midsave/census.rds")

census <- census %>% 
  mutate(REGION = ID_REGION,
         REGION = replace(ID_REGION, ID_REGION == 'Resto Región Central', 'Gran Área Metropolitana') %>% 
           as.factor() %>% 
           as.integer(),
         ID_PCD = as.character(ID_PCD),
         )

census$strata<-as.factor(paste0(census$ID_ZONA,census$REGION))



# Define attributes -------------------------------------------------------

attr_key <- census %>% 
  select(where(~sum(!is.na(.x)) > 0)) %>% # Remove vars that are all NA in the true survey
  select(starts_with('V', ignore.case = F),
         starts_with('P', ignore.case = F),
         starts_with('H', ignore.case = F)
  ) %>% 
  names()

attr_sens <- c('NBI')

attr_key_select <- c('ID_PCD', 'P02_SEXO', 'P03_EDAD')


########
# Risk #
########

# Uniqueness --------------------------------------------------------------

set.seed(1234)

unique_df <- data.frame()

attr_unique <- c(attr_key_select, attr_key[!attr_key %in% attr_key_select])

for (syn_level in c('country', 'strata', 'pcd')){
  unique_df <- foreach(i = 1:n_sim,
                       .combine=bind_rows) %dopar% 
    {
      # Load data
      ### True survey
      survey_true <- readRDS(paste0("./data/midsave/syn_surveys/survey_",i,".rds")) %>%  
        drop_na(NBI) %>% 
        mutate(ID_PCD = as.character(ID_PCD))
      
      ### Geoprivate survey
      survey_geo <- readRDS(paste0("./data/midsave/syn_surveys/geo_survey_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,"_",i,".rds"))
      
      ### Synthetic survey
      survey_syn <- pd$read_pickle(paste0("./data/midsave/syn_surveys/syn_survey_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",n_sim,"_",n_mse,"_",i,".pkl")) %>% 
        mutate(across(where(is.list), as.character),
               across(everything(), ~na_if(., "NaN")),
               ID_ZONA = str_sub(strata, end = -2),
               P00_NUMERO_LINEA = P00_NUMERO_LINEA - runif(length(P00_NUMERO_LINEA), min = 0, max = 0.001)
        ) %>% 
        group_by(hhid) %>% 
        mutate(P00_NUMERO_LINEA = as.integer(ordered(P00_NUMERO_LINEA))) %>%
        ungroup %>%
        rename_with(~str_c("syn_", .), .cols = starts_with('NBI')) %>% 
        nbi_calc() %>% 
        rename(NBI = nbi_type)
      
      attr(survey_syn, "pandas.index") <- NULL
      
      # Re-identified true survey (details on the approach below)
      rf_eval <- survey_true %>% 
        select(ID_PCD, strata, {attr_key}) %>% 
        select(where(~sum(!is.na(.x)) == length(.x))) %>%
        rename(y = ID_PCD) %>%
        mutate(across(c('y', 'strata'), as.character)) %>%
        drop_na() %>% 
        arrange(y, strata)
      
      rf_syn <- survey_syn %>%
        select(ID_PCD, PSUD, {attr_key}) %>% 
        select(where(~sum(!is.na(.x)) == length(.x))) %>%
        rename(y = ID_PCD) %>%
        mutate(strata = str_sub(PSUD, start = 6),
               across(c('y'), as.character)) %>%
        filter(y %in% as.character(rf_eval$y),
               strata %in% as.character(rf_eval$strata)) %>% 
        select(-PSUD) %>% 
        drop_na() %>% 
        arrange(y, strata)
      
      rf_eval <- rf_eval[,names(rf_eval) %in% names(rf_syn)]
      rf_syn <- rf_syn[,names(rf_syn) %in% names(rf_eval)]
      
      survey_reid <- data.frame()
      
      # Re-identification attempt for each stratum separately using a random forest approach
      for (j in unique(rf_eval$strata)){
        
        rf_eval_select <- rf_eval %>% 
          filter(strata == j) %>% 
          mutate(across(c('y'), as.factor))
        
        rf_syn_select <- rf_syn %>% 
          filter(strata == j,
                 y %in% as.character(rf_eval_select$y)) %>% 
          mutate(across(c('y'), as.factor))
        
        if((sum(nlevels(rf_syn_select$y) == nlevels(rf_eval_select$y)) != 1) | 
           (nlevels(rf_eval_select$y) == 1)
        ){
          next
        }

        # Synthetic Survey
        rf_model <- randomForest(y ~ .,
                                 data = rf_syn_select,
                                 ntree = 500,
                                 mtry = 3,
                                 importance = TRUE)
        
        survey_reid <- survey_true %>% 
          filter(strata == j) %>% 
          select(-ID_PCD) %>% 
          mutate(strata = as.character(strata),
                 ID_PCD = predict(rf_model, type = "class")) %>% 
          bind_rows(survey_reid)
      }
      
      df_run <- data.frame()
      
      # alternatively, we could mix up the ordering of attributes:
      attr_unique_mixed <- attr_unique %>% 
        as.data.frame() %>% 
        sample_n(size = length(attr_unique), replace = FALSE) %>% 
        .[,1]
      
      for(j in 1:length(attr_unique)){
        
        if(var_order == 'mixed'){
          attr_unique_select <- attr_unique_mixed[1:j]
        }
        
        if(var_order == 'identical'){
          attr_unique_select <- attr_unique[1:j]
        }
        
        # Recovery rate
        
        '
        How many masked records can also be found in the true survey?
        '
        
        ### Geomasked Survey
        geo_recovery <- mean(
          do.call(paste0,
                  survey_geo %>% 
                    select({attr_unique_select})
          ) %in% do.call(paste0, 
                         survey_true %>% 
                           select({attr_unique_select})
          )
        )
        
        ### Synthetic Survey
        syn_recovery <- mean(
          do.call(paste0,
                  survey_syn %>% 
                    select({attr_unique_select})
          ) %in% do.call(paste0, 
                         survey_true %>% 
                           select({attr_unique_select})
          )
        )
        
        ### Re-identified Survey
        reid_recovery <- mean(
          do.call(paste0,
                  survey_reid %>% 
                    select({attr_unique_select})
          ) %in% do.call(paste0, 
                         survey_true %>% 
                           select({attr_unique_select})
          )
        )

        
        # Uniqueness
        
        '
        Are the recovered records unique in the census?
        The do.call function checks for each row in the census whether it is also present in the respective survey.
        '
        
        ### True Survey
        true_uniqueness <- census %>% 
          select({attr_unique_select}) %>% 
          mutate(unique = do.call(paste0,.) %in% do.call(paste0, survey_true %>% 
                                                           select({attr_unique_select}))) %>% 
          filter(unique == T) %>% 
          group_by(across(everything())) %>% 
          mutate(n = n()) %>% 
          ungroup() %>% 
          summarise(n_recovered = length(n),
                    n_unique = length(n[n == 1])) %>% 
          mutate(run = i, 
                 n_attr = j,
                 type = 'true',
                 p_sample_recovered = NA,
                 n_sample = nrow(survey_true),
                 p_recovered = n_recovered/n_sample,
                 p_unique = n_unique/n_sample) %>% 
          select(run, n_attr, type, p_sample_recovered, n_sample, n_recovered, n_unique, p_recovered, p_unique)
        
        
        ### Geomasked Survey
        geo_uniqueness <- census %>% 
          select({attr_unique_select}) %>% 
          mutate(recovered = do.call(paste0,.) %in% do.call(paste0, survey_geo %>%
                                                              select({attr_unique_select})
          )) %>% 
          filter(recovered == T) %>% 
          group_by(across(everything())) %>% 
          mutate(n = n()) %>% 
          ungroup() %>% 
          summarise(n_recovered = length(n),
                    n_unique = length(n[n == 1])) %>% 
          mutate(run = i, 
                 n_attr = j,
                 type = 'geo',
                 p_sample_recovered = geo_recovery,
                 n_sample = nrow(survey_geo),
                 p_recovered = n_recovered/n_sample,
                 p_unique = n_unique/n_sample) %>% 
          select(run, n_attr, type, p_sample_recovered, n_sample, n_recovered, n_unique, p_recovered, p_unique)
        
        ### Synthetic Survey
        syn_uniqueness <- census %>% 
          select({attr_unique_select}) %>% 
          mutate(recovered = do.call(paste0,.) %in% do.call(paste0, survey_syn %>%
                                                              select({attr_unique_select})
          )) %>% 
          filter(recovered == T) %>% 
          group_by(across(everything())) %>% 
          mutate(n = n()) %>% 
          ungroup() %>% 
          summarise(n_recovered = length(n),
                    n_unique = length(n[n == 1])) %>% 
          mutate(run = i,
                 n_attr = j,
                 type = 'syn',
                 p_sample_recovered = syn_recovery,
                 n_sample = nrow(survey_syn),
                 p_recovered = n_recovered/n_sample,
                 p_unique = n_unique/n_sample) %>% 
          select(run, n_attr, type, p_sample_recovered, n_sample, n_recovered, n_unique, p_recovered, p_unique)
        
        ### Re-identified Survey
        reid_uniqueness <- census %>% 
          select({attr_unique_select}) %>% 
          mutate(recovered = do.call(paste0,.) %in% do.call(paste0, survey_reid %>%
                                                              select({attr_unique_select})
          )) %>% 
          filter(recovered == T) %>% 
          group_by(across(everything())) %>% 
          mutate(n = n()) %>% 
          ungroup() %>% 
          summarise(n_recovered = length(n),
                    n_unique = length(n[n == 1])) %>% 
          mutate(run = i,
                 n_attr = j,
                 type = 'reid',
                 p_sample_recovered = reid_recovery,
                 n_sample = nrow(survey_reid),
                 p_recovered = n_recovered/n_sample,
                 p_unique = n_unique/n_sample) %>% 
          select(run, n_attr, type, p_sample_recovered, n_sample, n_recovered, n_unique, p_recovered, p_unique)
        
        ### No-Geo Survey
        if(j != 1){
          nogeo_uniqueness <- census %>% 
            select({attr_unique_select[-1]}) %>% 
            mutate(recovered = do.call(paste0,.) %in% do.call(paste0, survey_true %>%
                                                                select({attr_unique_select[-1]})
            )) %>% 
            filter(recovered == T) %>% 
            group_by(across(everything())) %>% 
            mutate(n = n()) %>% 
            ungroup() %>% 
            summarise(n_recovered = length(n),
                      n_unique = length(n[n == 1])) %>% 
            mutate(run = i,
                   n_attr = j-1,
                   type = 'no_geo',
                   p_sample_recovered = NA,
                   n_sample = nrow(survey_true),
                   p_recovered = n_recovered/n_sample,
                   p_unique = n_unique/n_sample) %>% 
            select(run, n_attr, type, p_sample_recovered, n_sample, n_recovered, n_unique, p_recovered, p_unique)
        }
        
        if(j == 1){
          df_run <- df_run %>% 
            bind_rows(., true_uniqueness, geo_uniqueness, syn_uniqueness, reid_uniqueness)
        }else{
          df_run <- df_run %>% 
            bind_rows(., true_uniqueness, geo_uniqueness, syn_uniqueness, reid_uniqueness, nogeo_uniqueness)
        }
        
        
        print(paste0(j, ' of ', length(attr_unique), ' attributes for run ', i, ' out of ', n_sim,' done!'))
      }
      
      list(df_run, unique_df)
      
    }
  
  stopImplicitCluster()
  
  saveRDS(unique_df, file = paste0("./data/midsave/risk_uniqueness_",var_order,"_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

}



# Random Forest classifier ------------------------------------------------

'
Recall that we propose a new survey data publishing strategy with:
a) strata-level true microdata and
b) cluster-level synthetic data.

To ensure dataset b) cannot be used to infer lower-level geographic identifiers (e.g. PSUD) in the true microdata, we need to test it.

We use PSUD as our sensitive attribute. We fit a RF model on the sensitive attribute using key attributes from synthetic data.
Then, we use that model to predict the sensitive attribute in the true data.
'

for (syn_level in c('country', 'strata', 'pcd')){
  
  set.seed(1234)
  
  rf_output <- data.frame()
  
  rf_output <- foreach(i = 1:n_sim,
                       .combine=bind_rows) %dopar% 
    {
      # Load data
      ### True survey
      survey_true <- readRDS(paste0("./data/midsave/syn_surveys/survey_",i,".rds")) %>%  
        drop_na(NBI) %>% 
        mutate(ID_PCD = as.character(ID_PCD))
      
      ### Geoprivate survey
      survey_geo <- readRDS(paste0("./data/midsave/syn_surveys/geo_survey_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,"_",i,".rds"))
      
      ### Synthetic survey
      survey_syn <- pd$read_pickle(paste0("./data/midsave/syn_surveys/syn_survey_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",n_sim,"_",n_mse,"_",i,".pkl")) %>% 
        mutate(across(where(is.list), as.character),
               across(everything(), ~na_if(., "NaN")),
               ID_ZONA = str_sub(strata, end = -2),
               P00_NUMERO_LINEA = P00_NUMERO_LINEA - runif(length(P00_NUMERO_LINEA), min = 0, max = 0.001)
        ) %>% 
        group_by(hhid) %>% 
        mutate(P00_NUMERO_LINEA = as.integer(ordered(P00_NUMERO_LINEA))) %>% 
        ungroup %>%
        rename_with(~str_c("syn_", .), .cols = starts_with('NBI')) %>% 
        nbi_calc() %>% 
        rename(NBI = nbi_type) %>% 
        as.data.frame()
      
      attr(survey_syn, "pandas.index") <- NULL
      
      # Prepare datasets
      
      rf_eval <- survey_true %>% 
        select(ID_PCD, strata, {attr_key}) %>% 
        select(where(~sum(!is.na(.x)) == length(.x))) %>% 
        rename(y = ID_PCD) %>%
        mutate(across(c('y', 'strata'), as.character)) %>%
        drop_na() %>% 
        arrange(y, strata)
      
      rf_geo <- survey_geo %>%
        select(ID_PCD, PSUD, {attr_key}) %>% 
        select(where(~sum(!is.na(.x)) == length(.x))) %>% 
        rename(y = ID_PCD) %>%
        mutate(strata = str_sub(PSUD, start = 6),
               across(c('y'), as.character)) %>%
        filter(y %in% as.character(rf_eval$y),
               strata %in% as.character(rf_eval$strata)) %>% 
        select(-PSUD) %>% 
        drop_na() %>% 
        arrange(y, strata)
      
      rf_eval <- rf_eval %>% 
        filter(y %in% as.character(rf_geo$y),
               strata %in% as.character(rf_geo$strata)) %>%
        drop_na() %>% 
        arrange(y, strata)
      
      rf_syn <- survey_syn %>%
        select(ID_PCD, PSUD, {attr_key}) %>% 
        select(where(~sum(!is.na(.x)) == length(.x))) %>% 
        rename(y = ID_PCD) %>%
        mutate(across(c('y'), as.character)) %>%
        filter(y %in% as.character(rf_eval$y),
               strata %in% as.character(rf_eval$strata)) %>% 
        select(-PSUD) %>% 
        drop_na() %>% 
        arrange(y, strata)
      
      rf_eval <- rf_eval[,names(rf_eval) %in% names(rf_geo)]
      rf_geo <- rf_geo[,names(rf_geo) %in% names(rf_eval)]
      rf_syn <- rf_syn[,names(rf_syn) %in% names(rf_eval)]
      
      df_run <- data.frame()
      l = 1
      
      for (j in unique(rf_eval$strata)){
        
        rf_eval_select <- rf_eval %>% 
          filter(strata == j)
        
        rf_geo_select <- rf_geo %>% 
          filter(strata == j,
                 y %in% as.character(rf_eval_select$y)) %>% 
          mutate(across(c('y'), as.factor))
        
        rf_eval_select <- rf_eval %>% 
          filter(y %in% as.character(rf_geo_select$y)) %>% 
          mutate(across(c('y'), as.factor))
        
        rf_syn_select <- rf_syn %>% 
          filter(strata == j,
                 y %in% as.character(rf_eval_select$y)) %>% 
          mutate(across(c('y'), as.factor))
        
        if((sum(nlevels(rf_geo_select$y) == nlevels(rf_eval_select$y)) != 1) | 
           (sum(nlevels(rf_syn_select$y) == nlevels(rf_eval_select$y)) != 1) | 
           (nlevels(rf_eval_select$y) == 1)
        ){
          next
        }
        
        # Geomasked Survey
        rf_model <- randomForest(y ~ .,
                                 data = rf_geo_select,
                                 ntree = 500,
                                 mtry = 3,
                                 importance = TRUE)
        
        df_run <- df_run %>% 
          rbind(data.frame(
            'run' = i,
            'type' = 'geo',
            'strata' = j,
            'acc_in' = mean(predict(rf_model, rf_geo_select, type = "class") == rf_geo_select[,'y']),
            'acc_eval' = mean(predict(rf_model, rf_eval_select, type = "class") == rf_eval_select[,'y']),
            'acc_guess' = mean(sample(rf_geo_select[,'y'],
                                      size = nrow(rf_eval_select),
                                      replace = TRUE) == rf_eval_select[,'y']),
            'oos_error' = rf_model$err.rate[500,1],
            'n_clusters' = nlevels(rf_eval_select$y)
          ))
        
        # Synthetic Survey
        rf_model <- randomForest(y ~ .,
                                 data = rf_syn_select,
                                 ntree = 500,
                                 mtry = 3,
                                 importance = TRUE)
        
        df_run <- df_run %>% 
          rbind(data.frame(
            'run' = i,
            'type' = 'syn',
            'strata' = j,
            'acc_in' = mean(predict(rf_model, rf_syn_select, type = "class") == rf_syn_select[,'y']),
            'acc_eval' = mean(predict(rf_model, rf_eval_select, type = "class") == rf_eval_select[,'y']),
            'acc_guess' = mean(sample(rf_syn_select[,'y'],
                                      size = nrow(rf_eval_select),
                                      replace = TRUE) == rf_eval_select[,'y']),
            'oos_error' = rf_model$err.rate[500,1],
            'n_clusters' = nlevels(rf_eval_select$y)
          ))
        
        print(paste0(l, ' of ', length(unique(rf_eval$strata)), ' stratas for ', i, ' out of ', n_sim,' runs done!'))
        l = l + 1
      }
      
      list(df_run, rf_output)
      
    }
  
  stopImplicitCluster()
  
  rownames(rf_output) <- NULL
  
  saveRDS(rf_output, file = paste0("./data/midsave/risk_privacy_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
  
}


