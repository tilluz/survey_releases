#################################################

# In this file, we calculate the normalized KL divergence of the three survey types ('original', 'geomasked', 'synthetic')

#################################################

library(tidyverse)
library(reticulate)
library(philentropy, quietly = TRUE, warn.conflicts = FALSE)
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
model <- 'GC' # Options are: GC
pop_level = 'sample' # Detailing the sampling type for synthetic data. Options are: population or sample
nbi_type <- 'syn_NBI' # Options are: 'new_NBI', 'syn_NBI'
n_sim <- 100 # Number of simulation rounds
n_mse <- 1 # Number of anonymization re-runs per simulation

set.seed(1234)

source('helpers.R')

# Load data ---------------------------------------------------------------

# Census
census<-readRDS("./data/midsave/census.rds")

census <- census %>% 
  mutate(REGION = ID_REGION,
         REGION = replace(ID_REGION, ID_REGION == 'Resto Región Central', 'Gran Área Metropolitana') %>% 
           as.factor() %>% 
           as.integer(),
         ID_PCD = as.character(ID_PCD)
  )

census$strata<-as.factor(paste0(census$ID_ZONA,census$REGION))


# Define attributes -------------------------------------------------------

attr_key <- census %>% 
  select(where(~sum(!is.na(.x)) > 0)) %>% # Remove vars that are all NA
  select(starts_with('V', ignore.case = F),
         starts_with('P', ignore.case = F),
         starts_with('H', ignore.case = F),
         starts_with('acceso'),
         starts_with('Acceso'),
         starts_with('NBI')
  ) %>% 
  names()

attr_sens <- c('NBI')

attr_key_select <- c('ID_PCD')

for (field_dist in c('gaussian', 'parametric')){

  for (encoding in c('categorical', 'label_encoding')){
    
    for (syn_level in c('country', 'strata', 'pcd')){
      
      kl_div <- data.frame()
      
      kl_div <- foreach(run = 1:n_sim,
                        .combine=bind_rows) %dopar% 
        {
          # Load data
          ### True survey
          survey_true <- readRDS(paste0("./data/midsave/syn_surveys/survey_",run,".rds")) %>%  
            drop_na(NBI) %>% 
            mutate(
              # NBI = as.integer(as.factor(NBI)) - 1, #needs to be categorical/numeric
              # NBI = replace(NBI, NBI == 0, NA),
              ID_PCD = as.character(ID_PCD))
          
          ### Geoprivate survey
          survey_geo <- readRDS(paste0("./data/midsave/syn_surveys/geo_survey_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,"_",run,".rds"))
          
          ### Synthetic survey
          survey_syn <- pd$read_pickle(paste0("./data/midsave/syn_surveys/syn_survey_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",n_sim,"_",n_mse,"_",run,".pkl")) %>% 
            mutate(across(where(is.list), as.character),
                   across(everything(), ~na_if(., "NaN")),
                   ID_ZONA = str_sub(strata, end = -2),
                   P00_NUMERO_LINEA = P00_NUMERO_LINEA - runif(length(P00_NUMERO_LINEA), min = 0, max = 0.001) #fix the line number
            ) %>% 
            group_by(hhid) %>% 
            mutate(P00_NUMERO_LINEA = as.integer(ordered(P00_NUMERO_LINEA))) %>% # turn floats into ordered integers
            ungroup %>%
            rename_with(~str_c("syn_", .), .cols = starts_with('NBI')) %>% 
            nbi_calc() %>% 
            rename(NBI = nbi_type)
          
          attr(survey_syn, "pandas.index") <- NULL 

          # Remove attributes that are all NA in one of the datasets
          attr_key_run <- attr_key[attr_key %in% names(survey_true %>%
                                                         select(where(~sum(!is.na(.x)) > 0)))]
          
          attr_key_run <- attr_key_run[attr_key_run %in% names(survey_geo %>%
                                                                 select(where(~sum(!is.na(.x)) > 0)))]
          
          attr_key_run <- attr_key_run[attr_key_run %in% names(survey_syn %>%
                                                                 select(where(~sum(!is.na(.x)) > 0)))]
          
          kl_div_run <- data.frame()
          
          # Run calculation for each attribute individually
          
          for (i in attr_key_run){
            
            kl_div_run <- census %>%
              rename(kl_var = {i}) %>%
              select(ID_PCD, kl_var) %>%
              table() %>%
              as.data.frame() %>% 
              rename(n = Freq) %>% 
              group_by(ID_PCD) %>% 
              drop_na() %>%
              mutate(p = n/sum(n, na.rm = T)) %>% 
              ungroup() %>% 
              distinct() %>% 
              arrange(ID_PCD, kl_var) %>% 
              select(-n, p_census = p) %>% 
              inner_join(
                survey_true %>%
                  rename(kl_var = {i}) %>%
                  select(ID_PCD, kl_var) %>%
                  table() %>%
                  as.data.frame() %>% 
                  rename(n = Freq) %>% 
                  group_by(ID_PCD) %>% 
                  drop_na() %>%
                  mutate(p = n/sum(n, na.rm = T)) %>% 
                  ungroup() %>% 
                  distinct() %>% 
                  arrange(ID_PCD, kl_var) %>% 
                  select(-n, p_true = p),
                by = c('ID_PCD', 'kl_var')
              ) %>% 
              inner_join(
                survey_geo %>%
                  rename(kl_var = {i}) %>%
                  select(ID_PCD, kl_var) %>%
                  table() %>%
                  as.data.frame() %>% 
                  rename(n = Freq) %>% 
                  group_by(ID_PCD) %>% 
                  drop_na() %>%
                  mutate(p = n/sum(n, na.rm = T)) %>% 
                  ungroup() %>% 
                  distinct() %>% 
                  arrange(ID_PCD, kl_var) %>% 
                  select(-n, p_geo = p),
                by = c('ID_PCD', 'kl_var')
              ) %>% 
              drop_na %>% 
              left_join(
                survey_syn %>%
                  rename(kl_var = {i}) %>%
                  select(ID_PCD, kl_var) %>%
                  table() %>%
                  as.data.frame() %>% 
                  rename(n = Freq) %>% 
                  group_by(ID_PCD) %>% 
                  drop_na() %>%
                  mutate(p = n/sum(n, na.rm = T)) %>% 
                  ungroup() %>% 
                  distinct() %>% 
                  arrange(ID_PCD, kl_var) %>% 
                  select(-n, p_syn = p),
                by = c('ID_PCD', 'kl_var')
              ) %>% 
              replace(is.na(.), 0) %>% 
              pivot_longer(!c('ID_PCD', 'kl_var'), names_to = 'type', values_to = 'p') %>%
              group_by(ID_PCD) %>% 
              summarise(true = (1 / (1 + suppressMessages(KL(data.frame('census' = p[type == 'p_census'] , 'syn' = p[type == 'p_true']) %>% 
                                                               as.matrix() %>% 
                                                               t(),
                                                             test.na = TRUE,
                                                             unit = "log2",
                                                             est.prob = NULL)[[1]]))),
                        geo = (1 / (1 + suppressMessages(KL(data.frame('census' = p[type == 'p_census'] , 'syn' = p[type == 'p_geo']) %>% 
                                                              as.matrix() %>% 
                                                              t(),
                                                            test.na = TRUE,
                                                            unit = "log2",
                                                            est.prob = NULL)[[1]]))),
                        syn = (1 / (1 + suppressMessages(KL(data.frame('census' = p[type == 'p_census'] , 'syn' = p[type == 'p_syn']) %>% 
                                                              as.matrix() %>% 
                                                              t(),
                                                            test.na = TRUE,
                                                            unit = "log2",
                                                            est.prob = NULL)[[1]])))
              ) %>% 
              mutate(attr_key = i,
                     run = run) %>% 
              bind_rows(kl_div_run)

          }

          kl_div_run <- kl_div_run %>% 
            left_join(survey_true %>%
                        group_by(ID_PCD) %>%
                        summarise(n = n()) %>%
                        ungroup(), by = 'ID_PCD')
          
          list(kl_div_run, kl_div)
          
        }
      
      rownames(kl_div) <- NULL
      
      stopImplicitCluster()
      
      saveRDS(kl_div, file = paste0("./data/midsave/utility_kl_div_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
      
    }
  }
}