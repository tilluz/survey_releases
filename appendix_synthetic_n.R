#################################################

# In this file, we run the survey augmentation for different sample sizes from a synthetic population.

#################################################

library(survey)
library(pps)
library(tidyverse)
library(emdi)
library(reticulate)
library(MASS)
library(caret)
library(foreach)
library(doParallel)
library(data.table)

# Number of processors to use for parallelization
registerDoParallel(3)

# Some dplyr configs
options(dplyr.summarise.inform = FALSE)
select <- dplyr::select

# Helpers to read pickle files
py_install('pandas')
pd <- import('pandas')

# Set parameters
set.seed(1234)
n_sim <- 100 # Number of simulation rounds
n_mse <- 1 # Number of anonymization re-runs per simulation
model <- 'GC' # Options are: GC
syn_level <- 'strata' # Defines on which level the copula fitting for SDG was performed. Options are: country, strata or pcd
pop_level <-  'population' # Detailing the sampling type for synthetic data. Options are: population or sample
encoding <- 'categorical' # Options are: 'categorical' or 'label_encoding'
field_dist <- 'gaussian' # Options are: 'gaussian', 'parametric'
nbi_type <- 'syn_NBI' # Options are: 'new_NBI' or 'syn_NBI'

source('helpers.R')

# Prepare census ----------------------------------------------------------

census<-readRDS("./data/midsave/census.rds")

census <- census %>% 
  mutate(REGION = ID_REGION,
         REGION = replace(ID_REGION, ID_REGION == 'Resto Región Central', 'Gran Área Metropolitana') %>% 
           as.factor() %>% 
           as.integer()
  )

census$strata<-as.factor(paste0(census$ID_ZONA,census$REGION))

# Prepare satellite features ----------------------------------------------

## Read covariates
esaccilc_dst_pcd <- readRDS("./data/midsave/esaccilc_dst_pcd.rds")
other_dst_pcd <- readRDS("./data/midsave/other_dst_pcd.rds")
bsgmi_sum_pcd <- readRDS("./data/midsave/bsgmi_sum_pcd.rds")
srtm_pcd <- readRDS("./data/midsave/srtm_pcd.rds")

#Use only the districts available in the census 
covariates <- census %>% 
  select(geoid = ID_PCD) %>% 
  mutate(geoid = geoid %>% as.character()) %>% 
  distinct() %>% 
  left_join(esaccilc_dst_pcd, by = "geoid") %>% 
  left_join(other_dst_pcd %>% 
              rename(cri_bsgmi_v0a_100m_2011_mean_sd = cri_bsgmi_v0a_100m_2011_sd), by = "geoid") %>% 
  left_join(bsgmi_sum_pcd %>% 
              rename(cri_bsgmi_v0a_100m_2011_sum_sd = cri_bsgmi_v0a_100m_2011_sd), by = "geoid") %>% 
  left_join(srtm_pcd, by = "geoid") %>% 
  rename(ID_PCD = geoid)

# Remove linear combinations
covariates <- covariates[, -findLinearCombos(covariates)$remove]

# Center and scale the features
covariates <- predict(preProcess(covariates, method = c("center", "scale")), covariates)


# Prepare simulation ------------------------------------------------------


# Specify variable(s) of interest

census_nbi <- census %>%
  group_by(ID_PCD) %>%
  summarise(N = n(),
            pnbi = mean(NBI, na.rm = T)) %>% 
  mutate(ID_PCD = as.character(ID_PCD))


# Run simulation ----------------------------------------------------------

df_pred <- data.frame()

df_pred <- foreach(j = 1:n_sim, .combine=bind_rows) %dopar% {
  
  sample_ssu <- pd$read_pickle(paste0("./data/midsave/syn_surveys/syn_survey_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",n_sim,"_",n_mse,"_",j,".pkl")) %>% 
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
  attr(sample_ssu, "pandas.index") <- NULL
  
  df_run <- data.frame()
  
  for(rate in c(0.001, 0.005, 0.01, 0.05, seq(0.1,1, by = 0.1))){

    # Run simulation ------------------------------------------------------
    
    surv <- sample_ssu %>%
      filter(sub_sample == 1) %>%
      group_by(PSUD) %>%
      slice_sample(prop = rate, replace = FALSE) %>%
      ungroup %>%
      mutate(ID_PCD = as.character(ID_PCD)) %>% 
      drop_na(NBI)
    
    surv <- sample_ssu %>%
      filter(sub_sample == 1 & !(PSUD %in% surv$PSUD)) %>%
      group_by(PSUD) %>%
      slice_sample(n = 1, replace = FALSE) %>%
      ungroup %>%
      mutate(ID_PCD = as.character(ID_PCD)) %>% 
      drop_na(NBI) %>% 
      bind_rows(surv)
    
    # Combine survey with covariates
    
    surv_agg <- surv %>%
      group_by(ID_PCD) %>%
      summarise(n = n(), pnbi = weighted.mean(NBI, weight, na.rm = T))
    
    in_sample <- covariates %>%
      filter(ID_PCD %in% surv_agg$ID_PCD) %>% 
      left_join(surv_agg, by = 'ID_PCD')
    
    out_sample <- covariates %>% 
      filter(!(ID_PCD %in% surv_agg$ID_PCD)) %>% 
      mutate(pnbi = NA)
    
    
    ##############################
    ###    Prediction          ###
    ##############################
    
    # Account for the variance of the direct estimate due to the complex sampling design
    
    mod3_design <- svydesign(id= ~1, strata= ~strata, data=surv, weights = ~weight)
    mod3_var <- svyby(~NBI, ~ID_PCD, design = mod3_design, svymean, vartype = "var")
    
    # Model estimation
    mod3_data <- surv %>% 
      group_by(ID_PCD) %>% 
      summarise(n = n(),
                pnbi = weighted.mean(NBI, weight, na.rm = T)) %>% 
      cbind(sampling_var = mod3_var$var) %>% 
      full_join(covariates, by = 'ID_PCD') %>% 
      drop_na(cri_srtm_slope_100m_mean) %>% 
      as.data.frame()
    
    mod3 <- suppressMessages(fh(fixed = as.formula(paste("pnbi ~ ", paste(names(covariates)[-1], collapse = " + "))),
                                vardir = "sampling_var", combined_data = mod3_data, domains = "ID_PCD", 
                                method = "ml", transformation = "arcsin", backtransformation = "bc", 
                                eff_smpsize = "n", MSE = FALSE, mse_type = "boot", B = c(50,0)))
    
    
    # Save predictions
    df_run <- df_run %>% 
      bind_rows(data.frame(run = j,
                         rate = rate,
                         gamma = mean(summary(mod3)$model$gamma$Gamma, na.rm = T),
                         adj_r2 = summary(mod3)$model$model_select$AdjR2
                  )
      )
    
    print(paste0('Rate ',rate,' of survey ', j, ' of ', n_sim, ' processed!'))
    
  }
  
  print(paste0('Survey ', j, ' of ', n_sim, ' processed!'))
  
  return(df_run)
  
}

stopImplicitCluster()

saveRDS(df_pred, paste0("./data/midsave/appendix_utility_prediction_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

