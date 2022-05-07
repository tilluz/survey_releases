#################################################

# In this file, we run the survey augmentation using satellite-derived features. On the fly, we generate the 'geomasked' surveys.

# Inputs:
# - 'Original' surveys from sampling.R
# - 'Synthetic' survey from synthetic_survey.ipynb
# - Census data from census_preprocessing.R
# - Map data from map_preprocessing.R
# - Satellite-based covariates from wp_covariates.R

# Output: 
# - 'Geomasked' surveys
# - Survey augmentation predictions and its performances

#################################################

library(survey)
library(pps)
library(tidyverse)
library(rgdal)
library(rgeos)
library(sp)
library(emdi)
library(reticulate)
library(MASS)
library(caret)
library(foreach)
library(doParallel)
library(data.table)

# Number of processors to use for parallelization
registerDoParallel(7)

# Some dplyr configs
options(dplyr.summarise.inform = FALSE)
select <- dplyr::select

# Helpers to read pickle files
py_install('pandas')
pd <- import('pandas')

source('helpers.R')

# Set parameters
set.seed(1234)
n_sim <- 100 # Number of simulation rounds
n_mse <- 1 # Number of anonymization re-runs per simulation
n_k <- 100 # Number of re-starts for geoprivacy displacement
model <- 'GC' # Options are: GC
syn_level <- 'country' # Defines on which level the copula fitting for SDG was performed. Options are: country, strata or pcd
pop_level <-  'sample' # Detailing the sampling type for synthetic data. Options are: population or sample
encoding <- 'categorical' # Options are: 'categorical' or 'label_encoding'
field_dist <- 'gaussian' # Options are: 'gaussian' or 'parametric'
nbi_type <- 'syn_NBI' # Options are: 'new_NBI' or 'syn_NBI'


# Evaluation measures -----------------------------------------------------

# Helper: Sum of squared residuals
RSS <- function(Pred, Real) {
  ss <- sum((Real - Pred)^2)
  return(ss)
}

# Mean Squared Error
MSE <- function(Pred, Real) {
  N <- length(Real)
  ss <- (1/N) * RSS(Pred, Real)
  return(ss)
}

# Relative Error
RelativeError <- function(Pred, Real) {
  ss <- sum(abs(Real - Pred))/sum(abs(Real))
  return(ss)
}


# Load relevant data ------------------------------------------------------

cri_map <- readOGR(dsn = './data/midsave/', layer="map_pcd_harmonised", verbose = F)
census<-readRDS("./data/midsave/census.rds")


# Prepare census ----------------------------------------------------------

census <- census %>% 
  mutate(REGION = ID_REGION,
         REGION = replace(ID_REGION, ID_REGION == 'Resto Región Central', 'Gran Área Metropolitana') %>% 
           as.factor() %>% 
           as.integer()
  )

census$strata<-as.factor(paste0(census$ID_ZONA,census$REGION))


# Prepare mapping ---------------------------------------------------------

'
For the displacement procedure to work correctly, we need locations for the enumeration areas in the census. As they are not available,
we sample them from the districts.
'

# Transform to UTM
to_utm <- "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs"
cri_map_utm <- spTransform(cri_map, to_utm)

# Sample random EA locations
ea <- census %>% 
  mutate(ID_EA = paste0(ID_PCD, ID_AE_CONSECU)) %>% 
  select(ID_PCD, ID_EA, ID_ZONA) %>% 
  distinct(ID_EA, .keep_all = T)

ea_locations <- data.frame()

for(k in unique(ea$ID_PCD)){
  
  ea_locations <- ea %>% 
    filter(ID_PCD == k) %>% 
    bind_cols(
      spsample(cri_map_utm[cri_map_utm@data$CODDIST == substr(k, 1, 5),], n=sum(ea$ID_PCD == k), "random", iter=10)@coords %>% 
        as.data.frame(., col.names = c('x', 'y'))
    ) %>% 
    bind_rows(ea_locations)
}

ea_locations <- SpatialPointsDataFrame(
  coords = ea_locations[,c('x', 'y')],
  data = ea_locations[,c('ID_PCD', 'ID_EA', 'ID_ZONA')],
  proj4string = CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs"))


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

perf <- matrix(0, n_sim, 4)

colnames(perf) <- c("run", 
                   "type", 
                   "rb", 
                   "mse")

census_nbi <- census %>%
  group_by(ID_PCD) %>%
  summarise(N = n(),
            pnbi = mean(NBI, na.rm = T)) %>% 
  mutate(ID_PCD = as.character(ID_PCD))


# Run simulation ----------------------------------------------------------

for(nbi_type in c('new_NBI', 'syn_NBI')){
  
  df_pred <- data.frame()
  
  df_pred <- foreach(j = 1:n_sim, .combine=bind_rows, .errorhandling = 'pass') %dopar% {
    
    df_run <- data.frame()
    
    for(type in c('true', 'geo', 'syn')){
      
      # Read data
      
      if(type == 'true'){
        sample_ssu <- readRDS(paste0("./data/midsave/syn_surveys/survey_",j,".rds"))
        
        surv <- sample_ssu %>%  
          drop_na(NBI) %>% 
          mutate(ID_PCD = as.character(ID_PCD))
        
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
        
        # Estimate the design effect
        true_design <- svydesign(id= ~1, strata= ~strata, data=surv, weights = ~weight)
        true_var <- svyby(~NBI, ~ID_PCD, design = true_design, svymean, vartype = "var")
        
        # Model estimation
        true_data <- surv %>% 
          group_by(ID_PCD) %>% 
          summarise(n = n(), 
                    pnbi = weighted.mean(NBI, weight, na.rm = T)) %>% 
          cbind(sampling_var = true_var$var) %>% 
          full_join(covariates, by = 'ID_PCD') %>% 
          drop_na(cri_srtm_slope_100m_mean) %>% 
          as.data.frame()
        
        true_pred <- suppressMessages(fh(fixed = as.formula(paste("pnbi ~ ", paste(names(covariates)[-1], collapse = " + "))),
                                         vardir = "sampling_var", combined_data = true_data, domains = "ID_PCD", 
                                         method = "ml", transformation = "arcsin", backtransformation = "bc", 
                                         eff_smpsize = "n", MSE = FALSE, mse_type = "boot", B = c(50,0)))
        
        true_fh <- true_pred$ind$FH

        # Save predictions
        df_run <- df_run %>% 
          bind_rows(
            true_data %>% 
              select(ID_PCD, n) %>% 
              left_join(census_nbi, by = 'ID_PCD') %>% 
              mutate(run = j,
                     type = type,
                     pred = true_fh,
                     gamma = summary(true_pred)$model$gamma$Gamma,
                     in_sample = ifelse(ID_PCD %in% unique(in_sample$ID_PCD), 1, 0),
                     rb = RelativeError(Pred = pred, Real = pnbi),
                     mse = MSE(pred, pnbi),
                     adj_r2 = summary(true_pred)$model$model_select$AdjR2)
          )
        
      }
      
      if(type == 'geo'){
        sample_ssu <- readRDS(paste0("./data/midsave/syn_surveys/survey_",j,".rds"))
      }  
      
      if(type == 'syn'){
        sample_ssu <- pd$read_pickle(paste0("./data/midsave/syn_surveys/syn_survey_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",n_sim,"_",n_mse,"_",j,".pkl")) %>% 
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
        attr(sample_ssu, "pandas.index") <- NULL
      }
      
      
      # Run simulation ------------------------------------------------------
      
      if(type == 'geo' | type == 'syn'){
        
        
        sum_diff <- matrix(0,length(true_fh),n_mse)
        sum_stats <- matrix(0,length(true_fh),n_mse)
        sum_gammas <- matrix(0,length(true_fh),n_mse)
        sum_r2 <- matrix(0,n_mse,1)
        
        
        for(i in 1:n_mse){
          
          set.seed(1234*i)
          
          if(type == 'geo'){      
            
            k = 1
            
            while (k <= n_k){
              
              # Determine 1% of rural areas displaced by up to 10km
              rural <- ea_locations@data %>% 
                select(ID_EA, ID_ZONA) %>% 
                filter(ID_ZONA == 'Rural' & ID_EA %in% unique(sample_ssu$ID_EA)) %>% 
                slice_sample(prop = 0.01) %>% 
                .[,'ID_EA']
              
              # Apply displacement
              ea_locations_k <- ea_locations[ea_locations$ID_EA %in% unique(sample_ssu$ID_EA),]
              
              ea_locations_k@data <- ea_locations_k@data %>% 
                mutate(x_utm = coordinates(ea_locations_k)[,'x'],
                       y_utm = coordinates(ea_locations_k)[,'y'],
                       angle = runif(dim(.)[1], 0, 360)*pi/180, # random displacement angle for each area
                       dist = case_when(ID_ZONA == 'Rural' ~ runif(dim(.)[1], 0, 5000), # random rural displacement
                                        TRUE ~ runif(dim(.)[1], 0, 2000)), # random urban displacement
                       dist = replace(dist, ID_EA %in% rural, runif(length(rural), 0, 10000)), # random 1% rural displacement
                       x_utm_disp = x_utm + dist * cos(angle),
                       y_utm_disp = y_utm + dist * sin(angle),
                       dist = sqrt((x_utm -x_utm_disp)^2 + (y_utm -y_utm_disp)^2),
                       masked_pcd = NA)
              
              ea_locations_k_masked <- SpatialPointsDataFrame(
                coords = ea_locations_k@data[,c('x_utm_disp', 'y_utm_disp')],
                data = ea_locations_k@data,
                proj4string = CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs"))
              
              # Re-sample those displaced points out of cantonal (and thus also national) boundaries

              while_mask <- 0
              
              while(sum(is.na(ea_locations_k_masked@data$masked_pcd)) > 0){
                ea_locations_k_masked@data <- over(ea_locations_k_masked, cri_map_utm['CODDIST']) %>%
                  rename(masked_pcd = CODDIST) %>% 
                  mutate(ID_EA = ea_locations_k_masked@data$ID_EA) %>% 
                  right_join(ea_locations_k_masked@data %>% 
                               select(-masked_pcd), by = 'ID_EA') %>% 
                  mutate(masked_pcd = replace(masked_pcd, substr(masked_pcd, 1, 3) != substr(ID_PCD, 1, 3), NA),
                         angle = replace(angle, is.na(masked_pcd), runif(sum(is.na(masked_pcd)), 0, 360)*pi/180),
                         x_utm_disp = x_utm + dist * cos(angle),
                         y_utm_disp = y_utm + dist * sin(angle),
                         dist = sqrt((x_utm -x_utm_disp)^2 + (y_utm -y_utm_disp)^2))
                
                ea_locations_k_masked <- SpatialPointsDataFrame(
                  coords = ea_locations_k_masked@data[,c('x_utm_disp', 'y_utm_disp')],
                  data = ea_locations_k_masked@data,
                  proj4string = CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs"))
                
                while_mask = while_mask + 1
                if(while_mask > 100){
                  break
                }
              }
              
              k = k + 1
              
              if(k > n_k){
                warning('Number of restarts for displacement exceed threshold!')
              }
              
              # If displacement task cannot be finished in 100 iterations, start all over again
              if(while_mask > 100){
                next
              } else{
                print(paste0('Relocation successful: ', 
                             round(sum(ea_locations_k_masked@data$masked_pcd != ea_locations_k_masked@data$ID_PCD)/
                                     n_distinct(ea_locations_k_masked@data$ID_EA)*100,2), 
                             '% of the in-sample enumeration areas have changed the district.'))
                break
              }
            }
            
            # Reassign masked centroids to PCD via point-to-polygon
            surv <- sample_ssu %>% 
              left_join(ea_locations_k_masked@data %>% 
                          select(ID_EA, masked_pcd) %>% 
                          mutate(across(masked_pcd, as.character),
                                 across(masked_pcd, as.integer)), by = 'ID_EA') %>% 
              select(-ID_PCD) %>% 
              rename(ID_PCD = masked_pcd) %>%  
              drop_na(NBI) %>% 
              mutate(ID_PCD = as.character(ID_PCD),
                     PSUD = paste0(ID_PCD, strata))
            
            if(i == 1){
              saveRDS(surv, paste0("./data/midsave/syn_surveys/geo_survey_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,"_",j,".rds"))
              print(paste0('geo_survey_',j,' successfully saved!'))
            }
          }
          
          
          # Select variable of interest and remove NAs
          
          if(type == 'syn'){
            surv <- sample_ssu %>%
              filter(sub_sample == i) %>%
              mutate(ID_PCD = as.character(ID_PCD)) %>% 
              drop_na(NBI)
          }
          
          
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
          
          pred3 <- mod3$ind$FH
          
          # Save predictions for MSE
          sum_stats[,i] <- pred3 %>% t() %>% as.vector()
          
          diff_i <- pred3 - true_fh
          sum_diff[,i] <- diff_i %>% t() %>% as.vector()
          
          sum_gammas[,i] <- summary(mod3)$model$gamma$Gamma %>% t() %>% as.vector()
          
          sum_r2[i,] <- summary(mod3)$model$model_select$AdjR2
        }
        
        # Total statistics
        sum_quant <- sum_stats %>% apply(1,quantile,probs=c(0.025, 0.975)) %>% t()
        sum_stat <- sum_stats %>% apply(1,function(x) summary(x)) %>% t()
        sum_gamma <- sum_gammas %>% apply(1,function(x) mean(x, na.rm = T))

        # Save predictions
        df_run <- df_run %>% 
          bind_rows(mod3_data %>% 
                      select(ID_PCD, n) %>% 
                      left_join(census_nbi, by = 'ID_PCD') %>% 
                      mutate(run = j,
                             type = type,
                             pred = sum_stat[,'Mean'],
                             gamma = sum_gamma,
                             in_sample = ifelse(ID_PCD %in% unique(in_sample$ID_PCD), 1, 0),
                             rb = RelativeError(pred, pnbi),
                             mse = sqrt(MSE(pred, pnbi)),
                             adj_r2 = mean(sum_r2, na.rm = T)
                      )
          )
        
      }
    }
    
    print(paste0('Survey ', j, ' of ', n_sim, ' processed!'))
    
    return(df_run)
    
  }
  
  stopImplicitCluster()
  
  df_stats <- df_pred %>% 
    select(run, type, rb, mse, adj_r2, in_sample) %>% 
    distinct()
  
  df_pred <- df_pred %>% 
    select(-rb, -mse, -adj_r2)
  
  saveRDS(df_stats, paste0("./data/midsave/utility_performance_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
  saveRDS(df_pred, paste0("./data/midsave/utility_prediction_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

}

