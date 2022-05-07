#################################################

# In this file, we visualize the results for the appendix.

#################################################

library(tidyverse)
library(rgdal)
library(ggplot2)
library(ggridges)
library(viridis)
library(ggthemes)
library(broom)
library(xtable)
library(cowplot)
library(reticulate)
library(philentropy, quietly = TRUE, warn.conflicts = FALSE)

source('helpers.R')

theme_set(theme_minimal())

# Some dplyr configs
options(dplyr.summarise.inform = FALSE)
select <- dplyr::select

# Helpers to read pickle files
py_install('pandas')
pd <- import('pandas')

# Settings
model <- 'GC' # Options are: GC
var_order <- 'identical' # Options: mixed or identical
syn_level <- 'strata' # Defines on which level the copula fitting for SDG was performed. Options are: country, strata or pcd
pop_level <-  'sample' # Detailing the sampling type for synthetic data. Options are: population or sample
encoding <- 'categorical' # Options are: 'categorical' or 'label_encoding'
field_dist <- 'gaussian' # Options are: 'gaussian' or 'parametric'
nbi_type <- 'syn_NBI' # Options are: 'new_NBI' or 'syn_NBI'
n_sim <- 100 # Number of simulation rounds
n_mse <- 1 # Number of anonymization re-runs per simulation

# Select run
j <- 6


# Load census -------------------------------------------------------------

census <- readRDS("./data/midsave/census.rds") %>% 
  mutate(REGION = ID_REGION,
         REGION = replace(ID_REGION, ID_REGION == 'Resto Región Central', 'Gran Área Metropolitana') %>% 
           as.factor() %>% 
           as.integer(),
         strata = as.factor(paste0(ID_ZONA, REGION))
  )


# Load survey data --------------------------------------------------------

# True survey
survey_true <- readRDS(paste0("./data/midsave/syn_surveys/survey_",j,".rds")) %>%  
  drop_na(NBI) %>% 
  mutate(ID_PCD = as.character(ID_PCD))

# Geoprivate survey
survey_geo <- readRDS(paste0("./data/midsave/syn_surveys/geo_survey_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,"_",j,".rds"))

# Synthetic survey
survey_syn <- pd$read_pickle(paste0("./data/midsave/syn_surveys/syn_survey_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",n_sim,"_",n_mse,"_",j,".pkl")) %>% 
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


# Load remaining required data --------------------------------------------

df_pred_country <- readRDS(paste0("./data/midsave/utility_prediction_",model,"_",pop_level,"_country_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
df_pred_strata <- readRDS(paste0("./data/midsave/utility_prediction_",model,"_",pop_level,"_strata_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
df_pred_pcd <- readRDS(paste0("./data/midsave/utility_prediction_",model,"_",pop_level,"_pcd_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

df_perf_country <- readRDS(paste0("./data/midsave/utility_performance_",model,"_",pop_level,"_country_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
df_perf_strata <- readRDS(paste0("./data/midsave/utility_performance_",model,"_",pop_level,"_strata_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
df_perf_pcd <- readRDS(paste0("./data/midsave/utility_performance_",model,"_",pop_level,"_pcd_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

kl_div_country <- readRDS(paste0("./data/midsave/utility_kl_div_",model,"_",pop_level,"_country_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
kl_div_strata <- readRDS(paste0("./data/midsave/utility_kl_div_",model,"_",pop_level,"_strata_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
kl_div_pcd <- readRDS(paste0("./data/midsave/utility_kl_div_",model,"_",pop_level,"_pcd_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

unique_df_country <- readRDS(paste0("./data/midsave/risk_uniqueness_",var_order,"_",model,"_",pop_level,"_country_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
unique_df_strata <- readRDS(paste0("./data/midsave/risk_uniqueness_",var_order,"_",model,"_",pop_level,"_strata_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
unique_df_pcd <- readRDS(paste0("./data/midsave/risk_uniqueness_",var_order,"_",model,"_",pop_level,"_pcd_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

privacy_df_country <- readRDS(paste0("./data/midsave/risk_privacy_",model,"_",pop_level,"_country_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
privacy_df_strata <- readRDS(paste0("./data/midsave/risk_privacy_",model,"_",pop_level,"_strata_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
privacy_df_pcd <- readRDS(paste0("./data/midsave/risk_privacy_",model,"_",pop_level,"_pcd_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

df_pred_synthetic_n <- readRDS(paste0("./data/midsave/appendix_utility_prediction_",model,"_population_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

df_perf_strata_new_nbi <- readRDS(paste0("./data/midsave/utility_performance_",model,"_",pop_level,"_strata_",field_dist,"_",encoding,"_new_NBI_",n_sim,"_",n_mse,".rds"))
df_perf_strata_syn_nbi <- readRDS(paste0("./data/midsave/utility_performance_",model,"_",pop_level,"_strata_",field_dist,"_",encoding,"_syn_NBI_",n_sim,"_",n_mse,".rds"))

df_pred_strata_new_nbi <- readRDS(paste0("./data/midsave/utility_prediction_",model,"_",pop_level,"_strata_",field_dist,"_",encoding,"_new_NBI_",n_sim,"_",n_mse,".rds"))
df_pred_strata_syn_nbi <- readRDS(paste0("./data/midsave/utility_prediction_",model,"_",pop_level,"_strata_",field_dist,"_",encoding,"_syn_NBI_",n_sim,"_",n_mse,".rds"))

kl_div_gaussian_categorical <- readRDS(paste0("./data/midsave/utility_kl_div_",model,"_",pop_level,"_strata_gaussian_categorical_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
kl_div_gaussian_label <- readRDS(paste0("./data/midsave/utility_kl_div_",model,"_",pop_level,"_strata_gaussian_label_encoding_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
kl_div_parametric_categorical <- readRDS(paste0("./data/midsave/utility_kl_div_",model,"_",pop_level,"_strata_parametric_categorical_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
kl_div_parametric_label <- readRDS(paste0("./data/midsave/utility_kl_div_",model,"_",pop_level,"_strata_parametric_label_encoding_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

# Outcome predictions -----------------------------------------------------

'
Survey augmentation performance for different copula fitting levels
'

df_perf <- df_perf_country %>% 
  pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
  mutate(type = replace(type, type == 'syn', 'country')) %>% 
  bind_rows(
    df_perf_strata %>% 
      pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
      filter(type == 'syn') %>% 
      mutate(type = replace(type, type == 'syn', 'strata'))
  ) %>% 
  bind_rows(
    df_perf_pcd %>% 
      pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
      filter(type == 'syn') %>% 
      mutate(type = replace(type, type == 'syn', 'pcd'))
  ) %>% 
  mutate(metric = replace(metric, metric == 'adj_r2', 'Adjusted R2'),
         metric = replace(metric, metric == 'mse', 'RMSE'),
         metric = replace(metric, metric == 'rb', 'Relative Bias'))


df_perf <- df_pred_country %>% 
  group_by(run, type, in_sample) %>% 
  summarise(rb = sum(abs(pnbi - pred), na.rm = T)/sum(abs(pnbi), na.rm = T), 
            rmse = sqrt((1/length(pnbi)) * sum((pnbi - pred)^2, na.rm = T))) %>%
  ungroup() %>% 
  pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
  mutate(type = replace(type, type == 'syn', 'country')) %>% 
  bind_rows(
    df_pred_strata %>% 
      group_by(run, type, in_sample) %>% 
      summarise(rb = sum(abs(pnbi - pred), na.rm = T)/sum(abs(pnbi), na.rm = T), 
                rmse = sqrt((1/length(pnbi)) * sum((pnbi - pred)^2, na.rm = T))) %>%
      ungroup() %>% 
      pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
      filter(type == 'syn') %>% 
      mutate(type = replace(type, type == 'syn', 'strata'))
  ) %>% 
  bind_rows(
    df_pred_pcd %>% 
      group_by(run, type, in_sample) %>% 
      summarise(rb = sum(abs(pnbi - pred), na.rm = T)/sum(abs(pnbi), na.rm = T), 
                rmse = sqrt((1/length(pnbi)) * sum((pnbi - pred)^2, na.rm = T))) %>%
      ungroup() %>% 
      pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
      filter(type == 'syn') %>% 
      mutate(type = replace(type, type == 'syn', 'pcd'))
  ) %>% 
  bind_rows(
    df_perf_country %>% 
      pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
      filter(metric == 'adj_r2') %>% 
      mutate(type = replace(type, type == 'syn', 'country'))
  ) %>% 
  bind_rows(
    df_perf_strata %>% 
      pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
      filter(type == 'syn' & metric == 'adj_r2') %>% 
      mutate(type = replace(type, type == 'syn', 'strata'))
  ) %>% 
  bind_rows(
    df_perf_pcd %>% 
      pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
      filter(type == 'syn' & metric == 'adj_r2') %>% 
      mutate(type = replace(type, type == 'syn', 'pcd'))
  ) %>% 
  mutate(metric = replace(metric, metric == 'adj_r2', 'Adjusted R2'),
         metric = replace(metric, metric == 'rmse', 'RMSE'),
         metric = replace(metric, metric == 'rb', 'Relative Bias'))

# Adjusted R2
pdf(file = paste0("./viz/appendix_utility_performance_Adjusted_R2_",model,"_",pop_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 3, height = 3)

df_perf %>% 
  filter(metric == 'Adjusted R2') %>% 
  mutate(type = fct_relevel(type, 
                            "country", "strata", "pcd", 
                            "geo", "true")) %>% 
  ggplot(aes(x = metric, y = value, fill = type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2) + 
  scale_fill_viridis_d(name = "Survey type",
                       alpha = .7,
                       labels = c("Country", "Strata", "Zip Code", "Geomasked", "True")
                       ) +
  labs(x = '',
       y = '',
       fill = '',
       color = '') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
  )

dev.off()

# Relative Bias
pdf(file = paste0("./viz/appendix_utility_performance_Relative_Bias_",model,"_",pop_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 3, height = 3)

df_perf %>% 
  filter(metric == 'Relative Bias') %>% 
  mutate(type = fct_relevel(type, 
                            "country", "strata", "pcd", 
                            "geo", "true")) %>% 
  ggplot(aes(x = metric, y = value, fill = type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2) + 
  scale_fill_viridis_d(name = "Survey type",
                       alpha = .7,
                       labels = c("Country", "Strata", "Zip Code", "Geomasked", "True")
  ) +
  labs(x = '',
       y = '',
       fill = '',
       color = '') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
  )

dev.off()

# RMSE
pdf(file = paste0("./viz/appendix_utility_performance_RMSE_",model,"_",pop_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 3, height = 3)

df_perf %>% 
  filter(metric == 'RMSE') %>% 
  mutate(type = fct_relevel(type, 
                            "country", "strata", "pcd", 
                            "geo", "true")) %>% 
  ggplot(aes(x = metric, y = value, fill = type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2) + 
  scale_fill_viridis_d(name = "Survey type",
                       alpha = .7,
                       labels = c("Country", "Strata", "Zip Code", "Geomasked", "True")
  ) +
  labs(x = '',
       y = '',
       fill = '',
       color = '') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
  )

dev.off()

# Legend
pdf(file = paste0("./viz/appendix_utility_performance_legend_",model,"_",pop_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 10, height = 10)

df_perf %>% 
  filter(metric == 'MSE') %>% 
  mutate(type = fct_relevel(type, 
                            "country", "strata", "pcd", 
                            "geo", "true")) %>% 
  ggplot(aes(x = metric, y = value, fill = type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2) + 
  scale_fill_viridis_d(name = "Survey type",
                       alpha = .7,
                       labels = c("Country", "Strata", "Zip Code", "Geomasked", "True")
  ) +
  labs(x = '',
       y = '',
       fill = '',
       color = '') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        # legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
  )

dev.off()

# Predicting sensitive attribute ------------------------------------------

'
Predicting sensitive attribute for different copula fitting levels
'

privacy_p <- privacy_df_country %>% 
  select(-acc_in, -oos_error) %>% 
  pivot_longer(!c('run', 'type', 'strata', 'n_clusters'), names_to = "acc_type", values_to = "acc_value") %>% 
  filter(!(acc_type == 'acc_guess' & type == 'geo')) %>% 
  mutate(type = replace(type, acc_type == 'acc_guess', 'Guess'),
         type = replace(type, type == 'geo', 'Geomasked'),
         type = replace(type, type == 'syn', 'Country')) %>% 
  bind_rows(
    privacy_df_strata %>%
      select(-acc_in, -oos_error) %>% 
      pivot_longer(!c('run', 'type', 'strata', 'n_clusters'), names_to = "acc_type", values_to = "acc_value") %>% 
      filter(!(acc_type == 'acc_guess' & type == 'geo')) %>% 
      filter(type == 'syn') %>% 
      mutate(type = replace(type, type == 'syn', 'Strata'))
  ) %>% 
  bind_rows(
    privacy_df_pcd %>%
      select(-acc_in, -oos_error) %>% 
      pivot_longer(!c('run', 'type', 'strata', 'n_clusters'), names_to = "acc_type", values_to = "acc_value") %>% 
      filter(!(acc_type == 'acc_guess' & type == 'geo')) %>% 
      filter(type == 'syn') %>% 
      mutate(type = replace(type, type == 'syn', 'Zip Code'))
  ) %>% 
  select(-acc_type) %>% 
  group_by(type, strata) %>% 
  summarise(acc_median = quantile(acc_value, probs = c(.5)),
            lower = quantile(acc_value, probs = c(.1)),
            upper = quantile(acc_value, probs = c(.90)),
            n_clusters = mean(n_clusters, na.rm = T)) %>% 
  ungroup %>% 
  mutate(type = fct_relevel(type, 
                            "Country", "Strata", "Zip Code", 
                            "Geomasked", "Guess")) %>% 
  filter(type == 'Country' | type == 'Strata' | type == 'Zip Code')


pdf(file = paste0("./viz/appendix_risk_privacy_",model,"_",pop_level,"_",field_dist,"_",encoding,"_",nbi_type,"",n_sim,"_",n_mse,".pdf"), width = 5, height = 3.33)

privacy_p %>% 
  ggplot(aes(x = reorder(strata, n_clusters), y = acc_median, group = type, colour = type, fill = type)) +
  geom_point() + 
  geom_line() + 
  scale_colour_viridis_d(option = "D", 
                         name = "Survey type", 
                         end = 0.6
                         ) +
  scale_fill_viridis_d(option = "D",
                       name = "Survey type", 
                       end = 0.6
                       ) +
  labs(x = "Strata, ordered by number of clusters",
       y = 'Median accuracy',
       fill = 'Survey type',
       color = 'Survey type') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
  )

dev.off()


# Uniqueness --------------------------------------------------------------

'
Calculating uniqueness for different copula fitting levels
'

# pdf(file = paste0("./viz/appendix_risk_uniqueness_",var_order,"_",model,"_",pop_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 5, height = 3.33)
png(file = paste0("./viz/appendix_risk_uniqueness_",var_order,"_",model,"_",pop_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".png"), width = 500, height = 333)

unique_df_country %>% 
  filter(type == 'true' | type == 'geo'| type == 'syn') %>% 
  mutate(type = replace(type, type == 'true', 'True'),
         type = replace(type, type == 'geo', 'Geomasked'),
         type = replace(type, type == 'syn', 'Country')) %>%
  bind_rows(
    unique_df_strata %>%
      filter(type == 'syn') %>% 
      mutate(type = replace(type, type == 'syn', 'Strata'))
  ) %>% 
  bind_rows(
    unique_df_pcd %>%
      filter(type == 'syn') %>% 
      mutate(type = replace(type, type == 'syn', 'Zip Code'))
  ) %>% 
  mutate(type = fct_relevel(type, 
                            "Country", "Strata", "Zip Code", 
                            "Geomasked", "True")) %>% 
  filter(n_attr <= 50) %>% 
  ggplot(aes(x = n_attr, y = p_unique, group = interaction(run, type), color = type)) +
  geom_line(alpha = 0.5, size = 0.1) +
  stat_summary(aes(group = type), fun = mean, geom = "line", alpha = 1, size = 1) +
  scale_colour_viridis_d(option = "D") +
  labs(
    # title = 'Population uniqueness across survey types',
    #    subtitle = 'Share of population-unique respondents',
    x = "Number of Attributes",
    y = expression(Xi['x']),
    fill = '',
    color = 'Survey types') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank())

dev.off()


# Synthetic sample size effects -------------------------------------------

'
Visualizing the connection of gamma and adj. R² across different sample sizes from a synthetic population
'

png(file = paste0("./viz/appendix_gamma_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".png"), width = 1000, height = 333)

df_pred_synthetic_n %>% 
  pivot_longer(!c('run', 'rate'), names_to = "metric", values_to = "value") %>% 
  mutate(metric = replace(metric, metric == 'adj_r2', 'Adjusted R²'),
         metric = replace(metric, metric == 'gamma', 'Gamma')) %>% 
  ggplot(aes(x = rate, y = value, group = interaction(run, metric), color = metric)) +
  geom_line(alpha = 0.5, size = 0.1) + 
  stat_summary(aes(group = metric), fun = mean, geom = "line", alpha = 1, size = 1) +
  scale_colour_viridis_d(option = "D", end = 0.5) +
  labs(
    # title = 'Population uniqueness across survey types',
    #    subtitle = 'Share of population-unique respondents',
    x = "Sampling rate",
    y = "",
    fill = '',
    color = 'Metrics') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        # legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank())

dev.off()


# Different encoding schemes and marginals --------------------------------

# Gaussian categorical
kl_div_gc <- kl_div_gaussian_categorical %>% 
  pivot_longer(!c('ID_PCD', 'attr_key', 'run', 'n'), names_to = "type", values_to = "kl_value") %>% 
  drop_na(kl_value) %>% 
  group_by(ID_PCD, attr_key, type) %>% 
  summarise(kl_value = mean(kl_value, na.rm = T),
            n = mean(n, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(census %>% 
              summarise(across(everything(), ~n_distinct(.))) %>% 
              mutate(blab = 1) %>% 
              pivot_longer(!blab, names_to = "attr_key", values_to = "n_levels") %>% 
              select(-blab), by = 'attr_key') %>% 
  filter(type == 'syn') %>% 
  mutate(field_dist = 'Gaussian', encoding = 'Frequency encoding')

# Gaussian label
kl_div_gl <- kl_div_gaussian_label %>% 
  pivot_longer(!c('ID_PCD', 'attr_key', 'run', 'n'), names_to = "type", values_to = "kl_value") %>% 
  drop_na(kl_value) %>% 
  group_by(ID_PCD, attr_key, type) %>% 
  summarise(kl_value = mean(kl_value, na.rm = T),
            n = mean(n, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(census %>% 
              summarise(across(everything(), ~n_distinct(.))) %>% 
              mutate(blab = 1) %>% 
              pivot_longer(!blab, names_to = "attr_key", values_to = "n_levels") %>% 
              select(-blab), by = 'attr_key') %>% 
  filter(type == 'syn') %>% 
  mutate(field_dist = 'Gaussian', encoding = 'Ordinal encoding')

# Parametric categorical
kl_div_pc <- kl_div_parametric_categorical %>% 
  pivot_longer(!c('ID_PCD', 'attr_key', 'run', 'n'), names_to = "type", values_to = "kl_value") %>% 
  drop_na(kl_value) %>% 
  group_by(ID_PCD, attr_key, type) %>% 
  summarise(kl_value = mean(kl_value, na.rm = T),
            n = mean(n, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(census %>% 
              summarise(across(everything(), ~n_distinct(.))) %>% 
              mutate(blab = 1) %>% 
              pivot_longer(!blab, names_to = "attr_key", values_to = "n_levels") %>% 
              select(-blab), by = 'attr_key') %>% 
  filter(type == 'syn') %>% 
  mutate(field_dist = 'Parametric', encoding = 'Frequency encoding')

# Parametric label
kl_div_pl <- kl_div_parametric_label %>% 
  pivot_longer(!c('ID_PCD', 'attr_key', 'run', 'n'), names_to = "type", values_to = "kl_value") %>% 
  drop_na(kl_value) %>% 
  group_by(ID_PCD, attr_key, type) %>% 
  summarise(kl_value = mean(kl_value, na.rm = T),
            n = mean(n, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(census %>% 
              summarise(across(everything(), ~n_distinct(.))) %>% 
              mutate(blab = 1) %>% 
              pivot_longer(!blab, names_to = "attr_key", values_to = "n_levels") %>% 
              select(-blab), by = 'attr_key') %>% 
  filter(type == 'syn') %>% 
  mutate(field_dist = 'Parametric', encoding = 'Ordinal encoding')


kl_div_all <-  kl_div_gc %>% 
  bind_rows(kl_div_gl) %>% 
  bind_rows(kl_div_pc) %>% 
  bind_rows(kl_div_pl) 


pdf(file = paste0("./viz/appendix_kl_div_grid_",model,"_",pop_level,"_",syn_level,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 10, height = 10)

kl_div_all %>% 
  ggplot(aes(x = reorder(ID_PCD, n), y = reorder(attr_key, n_levels), fill = kl_value)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 1)) + 
  labs(x = "Zip Codes, ordered by average sample size",
       y = 'Attributes, ordered by number of classes',
       fill = expression(Z["KL"])) +
  theme_tufte() +
  theme(legend.position="none",
    text =  element_text(size=20),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) + 
  facet_grid(vars(field_dist), vars(encoding))

dev.off()


# Different NBI calculations ----------------------------------------------

'
Calculate goodness-of-fit outcomes for the indicators and dimensions underlying the NBI
'

# Correlation matrix

nbi_cor <- data.frame()

for(j in 1:n_sim){
  survey_syn <- pd$read_pickle(paste0("./data/midsave/syn_surveys/syn_survey_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",n_sim,"_",n_mse,"_",j,".pkl")) %>% 
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
  
  nbi_cor <- data.frame(
    run = j,
    metric = 'cor',
    NBI_1.X = mean(
      c(cor(survey_syn$NBI_111, survey_syn$syn_NBI_111, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_112, survey_syn$syn_NBI_112, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_113, survey_syn$syn_NBI_113, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_121, survey_syn$syn_NBI_121, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_131, survey_syn$syn_NBI_131, use = 'pairwise.complete.obs'))
      ,
      na.rm = T
    ),
    NBI_1 = cor(survey_syn$NBI_1, survey_syn$syn_NBI_1, use = 'pairwise.complete.obs'),
    NBI_2.X = mean(
      c(cor(survey_syn$NBI_211, survey_syn$syn_NBI_211, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_212, survey_syn$syn_NBI_212, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_213, survey_syn$syn_NBI_213, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_214, survey_syn$syn_NBI_214, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_215, survey_syn$syn_NBI_215, use = 'pairwise.complete.obs'))
      ,
      na.rm = T
    ),
    NBI_2 = cor(survey_syn$NBI_2, survey_syn$syn_NBI_2, use = 'pairwise.complete.obs'),
    NBI_3.X = mean(
      c(cor(survey_syn$NBI_310, survey_syn$syn_NBI_310, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_311, survey_syn$syn_NBI_311, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_320, survey_syn$syn_NBI_320, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_321, survey_syn$syn_NBI_321, use = 'pairwise.complete.obs'))
      ,
      na.rm = T
    ),
    NBI_3 = cor(survey_syn$NBI_3, survey_syn$syn_NBI_3, use = 'pairwise.complete.obs'),
    NBI_4.X = mean(
      c(cor(survey_syn$NBI_411, survey_syn$syn_NBI_411, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_412, survey_syn$syn_NBI_412, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_413, survey_syn$syn_NBI_413, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_414, survey_syn$syn_NBI_414, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_415, survey_syn$syn_NBI_415, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_416, survey_syn$syn_NBI_416, use = 'pairwise.complete.obs'),
        cor(survey_syn$NBI_417, survey_syn$syn_NBI_417, use = 'pairwise.complete.obs'))
      ,
      na.rm = T
    ),
    NBI_4 = cor(survey_syn$NBI_4, survey_syn$syn_NBI_4, use = 'pairwise.complete.obs'),
    NBI = cor(survey_syn$new_NBI, survey_syn$NBI, use = 'pairwise.complete.obs')
  ) %>% 
    bind_rows(nbi_cor)
  
  nbi_cor <- data.frame(
    run = j,
    metric = 'sum',
    NBI_1.X = mean(
      c(sum(survey_syn$NBI_111, survey_syn$syn_NBI_111, na.rm = T)/2,
        sum(survey_syn$NBI_112, survey_syn$syn_NBI_112, na.rm = T)/2,
        sum(survey_syn$NBI_113, survey_syn$syn_NBI_113, na.rm = T)/2,
        sum(survey_syn$NBI_121, survey_syn$syn_NBI_121, na.rm = T)/2,
        sum(survey_syn$NBI_131, survey_syn$syn_NBI_131, na.rm = T)/2)
      ,
      na.rm = T
    ),
    NBI_1 = sum(survey_syn$NBI_1, survey_syn$syn_NBI_1, na.rm = T)/2,
    NBI_2.X = mean(
      c(sum(survey_syn$NBI_211, survey_syn$syn_NBI_211, na.rm = T)/2,
        sum(survey_syn$NBI_212, survey_syn$syn_NBI_212, na.rm = T)/2,
        sum(survey_syn$NBI_213, survey_syn$syn_NBI_213, na.rm = T)/2,
        sum(survey_syn$NBI_214, survey_syn$syn_NBI_214, na.rm = T)/2,
        sum(survey_syn$NBI_215, survey_syn$syn_NBI_215, na.rm = T)/2)
      ,
      na.rm = T
    ),
    NBI_2 = sum(survey_syn$NBI_2, survey_syn$syn_NBI_2, na.rm = T)/2,
    NBI_3.X = mean(
      c(sum(survey_syn$NBI_310, survey_syn$syn_NBI_310, na.rm = T)/2,
        sum(survey_syn$NBI_311, survey_syn$syn_NBI_311, na.rm = T)/2,
        sum(survey_syn$NBI_320, survey_syn$syn_NBI_320, na.rm = T)/2,
        sum(survey_syn$NBI_321, survey_syn$syn_NBI_321, na.rm = T)/2)
      ,
      na.rm = T
    ),
    NBI_3 = sum(survey_syn$NBI_3, survey_syn$syn_NBI_3, na.rm = T)/2,
    NBI_4.X = mean(
      c(sum(survey_syn$NBI_411, survey_syn$syn_NBI_411, na.rm = T)/2,
        sum(survey_syn$NBI_412, survey_syn$syn_NBI_412, na.rm = T)/2,
        sum(survey_syn$NBI_413, survey_syn$syn_NBI_413, na.rm = T)/2,
        sum(survey_syn$NBI_414, survey_syn$syn_NBI_414, na.rm = T)/2,
        sum(survey_syn$NBI_415, survey_syn$syn_NBI_415, na.rm = T)/2,
        sum(survey_syn$NBI_416, survey_syn$syn_NBI_416, na.rm = T)/2,
        sum(survey_syn$NBI_417, survey_syn$syn_NBI_417, na.rm = T)/2)
      ,
      na.rm = T
    ),
    NBI_4 = sum(survey_syn$NBI_4, survey_syn$syn_NBI_4, na.rm = T)/2,
    NBI = sum(survey_syn$new_NBI, survey_syn$NBI, na.rm = T)/2
  ) %>% 
    bind_rows(nbi_cor)
  
  nbi_cor <- data.frame(
    run = j,
    metric = 'KL',
    NBI_1.X = mean(
      c(1 / (1 + suppressMessages(
        KL(
          rbind(
            table(factor(survey_syn$NBI_111, levels = c(0,1)))/length(survey_syn$NBI_111), 
            table(factor(survey_syn$syn_NBI_111, levels = c(0,1)))/length(survey_syn$syn_NBI_111)),
          test.na = TRUE,
          unit = "log2",
          est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_112, levels = c(0,1)))/length(survey_syn$NBI_112), 
              table(factor(survey_syn$syn_NBI_112, levels = c(0,1)))/length(survey_syn$syn_NBI_112)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_113, levels = c(0,1)))/length(survey_syn$NBI_113), 
              table(factor(survey_syn$syn_NBI_113, levels = c(0,1)))/length(survey_syn$syn_NBI_113)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_121, levels = c(0,1)))/length(survey_syn$NBI_121), 
              table(factor(survey_syn$syn_NBI_121, levels = c(0,1)))/length(survey_syn$syn_NBI_121)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_131, levels = c(0,1)))/length(survey_syn$NBI_131), 
              table(factor(survey_syn$syn_NBI_131, levels = c(0,1)))/length(survey_syn$syn_NBI_131)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])))
      ,
      na.rm = T
    ),
    NBI_1 = 1 / (1 + suppressMessages(
      KL(
        rbind(
          table(factor(survey_syn$NBI_1, levels = c(0,1)))/length(survey_syn$NBI_1), 
          table(factor(survey_syn$syn_NBI_1, levels = c(0,1)))/length(survey_syn$syn_NBI_1)),
        test.na = TRUE,
        unit = "log2",
        est.prob = NULL)[[1]])),
    NBI_2.X = mean(
      c(1 / (1 + suppressMessages(
        KL(
          rbind(
            table(factor(survey_syn$NBI_211, levels = c(0,1)))/length(survey_syn$NBI_211), 
            table(factor(survey_syn$syn_NBI_211, levels = c(0,1)))/length(survey_syn$syn_NBI_211)),
          test.na = TRUE,
          unit = "log2",
          est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_212, levels = c(0,1)))/length(survey_syn$NBI_212), 
              table(factor(survey_syn$syn_NBI_212, levels = c(0,1)))/length(survey_syn$syn_NBI_212)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_213, levels = c(0,1)))/length(survey_syn$NBI_213), 
              table(factor(survey_syn$syn_NBI_213, levels = c(0,1)))/length(survey_syn$syn_NBI_213)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_214, levels = c(0,1)))/length(survey_syn$NBI_214), 
              table(factor(survey_syn$syn_NBI_214, levels = c(0,1)))/length(survey_syn$syn_NBI_214)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_215, levels = c(0,1)))/length(survey_syn$NBI_215), 
              table(factor(survey_syn$syn_NBI_215, levels = c(0,1)))/length(survey_syn$syn_NBI_215)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])))
      ,
      na.rm = T
    ),
    NBI_2 = 1 / (1 + suppressMessages(
      KL(
        rbind(
          table(factor(survey_syn$NBI_2, levels = c(0,1)))/length(survey_syn$NBI_2), 
          table(factor(survey_syn$syn_NBI_2, levels = c(0,1)))/length(survey_syn$syn_NBI_2)),
        test.na = TRUE,
        unit = "log2",
        est.prob = NULL)[[1]])),
    NBI_3.X = mean(
      c(1 / (1 + suppressMessages(
        KL(
          rbind(
            table(factor(survey_syn$NBI_310, levels = c(0,1)))/length(survey_syn$NBI_310), 
            table(factor(survey_syn$syn_NBI_310, levels = c(0,1)))/length(survey_syn$syn_NBI_310)),
          test.na = TRUE,
          unit = "log2",
          est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_311, levels = c(0,1)))/length(survey_syn$NBI_311), 
              table(factor(survey_syn$syn_NBI_311, levels = c(0,1)))/length(survey_syn$syn_NBI_311)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_320, levels = c(0,1)))/length(survey_syn$NBI_320), 
              table(factor(survey_syn$syn_NBI_320, levels = c(0,1)))/length(survey_syn$syn_NBI_320)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_321, levels = c(0,1)))/length(survey_syn$NBI_321), 
              table(factor(survey_syn$syn_NBI_321, levels = c(0,1)))/length(survey_syn$syn_NBI_321)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])))
      ,
      na.rm = T
    ),
    NBI_3 = 1 / (1 + suppressMessages(
      KL(
        rbind(
          table(factor(survey_syn$NBI_3, levels = c(0,1)))/length(survey_syn$NBI_3), 
          table(factor(survey_syn$syn_NBI_3, levels = c(0,1)))/length(survey_syn$syn_NBI_3)),
        test.na = TRUE,
        unit = "log2",
        est.prob = NULL)[[1]])),
    NBI_4.X = mean(
      c(1 / (1 + suppressMessages(
        KL(
          rbind(
            table(factor(survey_syn$NBI_411, levels = c(0,1)))/length(survey_syn$NBI_411), 
            table(factor(survey_syn$syn_NBI_411, levels = c(0,1)))/length(survey_syn$syn_NBI_411)),
          test.na = TRUE,
          unit = "log2",
          est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_412, levels = c(0,1)))/length(survey_syn$NBI_412), 
              table(factor(survey_syn$syn_NBI_412, levels = c(0,1)))/length(survey_syn$syn_NBI_412)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_413, levels = c(0,1)))/length(survey_syn$NBI_413), 
              table(factor(survey_syn$syn_NBI_413, levels = c(0,1)))/length(survey_syn$syn_NBI_413)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_414, levels = c(0,1)))/length(survey_syn$NBI_414), 
              table(factor(survey_syn$syn_NBI_414, levels = c(0,1)))/length(survey_syn$syn_NBI_414)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_415, levels = c(0,1)))/length(survey_syn$NBI_415), 
              table(factor(survey_syn$syn_NBI_415, levels = c(0,1)))/length(survey_syn$syn_NBI_415)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_416, levels = c(0,1)))/length(survey_syn$NBI_416), 
              table(factor(survey_syn$syn_NBI_416, levels = c(0,1)))/length(survey_syn$syn_NBI_416)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])),
        1 / (1 + suppressMessages(
          KL(
            rbind(
              table(factor(survey_syn$NBI_417, levels = c(0,1)))/length(survey_syn$NBI_417), 
              table(factor(survey_syn$syn_NBI_417, levels = c(0,1)))/length(survey_syn$syn_NBI_417)),
            test.na = TRUE,
            unit = "log2",
            est.prob = NULL)[[1]])))
      ,
      na.rm = T
    ),
    NBI_4 = 1 / (1 + suppressMessages(
      KL(
        rbind(
          table(factor(survey_syn$NBI_4, levels = c(0,1)))/length(survey_syn$NBI_4), 
          table(factor(survey_syn$syn_NBI_4, levels = c(0,1)))/length(survey_syn$syn_NBI_4)),
        test.na = TRUE,
        unit = "log2",
        est.prob = NULL)[[1]])),
    NBI = 1 / (1 + suppressMessages(
      KL(
        rbind(
          table(factor(survey_syn$new_NBI, levels = c(0,1)))/length(survey_syn$new_NBI), 
          table(factor(survey_syn$NBI, levels = c(0,1)))/length(survey_syn$NBI)),
        test.na = TRUE,
        unit = "log2",
        est.prob = NULL)[[1]]))
  ) %>% 
    bind_rows(nbi_cor)

}

nbi_cor_summary <- nbi_cor %>% 
  group_by(metric) %>% 
  summarise(across(everything(), mean)) %>% 
  ungroup %>% 
  select(-run) %>% 
  pivot_longer(-metric, names_to = 'indicators', values_to = 'value') %>% 
  pivot_wider(names_from = 'metric', values_from = 'value') %>% 
  xtable(digits = 2)

# Outcomes

df_perf <- df_perf_strata_new_nbi %>%
  pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
  mutate(metric = replace(metric, metric == 'adj_r2', 'Adjusted R2'),
         metric = replace(metric, metric == 'mse', 'MSE'),
         metric = replace(metric, metric == 'rb', 'Relative Bias'),
         type = replace(type, type == 'syn', 'gen')) %>% 
  bind_rows(
    df_perf_strata_syn_nbi %>%
      pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
      filter(type == 'syn') %>% 
      mutate(metric = replace(metric, metric == 'adj_r2', 'Adjusted R2'),
             metric = replace(metric, metric == 'mse', 'MSE'),
             metric = replace(metric, metric == 'rb', 'Relative Bias'))
  )

# Adjusted R2
pdf(file = paste0("./viz/appendix_utility_nbi_Adjusted_R2_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",n_sim,"_",n_mse,".pdf"), width = 3, height = 3)



df_perf %>%
  filter(metric == 'Adjusted R2') %>% 
  ggplot(aes(x = metric, y = value, fill = type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2) +
  scale_fill_viridis_d(name = "NBI type",
                       alpha = .7,
                       labels = c("Computed" ,"Geomasked", "Synthetic", "True")) +
  labs(x = '',
       y = '',
       fill = '',
       color = '') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
  )

dev.off()

# Relative Bias
pdf(file = paste0("./viz/appendix_utility_nbi_Relative Bias_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",n_sim,"_",n_mse,".pdf"), width = 3, height = 3)

df_perf %>%
  filter(metric == 'Relative Bias') %>% 
  ggplot(aes(x = metric, y = value, fill = type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2) +
  scale_fill_viridis_d(name = "Survey type",
                       alpha = .7,
                       labels = c("Computed" ,"Geomasked", "Synthetic", "True")) +
  labs(x = '',
       y = '',
       fill = '',
       color = '') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
  )

dev.off()

# MSE
pdf(file = paste0("./viz/appendix_utility_nbi_MSE_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",n_sim,"_",n_mse,".pdf"), width = 3, height = 3)

df_perf %>%
  filter(metric == 'MSE') %>% 
  ggplot(aes(x = metric, y = value, fill = type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2) +
  scale_fill_viridis_d(name = "Survey type",
                       alpha = .7,
                       labels = c("Computed" ,"Geomasked", "Synthetic", "True")) +
  labs(x = '',
       y = '',
       fill = '',
       color = '') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
  )

dev.off()

# Legend
pdf(file = paste0("./viz/appendix_utility_nbi_legend_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",n_sim,"_",n_mse,".pdf"), width = 3, height = 3)

df_perf %>%
  filter(metric == 'MSE') %>% 
  ggplot(aes(x = metric, y = value, fill = type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2) +
  scale_fill_viridis_d(name = "Survey type",
                       alpha = .7,
                       labels = c("Computed" ,"Geomasked", "Synthetic", "True")) +
  labs(x = '',
       y = '',
       fill = '',
       color = '') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        # legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
  )

dev.off()

# Plot NBI densities

j <- 6

pcd_nbi_long <- census %>% 
  group_by(ID_PCD) %>% 
  summarise(N = n(), pnbi = mean(NBI, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ID_PCD = as.character(ID_PCD)) %>% 
  left_join(df_pred_strata_new_nbi %>% 
              filter(run == j) %>%
              select(ID_PCD, type, pred) %>% 
              pivot_wider(names_from = type, values_from = pred),
            by = 'ID_PCD') %>%
  left_join(df_pred_strata_syn_nbi %>% 
              filter(run == j) %>%
              select(ID_PCD, type, pred) %>% 
              filter(type == 'syn') %>% 
              mutate(type = replace(type, type == 'syn', 'gen')) %>% 
              pivot_wider(names_from = type, values_from = pred),
            by = 'ID_PCD') %>% 
  left_join(survey_true %>% 
              select(ID_PCD, weight) %>% 
              distinct(ID_PCD, .keep_all = T), by = 'ID_PCD'
  ) %>% 
  drop_na(true) %>% 
  pivot_longer(!c('ID_PCD', 'N', 'weight'), names_to = "type", values_to = "NBI") %>% 
  mutate(type = replace(type, type == 'pnbi', 'Census'),
         type = replace(type, type == 'true', 'True'),
         type = replace(type, type == 'geo', 'Geomasked'),
         type = replace(type, type == 'syn', 'Synthetic'),
         type = replace(type, type == 'gen', 'Computed'),
         type = fct_relevel(type, 
                            "Census", "True", "Geomasked", 
                            "Synthetic", "Computed")) %>% 
  drop_na() %>% 
  mutate(qNBI = cut(NBI, quantile(NBI,  seq(0,1, 0.25)), include.lowest = T)) %>% 
  ungroup

pdf(file = paste0("./viz/appendix_utility_NBI_densities_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",n_sim,"_",n_mse,".pdf"), width = 10, height = 5)

ggplot(pcd_nbi_long, aes(x = NBI, 
                         y = type, 
                         fill = qNBI
)) +
  geom_density_ridges(aes(height=..density..,
                          weight=weight),
                      scale= 0.95,
                      stat="density") +
  scale_fill_viridis_d(name = "Quartile",
                       alpha = .7,
                       labels = c("1st", "2nd", "3rd", "4th")) +
  labs(x= "NBI",
       y = 'Data Source'
  ) +
  theme_tufte() +
  theme(text = element_text(size=20),
        axis.ticks = element_blank())

dev.off()
