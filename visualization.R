#################################################

# In this file, we visualize the results for the main paper.

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
field_dist <- 'gaussian' # Options are: 'gaussian', 'parametric'
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

df_pred <- readRDS(paste0("./data/midsave/utility_prediction_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))
df_perf <- readRDS(paste0("./data/midsave/utility_performance_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

kl_div <- readRDS(paste0("./data/midsave/utility_kl_div_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

unique_df <- readRDS(paste0("./data/midsave/risk_uniqueness_",var_order,"_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))

privacy_df <- readRDS(paste0("./data/midsave/risk_privacy_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".rds"))


# Plot NBI densities ------------------------------------------------------

pcd_nbi_long <- census %>% 
  group_by(ID_PCD) %>% 
  summarise(N = n(), pnbi = mean(NBI, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ID_PCD = as.character(ID_PCD)) %>% 
  left_join(df_pred %>% 
              filter(run == j) %>%
              select(ID_PCD, type, pred) %>% 
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
         type = replace(type, type == 'syn', 'Synthetic')) %>% 
  mutate(type = fct_relevel(type, 
                            "Census", "True", "Geomasked", 
                            "Synthetic")) %>% 
  drop_na() %>% 
  mutate(qNBI = cut(NBI, quantile(NBI,  seq(0,1, 0.25)), include.lowest = T)) %>% 
  ungroup

pdf(file = paste0("./viz/utility_NBI_densities_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 10, height = 5)

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
  # xlim(0,1) +
  theme(text = element_text(size=20),
        axis.ticks = element_blank())

dev.off()

# Predictive performance (Bias, RMSE, R2) ---------------------------------

# Adjusted R2
pdf(file = paste0("./viz/utility_performance_Adjusted_R2_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 3, height = 3)

df_perf %>%
  pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
  mutate(metric = replace(metric, metric == 'adj_r2', 'Adjusted R2'),
         metric = replace(metric, metric == 'mse', 'MSE'),
         metric = replace(metric, metric == 'rb', 'Relative Bias')) %>% 
  filter(metric == 'Adjusted R2') %>% 
  ggplot(aes(x = metric, y = value, fill = type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2) +
  scale_fill_viridis_d(name = "Survey type",
                       alpha = .7,
                       labels = c("Geomasked", "Synthetic", "True")) +
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
pdf(file = paste0("./viz/utility_performance_Relative Bias_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 3, height = 3)

df_perf %>%
  pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
  mutate(metric = replace(metric, metric == 'adj_r2', 'Adjusted R2'),
         metric = replace(metric, metric == 'mse', 'MSE'),
         metric = replace(metric, metric == 'rb', 'Relative Bias')) %>% 
  filter(metric == 'Relative Bias') %>% 
  ggplot(aes(x = metric, y = value, fill = type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2) +
  scale_fill_viridis_d(name = "Survey type",
                       alpha = .7,
                       labels = c("Geomasked", "Synthetic", "True")) +
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
pdf(file = paste0("./viz/utility_performance_MSE_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 3, height = 3)

df_perf %>%
  pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
  mutate(metric = replace(metric, metric == 'adj_r2', 'Adjusted R2'),
         metric = replace(metric, metric == 'mse', 'MSE'),
         metric = replace(metric, metric == 'rb', 'Relative Bias')) %>% 
  filter(metric == 'MSE') %>% 
  ggplot(aes(x = metric, y = value, fill = type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2) +
  scale_fill_viridis_d(name = "Survey type",
                       alpha = .7,
                       labels = c("Geomasked", "Synthetic", "True")) +
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
pdf(file = paste0("./viz/utility_performance_legend_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 3, height = 3)

df_perf %>%
  pivot_longer(!c('run', 'type', 'in_sample'), names_to = "metric", values_to = "value") %>% 
  mutate(metric = replace(metric, metric == 'adj_r2', 'Adjusted R2'),
         metric = replace(metric, metric == 'mse', 'MSE'),
         metric = replace(metric, metric == 'rb', 'Relative Bias')) %>% 
  filter(metric == 'MSE') %>% 
  ggplot(aes(x = metric, y = value, fill = type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.2) +
  scale_fill_viridis_d(name = "Survey type",
                       alpha = .7,
                       labels = c("Geomasked", "Synthetic", "True")) +
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

# KL Divergence -----------------------------------------------------------

'
The closer the normalized KL value to 1, the closer the anonymized distribution to the true distribution.
'


kl_div_p <- kl_div %>% 
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
              select(-blab), by = 'attr_key')

# Normalized KL: True Survey
pdf(file = paste0("./viz/utility_kl_div_true_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 6, height = 6)

kl_div_p %>% 
  filter(type == 'true') %>% 
  ggplot(aes(x = reorder(ID_PCD, n), y = reorder(attr_key, n_levels), fill = kl_value)) + # order variables by numbers of categories, order PCDs by sample size, fill with mean
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 1)) + 
  labs(x = "Zip Codes, ordered by average sample size",
       y = 'Attributes, ordered by number of classes',
       fill = expression(Z["KL"])) +
  theme_tufte() +
  theme(
    legend.position="none",
    text =  element_text(size=20),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

dev.off()

# Normalized KL: Geomasked Survey
pdf(file = paste0("./viz/utility_kl_div_geo_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 6, height = 6)

kl_div_p %>% 
  filter(type == 'geo') %>% 
  ggplot(aes(x = reorder(ID_PCD, n), y = reorder(attr_key, n_levels), fill = kl_value)) + # order variables by numbers of categories, order PCDs by sample size, fill with mean
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 1)) + 
  labs(x = "Zip Codes, ordered by average sample size",
       y = 'Attributes, ordered by number of classes',
       fill = 'Normalized\nKL divergence') +
  theme_tufte() +
  theme(
    legend.position="none",
    text =  element_text(size=20),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

dev.off()

# Normalized KL: Synthetic Survey
pdf(file = paste0("./viz/utility_kl_div_syn_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 6, height = 6)

kl_div_p %>% 
  filter(type == 'syn') %>% 
  ggplot(aes(x = reorder(ID_PCD, n), y = reorder(attr_key, n_levels), fill = kl_value)) + # order variables by numbers of categories, order PCDs by sample size, fill with mean
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 1)) + 
  labs(x = "Zip Codes, ordered by average sample size",
       y = 'Attributes, ordered by number of classes',
       fill = 'Normalized\nKL divergence') +
  theme_tufte() +
  theme(
    legend.position="none",
    text =  element_text(size=20),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

dev.off()

# Normalized KL: Legend
pdf(file = paste0("./viz/utility_kl_div_legend_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 10, height = 10,
    bg = "transparent")

kl_div_p %>% 
  filter(type == 'true') %>% 
  ggplot(aes(x = reorder(ID_PCD, n), y = reorder(attr_key, n_levels), fill = kl_value)) + # order variables by numbers of categories, order PCDs by sample size, fill with mean
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 1)) + 
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10, title.vjust = 1)) +
  labs(x = "Zip Codes, ordered by average sample size",
       y = 'Attributes, ordered by number of classes',
       fill = expression(Z["KL"])) +
  theme_tufte() +
  theme(
    text =  element_text(size=20),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.direction="vertical"
  )

dev.off()

########
# Risk #
########

# Uniqueness --------------------------------------------------------------

# pdf(file = paste0("./viz/risk_uniqueness_",var_order,"_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 10, height = 3.33)
png(file = paste0("./viz/risk_uniqueness_",var_order,"_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".png"), width = 1000, height = 333)

unique_df %>% 
  filter(n_attr <= 50 & type != 'no_geo') %>% 
  mutate(type = replace(type, type == 'true', 'True'),
         type = replace(type, type == 'geo', 'Geomasked'),
         type = replace(type, type == 'syn', 'Synthetic'),
         type = replace(type, type == 'reid', 'Re-identified'),
         type = replace(type, type == 'no_geo', 'No zip code')) %>%
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
        # legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank())

dev.off()

# Predicting sensitive attribute ------------------------------------------


privacy_p <- privacy_df %>% 
  select(-acc_in, -oos_error) %>% 
  pivot_longer(!c('run', 'type', 'strata', 'n_clusters'), names_to = "acc_type", values_to = "acc_value") %>% 
  filter(!(acc_type == 'acc_guess' & type == 'geo')) %>% 
  mutate(type = replace(type, acc_type == 'acc_guess', 'Guess'),
         type = replace(type, type == 'geo', 'Geomasked'),
         type = replace(type, type == 'syn', 'Synthetic')) %>% 
  select(-acc_type) %>% 
  group_by(type, strata) %>% 
  summarise(acc_median = quantile(acc_value, probs = c(.5)),
            lower = quantile(acc_value, probs = c(.1)),
            upper = quantile(acc_value, probs = c(.90)),
            n_clusters = mean(n_clusters, na.rm = T)) %>% 
  ungroup


pdf(file = paste0("./viz/risk_privacy_",model,"_",pop_level,"_",syn_level,"_",field_dist,"_",encoding,"_",nbi_type,"_",n_sim,"_",n_mse,".pdf"), width = 10, height = 3.33)

privacy_p %>% 
  ggplot(aes(x = reorder(strata, n_clusters), y = acc_median, group = type, colour = type, fill = type)) +
  geom_point() + 
  geom_line() + 
  scale_colour_viridis_d(option = "D", name = "Survey type", labels = c("Geomasked", "Guess", "Synthetic")) +
  scale_fill_viridis_d(option = "D", name = "Survey type", labels = c("Geomasked", "Guess", "Synthetic")) +
  labs(x = "Strata, ordered by number of clusters",
       y = 'Median accuracy',
       fill = 'Survey type',
       color = 'Survey type') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        # legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
        )

dev.off()

