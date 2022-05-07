#################################################

# In this file, we sample what we call the 'original' survey. We do a stratified two-stage cluster design with PPS sampling
# in the first stage and simple random sampling without replacement in the second stage having households as unit of observation.
# This corresponds to the usual sampling design in DHS surveys:
# https://dhsprogram.com/pubs/pdf/DHSM4/DHS6_Sampling_Manual_Sept2012_DHSM4.pdf

#################################################

library(survey)
library(pps)
library(tidyverse)
library(labelled)
library(sampling)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

select <- dplyr::select

##########################################
### Setting main simulation parameters ###
##########################################

n.iter <- 100
set.seed(123)

k <- 1


##############################
###         Data           ###
##############################

census<-readRDS("./data/midsave/census.rds")

# Correct Region identifier
census <- census %>% 
  mutate(REGION = ID_REGION,
         REGION = replace(ID_REGION, ID_REGION == 'Resto Región Central', 'Gran Área Metropolitana') %>% 
           as.factor() %>% 
           as.integer()
         )

# Create Strata
census$strata<-as.factor(paste0(census$ID_ZONA,census$REGION))


# Sampling procedure ------------------------------------------------------

# Select unit of observation
census$hhid <- as.factor(census$hhid)


# Select sample size
N <- n_distinct(census$hhid) # Number of units
p_survey = 0.05 # Sample fraction of total population
p_census = 1 # If 1, we treat the 10% census sample as a pseudo-population
p_ssu = 0.1 # Share of households to be sampled within PSU
n <- round(N*p_survey/p_census,0)


# Determine sample size within each PSU

'
DHS recommends a sample take of 20-30 households per PSU. However, DHS usually uses census enumeration areas as PSUs, which
are much smaller than our PSU (actually by factor 20-30). Applying this factor in this study would reduce the number of clusters to < 10,
which is hardly feasible, so we strike a balance by selecting on average 50 households as sample take per PSU.
'

b=50 # b = average number of observations sampled in each PSU -> n/b = number of required clusters
print(paste('Expected number of required clusters:', round(n/b, 0)))

# Define PSUs
census$PSUD<-as.factor(paste0(census$ID_PCD, census$strata))


###########################################################################
###  Stage 1: Selecting PSUs with a probability proportional to size    ###
###########################################################################

# Calculate sample size by strata
PSUs <- census %>% 
  group_by(PSUD, strata)  %>% 
  distinct(hhid) %>%
  summarise(n=n()) %>% 
  filter(n >= 10) %>% # to make sure that only PSUs with enough units are available in the second stage. Otherwise decrease b or srswr
  arrange(strata) %>% 
  distinct(PSUD, .keep_all = T)

tableA <- PSUs %>% 
  group_by(strata) %>% 
  summarise(Nh = sum(n), # Number of units per strata
            nh =sum(n)/N*{{n}}, # Number of sampled units per strata
            Ah = n_distinct(PSUD),# Number of PSUs per strata
            ah = ifelse(nh/b >= Ah, Ah, round(nh/b, 0))) # Number of sampled PSUs per strata       


# Run survey loop ---------------------------------------------------------

sample_descriptives <- matrix(NA, n.iter, 2)
colnames(sample_descriptives) <- c('n', 'n_clu')

while (k <= n.iter) {
  
  # Select PSUs within each strata
  smp.IDs <- sampling::strata(data=PSUs, 
                              stratanames= "strata",
                              size = tableA$ah,
                              method= "srswor",
                              pik = inclusionprobabilities(PSUs$n, sum(tableA$ah)),
                              description = F)
  
  sample_psu <- getdata(PSUs, smp.IDs) %>% 
    inner_join(census, by =c("PSUD", 'strata')) %>% 
    select(PSUD, n, p_psu = Prob, strata, Stratum, hhid) %>% 
    distinct(hhid, .keep_all = T) %>% 
    rowid_to_column("idviv")

  '
  sample_psu are the total number of units within selected clusters. In the next stage we sample units from there to get our final sample.
  '
  
  ####################################################################################
  ###  Stage 2: Selecting units with simple random sampling without replacement    ###
  ####################################################################################

  # Select units within each PSU
  smp.IDs2 <- sampling::strata(data = sample_psu,
                               stratanames = "PSUD",
                               size = (ceiling(p_ssu*sample_psu %>% 
                                                 distinct(PSUD, .keep_all = T) %>% 
                                                 select(n) %>% 
                                                 mutate(n = replace(n, n < 10, 10))))[,1],
                               method= "srswor")

  sample_ssu <- getdata(sample_psu, smp.IDs2) %>% 
    inner_join(census, by = c('hhid', 'PSUD', 'strata')) %>% 
    rename(p_ssu = Prob) %>% 
    mutate(weight = 1/(p_psu*p_ssu*p_census))
  
  # Save each sample separately
  saveRDS(sample_ssu, paste0('./data/midsave/syn_surveys/survey_',k,'.rds'))
  
  sample_descriptives[k,1] <- dim(sample_ssu)[1]
  sample_descriptives[k,2] <- n_distinct(sample_ssu$PSUD)
  
  print(paste0('Samples saved: ',k))
  k <- k + 1
  
}

sample_descriptives <- sample_descriptives %>% 
  as.data.frame()