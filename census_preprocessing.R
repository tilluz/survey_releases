#################################################

# In this file, we prepare the census file downloaded from http://sistemas.inec.cr/pad5/index.php/catalog/113.

# Following steps are done:
# 1) Align data types
# 2) Correct inconsistencies
# 3) Generate missing identifiers
# 4) Recalculate target index: NBI

#################################################

library(haven)
library(tidyverse)

# Dplyr options
options(dplyr.summarise.inform = FALSE)
select <- dplyr::select

source('helpers.R')

census <- read_sav('./data/censo2011/CR_CENSO2011_Muestra10__NBI.sav')


# Encode variables correctly ----------------------------------------------

censo <- census %>% 
  mutate(across(!matches('ID_P|P05B|P19B|P29B'), as_factor),
         across(matches('ID_P|P05B|P19B|P29B'), as.character),
         across(matches('ID_P|V09|V01B|V02B|V10|V19|V01B|H05|H07B|P00|P03|P30|P31|P40|P41|carencias'), as.numeric),
         across(matches('V09|V19|H05A|H05B|P03|P30|P31|P40|P41|carencias'),~ .x - 1),
         # across(where(is.numeric), as.integer), #Synthetic data generation needs floats
         across(where(is.factor), as.character) #Synthetic data generation cannot handle complex factors
         ) %>% 
  select(-starts_with('VAR'))

# Filtering out unlogical values

### Men and women per household do not add up to total number of household members
censo <- censo %>% 
  filter(H05C_TOTAL_PERSONAS == (H05A_TOTAL_HOMBRES + H05B_TOTAL_MUJERES))

### H04 is missing the 'No'
censo <- censo %>% 
  mutate(H04_JEFATURA_COMPARTIDA = replace_na(H04_JEFATURA_COMPARTIDA, "No"))

### Drop V02_OCCUPACION as it has only one category (zero variation)
censo <- censo %>% 
  select(-V02_OCUPACION)

# ### Migrant arrived to CR before birth (arrival is set to age of 0)
# censo <- censo %>% 
#   mutate(P06A_ANO_LLEGADA_CR = as.numeric(P06A_ANO_LLEGADA_CR),
#          P06A_ANO_LLEGADA_CR = replace_na(P06A_ANO_LLEGADA_CR, 9999),
#          diff = 2010 - P03_EDAD,
#          P06A_ANO_LLEGADA_CR = replace(P06A_ANO_LLEGADA_CR, P06A_ANO_LLEGADA_CR <= diff, diff)),
#          P06A_ANO_LLEGADA_CR = na_if(P06A_ANO_LLEGADA_CR == 9999))



# Add unique identifiers for different hierarchies ------------------------

censo <- censo %>%
  mutate(hhid = paste0(ID_PCD, ID_AE_CONSECU, ID_VIVIENDA, ID_HOGAR),
         hid = paste0(ID_PCD, ID_AE_CONSECU, ID_VIVIENDA),
         ID_EA = paste0(ID_PCD, ID_AE_CONSECU)) %>% 
  group_by(hhid) %>% 
  mutate(INDIVIDUAL_ID = 100 + seq_len(n())) %>% 
  ungroup %>% 
  mutate(pid = paste0(ID_PCD, ID_AE_CONSECU, ID_VIVIENDA, ID_HOGAR, INDIVIDUAL_ID)) %>% 
  select(-`filter_$`)


# Recalculate NBI as the recalculated NBI slightly differs with th --------

censo <- censo %>%
  nbi_calc() %>%
  select(-c(school_head, school_head, earner0, recipient0, earner, depend, dependents, educ, new_dep,
            acceso_albergue_digno, Acceso_vida_saludable, acceso_conocimiento, Acceso_otros_bienes_servicios, Total_carencias, NBI)) %>%
  rename(NBI = new_NBI) %>%
  mutate(across(contains('NBI'), ~as.integer(.)))

# Save new file -----------------------------------------------------------

saveRDS(censo, './data/midsave/census.rds')
