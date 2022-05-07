# "Releasing survey microdata with exact cluster locations and additional privacy safeguards"
by Till Koebe, Alejandra Arias-Salazar and Timo Schmid

# Abstract
Household survey programs around the world publish fine-granular georeferenced microdata to support research on the interdependence of human livelihoods and their surrounding environment. To safeguard the respondents' privacy, micro-level survey data is usually (pseudo)-anonymized through deletion or perturbation procedures such as obfuscating the true location of data collection. This, however, poses a challenge to emerging approaches that augment survey data with auxiliary information on a local level. Here, we propose an alternative microdata dissemination strategy that leverages the utility of the original microdata with additional privacy safeguards through synthetically generated data using generative models.  We back our proposal with experiments using data from the 2011 Costa Rican census and satellite-derived auxiliary information. Our strategy reduces the respondents' re-identification risk for any number of disclosed attributes by 60-80% even under re-identification attempts.

# Requirements
- Python (Python 3.7.3): sdv==0.12.1; numpy==1.21.5; pandas==1.1.4; pyreadr==0.4.2
- R (R version 4.2.0): RStudio 2021.09.2+382; tidyverse_1.3.1; rgeos_0.5-9; rgdal_1.5-28; sp_1.4-6; reticulate_1.24; emdi_2.1.1; pps_1.0; survey_4.1-1; philentropy_0.5.0

All computations were performed on a standard 64-bit machine with Kubuntu 20.04, 8 × Intel Core i7-8550U CPU @ 1.80GHz and 23,4 GiB of RAM.

# Instructions for replication
1) Go to http://sistemas.inec.cr/pad5/index.php/catalog/113 and download census data
2) Go to https://www.worldpop.org/doi/10.5258/SOTON/WP00644 and download covariates and global settlement growth for Costa Rica in 2011
3) Create following folder structure in your directory: ./data/midsave - censo2011 - world_pop/; ./data/midsave/syn_surveys; ./viz
4) Run code in the following order: census_preprocessing.R; map_processing.R; wp_covariates.R; sampling.R; synthetic_survey.ipynb; prediction.R; utility.R; risk.R; visualization.R; appendix_synthetic_n.R; visualization_appendix.R

For questions or feedback, please contact: Till Koebe (till.koebe@fu-berlin.de)
