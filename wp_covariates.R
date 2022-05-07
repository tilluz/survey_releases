#################################################

# In this file, we prepare the satellite-based covariates downloaded from he WorldPop repository at: 
# https://www.worldpop.org/doi/10.5258/SOTON/WP00644

#################################################

library(raster)
library(tidyverse)
library(rgdal)
library(rgeos)
library(data.table)


# Harmonised Map ----------------------------------------------------------

level <- 'pcd'

# 2011 Disctricts
if (level == 'pcd'){
  cri_map <- readOGR(dsn = './data/midsave/', layer="map_pcd_harmonised", verbose = F)
  cri_map$geoid <- as.character(cri_map$CODDIST)
}


# Planar Map --------------------------------------------------------------

to_utm <- "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs"
cri_map_utm <- spTransform(cri_map, to_utm)
row.names(cri_map_utm) <- cri_map_utm$geoid


# Flag for pixel bootstrap ------------------------------------------------

boot <- FALSE
b <- 100
bb <- 100
set.seed(1234)

# Function to allocate out-of-bounds pixels to areas ----------------------

'
This applies mostly to pixels directly at the border.
'

pixel_allocate <- function(tmp){
  tmp <- tmp %>% 
    as.data.frame()
  
  coordinates(tmp) <- ~x+y
  proj4string(tmp) <- proj4string(cri_map)
  
  dat <- tmp@data %>% 
    mutate(pixel_id = as.character(1:n()),
           geoid = over(tmp, cri_map[,'geoid'])[[1]],
           x = tmp@coords[,'x'],
           y = tmp@coords[,'y'])
  
  missings <- dat %>% 
    filter(is.na(geoid)) %>% 
    column_to_rownames('pixel_id')
  
  coordinates(missings) <- ~x+y
  proj4string(missings) <- proj4string(cri_map)
  missings_utm <- spTransform(missings, to_utm)
  
  missings <- gDistance(cri_map_utm, missings_utm, byid = TRUE) %>% 
    as.data.frame() %>% 
    rownames_to_column('pixel_id')  %>%
    mutate(geoid = colnames(.[,-1])[max.col(-.[,-1],"first")]) %>%
    select(pixel_id, geoid)
  
  dat <- dat %>% 
    filter(!is.na(geoid)) %>% 
    select(pixel_id, geoid) %>% 
    bind_rows(missings) %>% 
    left_join(dat %>% select(-geoid), by = 'pixel_id') %>% 
    select(-pixel_id, -x, -y)
}

# Distance to Areas -------------------------------------------------------

# Area types

'
          011: Cultivated area
          040: Woody-tree area
          130: Shrub area
          140: Herbacious area
          150: Sparse vegetation area
          160: Aquatic vegetation area
          190: Artificial surface area
          200: Bare area
          '

if(boot == TRUE){
  esaccilc_dst <- data.frame(geoid = sort(rep(cri_map@data$geoid, b)),
                             b = rep((1:b), length(unique(cri_map@data$geoid)))) %>% 
    mutate(geoid = geoid %>% as.character())
}else{
  esaccilc_dst <- cri_map %>% as.data.frame() %>% mutate(geoid = geoid %>% as.character()) %>% select(geoid)
}



for (i in c('esaccilc_dst011',
              'esaccilc_dst040',
              'esaccilc_dst130',
              'esaccilc_dst140',
              'esaccilc_dst150',
              'esaccilc_dst160',
              'esaccilc_dst190',
              'esaccilc_dst200')){ #2002:2015
    if (exists("tmp") == TRUE){
      tmp <- raster(paste0("./data/world_pop/cri_",i,"_100m_2011.tif")) %>% 
        rasterToPoints() %>% 
        .[, 3, drop = F] %>% 
        cbind(tmp)
    } else{
      tmp <- raster(paste0("./data/world_pop/cri_",i,"_100m_2011.tif")) %>% 
        rasterToPoints()
    }
    print(i)
  }
  
  if(boot == TRUE){
    pixel_df <- pixel_allocate(tmp)
    
    ind_df <- data.frame()
    for (i in 1:b){
      ind_df <- pixel_df %>%
        group_by(geoid) %>%
        sample_n(size = n(), replace = TRUE) %>%
        transmute_at(vars(-group_cols()), abs) %>%
        summarise_all(list(mean = mean, sd = sd)) %>%
        mutate(b = i) %>%
        bind_rows(ind_df)
    }
    esaccilc_dst <- ind_df %>% 
      left_join(esaccilc_dst, by = c('geoid', 'b'))  
  }else{
    esaccilc_dst <- pixel_allocate(tmp) %>% 
      group_by(geoid) %>% 
      transmute_at(vars(-group_cols()), abs) %>%
      summarise_all(list(mean = mean, sd = sd))
  }

if(boot == TRUE){
  saveRDS(esaccilc_dst, file = paste0("./data/midsave/esaccilc_dst_",level,"_boot.rds"))
}else{
  saveRDS(esaccilc_dst, file = paste0("./data/midsave/esaccilc_dst_",level,".rds"))
}


rm(tmp, esaccilc_dst)

# Other Covariates

'
- Coastline
- Major roads
- Major road intersections
- Major waterways
- Open coastlines
- Nature reserves
- Nighttime lights
'

for (i in c('cri_dst_coastline_100m_2000_2020',
            'cri_osm_dst_road_100m_2016',
            'cri_osm_dst_roadintersec_100m_2016',
            'cri_osm_dst_waterway_100m_2016',
            'cri_esaccilc_dst_water_100m_2000_2012',
            'cri_wdpa_dst_cat1_100m_2011',
            'cri_dmsp_100m_2011',
            'cri_bsgmi_v0a_100m_2011'
            
)){
  if (exists("tmp") == TRUE){
    tmp <- raster(paste0("./data/world_pop/",i,".tif")) %>% 
      rasterToPoints() %>% 
      .[, 3, drop = F] %>% 
      cbind(tmp)
  } else{
    tmp <- raster(paste0("./data/world_pop/",i,".tif")) %>% 
      rasterToPoints()
  }
  print(i)
}

  if(boot == TRUE){
  pixel_df <- pixel_allocate(tmp)
  

  other_dst <- data.frame()
  for (i in 1:b){
    other_dst <- pixel_df %>%
      group_by(geoid) %>%
      sample_n(size = n(), replace = TRUE) %>%
      transmute_at(vars(-group_cols()), abs) %>%
      summarise_all(list(mean = mean, sd = sd)) %>%
      mutate(b = i) %>%
      bind_rows(other_dst)

    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(other_dst, file = paste0("./data/midsave/other_dst_",level,"_boot.rds"))
  
}else{
  other_dst <- pixel_allocate(tmp) %>% 
    group_by(geoid) %>% 
    transmute_at(vars(-group_cols()), abs) %>% 
    summarise_all(list(mean = mean, sd = sd))
  
  saveRDS(other_dst, file = paste0("./data/midsave/other_dst_",level,".rds"))
}

    rm(tmp, other_dst)


# Settlement areas --------------------------------------------------------

# Years 2002-2013

for (i in c(2011)){ #2002:2011,2013
  if (exists("tmp") == TRUE){
    tmp <- raster(paste0("./data/world_pop/cri_bsgmi_v0a_100m_",i,".tif")) %>% 
      rasterToPoints() %>% 
      .[, 3, drop = F] %>% 
      cbind(tmp)
  } else{
    tmp <- raster(paste0("./data/world_pop/cri_bsgmi_v0a_100m_",i,".tif")) %>% 
      rasterToPoints()
  }
}

pixel_df <- pixel_allocate(tmp)

if(boot == TRUE){
  
  
  bsgmi_sum <- data.frame()
  for (i in 1:b){
    bsgmi_sum <- pixel_df %>%
      group_by(geoid) %>%
      sample_n(size = n(), replace = TRUE) %>%
      summarise_all(list(cri_bsgmi_v0a_100m_2011_sum = sum, cri_bsgmi_v0a_100m_2011_sd = sd)) %>%
      mutate(b = i) %>%
      bind_rows(bsgmi_sum)

    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(bsgmi_sum, file = paste0("./data/midsave/bsgmi_sum_",level,"_boot.rds"))
  
}else{
  bsgmi_sum <- pixel_df %>% 
    group_by(geoid) %>% 
    summarise_all(list(cri_bsgmi_v0a_100m_2011_sum = sum, cri_bsgmi_v0a_100m_2011_sd = sd))
  
  saveRDS(bsgmi_sum, file = paste0("./data/midsave/bsgmi_sum_",level,".rds"))
}

rm(tmp, bsgmi_sum)

# Slope and topography (SRTM)

for (i in c('cri_srtm_slope_100m',
            'cri_srtm_topo_100m'
            
)){
  if (exists("tmp") == TRUE){
    tmp <- raster(paste0("./data/world_pop/",i,".tif")) %>% 
      rasterToPoints() %>% 
      .[, 3, drop = F] %>% 
      cbind(tmp)
  } else{
    tmp <- raster(paste0("./data/world_pop/",i,".tif")) %>% 
      rasterToPoints()
  }
}

if(boot == TRUE){
  pixel_df <- pixel_allocate(tmp)
  
  
  srtm <- data.frame()
  for (i in 1:b){
    srtm <- pixel_df %>%
      group_by(geoid) %>%
      sample_n(size = n(), replace = TRUE) %>%
      transmute_at(vars(-group_cols()), abs) %>%
      summarise_all(list(mean = mean, sd = sd)) %>%
      mutate(b = i) %>%
      bind_rows(srtm)
    
    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(srtm, file = paste0("./data/midsave/srtm_",level,"_boot.rds"))
  
}else{
  srtm <- pixel_allocate(tmp) %>% 
    group_by(geoid) %>% 
    transmute_at(vars(-group_cols()), abs) %>% 
    summarise_all(list(mean = mean, sd = sd))
  
  saveRDS(srtm, file = paste0("./data/midsave/srtm_",level,".rds"))
}

rm(tmp, srtm)

