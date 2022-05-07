#################################################

# In this file, we prepare a district-level map of Costa Rica from the census year 2011.
# Maps from other years may not be identical due to land reforms at that time.

#################################################

library(rgdal)
library(tidyverse)
library(rgeos)
library(sp)

# Dplyr options
options(dplyr.summarise.inform = FALSE)
select <- dplyr::select


# Census map (2011) ---------------------------------------------------------

map11 <- readOGR(dsn = './data/censo2011', layer="distritos", verbose = F)

map11@data <- map11@data %>% 
  rename(CODDIST = CODIGO)


# Buffer to remove bad polygon parts --------------------------------------

'
We had to buffer with more than zero (with 1cm) due to some invalid polygons.
'

map11 <- gBuffer(map11, byid=TRUE, width=0.01)


# Add urban/rural identifier ----------------------------------------------

'
We can see that some districts host both rural and urban areas, however, going forward, we need a unique zone identification.
In the following, we assign the zone that hosts more people to a district.
'

census <- readRDS('./data/midsave/census.rds') %>% 
  select(CODDIST = ID_PCD,
         zone = ID_ZONA) %>% 
  mutate(CODDIST = as.character(CODDIST),
         zone = as.character(as_factor(zone))) %>% 
  group_by(CODDIST, zone) %>% 
  mutate(n = n()) %>% 
  distinct() %>% 
  group_by(CODDIST) %>% 
  filter(n==max(n)) %>% # Assign by majority vote
  select(CODDIST, zone)


# Clean up after shape shifting -------------------------------------------

poly_map11 <- gUnaryUnion(map11, map11$CODDIST)

map11_id <- sapply(slot(poly_map11, "polygons"), function(x) slot(x, "ID"))

data_map11 <- data.frame(map11_id, row.names = map11_id) %>% 
  mutate(CODDIST = as.character(map11_id)) %>% 
  left_join(census, by = 'CODDIST') %>% 
  column_to_rownames(var='map11_id')

map11_pcd <- SpatialPolygonsDataFrame(poly_map11, data_map11) %>% 
  spTransform(., "+proj=longlat +datum=WGS84 +no_defs")

map11_pcd@data <- map11_pcd@data %>% 
  mutate(CODDIST = as.character(CODDIST),
         pc = str_sub(CODDIST, 1, 3),
         p = str_sub(CODDIST, 1, 1))


# Get centroids for pixel matching ----------------------------------------

map11_pcd$x <- gCentroid(map11_pcd, byid = TRUE)@coords[,'x']
map11_pcd$y <- gCentroid(map11_pcd, byid = TRUE)@coords[,'y']


# Remove uninhabitated areas ----------------------------------------------

map11_pcd <- map11_pcd[map11_pcd$CODDIST != '60110',] # Isla del Coco


# Save shapefile ----------------------------------------------------------

writeOGR(map11_pcd, "./data/midsave/", "map_pcd_harmonised", driver="ESRI Shapefile", overwrite_layer=TRUE)
