##
## Prepare geographical boundaries for use in the Shiny app
##
library(tidyverse)
library(sf)

# Local Authority Districts (December 2019) Boundaries UK BUC
# Source: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc
lad_shp <- read_sf("https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson")

# Save a shapefile that we'll join onto later
lad_shp %>% 
  filter(str_sub(lad19cd, 1, 1) == "E") %>%  # England only for now
  
  st_transform(crs = 4326) %>% 
  select(lad19cd, lad19nm) %>% 
  write_sf("data/lad.shp")

# ---- MSOAs ----
# Middle Layer Super Output Areas (December 2011) EW BSC V2
# Source: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-ew-bsc-v2
msoa_shp <- read_sf("https://opendata.arcgis.com/datasets/23cdb60ee47e4fef8d72e4ee202accb0_0.geojson")

# Save a shapefile that we'll join onto later
msoa_shp %>% 
  filter(str_sub(MSOA11CD, 1, 1) == "E") %>%  # England only for now
  
  st_transform(crs = 4326) %>% 
  select(MSOA11CD, MSOA11NM) %>% 
  write_sf("data/msoa.shp")

# ---- LRFs ----
# Local Resilience Forums (December 2019) EW BUC
# Source: https://geoportal.statistics.gov.uk/datasets/local-resilience-forums-december-2019-ew-buc
lrf_shp <- read_sf("https://opendata.arcgis.com/datasets/578216055f8c45a98f6d692856e0c03a_0.geojson")

# Save a shapefile that we'll join onto later
lrf_shp %>% 
  filter(str_sub(LRF19CD, 1, 1) == "E") %>%  # England only for now
  
  st_transform(crs = 4326) %>% 
  select(LRF19CD, LRF19NM) %>% 
  write_sf("data/lrf.shp")

