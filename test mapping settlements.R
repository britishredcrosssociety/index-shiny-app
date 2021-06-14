library(tidyverse)
library(arrow)
library(leaflet)
library(maptiles)
library(sf)

# devtools::install_github("rstudio/leaflet.mapboxgl")
library(leaflet.mapboxgl)

lad_shp <- read_sf("data/lad.shp")
ri <- read_feather("data/resilience-index.feather")
ri_shp <- lad_shp %>% 
  left_join(ri, by = c("lad19cd" = "LAD19CD"))

# const lsoabldg = {
#   url: "https://cdn.ons.gov.uk/maptiles/buildings/v1/{z}/{x}/{y}.pbf",
#   layer: "buildings",
#   code: "lsoa11cd"
# };

osmnolbl <- list(src = 'osmnolabel',
                 q = 'https://{s}.tiles.wmflabs.org/osm-no-labels/{z}/{x}/{y}.png',
                 sub = c('a','b', 'c'), 
                 cit = '© OpenStreetMap contributors.')

# Define the tile server parameters
ons_tiles <- list(src = "ons",
                  q = 'https://cdn.ons.gov.uk/maptiles/t18/tiles/{z}/{x}/{y}.pbf',
                  sub = c('a','b', 'c'), 
                  cit = '© ONS')

nc_raw <- st_read(system.file("shape/nc.shp", package="sf"), 
                  quiet = TRUE)
# Project to EPSG:3857
nc <- st_transform(nc_raw, "EPSG:3857")

nc_ons <- get_tiles(x = nc, provider = osmnolbl, crop = TRUE, zoom = 10, verbose = TRUE)


options(mapbox.accessToken = NA)

leaflet(ri_shp,
        options = leafletOptions(minZoom = 5, maxZoom = 15, attributionControl = F)) %>%
  
  setView(lat = 54.00366, lng = -2.547855, zoom = 12) %>% # centre map on Whitendale Hanging Stones, the centre of GB: https://en.wikipedia.org/wiki/Centre_points_of_the_United_Kingdom

  addMapboxGL(style = "https://cdn.ons.gov.uk/maptiles/buildings/v1/{z}/{x}/{y}.pbf") %>% 
  
  addPolygons(fill = NA, color = "black")

  # addTiles(urlTemplate = "https://cdn.ons.gov.uk/maptiles/t18/tiles/{z}/{x}/{y}.pbf")
  
  
