library(tidyverse)
library(magrittr)
library(lubridate)
library(sf)
library(stars)
library(metR)
library(pals)
library(ggnewscale)
library(extrafont)
library(ggpubr)

library(ctmm)

theme_set(
  theme_pubr(legend = "right",
             base_family = "CMU Serif") +
    theme(title = element_text(face = "bold",
                               size = 16),
          strip.text = element_text(face = "bold",
                                    size = 16),
          plot.title = element_text(face = "plain",
                                    size = 20,
                                    hjust = .5),
          legend.title = element_text(hjust = .5),
          aspect.ratio = 1)
)

setwd("Final_project")

tracks <- read_delim("../data/Wildboar_gpstracks.csv", 
                     delim  = ";", 
                     locale = locale(decimal_mark = ".", 
                                     grouping_mark = ","))

tracks_telemetry <- tracks %>% 
  filter(Latitude > 56 & between(Longitude, 10.1, 10.4)) %>% 
  mutate(timestamp = as_datetime(UTC_Date, format = "%d-%m-%Y", "utc") + UTC_Time) %>% 
  rename("location.lat" = "Latitude", "location.long" = "Longitude", "individual.local.identifier" = "UniqueID") %>% 
  as.telemetry(tz = "utc") 

tracks_telemetry <- tracks_telemetry %>% 
  extract(1)

ctmm_fit <- lapply(tracks_telemetry, function(x) ctmm.fit(x, ctmm.guess(x, interactive = F)))

akde_fit <- akde(tracks_telemetry, ctmm_fit$`1_11855`)

akde_bbox <- function(x) {
  x@info$projection %>%
    str_extract_all("(?<=\\+(lon|lat)_\\d=)[\\d\\.]+") %>% 
    unlist %>% 
    as.numeric() %>% 
    set_names(c("ymin", "xmin", "ymax", "xmax")) %>% 
    st_bbox %>% 
    st_set_crs(st_crs("EPSG:4326"))
}

st_as_stars.akde <- function(x) {
  bbox <- akde_bbox(x)
  dims <- dim(x$CDF)
  
  st_as_stars(
    bbox,
    nx = dims[1],
    ny = dims[2]
  ) %>% 
    mutate(
      PDF = as.vector(x$PDF),
      CDF = as.vector(x$CDF)
    ) %>% 
    select(!values)
}

library(metR)

akde_fit[[1]] %>% 
  st_as_stars.akde() %>%
  st_transform(st_crs("EPSG:25832")) %>% 
  st_warp(st_as_stars(st_bbox(.), dx = 100, dy = 100, pretty = T)) %>% 
  st_as_sf %>% 
  st_set_geometry(st_centroid(st_geometry(.))) %>%
  as_tibble %>% 
  mutate(geometry = st_coordinates(geometry) %>% 
           set_colnames(c("lat", "lon")) %>% 
           as_tibble) %>% 
  # unnest(geometry) %>% 
  # pull("lon") %>% 
  # table
  # {ggplot2:::xyz_to_isolines(., ggplot2:::contour_breaks(.$CDF))}
  ggplot(aes(x = lat, y = lon, z = CDF)) +
  stat_contour()
  # geom_tile(width = 0.0014, height = 0.0002) +
  scale_fill_viridis_c(trans = "identity") 
  # coord_sf(expand = F)
