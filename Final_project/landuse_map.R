library(raster)
library(ggplot2)
library(tidyverse)
library(raster)
library(rasterVis)
library(rgdal)
library(stars)
library(metR)
library(RColorBrewer)
library(pals)

setwd("Final_project")

land_code_dict <- read_file("classes_landuse") %>% 
  str_split("\n") %>% 
  unlist %>% 
  str_split_fixed("(?<=\\d)\\s|(?<=[a-z])\\s(?=[A-Z])", 3) %>% 
  as_tibble() %>% 
  rename("Code" = "V1",
         "Name" = "V2",
         "Description" = "V3") %>% 
  nest(data = !Code) %>% 
  # mutate(data = map(data, as.list)) %>% 
  {set_names(.$data, .$Code)}

str_name<-'lu_agg_2021.tif' 
areause_raster=read_stars(str_name) 

tofte_bbox <- c(xmin = 10.1315,
                xmax = 10.3266,
                ymin = 56.8,
                ymax = 56.9) %>%
  st_bbox(crs = 4326) %>% 
  st_as_sfc %>% 
  st_transform(st_crs(areause_raster)) %>% 
  st_bbox

areause_raster <- areause_raster %>% 
  st_crop(tofte_bbox)

combine_class_dict <- c(
  "Building" = "Artificial",
  "Other built up" = "Artificial",
  "Industry/business" = "Artificial",
  "Resource extraction" = "Artificial",
  "Road, paved" = "Artificial",
  "Low built up" = "Artificial",
  "Recreation area/sports ground" = "Artificial",
  "Agriculture, intensive, temporary crops" = "Agriculture, intensive",
  "Agriculture, intensive, permanent crops" = "Agriculture, intensive",
  "Agriculture, extensive" = "Agriculture, extensive",
  "Agriculture, not classified" = "Agriculture, extensive",
  "Unmapped" = NA
)

simplify_classes <- Vectorize(function(cls) {
  # if (cls == "Nature, wet; Agriculture, extensive") return("Agriculture, extensive")
  cls <- str_extract(cls, ".+(?=;)|.+")
  cls[cls %in% names(combine_class_dict)] <- combine_class_dict[cls[cls %in% names(combine_class_dict)]]
  
  cls
}, USE.NAMES = F, SIMPLIFY = T)

empty_tofte_grid <- areause_raster %>% 
  stars::st_downsample(1) %>%
  st_transform(st_crs(4326)) %>% 
  st_bbox() %>% 
  st_as_stars(nx = 605, ny = 567, values = 0)

tofte_landuse <- areause_raster %>% 
  stars::st_downsample(1) %>%
  st_transform(st_crs(4326)) %>% 
  st_as_sf %>% 
  st_rasterize(empty_tofte_grid) %>% 
  as_tibble() %>% 
  # head %>% 
  rename("land_code" = 3) %>%
  mutate(land_code = format(land_code, digits = 20)) %>% 
  mutate(data = land_code_dict[land_code]) %>% 
  unnest(data) %>% 
  mutate(type = simplify_classes(Name)) 

tofte_landuse %>% 
  # slice_sample(n = 1000) %>% 
  ggplot(aes(x,y,fill=type)) +
  geom_tile() +
  scale_fill_manual(values = pals::stepped()[c(5,6,2,9,10,13,11,12,7,14,15,22)]) +
  scale_x_continuous(labels = LonLabel) +
  scale_y_continuous(labels = LatLabel) +
  coord_sf(expand = F) +
  theme(aspect.ratio = 1)

tofte_landuse %>% 
  write_csv2("tofte_landuse.csv")


  
