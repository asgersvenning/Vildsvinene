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

tracks <- read_delim("../data/Wildboar_gpstracks.csv", delim = ";", locale = locale(decimal_mark = ".", grouping_mark = ","))

tracks_telemetry <- tracks %>% 
  filter(Latitude > 56 & between(Longitude, 10.1, 10.4)) %>% 
  mutate(timestamp = as_datetime(UTC_Date, format = "%d-%m-%Y", "utc") + UTC_Time) %>% 
  rename("location.lat" = "Latitude", "location.long" = "Longitude", "individual.local.identifier" = "UniqueID") %>% 
  as.telemetry(tz = "utc") 

tracks_telemetry <- tracks_telemetry %>% 
  extract(1:5)

ctmm_fit <- lapply(tracks_telemetry, function(x) ctmm.fit(x, ctmm.guess(x, interactive = F)))
