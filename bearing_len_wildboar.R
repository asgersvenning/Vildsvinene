library(readr)
library(tidyverse)
library(geosphere)



Wildboar_gpstracks <- read_delim("data/Wildboar_gpstracks.csv", 
                                 delim = ";", 
                                 escape_double = FALSE, 
                                 trim_ws = TRUE)


calculate_bearing <- function(lat1, lon1, lat2, lon2) {
  y <- lat2 - lat1
  x <- lon2 - lon1
  bearing <- atan2(y, x) / (2 * pi) * 360
  return(bearing)
}


Wildboar_gpstracks %>% 
  arrange(UniqueID) %>% 
  group_by(UniqueID) %>% 
  mutate(UTC_Date = as.Date(UTC_Date, format = "%d-%m-%Y")) %>% 
  arrange(UTC_Date, UTC_Time) %>% 
  mutate(bearing = calculate_bearing(Latitude, 
                                     Longitude, 
                                     lag(Latitude, default = NA), 
                                     lag(Longitude, default = NA))) %>%
  select(c(Longitude, Latitude, UTC_Date, UTC_Time, bearing)) %>% 
  mutate(bearing_diff = abs(bearing - lag(bearing))) %>% 
  mutate(
    distance = distVincentySphere(cbind(Longitude, Latitude),
                                  cbind(lag(Longitude), lag(Latitude)))
  ) %>% 
  mutate(distance_diff = abs(distance - lag(distance))) %>% 
  filter(!(is.na(bearing))) %>% 
  view
  
