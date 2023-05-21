library(readr)
library(tidyverse)
library(geosphere)
library(broom)


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


bearing_step_length_data <- Wildboar_gpstracks %>% 
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
  mutate(step_length = abs(distance - lag(distance)))

## Creating a shared tibble of step_length and indivduals
tracks_hab_class <- read_delim("Final_project/tracks_hab_class.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

view(bearing_step_length_data)

tracks_hab_class %>% 
  arrange(UniqueID) %>% 
  arrange(UTC_Date, UTC_Time) %>% 
  mutate(direction_angle = bearing_step_length_data$bearing_diff,
         step_len = bearing_step_length_data$step_length,
         type = as.factor(type)) %>% 
  na.omit() %>% 
  select(c(type, step_len, direction_angle)) %>% 
  filter(step_len < 10000) %>% 
  #group_by(type) %>% 
  #summarize(mean_step_len = mean(step_len),
 #           mean_direction_angle = 180 - mean(direction_angle)) %>% 
  ggplot(aes(x = step_len, y = type)) + 
  geom_boxplot()
  
  






