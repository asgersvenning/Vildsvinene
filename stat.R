library(readr)
library(tidyverse)
library(sf)

birds_weather_obs_data <- read_delim("BirdAssignment/combined_weather_and_observations.csv", ";",
                                     locale = locale(
                                       decimal_mark = ",", grouping_mark = "."
                                     ))

areas <- read_sf("BirdAssignment/combined_weather_and_observations.shp") %>% 
  group_by(area_nm) %>% 
  summarize(do_union = FALSE) %>% 
  select(area_nm) %>% 
  rename("Area name" = "area_nm") %>% 
  mutate(area = st_area(.))



birds_weather_obs_data %>% 
  pivot_longer(c(Blishøne, `Grønbenet rørhøne`), names_to = 'Birds', values_to = 'bird_count') %>% 
  ggplot(aes(`Census round`, bird_count, fill = Birds)) + 
  geom_col(position = position_dodge(1, "total"))


modelling_data <- birds_weather_obs_data %>% 
  mutate(
    geometry = map(`Area name`, function(x) {
      if (x == "Fodringshjørne") return(NA)
      read_sf(paste0("BirdAssignment/data/layer_", str_remove(x, "areal"), ".shp")) %>% 
        st_combine
    })
  ) %>% 
  # pull(geometry) %>% 
  # first %>% 
  # st_area
  mutate(
    area = map_dbl(geometry, function(x) if (is.na(x)) NA else st_area(x))
  ) %>% 
  mutate(
    across(7:11, ~ .x / ifelse(!is.na(area), area, 1))
  ) %>% 
  select(
    c(Temperature, `Wind speed`, Weather, `Area name`, `Interacting humans`, `Non-interacting humans`, Dogs, Blishøne, `Grønbenet rørhøne`)
  ) 
  
modelling_data %>% 
  pivot_longer(!c(`Grønbenet rørhøne`, `Area name`, Weather)) %>% 
  filter(`Area name` != "Fodringshjørne") %>% 
  ggplot(aes(value, `Grønbenet rørhøne`, color = `Area name`, shape = Weather)) +
  geom_point() +
  facet_wrap(~name, scales = "free")

modelling_data %>% 
  select(!c(Temperature, `Wind speed`, Weather)) %>% 
  mutate(`Area name` = str_remove(`Area name`, " *[:digit:]$")) %>% 
  group_by(`Area name`) %>% 
  # summarise(
  #   across(everything(), mean),
  #   .groups = "drop"
  # ) %>% 
  filter(`Area name` != "Fodringshjørne") %>% 
  pivot_longer(!`Area name`, names_to = "Class", values_to = "count") %>% view
  ggplot(aes(count, Class, fill = `Area name`)) +
  # geom_violin(scale = "width") 
  stat_summary(
    fun.data = mean_se,
    geom = "col",
    position = position_dodge(1)
  ) +
  stat_summary(
    fun.data = mean_se,
    position = position_dodge(1)
  )

library(emmeans)
library(mgcv)
conflicted::conflict_prefer_all("dplyr")
modelling_data %>% 
  select(!c(Temperature, `Wind speed`, Weather)) %>% 
  mutate(`Area name` = str_remove(`Area name`, " *[:digit:]$")) %>% 
  filter(`Area name` != "Fodringshjørne") %>% 
  pivot_longer(!`Area name`, names_to = "Class", values_to = "count") %>% 
  filter(Class == "Grønbenet rørhøne") %>% 
  rename("AreaName" = `Area name`) %>% 
  mutate(
    residuals = gam(list(count ~ AreaName, ~ AreaName), 
                    data = ., family = "gaulss") %>%
      residuals(type = "deviance")
  ) %>% 
  ggplot(aes(AreaName, residuals, color = AreaName)) +
  geom_jitter(width = .1)
  
  
  emmeans(pairwise ~ `Area name`, adjust = "holm", type = "resp")
  broom::tidy() %>% 
  ggplot(aes(estimate, term, 
             xmin = estimate - std.error * 1.94, xmax = estimate + std.error * 1.94,
             label = scales::label_pvalue()(p.value))) +
  geom_col() +
  geom_pointrange() +
  geom_label(nudge_x = .1)
  

  
  
  
  
  
  
  
  
  
  
  
  

