library(readr)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(ggpubr)

theme_set(
  theme_pubr(legend = "right",
             base_family = "Times New Roman") +
    theme(
      strip.text.x = element_text(face = "bold", size = 12)
    )
)

birds_weather_obs_data <- read_delim("BirdAssignment/combined_weather_and_observations.csv", ";",
                                     locale = locale(
                                       decimal_mark = ",", grouping_mark = "."
                                     ))

birds_weather_obs_data %>% 
  mutate(blishøne_binary = 1 * (Blishøne > 0)) %>% 
  mutate(grønbenet_rørhøne_binary = 1 * (`Grønbenet rørhøne` > 0)) %>% 
  select(c(`Area name`, grønbenet_rørhøne_binary, blishøne_binary)) %>% 
  mutate(`Area name` = factor(str_remove(`Area name`, " *[:digit:]$"))) %>% 
  pivot_longer(!`Area name`, names_to = "bird_species", values_to = "observed") %>% 
  mutate(bird_species = if_else(bird_species == "grønbenet_rørhøne_binary", "G. chloropus", "F. atra")) %>% 
  ggplot(aes(bird_species, observed, fill = `Area name`)) +
  #geom_violin(scale = "width") 
  stat_summary(
    fun.data = mean_se,
    geom = "col",
    position = position_dodge(1),
    key_glyph = draw_key_point
  ) +
  stat_summary(
    fun.data = mean_se,
    position = position_dodge(1),
    show.legend = F
  ) +
  scale_fill_brewer(labels = c("Shore", "Feeding corner", "Mowed park area", "Island", "Lake"),
                    palette = "Dark2") +
  scale_y_continuous(expand = expansion(add = c(0, .1)),
                     labels = scales::label_percent()) +
  facet_wrap(~bird_species, scales = "free") +
  labs(fill = "Habitat type", x = NULL, y = "Oberservation probability") +
  guides(fill = guide_legend(override.aes = list(shape = 21,
                                                 size = 5))) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(face = "bold"),
        aspect.ratio = 1.25)
  

birds_weather_obs_data %>% 
  mutate(blishøne_binary = 1 * (Blishøne > 0)) %>% 
  mutate(grønbenet_rørhøne_binary = 1 * (`Grønbenet rørhøne` > 0)) %>% 
  select(c(`Area name`, grønbenet_rørhøne_binary, blishøne_binary)) %>% 
  mutate(`Area name` = str_remove(`Area name`, " *[:digit:]$")) %>% 
  pivot_longer(!`Area name`, names_to = "bird_species", values_to = "observed") %>% 
  filter(bird_species == "grønbenet_rørhøne_binary") %>% 
  glm(observed ~ `Area name`, data = ., family = "binomial") %>% 
    broom::tidy() %>% 
  kable("html", digits = 3) %>% 
  kable_paper(full_width = F) %>% 
  row_spec(0, bold = T, color = "black")


# Blishøne områdebrug
birds_weather_obs_data %>% 
  mutate(blishøne_binary = 1 * (Blishøne > 0)) %>% 
  mutate(grønbenet_rørhøne_binary = 1 * (`Grønbenet rørhøne` > 0)) %>% 
  select(c(`Area name`, grønbenet_rørhøne_binary, blishøne_binary)) %>% 
  mutate(`Area name` = str_remove(`Area name`, " *[:digit:]$")) %>% 
  pivot_longer(!`Area name`, names_to = "bird_species", values_to = "observed") %>% 
  filter(bird_species == "blishøne_binary") %>% 
  glm(observed ~ `Area name`, data = ., family = "binomial") %>% 
  broom::tidy() %>% 
  kable("html", digits = 3) %>% 
  kable_paper(full_width = F) %>% 
  row_spec(0, bold = T, color = "black") %>% 
  add_footnote("Parameter summary table for binomial GLM investigating mean differences of F. Atra counts in different zones of Universitetsparken. The GLM revealed no differences in mean counts in the different zones", 
               notation = "number") 

library(kableExtra)

birds_weather_obs_data %>% 
  mutate(blishøne_binary = 1 * (Blishøne > 0)) %>% 
  mutate(grønbenet_rørhøne_binary = 1 * (`Grønbenet rørhøne` > 0)) %>% 
  #mutate(weather_status = if_else(Weather == "Fair", "Fair", "Not_fair")) %>% 
  select(c(Weather, `Grønbenet rørhøne`, `Blishøne`)) %>% 
  pivot_longer(!Weather, names_to = "bird_species", values_to = "observed") %>% 
  filter(bird_species == "Blishøne") %>% 
  mutate(
    Weather = Weather %>% 
      factor(levels = c(
        "Fair", "Fog", "Light Rain", "Cloudy", "Mostly Cloudy"
      ))
  ) %>% 
  glm(observed ~ Weather, data = ., family = "poisson") %>% 
  broom::tidy() %>% 
  kable("html", digits = 3) %>% 
  kable_paper(full_width = F) %>% 
  row_spec(0, bold = T, color = "black")
  







  
