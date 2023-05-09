library(tidyverse)
library(magrittr)
library(sf)
library(stars)
library(metR)
library(pals)
library(ggnewscale)
library(extrafont)
library(ggpubr)

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

landuse <- read_csv2("tofte_landuse.csv", locale = locale(decimal_mark = ",", grouping_mark = ".")) %>% 
  select(x,y,type) %>% 
  st_as_stars(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(4326))
fence <- read_sf("../data/Tofte_hegn.shp")
tracks <- read_delim("../data/Wildboar_gpstracks.csv", delim = ";", locale = locale(decimal_mark = ".", grouping_mark = ","))

track_int <- landuse %>% 
  st_as_sf %>% 
  st_intersects(
    tracks %>% 
      select(Latitude, Longitude) %>% 
      st_as_sf(coords = c("Longitude", "Latitude")) %>%
      st_set_crs(st_crs(4326))
  ) 

in_fence <- landuse %>% 
  st_as_sf %>% 
  st_intersects(fence) %>% 
  sapply(is_empty) 

landuse_int <- landuse %>% 
  as_tibble %>%
  filter(!is.na(type)) %>% 
  mutate(n = track_int %>% 
           sapply(function(x) if (is_empty(x)) 0 else length(x)))

firstDensityPlot <- ggplot() +
  geom_tile(
    data = landuse %>% 
      as_tibble,
    aes(x,y, fill=type, colour = type)
  ) +
  scale_fill_manual(values = pals::stepped()[c(5,6,18,9,10,13,11,12,7,14,15,22)], na.value = "transparent") +
  scale_colour_manual(values = pals::stepped()[c(5,6,18,9,10,13,11,12,7,14,15,22)], na.value = "transparent") +
  geom_sf(data = fence, color = "black", fill = "transparent", linewidth = .75) +
  geom_tile(
    data = landuse_int,
    aes(x,y, alpha = n),
    fill = "firebrick2"
  ) +
  scale_alpha_continuous(trans = "log10", range = c(0.2, 0.95)) + 
  coord_sf(expand = F) +
  labs(x = NULL, y = NULL, fill = "Land Type", alpha = "# GPS-fixes\nin\ngrid cell") +
  guides(colour = guide_none())

ggsave("firstDensityPlot.pdf", firstDensityPlot,
       device = cairo_pdf, width = 4, height = 4, scale = 2, dpi = 400)
ggsave("firstDensityPlot.png", firstDensityPlot,
       width = 4, height = 4, scale = 2, dpi = 600)


landuse_hist <- landuse_int %>% 
  filter(!in_fence) %>%
  mutate(type = str_replace(type, ", ", "\n")) %>% 
  ggplot(aes(n)) +
  geom_histogram(bins = 35,
                 color = "gray55",
                 fill = "gray55") +
  scale_y_log10() +
  coord_cartesian(expand = F) +
  facet_wrap(~type, ncol = 1, switch = "y", scales = "free_y") +
  theme(aspect.ratio = .1, 
        strip.text = element_text(size = 12)) +
  labs(x = "# GPS-fixes in grid cell", y = "Number of cells in land type")


ggsave("landuse_hist.png", landuse_hist,
       width = 6, height = 6, dpi = 400, scale = 2.5)


pa_gps_plt <- landuse_int %>% 
  filter(!in_fence) %>% 
  group_by(type) %>% 
  summarize(
    Present = mean(n != 0),
    Absent = 1 - Present) %>% 
  pivot_longer(c(Present, Absent), names_to = "presence", values_to = "prop") %>% 
  ggplot(aes(prop, type, fill = presence)) +
  geom_col(position = "stack", color = "black") +
  scale_x_continuous(expand = expansion(),
                     labels = scales::label_percent()) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Proportion of pixels", y = NULL, fill = "Presence\n/Absence",
       title = "Presence/Absence of any\nGPS-fixes in different habitats")


ggsave("pa_gps.pdf", pa_gps_plt,
       device = cairo_pdf, width = 4, height = 3, dpi = 400, scale = 2.5)
