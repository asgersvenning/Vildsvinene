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
  theme() +
    theme_pubr(
      legend = "right",
      base_family = "CMU Serif"
    ) +
    theme(
      title = element_text(face = "bold",
                           size = 16),
      strip.text = element_text(face = "bold",
                                size = 16),
      plot.title = element_text(face = "plain",
                                size = 20,
                                hjust = .5),
      legend.title = element_text(hjust = .5),
      aspect.ratio = 1
    )
)

setwd("Final_project")

landuse <- read_csv2("tofte_landuse.csv", locale = locale(decimal_mark = ",", grouping_mark = ".")) %>% 
  select(x,y,type) %>% 
  st_as_stars(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(4326))

fence <- read_sf("../data/Tofte_hegn.shp")
tracks <- read_delim("../data/Wildboar_gpstracks.csv", delim = ";", locale = locale(decimal_mark = ".", grouping_mark = ","))

track_int <- landuse %>% 
  # st_as_sf %>% 
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
  mutate(
    n = track_int %>% 
      sapply(function(x) if (is_empty(x)) 0 else length(x))
  )


blocked_resamp <- function(df, block_col, block_width) {
  cur_group <- df %>% 
    mutate(
      grp = names(cur_group())
    ) %>% 
    pull(grp) %>% 
    first
  
  df %>% 
    ungroup %>% 
    mutate(
      internalBlockColumn = cut_width(pick(any_of(block_col))[[1]], block_width, boundary = 0)
    ) %>% 
    nest(internalBlockData = !c(internalBlockColumn, all_of(cur_group))) %>% 
    mutate(internalResampleCount = map_int(internalBlockData, nrow),
           internalBlockData = map2(internalBlockData, internalResampleCount, function(d, n) {
             d %>% 
               slice_sample(n = n, replace = T)
           })) %>% 
    unnest(internalBlockData) %>% 
    select(!c(internalBlockColumn, internalResampleCount)) %>% 
    group_by(across(all_of(cur_group)))
}

stars_intersect <- function(df, stars) {
  stars %>% 
    mutate(
      n = st_intersects(
        .,
        df %>% 
          select(Latitude, Longitude) %>% 
          st_as_sf(coords = c("Longitude", "Latitude")) %>%
          st_set_crs(st_crs(4326)),
        as_points = F
      ) %>% 
        map_int(~ if (is_empty(.x)) 0 else length(.x))
    )
}

landuse_fence <- landuse %>% 
  mutate(
    in_fence = st_intersects(
      .,
      fence,
      as_points = F
    ) %>% 
      map_lgl(is_empty) %>% 
      not
  )


boot_habuse <- tibble(
  boot = seq(1, 100, 1) %>% 
    factor
) %>% 
  mutate(
    res = map(boot, function(DUMMY) {
      tracks %>% 
        mutate(time = as.numeric(UTC_Time) / 86400 + as.numeric(as.Date(UTC_Date, "%d-%m-%Y"))) %>%
        group_by(UniqueID) %>% 
        blocked_resamp("time", 7) %>% 
        ungroup %>% 
        stars_intersect(landuse_fence) %>% 
        as_tibble %>% 
        filter(in_fence) %>% 
        group_by(type) %>% 
        summarize(
          n_sum = sum(n),
          n_mean = mean(n),
          presence_prop = mean(n > 0)
        )
    })
  )

boot_habuse %>% 
  unnest(res) %>% 
  ggplot(aes(n_mean, type)) +
  geom_violin(scale = "width") +
  scale_x_log10() + 
  theme(aspect.ratio = .5)


boot_habuse %>% 
  unnest(res) %>% 
  select(boot, type, n_mean) %>% 
  pivot_wider(
    id_cols = boot, names_from = type, values_from = n_mean
  ) %>% 
  select(!boot) %>% 
  transmute(
    comp_mat = across(everything()) %>% 
      apply(1, function(x) outer(x,x,FUN = "-"), simplify = F)
  ) %>% 
  pull(comp_mat) %>% 
  map(function(m) {
    m[lower.tri(m)] <- NA
    as.data.frame(m) %>% 
      rownames_to_column("left_hab") %>% 
      as_tibble %>% 
      pivot_longer(!left_hab, names_to = "right_hab", values_to = "diff") %>% 
      drop_na
  }) %>% 
  bind_rows %>% 
  mutate(
    left_hab = factor(left_hab, unique(left_hab)),
    right_hab = factor(right_hab, levels(left_hab))
  ) %>% 
  ggplot(aes(left_hab, right_hab, z = diff)) +
  stat_summary_2d(
    fun = ~summary(lm(.x ~ 1))$coefficients[4]
  ) +
  # scale_fill_brewer(palette = "Set1") +
  scale_fill_viridis_c(trans = "log10", option = "A") +
  scale_x_discrete(expand = expansion(0, .5)) +
  scale_y_discrete(expand = expansion(0, .5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
