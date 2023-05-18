library(tidyverse)
library(magrittr)
library(sf)
library(stars)
library(metR)
library(pals)
library(ggnewscale)
library(extrafont)
library(ggpubr)
library(furrr)

source("plotHelpers.R")

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
tracks <- read_delim(
  file = "../data/Wildboar_gpstracks.csv", 
  delim = ";", 
  locale = locale(decimal_mark = ".", grouping_mark = ",")) %>% 
  mutate(UTC_Date = as.Date(UTC_Date, format = "%d-%m-%Y"),
         UniqueID = factor(UniqueID))

in_fence <- landuse %>% 
  st_as_sf %>% 
  st_intersects(fence) %>% 
  sapply(is_empty) 

# Old helper code for temporal bootstrapping
######
# blocked_resamp <- function(df, block_col, block_width) {
#   cur_group <- df %>% 
#     mutate(
#       grp = names(cur_group())
#     ) %>% 
#     pull(grp) %>% 
#     first
#   
#   df %>% 
#     ungroup %>% 
#     mutate(
#       internalBlockColumn = cut_width(pick(any_of(block_col))[[1]], block_width, boundary = 0)
#     ) %>% 
#     nest(internalBlockData = !c(internalBlockColumn, all_of(cur_group))) %>% 
#     mutate(internalResampleCount = map_int(internalBlockData, nrow),
#            internalBlockData = map2(internalBlockData, internalResampleCount, function(d, n) {
#              d %>% 
#                slice_sample(n = n, replace = T)
#            })) %>% 
#     unnest(internalBlockData) %>% 
#     select(!c(internalBlockColumn, internalResampleCount)) %>% 
#     group_by(across(all_of(cur_group)))
# }
######

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

landuse_fence_sf <- st_as_sf(landuse_fence) %>% 
  mutate(
    type = factor(ifelse(is.na(type), "Missing", as.character(type)))
  )

expectedFrequency <- landuse_fence_sf %>% 
  st_drop_geometry() %>% 
  as_tibble %>% 
  filter(in_fence) %>% 
  count(type) %>% 
  mutate(n = n/sum(n)) %>% 
  {set_names(.$n, .$type)}

tracks_hab_class <- st_intersects(tracks %>%
                                    st_as_sf(coords = c("Longitude", "Latitude")) %>%
                                    st_set_crs(st_crs(4326)),
                                  landuse_fence_sf) %>% 
  sapply(function(x) if (is_empty(x)) NA else x) %>% 
  {landuse_fence_sf$type[.]} %>% 
  {mutate(tracks, type = .)} 

write_csv2(tracks_hab_class, "tracks_hab_class.csv")

tracks_hab_class_blocks <- tracks_hab_class %>%
  mutate(
    time = as.numeric(UTC_Time) / 86400 + as.numeric(as.Date(UTC_Date, "%d-%m-%Y")),
    time_block = cut_width(time, 7, boundary = 0, dig.lab = 50),
    lat_block = cut_interval(Latitude, 1),
    lon_block = cut_interval(Longitude, 1),
    type = type %>% 
      replace_na("Missing") %>% 
      factor
  ) %>% 
  count(UniqueID, time_block, lat_block, lon_block, type) %>% 
  complete(UniqueID, time_block, lat_block, lon_block, type) %>% 
  group_by(UniqueID, time_block, lat_block, lon_block) %>% 
  filter(sum(n, na.rm = T) > 0) %>% 
  mutate(f = n / sum(n, na.rm = T),
         w = sum(n, na.rm = T)) %>% 
  ungroup %>% 
  mutate(
    time = str_extract_all(time_block, "\\d+", T) %>% 
      set_colnames(c("start", "end")) %>% 
      as_tibble
  ) %>% 
  unnest(time, names_sep = "_") %>% 
  rename("Habitat" = "type")

# Bootstrap
plan("multisession", workers = 6)
boot_habuse <- tibble(
  boot = seq(1, 10000, 1) %>% 
    factor
) %>% 
  mutate(
    res = future_map(boot, function(DUMMY) {
      tracks_hab_class_blocks %>% 
        nest(dat = !UniqueID) %>%
        slice_sample(n = nrow(.), replace = T) %>%
        mutate(dat = map(dat, ~ slice_sample(.x, n = nrow(.x), replace = T))) %>%
        unnest(dat) %>%
        group_by(Habitat) %>% 
        summarize(
          Frequency = mean(f, na.rm = T)
        ) %>%
        drop_na %>% 
        complete(Habitat) %>% 
        mutate(
          Frequency = Frequency / sum(Frequency, na.rm = T),
          Anomaly = Frequency - expectedFrequency[Habitat]
        ) 
    }, .progress = T, .options = furrr_options(seed = T))
  ) %>% #
  unnest(res) 
plan("sequential")
# Save bootstrap
saveRDS(boot_habuse, "boot_habuse.RDS")

# Plot
habuse_anomaly_plt <- boot_habuse %>% 
  mutate(Expected = Frequency - Anomaly) %>% 
  group_by(Habitat) %>%
  mutate(
    first = row_number() == first(which(!is.na(Frequency))),
    aM = mean(Anomaly, na.rm = T),
    fM = mean(Frequency, na.rm = T),
    aMax = max(Anomaly, na.rm = T),
    aMin = min(Anomaly, na.rm = T),
    Expected = 0
  ) %>% 
  mutate(
    pVal = (n() - max(table(factor(sign(Anomaly), c(-1, 1)))) + 1) / (n() + 1),
    pVal = pmin(1, pVal * length(levels(Habitat))),
    significant = factor(pVal <= 0.05, c(TRUE, FALSE)),
    pVal = map2_chr(pVal, first, function(p, f) {
      if(f) label_pvalue_manual(p, type = "richtext", digits = 3) else NA
      }),
    bw = bw.nrd0(Anomaly[!is.na(Anomaly)])
  ) %>%  
  ungroup %>% 
  ggplot(aes(Anomaly, Habitat, fill = significant)) +
  geom_violin(
    scale = "width", trim = F, linewidth = .5, adjust = 1, na.rm = T
  ) +
  ggtext::geom_richtext(
    aes(x = ifelse(first, aMax + bw * 5, NA),
        label = pVal),
    color = "transparent",
    fill = "white",
    label.padding = unit(c(0,.1,0,.1), "lines"),
    text.colour = "black",
    family = "Cambria",
    hjust = 0
  ) +
  geom_vline(xintercept = 0, color = "firebrick", linetype = "dashed", linewidth = .75) +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  scale_x_continuous(n.breaks = 10,
                     labels = scales::label_percent(),
                     expand = expansion(0, c(0, 0.1))) +
  coord_cartesian(clip = "off") +
  theme(aspect.ratio = .5,
        legend.position = "none",
        panel.grid.major.y = element_line(colour = "gray75", linetype = "solid", linewidth = .25),
        axis.title.x = element_text(margin = margin(.5,0,0,0,"cm"))) +
  labs(y = NULL, x = "Habitat Use Anomaly")

ggsave("habuse_anomaly.pdf", habuse_anomaly_plt,
       device = cairo_pdf,
       width = 8, height = 4, scale = 2)


habuse_frequency_plt <- boot_habuse %>% 
  ggplot(aes(Frequency, Habitat, fill = Habitat)) +
  geom_violin(
    scale = "width", trim = F, linewidth = .5, adjust = 1, na.rm = T
  ) +
  scale_fill_brewer(palette = "Set3", direction = 1) +
  scale_x_continuous(labels = scales::label_percent(),
                     expand = expansion(0, c(0, .05))) +
  coord_cartesian(clip = "off", expand = T) +
  theme(aspect.ratio = .5,
        legend.position = "none",
        panel.grid.major.y = element_line(colour = "gray75", linetype = "solid", linewidth = .25),
        axis.title.x = element_text(margin = margin(.5,0,0,0,"cm"))) +
  labs(x = "Habitat Use Frequency", y = NULL)

ggsave("habuse_frequency.pdf", habuse_frequency_plt,
       device = cairo_pdf,
       width = 8, height = 4, scale = 2)

habuse_comparison <- boot_habuse %>% 
  select(!c(Anomaly)) %>% 
  group_by(boot) %>% 
  arrange(Habitat) %>% 
  summarize(
    mat = set_names(Frequency, Habitat) %>% 
      outer(
        ., .,
        FUN = "-"
      ) %>%
      list
  ) %>% 
  mutate(mat = map(mat, function(m) {
    as.data.frame(m) %>% 
      rownames_to_column("H1") %>% 
      as_tibble %>% 
      pivot_longer(!H1, names_to = "H2", values_to = "diff")
  })) %>% 
  unnest(mat) %>% 
  group_by(H1, H2) %>% 
  summarize(
    mean = mean(diff, na.rm = T),
    median = median(diff, na.rm = T),
    lower = quantile(diff, .05, na.rm = T),
    upper = quantile(diff, .95, na.rm = T),
    pVal = (n() - max(table(factor(sign(diff), c(-1, 1)))) + 1) / (n() + 1),
    # significant = factor(pVal <= 0.05, c(TRUE, FALSE)),
    # pVal = if (!is.na(pVal)) label_pvalue_manual(p, type = "richtext", digits = 3) else NA,
    .groups = "drop"
  )

write_csv2(habuse_comparison, "habuse_comparison.csv")

habuse_comparison_table <- habuse_comparison %>% 
  filter(H1 != H2 & sign(mean) == 1) %>% 
  mutate(pVal = pmin(1, pVal * n())) %>% 
  filter(pVal <= 0.05) %>% 
  # distinct(comb = map2_chr(H1, H2, ~paste0(sort(c(.x,.y)), collapse=",")),.keep_all = T) %>% 
  # select(!comb) %>% 
  arrange(H1, H2) %>% 
  mutate(pVal = label_pvalue_manual(pVal, "\\star", type = "latex", digits = 3)) 

habuse_comparison_table %>% 
  # select(!H1) %>%
  mutate(H1 = "") %>% 
  kable(
    format = "latex", 
    digits = 3,
    col.names = paste0(
      "\\multicolumn{1}{c}{", 
      c("\\textbf{Habitat}",
        "\\textbf{Compared habitat}",
        "$\\mathbf{\\mu}_\\Delta$",
        "$\\mathbf{\\mathrm{Q}_{50\\%}}(\\Delta)$",
        "$\\mathbf{\\mathrm{Q}_{5\\%}}(\\Delta)$",
        "$\\mathbf{\\mathrm{Q}_{95\\%}}(\\Delta)$",
        "\\textbf{\\textsuperscript{*}P-value}"),
      "}"),
    align = "r", 
    booktabs = T, 
    escape = F) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, latex_column_spec = ">{\\\\centering\\\\arraybackslash}p{0cm}") %>% 
  column_spec(2, latex_column_spec = ">{\\\\raggedright\\\\arraybackslash}p{4cm}") %>% 
  column_spec(3:6, latex_column_spec = ">{\\\\centering\\\\arraybackslash}p{0.75cm}") %>%
  column_spec(7, latex_column_spec = ">{\\\\raggedleft\\\\arraybackslash}p{2.5cm}") %>%
  group_rows(
    index = habuse_comparison_table$H1 %>% 
      table
  ) %>% 
  write_lines("habuse_comparison.txt")

