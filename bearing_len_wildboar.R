library(readr)
library(tidyverse)
library(magrittr)
library(dplyr)
library(geosphere)
library(broom)
library(units)
library(mgcViz)
library(extrafont)
library(kableExtra)
source("statistical_helpers.R")
source("plotHelpers.R")
library(patchwork)
library(furrr)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")

calculate_bearing <- function(lat1, lon1, lat2, lon2) {
  y <- lat2 - lat1
  x <- lon2 - lon1
  bearing <- atan2(y, x) / (2 * pi) * 360
  return(bearing)
}

continuous_blocks <- function(n, width) {
  current_block <- 1
  blocks <- vector("integer", length(n))
  for (i in 1:(length(n) - 1)) {
    blocks[i] <- current_block
    if ((n[i + 1] - n[i]) > width) {
      current_block <- current_block + 1
    }
  }
  blocks[length(n)] <- current_block
  
  blocks
}

angle_three <- function(x,y, type = "degrees") {
  a <- c(x[1], y[1])
  b <- c(x[2], y[2])
  c <- c(x[3], y[3])
  
  ab <- b - a
  bc <- c - b
  
  type <- pmatch(type, c("degrees", "radians", "cosine", "turning"))
  
  out <- sum(ab * bc) / (sqrt(sum(ab^2)) * sqrt(sum(bc^2)))
  if (type == 4) return(1 - ((out + 1) / 2))
  if (type == 3) return(out)
  out <- acos(out)
  if (type == 2) return(out)
  if (type == 1) return(out / (2 * pi) * 360)
  stop("type must be one of degrees, radians, cosine or turning, or an abbreviation of these.")
}

angle_three_vec <- function(x, y, ...) {
  angles <- vector("double", length(x))
  for (i in 1:length(x)) {
    if (i == 1 | i == length(x)) {
      angles[i] <- NA
    }
    else {
      angles[i] <- angle_three(x[(i - 1):(i + 1)], y[(i - 1):(i + 1)], ...)
    }
  }
  angles
}

tracks_hab_class <- read_delim("Final_project/tracks_hab_class.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE,
                               locale = locale(decimal_mark = ",", grouping_mark = ".")) %>% 
  mutate(UTC_Date = as.Date(UTC_Date, format = "%d-%m-%Y")) %>% 
  arrange(UniqueID, UTC_Date, UTC_Time) %>% 
  group_by(UniqueID) %>% 
  arrange(Timestamp) %>% 
  mutate(
    block = continuous_blocks(as.numeric(Timestamp), 3000)
  ) %>% 
  group_by(UniqueID, block) %>% 
  slice_head(n = 1) %>% 
  ungroup %>% 
  rename("Habitat" = "type") %>% 
  select(!block) %>% 
  drop_na

tracks_turning <- tracks_hab_class %>% 
  group_by(UniqueID) %>% 
  arrange(Timestamp) %>% 
  mutate(
    turning = angle_three_vec(Longitude, Latitude, "turning"),
    length = distVincentySphere(
      cbind(Longitude, Latitude),
      cbind(lag(Longitude), lag(Latitude))
    ),
    deltaTime = (Timestamp - lag(Timestamp)) %>% 
      as_units %>% 
      set_units(s) %>% 
      drop_units,
    velocity = length / deltaTime,
    time = Timestamp
  ) %>% 
  ungroup %>% 
  filter(is.finite(length)) %>% 
  drop_na %>% 
  mutate(
    day = lubridate::yday(Timestamp) + lubridate::year(Timestamp) / 365.25,
    hour = as.numeric(hms::as_hms(Timestamp)) / 86400 * 24,
    UniqueID = factor(UniqueID),
    Habitat = factor(Habitat),
    Sex = factor(Sex)
  ) %>% 
  arrange(Timestamp)

turning_trans <- beta_trans(tracks_turning$turning, threshold = c(1e-09, 1 - 1e-09))
oldPar <- par(mfrow = c(1,2))
hist(turning_trans$transform(tracks_turning$turning),100)
curve(turning_trans$transform(x),0,1)
par(oldPar)

# velocity_trans <- dist_trans(tracks_turning$velocity, dist = "gamma", start = list(shape = -1, rate = .01))
# oldPar <- par(mfrow = c(1,2))
# hist(velocity_trans$transform(tracks_turning$velocity),100)
# curve(velocity_trans$transform(x),0,max(tracks_turning$velocity))
# par(oldPar)

turning_velocity_hist <- tracks_turning %>% 
  select(turning, velocity) %>% 
  rename_with(~ifelse(. == "turning", "\\textbf{Turning} ($\\alpha$)", "\\textbf{Velocity} ($m/s$)") %>% 
                latex2exp::TeX(output = "character")) %>%
  pivot_longer(everything(), names_to = "trait") %>% 
  mutate(
    transformed = ifelse(
      str_detect(trait, "Turning"),
      turning_trans$transform(value),
      log10(value)
    )
  ) %>% 
  rename("raw" = "value") %>% 
  pivot_longer(!trait, names_to = "type") %>% 
  nest(dat = !type) %>% 
  mutate(
    plt = map2(type, dat, function(t, d) {
      d %>% 
        group_by(trait) %>% 
        mutate(value = if (t == "transformed") as.vector(scale(value)) else value) %>% 
        ggplot(aes(value)) +
        geom_histogram(color = "gray35",
                       bins = 200) +
        facet_wrap(~trait, nrow = 1,
                   scales = "free_x", labeller = label_parsed) +
        coord_cartesian(expand = F) + 
        labs(x = NULL) +
        theme(panel.spacing.x = unit(10, "mm"),
              strip.background = if (t == "transformed") element_blank() else element_rect(),
              strip.text = if (t == "transformed") element_blank() else element_text())
    })
  ) %>% 
  pull(plt) %>% 
  wrap_plots(nrow = 2) +
  plot_annotation(
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  )

ggsave("turning_velocity_transformed_hist.pdf", turning_velocity_hist,
       device = cairo_pdf,
       width = 4, height = 4, scale = 2)

velocity_turning_scaled <- tracks_turning %>% 
  select(velocity, turning) %>% 
  mutate(velocity = log10(velocity),
         turning = turning_trans$transform(turning)) %>%
  mutate(across(everything(), scale))

velocity_turning_pca <- velocity_turning_scaled %>% 
  mutate(across(everything(), as.vector)) %>% 
  princomp

velocity_turning_pca_vectors <- velocity_turning_pca$loadings %>% 
  unclass() %>% 
  t %>% 
  multiply_by(velocity_turning_pca$sdev^2) %>% 
  as.data.frame() %>% 
  rownames_to_column("variable") %>% 
  as_tibble %>% 
  mutate(
    across(!variable, ~ tibble(
      end = (.x * attr(velocity_turning_scaled[[cur_column()]], "scaled:scale")) + attr(velocity_turning_scaled[[cur_column()]], "scaled:center"),
      start = attr(velocity_turning_scaled[[cur_column()]], "scaled:center")
    ))
  ) %>% 
  unnest(everything(), names_sep = "_") %>% 
  mutate(
    velocity_end_adj = velocity_end + 0 * (velocity_end - velocity_start),
    turning_end_adj  = turning_end  + 0.5 * (turning_end  - turning_start)
  ) %>%
  mutate(
    across(contains("velocity"), ~10^.x),
    across(contains("turning"), ~turning_trans$inverse(.x))
  ) %>%
  mutate(
    sdev = velocity_turning_pca$sdev,
    variance = sdev^2,
    explained = variance/sum(variance),
    variable = str_replace(variable, "^Comp\\.", "PC")
  ) 


velocity_turning_plt <- tracks_turning %>% 
  ggplot(aes(velocity, turning)) +
  geom_hex(
    aes(color = after_scale(colorspace::darken(fill, .01, "absolute"))),
    bins = 25,
    linewidth = .05
  ) +
  geom_segment(
    aes(x = velocity_start, xend = velocity_end,
        y = turning_start, yend = turning_end),
    data = velocity_turning_pca_vectors,
    color = "white",
    inherit.aes = F,
    arrow = arrow(angle = 30, length = unit(6, "mm"), type = "open"),
    linewidth = 1.5
  ) +
  geom_label(
    aes(x = velocity_end_adj,
        y = turning_end_adj,
        label = paste0(variable, " (", scales::label_percent(.1)(explained), ")")),
    data = velocity_turning_pca_vectors,
    label.padding = unit(2.25, "mm"),
    color = "white",
    fill = "#00000099",
    fontface = "bold",
    vjust = 0.3,
    size = 6,
    inherit.aes = F
  ) +
  scale_y_continuous(trans = turning_trans,
                     n.breaks = 10,
                     labels = ~label_prettify_scientific_beta(T, 2, c(0, 0))(print(.))) +
  scale_x_log10(labels = prettify_scientific) +
  scale_fill_viridis_c(
    trans = abslog(10),
    option = "A",
    limits = c(0, 1000),
    labels = label_prettify_scientific(T, 2, baseRange = c(-1,1)),
    n.breaks = 8,
    guide = guide_colorbar()
  ) +
  coord_cartesian(expand = F) +
  ggpubr::theme_pubr(
    base_family = "CMU Serif",
    legend = "right"
  ) +
  theme(
    aspect.ratio = 1,
    title = element_text(face = "bold",
                         size = 14),
    plot.background = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    legend.key.height = unit(3, "cm")
  ) +
  labs(fill = "Count", x = "Velocity (m/s)", y = "Turning")

ggsave("velocity_turning_pca.pdf", align_legend(velocity_turning_plt),
       device = cairo_pdf,
       width = 4.5, height = 4, scale = 2)

tracks_turning %>% 
  bind_cols(
    velocity_turning_pca$scores %>% 
      set_colnames(paste0("PC", 1:ncol(.))) %>% 
      as_tibble
  ) %>% 
  filter(Habitat != "Missing") %>%
  mutate(
    Habitat = c("Open", "Closed")[1 + str_detect(Habitat, regex("forest", T))]
  ) %>% 
  ggplot(aes(velocity, turning, z = PC1)) +
  stat_summary_hex() +
  scale_x_log10() +
  scale_y_continuous(trans = turning_trans,
                     n.breaks = 10,
                     labels = label_prettify_scientific_beta(T,2)) +
  scale_fill_viridis_c()

spurtiness_blocks <- tracks_turning %>% 
  bind_cols(
    velocity_turning_pca$scores %>% 
      set_colnames(paste0("PC", 1:ncol(.))) %>% 
      as_tibble
  ) %>% 
  filter(Habitat != "Missing") %>%
  mutate(
    Habitat = c("Open", "Closed")[1 + str_detect(Habitat, regex("forest", T))]
  ) %>% 
  mutate(
    time = as.numeric(UTC_Time) / 86400 + as.numeric(as.Date(UTC_Date, "%d-%m-%Y")),
    time_block = cut_width(time, 7, boundary = 0, dig.lab = 50),
    lat_block = cut_interval(Latitude, 1),
    lon_block = cut_interval(Longitude, 1),
    time_of_day = c("[00-06)","[06-12)","[12-18)","[18-24)")[1 + floor(lubridate::hour(UTC_Time) / 6)] %>% 
      factor
  ) %>% 
  group_by(UniqueID, time_block, Habitat, time_of_day) %>% 
  summarize(
    across(contains("PC"), mean),
    .groups = "drop"
  ) %>% 
  select(!PC2) %>% 
  pivot_wider(
    id_cols = c(UniqueID, time_block, time_of_day),
    names_from = Habitat,
    values_from = PC1
  ) %>% 
  mutate(
    Difference = Open - Closed
  ) %>% 
  nest(dat = !UniqueID)

plan("multisession", workers = 6)
spurtines_boot <- future.apply::future_lapply(1:100000, function(boot) {
  spurtiness_blocks %>% 
    slice_sample(replace = T) %>% 
    mutate(
      dat = map(dat, ~slice_sample(.x, replace = T))
    ) %>% 
    unnest(dat) %>% 
    group_by(time_of_day) %>% 
    summarize(
      across(c(Open, Closed, Difference), ~mean(.x, na.rm = T))
    ) %>% 
    mutate(
      boot = boot
    )
})
plan("sequential")

spurtines_boot %>% 
  bind_rows %>% 
  pivot_longer(!c(time_of_day, boot), names_to = "Variable", values_to = "PC1") %>% 
  filter(Variable != "Difference") %>% 
  ggplot(aes(PC1, time_of_day, fill = Variable)) +
  geom_violin(scale = "width") 

spurtiness_tab <- spurtines_boot %>% 
  bind_rows %>% 
  group_by(time_of_day) %>% 
  summarize(
    total = mean(Open + Closed, na.rm = T)/2,
    mean = mean(Difference, na.rm = T),
    median = median(Difference, na.rm = T),
    ll = quantile(Difference, .05, na.rm = T),
    ul = quantile(Difference, .95, na.rm = T),
    pVal = factor(sign(Difference), c(-1, 1)) %>% 
      table %>% 
      min %>% 
      add(1) %>% 
      divide_by(n() + 1)
  ) 

spurtiness_tab %>% 
  mutate(pVal = label_pvalue_manual(pVal, digits = 3, type = "latex")) %>% 
  mutate(across(where(is.numeric), ~format_num_for_table(.x, digits = .01, nsmall = 2))) %>% 
  kable(
    format = "latex", 
    # digits = 3,
    col.names = paste0(
      "\\multicolumn{1}{c}{", 
      c("\\makecell{\\textbf{Time of}\\\\\\textbf{Day}}",
        "$\\mathbf{\\mu_{PC1}}$",
        "$\\mathbf{\\mu_\\Delta}$",
        "$\\mathbf{\\mathrm{Q}_{50\\%}}$",
        "$\\mathbf{\\mathrm{Q}_{5\\%}}$",
        "$\\mathbf{\\mathrm{Q}_{95\\%}}$",
        "\\textbf{P-value}"),
      "}"),
    align = "r", 
    booktabs = T, 
    escape = F) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, latex_column_spec = ">{\\\\centering\\\\arraybackslash}p{1.5cm}") %>% 
  column_spec(2:6, latex_column_spec = ">{\\\\raggedleft\\\\arraybackslash}p{1cm}") %>%
  column_spec(7, latex_column_spec = ">{\\\\raggedleft\\\\arraybackslash}p{1cm}") %>%
  write_lines("spurtiness_comparison.txt")
