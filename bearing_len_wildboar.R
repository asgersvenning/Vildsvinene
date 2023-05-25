library(readr)
library(tidyverse)
library(magrittr)
library(dplyr)
library(geosphere)
library(broom)


Wildboar_gpstracks <- read_delim("data/Wildboar_gpstracks.csv", 
                                 delim = ";", 
                                 escape_double = FALSE, 
                                 trim_ws = TRUE) %>% 
  mutate(UTC_Date = as.Date(UTC_Date, format = "%d-%m-%Y"))


calculate_bearing <- function(lat1, lon1, lat2, lon2) {
  y <- lat2 - lat1
  x <- lon2 - lon1
  bearing <- atan2(y, x) / (2 * pi) * 360
  return(bearing)
}

tracks_hab_class <- read_delim("Final_project/tracks_hab_class.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE,
                               locale = locale(decimal_mark = ",", grouping_mark = ".")) %>% 
  mutate(UTC_Date = as.Date(UTC_Date, format = "%d-%m-%Y")) %>% 
  arrange(UniqueID, UTC_Date, UTC_Time)


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

bearing_step_length_data <- Wildboar_gpstracks %>% 
  mutate(habitat_type = tracks_hab_class$type) %>% 
  group_by(UniqueID) %>% 
  arrange(Timestamp) %>% 
  mutate(
    block = continuous_blocks(as.numeric(Timestamp), 3000)
  ) %>% 
  group_by(UniqueID, block) %>% 
  slice_head(n = 1) %>% 
  group_by(UniqueID) %>% 
  arrange(Timestamp) %>% 
  reframe(
    bearing = calculate_bearing(
      Latitude, 
      Longitude, 
      lag(Latitude, default = NA), 
      lag(Longitude, default = NA)
    )[-1],
    
    distance = distVincentySphere(
      cbind(Longitude, Latitude),
      cbind(lag(Longitude), lag(Latitude))
    )[-1],
    
    habitat_type = ifelse(
      habitat_type != lag(habitat_type, default = "DUMMY"),
      paste0(habitat_type, "-->", lag(habitat_type)),
      habitat_type
    )[-1],
    
    time_start = Timestamp[-n()],
    time_length = diff(Timestamp) %>% 
      as_units %>% 
      set_units(s) %>% 
      drop_units
  ) %>% 
  mutate(
    velocity = distance / time_length
  ) %>% 
  filter(velocity <= 10 & time_length <= 4000) 

bearing_step_length_data$time_length %>% 
  hist(1000)

tracks_bearing <- bearing_step_length_data %>% 
  group_by(UniqueID) %>% 
  arrange(time_start) %>% 
  reframe(
    bearing_change = diff(bearing),
    time = time_start[-n()],
    habitat_type = ifelse(
      str_detect(habitat_type, "-->"),
      str_extract(habitat_type, "(?<=-->).+"),
      habitat_type
    )[-n()]
  )

tracks_len_block <- bearing_step_length_data %>% 
  mutate(
    # lat_bin = cut_interval(Latitude, 10, boundary = 0),
    # lon_bin = cut_interval(Longitude, 10, boundary = 0)
    time_bin = cut_width(time_start, 7 * 86400, boundary = 0)
  ) %>% 
  group_by(UniqueID, habitat_type, time_bin) %>% 
  summarize(
    velocity = mean(velocity),
    .groups = "drop"
  ) 

tracks_len_block %>% 
  filter(!str_detect(habitat_type, "-->")) %>% 
  ggplot(aes(velocity, habitat_type)) +
  geom_violin(scale = "width") +
  scale_x_log10()

tracks_angle_block <- tracks_bearing %>% 
  mutate(
    # lat_bin = cut_interval(Latitude, 10, boundary = 0),
    # lon_bin = cut_interval(Longitude, 10, boundary = 0)
    time_bin = cut_width(time, 7 * 86400, boundary = 0)
  ) %>% 
  group_by(UniqueID, habitat_type, time_bin) %>% 
  summarize(
    cos_angle = mean(cos(bearing_change / 360 * pi)),
    .groups = "drop"
  ) 

tracks_angle_block %>% 
  ggplot(aes(cos_angle, habitat_type)) +
  geom_violin(scale = "width") 
  # scale_y_discrete(expand = expansion(add = c(10, 0))) +
  # coord_polar()


angle_type_boot <- map(1:1000, function(boot) {
  tracks_len_angle %>% 
    nest(dat = !type) %>% 
    mutate(
      dat = map(dat, ~slice_sample(.x, n = nrow(.x), replace = T))
    ) %>% 
    unnest(dat) %>% 
    group_by(type) %>% 
    summarize(
      m = mean(angle, na.rm = T),
      # v = sapply(c(0.05, .5, .95), function(q) quantile(angle, q)),
      # q = c(0.05, .5, 95),
      .groups = "drop"
    ) %>% 
    mutate(boot = boot)
}, .progress = T)


angle_type_boot %>% 
  bind_rows %>% 
  ggplot(aes(m, type)) +
  geom_violin(scale = "width")


angle_type_boot %>% 
  bind_rows %>% 
  filter(type %in% c("Nature, dry", "Artificial")) %>% 
  pivot_wider(
    id_cols = boot,
    names_from = type,
    values_from = m
  ) %>% 
  mutate(
    diff = `Nature, dry` - `Artificial`,
    sign = factor(sign(diff), c(-1, 0, 1))
  ) %>% 
  summarize(
    p.value = (min(table(sign)[c("-1", "1")]) + 1)/(n() + 1)
  )
  ggplot(aes(`Nature, dry`, `Artificial`, color = `Nature, dry` > `Artificial`)) +
  geom_point() +
  coord_equal(xlim = 0:1, ylim = 0:1) +
  geom_abline(slope = 1, intercept = 0) 
  
  
pair_emp_pval <- angle_type_boot %>% 
    bind_rows %>% 
    group_by(boot) %>% 
    arrange(type) %>% 
    summarize(
      mat = set_names(m, type) %>% 
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

pair_emp_pval %>% 
  ggplot(aes(H1, H2, fill = abs(median))) +
  geom_tile() +
  scale_fill_viridis_c(trans = "log10") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



