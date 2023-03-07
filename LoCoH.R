library(sf)
library(tidyverse)
library(flexclust)
library(fastcluster)

customClust <- function(x, k, type = "kmeanspp") {
  
  if (k == 1) return(rep(1, dim(x)[1]))
  
  if (type == "kmeanspp") {
    clusters <- flexclust::cclust(x, k = k, control = list(initcent="kmeanspp"), simple = T) %>% 
      flexclust::clusters()
  }
  else if (type == "base") {
    clusters <- kmeans(x, k)$cluster %>% 
      unname
  }
  else if (type == "ward") {
    clusters <- hclust.vector(x, method = "ward") %>% 
      cutree(k)
  }
  else {
    stop(paste0("Invalid type of cluster (\",", type, ",\") chosen!"))
  }
  
  return(clusters)
}

LoCoH_custom <- function(x, kmin, kmax, kstep, type, extra_vars) {
  
  extra_vars <- if (!missing(extra_vars)) enquo(extra_vars) else c()
  
  lapply(seq(kmin, kmax, kstep), function(nCluster) {
    x %>% 
      mutate(
        cluster = cbind(st_coordinates(x), across(!!extra_vars)) %>%
          apply(2, function(y) {
            y[!is.na(y)] <- scale(y[!is.na(y)])
            y
          }) %>% 
          customClust(nCluster, "ward")
      ) %>% 
      group_by(cluster) %>% 
      summarize %>% 
      st_convex_hull() %>% 
      mutate(nCluster = nCluster)
  }) 
}

boarRaw %>%
  st_as_sf(coords = c("Longitude", "Latitude")) %>% 
  group_by(UniqueID) %>% 
  # slice_sample(prop = 0.5) %>% 
  ungroup %>% 
  # filter(UniqueID == first(UniqueID)) %>%
  mutate(timeNum = as.numeric(UTC_Time) + as.numeric(UTC_Date)) %>% 
  LoCoH_custom(extra_vars = timeNum, 200, 200, 5, "kmeanspp") %>% 
  first %>% 
  ggplot() +
  geom_sf(color = NA,
          fill = "#00000011")
