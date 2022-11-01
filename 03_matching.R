#### Packages ####

library(tidyverse)
library(MatchIt)
library(elevatr)
library(sf)
library(optmatch)
library(ggpubr)
library(cobalt)

#### Data ####

header <-readRDS("header_mod.RDS") %>% 
  dplyr::select(-dataset,
                -tdwg) %>% 
  mutate(expertSystem = substr(expertSystem, 1, 1))

header2 <- data.table::fread("header2.csv", quote = "") %>%
  select(PlotObservationID, Country, `Relevé area (m²)`, `Expert System`, Bioreg_EEA)

#### Get elevation ####

elevs <- get_elev_point(header[c("longitude", "latitude")] %>% 
                          st_as_sf(coords = c("longitude", "latitude"),
                                   crs = 4326),
                        prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                        src = "aws") %>%
  pull(elevation)

header$elevation <- elevs

header <- header %>% 
  filter(!is.na(n2k) 
         & !is.na(country) 
         & !is.na(bioreg) 
         & bioreg != ""
         & bioreg != "ARC"
         & expertSystem != ""
         & expertSystem != "?") %>% 
  mutate(n2k = if_else(n2k == "outside", 0, 1))

dat <- header %>% 
  group_by(n2k,
           country,
           expertSystem) %>% 
  slice_sample(n = 50)

#### Matching ####

m1 <- matchit(n2k ~ country + bioreg + expertSystem + elevation,
              data = dat,
              distance = "glm",
              method = NULL)

m1_sum <- summary(m1)

m2 <- matchit(n2k ~ elevation,
              data = dat,
              distance = "glm",
              method = "full",
              exact = ~ country + bioreg + expertSystem)

m2_sum <- summary(m2)

m2_p <- love.plot(m2, binary = "std")

m3 <- matchit(n2k ~ country + bioreg + expertSystem + elevation,
              data = dat,
              distance = "glm",
              method = "full")
m3_sum <- summary(m3)

m3_p <- love.plot(m3, binary = "std")

m4 <- matchit(n2k ~ country + bioreg + expertSystem + elevation,
              data = dat,
              distance = "glm",
              method = "nearest")

m4_sum <- summary(m4)

m4_p <- love.plot(m4, binary = "std")

m5 <- matchit(n2k ~ elevation,
              data = dat,
              distance = "glm",
              method = "nearest",
              exact = ~ country + bioreg + expertSystem)

m5_sum <- summary(m5)

m5_p <- love.plot(m5, binary = "std")


ggarrange(m2_p, m3_p, m5_p, m4_p,
          labels = c("Full + exact matching", "Full matching",
                     "Nearest + exact matching", "Nearest neighbor matching"),
                     ncol = 2, nrow = 2
                    )

m6 <- matchit(n2k ~ elevation,
              data = dat,
              distance = "mahalanobis",
              method = "full",
              exact = ~ country + bioreg + expertSystem)

m6_sum <- summary(m6)

m6_p <- love.plot(m6, binary = "std")

ggarrange(m2_p, m6_p,
          labels = c("Full + exact glm", "Full + exact mahalanobis"),
          ncol = 2, nrow = 1)

m7 <- matchit(n2k ~ elevation + country,
              data = dat,
              distance = "glm",
              method = "full",
              exact = ~ bioreg + expertSystem)

m7_sum <- summary(m7)

m7_p <- love.plot(m7, binary = "std")

m8 <- matchit(n2k ~ elevation + country + longitude + latitude,
              data = dat,
              distance = "glm",
              method = "full",
              exact = ~ bioreg + expertSystem)

m8_sum <- summary(m8)

m8_p <- love.plot(m8, binary = "std")

ggarrange(m7_p, m8_p,
          labels = c("Full: elevation, country", 
                     "Full: elevation, country, longitude, latitude"),
          ncol = 2, nrow = 1)

m7_elev_p <- bal.plot(m7, var.name = "elevation", which = "both")
m8_elev_p <- bal.plot(m8, var.name = "elevation", which = "both")

ggarrange(m7_elev_p, m8_elev_p,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

m7_country_p <- bal.plot(m7, var.name = "country", which = "both", 
                         type = "histogram", mirror = TRUE)
m8_country_p <- bal.plot(m8, var.name = "country", which = "both",
                         type = "histogram", mirror = TRUE)

ggarrange(m7_country_p, m8_country_p,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


#### Matching (n = 80) ####

dat2 <- header %>% 
  group_by(n2k,
           country,
           expertSystem) %>% 
  slice_sample(n = 80)

# Checking initial balance: method = NULL

initial_balance <- matchit(n2k ~ country + bioreg + expertSystem + elevation,
                           data = dat2,
                           distance = "glm",
                           method = NULL)

summary(initial_balance)

# Nearest neighbor matching #

nearest <- matchit(n2k ~ country + bioreg + expertSystem + elevation,
                   data = dat2,
                   distance = "glm",
                   method = "nearest")

nearest_sum <- summary(nearest, un = FALSE)
nearest_sum

nearest_p <- love.plot(nearest, binary = "std")
nearest_p

# Full matching #

full <- matchit(n2k ~ country + bioreg + expertSystem + elevation,
                data = dat2,
                distance = "glm",
                method = "full")

full_sum <- summary(full, un = FALSE)
full_sum

full_p <- love.plot(full, binary = "std")
full_p

# Nearest + exact matching #

near_exact <- matchit(n2k ~ elevation,
        data = dat2,
        distance = "glm",
        method = "nearest",
        exact = ~ country + bioreg + expertSystem)

near_exact_sum <- summary(near_exact, un = FALSE)
near_exact_sum

near_ex_p <- love.plot(near_exact, binary = "std")
near_ex_p

# Full + exact matching #

full_exact <- matchit(n2k ~ elevation,
                      data = dat2,
                      distance = "glm",
                      method = "full",
                      exact = ~ country + bioreg + expertSystem)

full_exact_sum <- summary(full_exact, un = FALSE)
full_exact_sum

full_ex_p <- love.plot(full_exact, binary = "std")
full_ex_p

# Comparisons #

compare1 <- ggarrange(nearest_p, full_p, near_ex_p, full_ex_p,
                      labels = c("Nearest neighbor", "Full",
                                 "Nearest + exact", "Full + exact"),
                      ncol = 2, nrow = 2)

compare1

#### Matching (n = 100) ####

dat3 <- header %>% 
  group_by(n2k,
           country,
           expertSystem) %>% 
  slice_sample(n = 100)

# Checking initial balance: method = NULL

initial_balance_100 <- matchit(n2k ~ country + bioreg + expertSystem + elevation,
                               data = dat3,
                               distance = "glm",
                               method = NULL)

summary(initial_balance_100)

# Nearest + exact matching #

near_exact100 <- matchit(n2k ~ elevation,
                      data = dat3,
                      distance = "glm",
                      method = "nearest",
                      exact = ~ country + bioreg + expertSystem)

near_exact_sum100 <- summary(near_exact100, un = FALSE)
near_exact_sum100

near_ex100_p <- love.plot(near_exact100, binary = "std")
near_ex100_p

# Full + exact matching #

full_exact100 <- matchit(n2k ~ elevation,
                      data = dat3,
                      distance = "glm",
                      method = "full",
                      exact = ~ country + bioreg + expertSystem)

full_exact_sum100 <- summary(full_exact100, un = FALSE)
full_exact_sum100

full_ex100_p <- love.plot(full_exact100, binary = "std")
full_ex100_p

#### Matching (n = 150) ####

dat150 <- header %>% 
  group_by(n2k,
           country,
           expertSystem) %>% 
  slice_sample(n = 150)

# Checking initial balance: method = NULL

initial_balance_150 <- matchit(n2k ~ country + bioreg + expertSystem + elevation,
                               data = dat150,
                               distance = "glm",
                               method = NULL)

summary(initial_balance_150)

# Nearest + exact matching #

near_exact150 <- matchit(n2k ~ elevation,
                         data = dat150,
                         distance = "glm",
                         method = "nearest",
                         exact = ~ country + bioreg + expertSystem)

near_exact_sum150 <- summary(near_exact150, un = FALSE)
near_exact_sum150

near_ex150_p <- love.plot(near_exact150, binary = "std")
near_ex150_p

# Full + exact matching #

full_exact150 <- matchit(n2k ~ elevation,
                         data = dat150,
                         distance = "glm",
                         method = "full",
                         exact = ~ country + bioreg + expertSystem)

full_exact_sum150 <- summary(full_exact150, un = FALSE)
full_exact_sum150

full_ex150_p <- love.plot(full_exact150, binary = "std")
full_ex150_p


#### Matching (n = 200) ####

dat200 <- header %>% 
  group_by(n2k,
           country,
           expertSystem) %>% 
  slice_sample(n = 200)

# Checking initial balance: method = NULL #

initial_balance_200 <- matchit(n2k ~ country + bioreg + expertSystem + elevation,
                               data = dat200,
                               distance = "glm",
                               method = NULL)

summary(initial_balance_200)

# Nearest + exact matching #

near_exact200 <- matchit(n2k ~ elevation,
                         data = dat200,
                         distance = "glm",
                         method = "nearest",
                         exact = ~ country + bioreg + expertSystem)

near_exact_sum200 <- summary(near_exact200, un = FALSE)
near_exact_sum200

near_ex200_p <- love.plot(near_exact150, binary = "std")
near_ex200_p

near_ex_c200 <- matchit(n2k ~ elevation + country,
                        data = dat200,
                        distance = "glm",
                        method = "nearest",
                        exact = ~ bioreg + expertSystem)

near_ex_c200_sum <- summary(near_ex_c200, un = FALSE)
near_ex_c200_sum

near_ex_c200_p <- love.plot(near_ex_c200, binary = "std")
near_ex_c200_p

# Optimal + exact matching #

opt_exact200 <- matchit(n2k ~ elevation,
                        data = dat200,
                        distance = "glm",
                        method = "optimal",
                        exact = ~ country + bioreg + expertSystem)

opt_exact_200sum <- summary(opt_exact200, un = FALSE)
opt_exact_200sum

opt_exact200_p <- love.plot(near_ex_c200, binary = "std")
opt_exact200_p

# Full + exact matching #

full_exact200 <- matchit(n2k ~ elevation,
                         data = dat200,
                         distance = "glm",
                         method = "full",
                         exact = ~ country + bioreg + expertSystem)

full_exact_sum200 <- summary(full_exact200, un = FALSE)
full_exact_sum200

full_ex200_p <- love.plot(full_exact200, binary = "std")
full_ex200_p

# Comparisons #

compare2 <- ggarrange(near_ex200_p, near_ex_c200_p, opt_exact200_p, full_ex200_p,
                      labels = c("Nearest neighbor + exact", 
                                 "Nearest neighbor (incl. country) + exact",
                                 "Optimal + exact", "Full + exact"),
                      ncol = 2, nrow = 2,
                      vjust = 0.6)

compare2

matched_data <- match.data(full_exact200)
write_rds(matched_data, "matched_data.rds", "xz", compression = 9)