#### Packages ####

library(tidyverse)
#library(labdsv)
library(Rarity)
library(ggpubr)

#### Data ####

plot_inout_occurrences <- read.csv("plot_inout_occurrences.csv",
                                   header = TRUE,
                                   sep = ";",
                                   dec = ","
)

matched_data <- read_rds("matched_data.rds") %>%
  rename(PlotObservationID = ID)

species <- read_rds("species.RDS")

matched_occurrences <- left_join(matched_data, plot_inout_occurrences,
                                 by = "PlotObservationID") %>% 
  ungroup()

write_rds(matched_occurrences, "matchit_occurrences.rds", "xz", compression = 9)

#### Community matrix ####

comm_mat2 <- matched_occurrences %>%
     select(PlotObservationID, ScientificName, Cover) %>%
     mutate(PlotObservationID = as.character(PlotObservationID), ScientificName = as.character(ScientificName)) %>% 
     group_by(PlotObservationID, ScientificName) %>% 
     summarise(Cover = max(Cover)) %>% 
     pivot_wider(id_cols = PlotObservationID, names_from = ScientificName, values_from = Cover, values_fill = 0) %>%
     mutate_if(is.numeric, ~1 * (. > 0))

comm_mat3 <- comm_mat2 %>%
  remove_rownames() %>% 
  column_to_rownames(var = "PlotObservationID")

comm_mat4 <- comm_mat3[, -1049]

#comm_matrify <- matched_occurrences %>%
#  drop_na() %>%
#  select(PlotObservationID, ScientificName, Cover) %>%
#  matrify() %>%
#  mutate_if(is.numeric, ~1 * (. > 0))

write_rds(comm_mat4, "matchit_comm_mat.rds", "xz", compression = 9)

#comm_mat <- matched_occurrences %>%
#  select(PlotObservationID, ScientificName, Cover) %>%
#  mutate(PlotObservationID = as.character(PlotObservationID), ScientificName = as.character(ScientificName)) %>% 
#  pivot_wider(id_cols = PlotObservationID, names_from = ScientificName, values_from = Cover) %>%
#  mutate_if(is.numeric, ~1 * (. > 0))

#ids <- comm_mat$PlotObservationID
#comm_mat <- comm_mat[,-1]

#comm_mat[is.null(comm_mat)] <- 0
#comm_mat[comm_mat > 0] <- 1

species_plots <- matched_occurrences %>%
  filter(!is.na(ScientificName)) %>% 
  group_by(ScientificName, n2k_inout) %>%
  summarize("Occurr" = length(PlotObservationID)) %>%
  pivot_wider(names_from = n2k_inout, values_from = Occurr) %>%
  column_to_rownames(var = "ScientificName")

species_plots[is.na(species_plots)] <- 0

#### Missing data ####

missing <- matched_occurrences %>% 
  filter(is.na(ScientificName))

diff <- colnames(comm_mat3) %in% rownames(species_plots)

which(!diff)

colnames(diff) <- diff

#### Rarity ####

rarity_weights <- rWeights(species_plots)

rarity <- Irr(t(comm_mat4), rarity_weights)

rarity2 <- rarity %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "PlotObservationID")

rarity2$PlotObservationID <- as.numeric(rarity2$PlotObservationID)

rarity3 <- matched_occurrences %>% 
  left_join(rarity2, by = "PlotObservationID") %>% 
  select(PlotObservationID, n2k_inout, Country, Bioreg_EEA, HabitatType, Irr, Richness, elevation) %>% 
  distinct(PlotObservationID, .keep_all = TRUE)

#### Plots ####

p <- ggplot(rarity3, aes(x = n2k_inout, y = Irr)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  #  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 50)) +
  #stat_summary(fun.data = n_plots, geom = "text", color = "red") +
  stat_compare_means(method = "wilcox.test", label.x.npc = 0.3, label.y.npc = 0.1) +
  scale_y_log10()

p

countries_p <- rarity3 %>% 
  ggplot(aes(x = n2k_inout, y = Irr)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  stat_compare_means(method = "wilcox.test", label.x.npc = 0.3, label.y.npc = 0.1) +
  scale_y_log10() +
  facet_wrap(vars(Country), scales = "free_y")

countries_p

bioreg_p <- rarity3 %>% 
  filter(!(Bioreg_EEA %in% c("", "ARC")), !is.na(Bioreg_EEA)) %>%
  ggplot(aes(x = n2k_inout, y = Irr)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  stat_compare_means(method = "wilcox.test", label.x.npc = 0.3, label.y.npc = 0.1) +
  scale_y_log10() +
  facet_wrap(vars(Bioreg_EEA), scales = "free_y")

bioreg_p

habitat_p <- rarity3 %>% 
  filter(!(HabitatType %in% c("", "?")), !is.na(HabitatType)) %>%
  ggplot(aes(x = n2k_inout, y = Irr)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  stat_compare_means(method = "wilcox.test", label.x.npc = 0.3, label.y.npc = 0.1) +
  scale_y_log10() +
  facet_wrap(vars(HabitatType), scales = "free_y")

habitat_p

#### Model ####

model_binom <- glm(Irr ~ n2k_inout + elevation + Country + Bioreg_EEA + HabitatType,
                   data = rarity3, family = binomial)

summary(model_binom)
