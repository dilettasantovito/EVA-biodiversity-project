#### Packages ####

library(tidyverse)
library(labdsv)
library(ggpubr)

#### Data ####

matchit_occurrences <- read_rds("matchit_occurrences.rds")
matchit_comm_mat <- read_rds("matchit_comm_mat.rds")

plot_inout <- read.csv("plot_inout_n2k.csv",
                       header = TRUE,
                       sep = ","
) %>%
  rename(PlotObservationID = id) %>%
  arrange(PlotObservationID)

#### Alpha ####

inout_plots <- matchit_occurrences %>%
  group_by(PlotObservationID, n2k_inout) %>%
  summarise()

inout_plots$PlotObservationID <- as.character(inout_plots$PlotObservationID)

# In #

matchit_comm_mat2 <- matchit_comm_mat %>%
  rownames_to_column(var = "PlotObservationID")

comm_mat_inout <- left_join(matchit_comm_mat2, inout_plots, by = "PlotObservationID") %>%
  column_to_rownames(var = "PlotObservationID")

comm_mat_in <- comm_mat_inout %>%
  filter(n2k_inout == "in") %>%
  within(rm(n2k_inout))

alpha_in <- rowSums(comm_mat_in)
mean_in <- mean(alpha_in, na.rm = TRUE)
sd_in <- sd(alpha_in, na.rm = TRUE)

# Out #

comm_mat_out <- comm_mat_inout %>%
  filter(n2k_inout == "out") %>%
  within(rm(n2k_inout))

alpha_out <- rowSums(comm_mat_out)
mean_out <- mean(alpha_out, na.rm = TRUE)
sd_out <- sd(alpha_out, na.rm = TRUE)

# Table in and out #

means <- c(mean_in, mean_out)
sds <- c(sd_in, sd_out)

summary_inout <- data.frame(means, sds, row.names = c("In", "Out"))

# Preparing data for boxplots #

alpha_inout <- c(alpha_in, alpha_out)

alpha_df <- as.data.frame(alpha_inout) %>%
  rownames_to_column("PlotObservationID")

alpha_df$PlotObservationID <- as.numeric(alpha_df$PlotObservationID)

alpha_inout2 <- plot_inout$PlotObservationID %in%
  alpha_df$PlotObservationID

plot_short <- matchit_occurrences %>%
  select(PlotObservationID, Country, Bioreg_EEA, HabitatType, elevation)

alpha_inout3 <- plot_inout[alpha_inout2,] %>%
  left_join(alpha_df, by = "PlotObservationID") %>%
  left_join(plot_short, by = "PlotObservationID") %>%
  distinct(PlotObservationID, .keep_all = TRUE)

#### Alpha diversity ~ total ####

n_plots <- function(x){
  return(c(y = max(x) + 0.1, label = length(x))) 
}

alpha_tot_p <- ggplot(alpha_inout3, aes(x = n2k_inout, y = alpha_inout)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  #  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 50)) +
  stat_summary(fun.data = n_plots, geom = "text", color = "red") +
  stat_compare_means(method = "wilcox.test", label.x.npc = 0.3, label.y.npc = 0.1) +
  scale_y_log10() 

alpha_tot_p

#### Alpha diversity ~ countries ####

alpha_countries_p <- alpha_inout3 %>% 
  ggplot(aes(x = n2k_inout, y = alpha_inout)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  stat_summary(fun.data = n_plots, geom = "text", color = "red") +
  stat_compare_means(method = "wilcox.test", label.x.npc = 0.3, label.y.npc = 0.1) +
  scale_y_log10() +
  facet_wrap(vars(Country), scales = "free_y")

alpha_countries_p

#### Alpha diversity ~ biogeographic regions ####

alpha_bioreg_p <- alpha_inout3 %>%
  filter(!(Bioreg_EEA %in% c("", "ARC"))) %>%
  ggplot(aes(x = n2k_inout, y = alpha_inout)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  stat_summary(fun.data = n_plots, geom = "text", color = "red") +
  stat_compare_means(method = "wilcox.test", label.x.npc = 0.3, label.y.npc = 0.1) +
  scale_y_log10() +
  facet_wrap(vars(Bioreg_EEA), scales = "free_y")

alpha_bioreg_p

#### Alpha diversity ~ habitat types ####

alpha_habitat_p <- alpha_inout3 %>% 
  filter(!(HabitatType %in% c("", "?"))) %>%
  ggplot(aes(x = n2k_inout, y = alpha_inout)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  stat_summary(fun.data = n_plots, geom = "text", color = "red") +
  stat_compare_means(method = "wilcox.test", label.x.npc = 0.3, label.y.npc = 0.1) +
  scale_y_log10() +
  facet_wrap(vars(HabitatType), scales = "free_y")

alpha_habitat_p

#### Means and standard deviations ####

countries_mean <- alpha_inout3 %>%
  group_by(n2k_inout, Country) %>%
  summarize("Alpha_mean" = mean(alpha_inout, na.rm = TRUE), 
            "Alpha_sd" = sd(alpha_inout, na.rm = TRUE)) %>%
  pivot_wider(names_from = n2k_inout, values_from = c("Alpha_mean", "Alpha_sd"))

bioreg_mean <- alpha_inout3 %>%
  filter(!(Bioreg_EEA %in% c("", "ARC"))) %>%
  group_by(n2k_inout, Bioreg_EEA) %>%
  summarize("Alpha_mean" = mean(alpha_inout, na.rm = TRUE), 
            "Alpha_sd" = sd(alpha_inout, na.rm = TRUE)) %>%
  pivot_wider(names_from = n2k_inout, values_from = c("Alpha_mean", "Alpha_sd"))

habitat_mean <- alpha_inout3 %>%
  filter(!(HabitatType %in% c("", "?"))) %>%
  group_by(n2k_inout, HabitatType) %>%
  summarize("Alpha_mean" = mean(alpha_inout, na.rm = TRUE), 
            "Alpha_sd" = sd(alpha_inout, na.rm = TRUE)) %>%
  pivot_wider(names_from = n2k_inout, values_from = c("Alpha_mean", "Alpha_sd"))

#countries_table <- ggtexttable(countries_mean)
#bioreg_table <- ggtexttable(bioreg_mean)
#habitat_table <- ggtexttable(habitat_mean)

#ggarrange(countries_mean, bioreg_mean, habitat_mean,
#          labels = c("Countries", "Biogeographic regions", "Habitat Types"),
#          ncol = 3, nrow = 1
#)

#ggarrange(countries_table, labels = "Countries",
#          ncol = 1, nrow = 1
#)

#ggarrange(bioreg_table, habitat_table, 
#          labels = c("Biogeographic regions", "Habitat Types"),
#          ncol = 2, nrow = 1
#          )

#### Model ####

model_poisson <- glm(alpha_inout ~ n2k_inout + elevation + Country + Bioreg_EEA + HabitatType,
                     data = alpha_inout3, family = poisson)

summary(model_poisson)