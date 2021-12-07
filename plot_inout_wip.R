#### Packages ####

library(tidyverse)
library(ggpubr)

#### Loading data ####

plot_inout <- read.csv("plot_inout_n2k.csv",
  header = TRUE,
  sep = ","
) %>%
  rename(PlotObservationID = id) %>%
  arrange(PlotObservationID)

match_occurrences <- read.csv("match_occurrences.csv",
  header = TRUE,
  sep = ";",
  dec = ","
)

header2 <- data.table::fread("header2.csv", quote = "")
header <- header2 %>%
  select(PlotObservationID, Country, `Relevé area (m²)`, `Expert System`, Bioreg_EEA)

#### Plot in/out ####

plot_inout_id <- plot_inout %>%
  left_join(header, by = "PlotObservationID")

plot_inout_occurrences <- match_occurrences$PlotObservationID %in%
  plot_inout_id$PlotObservationID

plot_inout_occurrences2 <- match_occurrences[plot_inout_occurrences, ] %>%
  select(PlotObservationID, Matched.concept, ScientificName, Cover..) %>%
  left_join(plot_inout_id, by = "PlotObservationID") %>%
  mutate(HabitatType = substr(`Expert System`, 0, 1)) %>%
  filter(!grepl("uncertain", n2k_inout)) %>%
  as_tibble()

cols <- c("Country", "Bioreg_EEA", "n2k_inout", "HabitatType")
plot_inout_occurrences2[cols] <- map_dfr(plot_inout_occurrences2[cols], as.factor)

#### Divide the dataset in countries, biogeographic regions and habitat types ####

plot_inout_countries <- split(plot_inout_occurrences2, plot_inout_occurrences2$Country)
plot_inout_bioreg <- split(plot_inout_occurrences2, plot_inout_occurrences2$Bioreg_EEA)
plot_inout_habitat <- split(plot_inout_occurrences2, plot_inout_occurrences2$HabitatType)

#### Number of species ####

species_countries <- plot_inout_occurrences2 %>%
  group_by(Country, n2k_inout) %>%
  summarize(Species_richness = n_distinct(ScientificName))

species_bioreg <- plot_inout_occurrences2 %>%
  filter(!(Bioreg_EEA %in% "")) %>%
  group_by(Bioreg_EEA, n2k_inout) %>%
  summarize(Species_richness = n_distinct(ScientificName))

species_habitat <- plot_inout_occurrences2 %>%
  filter(!(HabitatType %in% c("", "?"))) %>%
  group_by(HabitatType, n2k_inout) %>%
  summarize(Species_richness = n_distinct(ScientificName))

#### Area ####

area_countries <- plot_inout_occurrences2 %>%
  group_by(n2k_inout) %>%
  summarize("Area" = sum(`Relevé area (m²)`, na.rm = TRUE))

area_bioreg <- plot_inout_occurrences2 %>%
  filter(!(Bioreg_EEA %in% "")) %>%
  group_by(n2k_inout) %>%
  summarize("Area" = sum(`Relevé area (m²)`, na.rm = TRUE))

area_habitat <- plot_inout_occurrences2 %>%
  filter(!(HabitatType %in% c("", "?"))) %>%
  group_by(n2k_inout) %>%
  summarize("Area" = sum(`Relevé area (m²)`, na.rm = TRUE))

#### Number of plots ####

nplot_countries <- plot_inout_occurrences2 %>%
  group_by(n2k_inout) %>%
  summarize("N° plot" = length(PlotObservationID))

nplot_bioreg <- plot_inout_occurrences2 %>%
  filter(!(Bioreg_EEA %in% "")) %>%
  group_by(n2k_inout) %>%
  summarize("N° plot" = length(PlotObservationID))

nplot_habitat <- plot_inout_occurrences2 %>%
  filter(!(HabitatType %in% c("", "?"))) %>%
  group_by(n2k_inout) %>%
  summarize("N° plot" = length(PlotObservationID))

#### Summaries ####

summary_countries <- plot_inout_occurrences2 %>%
  group_by(Country, n2k_inout) %>%
  summarise(
    sr = n_distinct(ScientificName),
    area = sum(`Relevé area (m²)`, na.rm = TRUE) / 1e6,
    n_plot = length(PlotObservationID)
  )

summary_bioreg <- plot_inout_occurrences2 %>%
  filter(!(Bioreg_EEA %in% "")) %>%
  group_by(Bioreg_EEA, n2k_inout) %>%
  summarise(
    sr = n_distinct(ScientificName),
    area = sum(`Relevé area (m²)`, na.rm = TRUE) / 1e6,
    n_plot = length(PlotObservationID)
  )

summary_habitat <- plot_inout_occurrences2 %>%
  filter(!(HabitatType %in% c("", "?"))) %>%
  group_by(HabitatType, n2k_inout) %>%
  summarise(
    sr = n_distinct(ScientificName),
    area = sum(`Relevé area (m²)`, na.rm = TRUE) / 1e6,
    n_plot = length(PlotObservationID)
  )

#### Plot theme ####

theme_set(theme_void(base_family = "Roboto"))
theme_update(
  axis.text.x = element_text(
    color = "black", face = "plain", size = 15,
    margin = margin(t = 6)
  ),
  axis.text.y = element_text(
    color = "black", size = 15, hjust = 1,
    margin = margin(r = 6), family = "Roboto Mono"
  ),
  axis.line.x = element_line(color = "black", size = 1),
  panel.grid.major.y = element_line(color = "grey90", size = .6),
  plot.background = element_rect(fill = "white", color = "white"),
  plot.margin = margin(rep(20, 4))
)

mypal <- c("deepskyblue2", "dodgerblue4")
options(ggrepel.max.overlaps = Inf)

#### Plots ####

# Countries #

countries_p1 <- ggplot(species_countries, aes(x = n2k_inout, y = Species_richness, color = n2k_inout, fill = n2k_inout)) +
  scale_color_manual(values = mypal, guide = "none") +
  scale_fill_manual(values = mypal, guide = "none") +
  geom_boxplot(aes(fill = n2k_inout, fill = after_scale(colorspace::lighten(fill, .7))),
    size = 1.5, outlier.shape = NA
  ) +
  geom_line(aes(group = Country), alpha = 0.5, colour = "darkgrey") +
  stat_compare_means(paired = TRUE, method = "wilcox.test", label.x.npc = 0.3, label.y.npc = 1) +
  geom_label(aes(label = Country), position = "identity", size = 3, color = "white")

countries_p2 <- ggplot(summary_countries) +
  geom_point(mapping = aes(x = n2k_inout, y = sr, colour = area, size = n_plot))

countries_table <- area_countries %>%
  ggtexttable(rows = NULL)

ggarrange(countries_p1, countries_p2, countries_table,
  labels = c("A", "B", "C"),
  ncol = 3, nrow = 1
)

# Biogeographic regions #

bioreg_p1 <- ggplot(species_bioreg, aes(x = n2k_inout, y = Species_richness, color = n2k_inout, fill = n2k_inout)) +
  scale_color_manual(values = mypal, guide = "none") +
  scale_fill_manual(values = mypal, guide = "none") +
  geom_boxplot(aes(fill = n2k_inout, fill = after_scale(colorspace::lighten(fill, .7))),
    size = 1.5, outlier.shape = NA
  ) +
  geom_line(aes(group = Bioreg_EEA), alpha = 0.5, colour = "darkgrey") +
  # stat_compare_means(paired = TRUE, method = "wilcox.test", label.x.npc = 0.3, label.y.npc = 1) +
  geom_label(aes(label = Bioreg_EEA), position = "identity", size = 3, color = "white")

bioreg_p2 <- ggplot(summary_bioreg) +
  geom_point(mapping = aes(x = n2k_inout, y = sr, colour = area, size = n_plot))

bioreg_table <- area_bioreg %>%
  ggtexttable(rows = NULL)

ggarrange(bioreg_p1, bioreg_p2, bioreg_table,
  labels = c("A", "B", "C"),
  ncol = 3, nrow = 1
)

# Habitat types #
