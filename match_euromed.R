#### Packages ####

library(tidyverse)
library(pbmcapply)

#### Loading data ####

# Euro+Med data

euromed_table <- read.csv("EuroMed.csv",
  header = TRUE,
  sep = ";"
)

euromed <- euromed_table[-grep("?", euromed_table$TaxonConcept, fixed = T), ]

# EVA data

species <- read_rds("species.RDS")

vascular_species <- species %>%
  filter(Taxon.group == "Vascular plant")

# Total unique species

vascular_species_distinct <- vascular_species %>%
  filter(Taxon.group == "Vascular plant") %>%
  distinct(Matched.concept)

# Species and occurrences matching Euro+Med

vascular_in_euromed <- vascular_species_distinct$Matched.concept %in%
  euromed$TaxonConcept

vascular_match <- vascular_species_distinct[vascular_in_euromed, ] %>%
  as_tibble() %>%
  rename(ScientificName = value) %>%
  mutate(Matched.concept = ScientificName)

match_in_occurrences <- vascular_species %>%
  filter(Matched.concept %in% vascular_match$Matched.concept)

# Non-matching species and occurrences

vascular_nonmatch <- vascular_species_distinct[!vascular_in_euromed, ] %>%
  as_tibble() %>%
  rename(ScientificName = value) %>%
  mutate(Matched.concept = ScientificName)

vascular_nonmatch_occurrences <- vascular_species %>%
  filter(Matched.concept %in% vascular_nonmatch$Matched.concept)

#### Filter ####

# I filter out non-matching occurrences containing mistakes or missing information
# and correct the remaining ones.
# I add a column named ScientificName2 containing the regular expressions that
# I'll need in order to match the remaining species and occurrences with Euro+Med.

vascular_filter <- vascular_nonmatch %>%
  filter(
    !grepl("species", fixed = TRUE, ScientificName),
    !grepl("/", fixed = TRUE, ScientificName),
    !grepl("sect.", fixed = TRUE, ScientificName),
    !grepl("ser.", fixed = TRUE, ScientificName),
    !grepl("Ser.", fixed = TRUE, ScientificName),
    !grepl(" et ", fixed = TRUE, ScientificName),
    !grepl("(group)", fixed = TRUE, ScientificName),
    !grepl("(Group)", fixed = TRUE, ScientificName),
    !grepl("+", fixed = TRUE, ScientificName),
    !grepl("subg.", fixed = TRUE, ScientificName),
    !grepl("(excl", fixed = TRUE, ScientificName),
    !grepl("Subs.", fixed = TRUE, ScientificName),
    !grepl("aggr.", fixed = TRUE, ScientificName),
    !grepl("gr.", fixed = TRUE, ScientificName),
    !grepl("subgen.", fixed = TRUE, ScientificName)
  ) %>%
  mutate(
    ScientificName = gsub(" x ", " ", fixed = TRUE, ScientificName),
    ScientificName = gsub(" s.l.", "", fixed = TRUE, ScientificName),
    ScientificName = gsub(" sl.", "", fixed = TRUE, ScientificName),
    ScientificName = gsub(" s.s.", "", fixed = TRUE, ScientificName),
    ScientificName = gsub(" f. ", " ", fixed = TRUE, ScientificName),
    ScientificName = gsub(" cf.", " ", fixed = TRUE, ScientificName),
    ScientificName = gsub(" a. ", " ", fixed = TRUE, ScientificName),
    ScientificName = gsub(" A. ", " ", fixed = TRUE, ScientificName),
    ScientificName = gsub(" (hybr.)", "", fixed = TRUE, ScientificName),
    ScientificName = gsub("x-", "", fixed = TRUE, ScientificName),
    ScientificName = gsub(" (dood)", "", fixed = TRUE, ScientificName)
  ) %>%
  mutate(ScientificName = word(ScientificName, 1, 2)) %>%
  mutate(ScientificName2 = paste("^",
    word(ScientificName, 1),
    ".*",
    word(ScientificName, 2),
    "$",
    sep = ""
  ))

# Filtered out species and occurrences

filteredout_species <- vascular_nonmatch %>%
  filter(grepl("species", fixed = TRUE, ScientificName) |
    grepl("/", fixed = TRUE, ScientificName) |
    grepl("sect.", fixed = TRUE, ScientificName) |
    grepl("ser.", fixed = TRUE, ScientificName) |
    grepl("Ser.", fixed = TRUE, ScientificName) |
    grepl(" et ", fixed = TRUE, ScientificName) |
    grepl("(group)", fixed = TRUE, ScientificName) |
    grepl("(Group)", fixed = TRUE, ScientificName) |
    grepl("+", fixed = TRUE, ScientificName) |
    grepl("subg.", fixed = TRUE, ScientificName) |
    grepl("(excl", fixed = TRUE, ScientificName) |
    grepl("Subs.", fixed = TRUE, ScientificName) |
    grepl("aggr.", fixed = TRUE, ScientificName) |
    grepl("gr.", fixed = TRUE, ScientificName) |
    grepl("subgen.", fixed = TRUE, ScientificName))

filteredout_occurrences <- vascular_species %>%
  filter(Matched.concept %in% filteredout_species$Matched.concept)

regex_nonmatch <- vascular_nonmatch_occurrences %>%
  filter(!Matched.concept %in% filteredout_species$Matched.concept)

#### Matching the fixed occurrences with Euro+Med ####

# I retrieve euromed$TaxonConcept codes using ScientificName2 and use them to
# match the fixed occurrences and species with Euro+Med

euromed_codes <- pbmclapply(vascular_filter$ScientificName2,
  function(x) grep(x, euromed$TaxonConcept),
  mc.cores = 2
)

euromed_codes2 <- pbmclapply(vascular_match$ScientificName,
  function(x) grep(x, euromed$TaxonConcept),
  mc.cores = 2
)

euromed_code <- euromed_codes %>%
  lapply(., function(x) x[1]) %>%
  do.call(c, .)

euromed_code2 <- euromed_codes2 %>%
  lapply(., function(x) x[1]) %>%
  do.call(c, .)

vascular_filter2 <- vascular_filter %>%
  mutate(
    Code = euromed_code,
    TaxonConcept = euromed$TaxonConcept[euromed_code]
  )

vascular_match2 <- vascular_match %>%
  mutate(
    Code = euromed_code2,
    TaxonConcept = euromed$TaxonConcept[euromed_code2]
  )

regex_species <- vascular_filter2 %>%
  drop_na()

regex_occurrences <- vascular_species %>%
  filter(Matched.concept %in% regex_species$Matched.concept)

# Total species and occurrences matching Euro+Med

match_species <- vascular_filter2 %>%
  select(Matched.concept, ScientificName, TaxonConcept) %>%
  rbind(select(vascular_match2, Matched.concept, ScientificName, TaxonConcept)) %>%
  drop_na()

match_species2 <- match_species %>%
  inner_join(euromed) %>%
  distinct(Matched.concept, .keep_all = TRUE) %>%
  select(Matched.concept, ScientificName, TaxonConcept, TaxonConceptID)

match_occurrences <- vascular_species %>%
  filter(Matched.concept %in% match_species2$Matched.concept)

match_occurrences2 <- vascular_species %>%
  inner_join(match_species2)

#### Non-matching species and occurrences ####

nonmatch_species <- vascular_filter2 %>%
  filter(is.na(TaxonConcept)) %>%
  select(Matched.concept, ScientificName)

nonmatch_occurrences <- vascular_species %>%
  filter(Matched.concept %in% nonmatch_species$Matched.concept)

write.table(match_occurrences2,
  "match_occurrences.csv",
  sep = ";",
  dec = ",",
  col.names = TRUE
)
