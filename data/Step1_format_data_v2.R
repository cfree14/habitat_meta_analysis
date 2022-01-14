
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(pdp)
library(caret)
library(tidyverse)
library(randomForest)

# Directories
plotdir <- "figures"
tabledir <- "tables"
datadir <- "data/raw/v2"
outdir <- "data/processed"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "lf_paired_db_clean.xlsx"), na=c("NA"), sheet=1)


# 1. Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(region=region_general,
         season=season_general,
         taxa_type=taxa_general,
         species=species_ind) %>%
  # Format id
  mutate(id=1:n()) %>%
  # Format country
  mutate(country=recode(country, "USA"="United States")) %>%
  # Format region
  mutate(region=gsub("coast", "Coast", region)) %>%
  # Format state
  mutate(state=stringr::str_to_title(state)) %>%
  # Format location
  # Could be cleaned up futher
  mutate(location=gsub("_", "/", location)) %>%
  # Format coordinates
  select(-coord) %>%
  rename(lat_dd=lat, long_dd=long) %>%
  # Format reference
  mutate(reference=gsub("&", "and", reference),
         reference=gsub("et al |et al., ", "et al. ", reference),
         reference=gsub("_|,", "", reference),
         reference=recode(reference,
                          "Sheridan Henderson McMahan 2003"="Sheridan, Henderson, and McMahan 2003")) %>%
  # Format year
  mutate(year=gsub(" - ", "-", year)) %>%
  # Format species name
  mutate(species=gsub("_", " ", species),
         species=stringr::str_squish(species),
         species=recode(species,
                        "Cyprinodon variegatus"="Cyprinodon variegatus",
                        "Gobionellus shufeldti"="Ctenogobius shufeldti",
                        "Haemulon plumieri"="Haemulon plumierii",
                        "Monacanthus hispidus"="Stephanolepis hispida",
                        "Dyspanopeus texana"="Dyspanopeus texanus",
                        "Metacarcinus gracilis"="Metacarcinus gracilis",
                        "Anchoa mischilli"="Anchoa mitchilli",
                        "Brevoorita patronus"="Brevoortia patronus",
                        "Cynoscion nebulous"="Cynoscion nebulosus",
                        "Leiosomus xanthurus"="Leiostomus xanthurus",
                        "Micropogonias undulates"="Micropogonias undulatus",
                        "Ophinodon elongatus"="Ophiodon elongatus",
                        "Panopeus depressus"="Eurypanopeus depressus",
                        "Paralichithys lethostigma"="Paralichthys lethostigma",
                        "Pontinus longispinus"="Pontinus longispinis",
                        "Scomberomorus maculates"="Scomberomorus maculatus",
                        "Syngnathus scoveili"="Syngnathus scovelli",
                        "Urophcis tenuis"="Urophycis tenuis")) %>%
  # Format sampling type
  mutate(sampling_type=gsub("_ |_", "/", sampling_type) %>% tolower()) %>%
  # Remove less important columns
  select(-c(no, x1)) %>%
  # Rearrange columns
  # Site attributes, response attributes, predictor attributes
  select(id, country, region, state, location, lat_dd, long_dd, # site
         reference, study_id, # reference
         year, season, season, # year/season
         taxa_type, family, genus, species, # taxa
         lifestage, # lifestage
         sampling_type, # sampling info
         # Habitat
         coastal_estuarine,
         # Environmental
         # sal_regime:sal_unit, # salinity
         # temp_mean:temp_range, # temperature
         # do_mean:do_unit, # dissolved oxygen
         # depth_mean:depth_standard, # depth
         everything())

# Inspect data
str(data)
freeR::complete(data)

# Is ID unique?
anyDuplicated(data$id) # must be 0

# Inspect site info
table(data$country)
table(data$region)
table(data$state)
table(data$location)

# Inspect reference info
sort(unique(data$reference))
sort(unique(data$study_id))

# Time of year
sort(unique(data$year))
table(data$season)

# Taxonomic info
table(data$taxa_type)
table(data$family)
table(data$genus)
sort(unique(data$species))

sort(unique(data$common_name))

# Build taxa key
# Lots of work to do on this
taxa_key <- data %>%
  select(taxa_type, family, genus, species) %>%
  unique() %>%
  arrange(taxa_type, family, genus, species)

# Any duplicated taxa?
freeR::which_duplicated(taxa_key$species)


# Check names
names_wrong <- freeR::check_names(taxa_key$species)
names_wrong_no_comma <- names_wrong[!grepl(",|spp", names_wrong)]
# freeR::suggest_names(names_wrong_no_comma)

# Life stage
sort(unique(data$lifestage))

# Sampling info
sort(unique(data$sampling_type))

# Habitat info
table(data$coastal_estuarine)

# Salinity
sort(unique(data$sal_regime))


# Habitat
sort(unique(data$habitat_struct))
sort(unique(data$habitat_struct_2))
sort(unique(data$habitat_species))
sort(unique(data$habitat_control)) # are unvegetated, unvegetated (open water), and nonvegetated all the same?
sort(unique(data$habitat_interfacing))
sort(unique(data$habitat_natural_status))
sort(unique(data$habitat_natural_info))
sort(unique(data$habitat_edge))
sort(unique(data$habitat_connectivity_dist)) # complex strings - could be broken apart
sort(unique(data$habitat_connectivity_general))
sort(unique(data$restoration_method))


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "lenfest_data_v1.Rds"))


# Plot data
################################################################################

# World
world <- rnaturalearth::ne_countries(scale="small", returnclas="sf")

# Map sites
range(data$lat_dd)
range(data$long_dd)
ggplot() +
  # World
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2) +
  # Sites
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, color=state)) +
  # Labels
  labs(x="", y="") +
  scale_fill_discrete(name="Region") +
  # Crop
  coord_sf(xlim=c(-160,-50), ylim=c(15, 70)) +
  # Theme
  theme_bw()

# Data coverage
data_coverage <- data %>%
  # Reduce to observations with LRR
  filter(!is.na(lrr)) %>%
  # Simplify
  select(id, temp_mean, sal_mean, do_mean, depth_mean) %>%
  # Gather
  gather(key="variable", value="value", 2:ncol(.)) %>%
  # Recode value
  mutate(value=ifelse(!is.na(value), TRUE, FALSE))

# Order observations
obs_order <- data_coverage %>%
  group_by(id) %>%
  summarize(nvar=sum(value)) %>%
  ungroup() %>%
  arrange(desc(nvar))

# Order variables
var_order <- data_coverage %>%
  group_by(variable) %>%
  summarize(nobs=sum(value)) %>%
  ungroup() %>%
  arrange(desc(nobs))

# Apply order
data_coverage_ordered <- data_coverage %>%
  # Factor ids
  mutate(id=factor(id, levels=obs_order$id %>% rev())) %>%
  # Factor variables
  mutate(variable=factor(variable, levels=var_order$variable))

# Plot coverage
ggplot(data_coverage_ordered, aes(y=id, x=variable, fill=value)) +
  geom_tile() +
  # Labels
  labs(x="Predictor variable", y="Observation id",
       title="Predictor variable data availability") +
  # Legend
  scale_fill_manual(name="Data available?", values=c("grey95", "grey30")) +
  # Theme
  theme_bw() +
  theme(axis.text.y=element_blank(),
        legend.position = "bottom")

