
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
datadir <- "data/raw/v1"
outdir <- "data/processed"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "lenfest_paired db_2021_1007_cleaned.xlsx"), na=c("NA"), sheet=1)

# A nice guide
# https://topepo.github.io/caret/variable-importance.html


# 1. Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  # Format id
  mutate(id=1:n()) %>%
  # Format country
  mutate(country=recode(country, "USA"="United States")) %>%
  # Format region
  rename(region1=region_general, region2=region) %>%
  mutate(region1=gsub("coast", "Coast", region1)) %>%
  mutate(region2=recode(region2,
                        "Atlantic_Mid"="Mid-Atlantic",
                        "Atlantic_MID"="Mid-Atlantic",
                        "Atlantic_NE"="Northeast Atlantic",
                        "Atlantic_SE"="Southeast Atlantic",
                        "Gulf of Mexico_N"="Northern Gulf of Mexico",
                        "Gulf of Mexico_NE"="Northeastern Gulf of Mexico",
                        "Gulf of Mexico_SW"="Southwestern Gulf of Mexico",
                        "Pacific_NW"="Northwest Pacific",
                        "Pacific_SW"="Southwest Pacific")) %>%
  # Format state
  mutate(state=stringr::str_to_title(state),
         state=recode(state, "Virginia_ Maryland"="Virginia/Maryland")) %>%
  # Format location
  mutate(location=gsub("_ ", "/", location)) %>%
  # Format site
  mutate(site=gsub("_ ", "/", site),
         site=stringr::str_to_title(site)) %>%
  # Format coordinates
  # separate(col=coord, sep=" ", into=c("lat_dd", "long_dd"), remove=F) %>%
  # mutate(lat_dd=as.numeric(lat_dd),
  #        long_dd=as.numeric(long_dd)) %>%
  rename(lat_dd=lat, long_dd=long) %>%
  # Format reference
  mutate(reference=gsub("&", "and", reference),
         reference=gsub("et al ", "et al. ", reference),
         reference=recode(reference,
                          "Lazzari_ 2002"="Lazzari 2002",
                          "Sheridan_ Henderson_ McMahan 2003"="Sheridan, Henderson, and McMahan 2003")) %>%
  # Format season
  rename(season1=season_general, season2=season) %>%
  mutate(season1=recode(season1, "all season"="all seasons", "multiple"="multiple seasons"),
         season2=tolower(season2),
         season2=recode(season2,
                        "all season"="all seasons")) %>%
  # Format taxonomy
  rename(taxa1=taxa_general, taxa2=taxa) %>%
  mutate(taxa2=gsub("_ |_", "/", taxa2),
         taxa1=ifelse(is.na(taxa1) & taxa2=="crustacean", "invertebrate", taxa1)) %>%
  # Format species name
  mutate(species=gsub("_ ", ", ", species),
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
  # Format common name
  rename(comm_name=common_name) %>%
  mutate(comm_name=gsub("_ ", ", ", comm_name),
         comm_name=stringr::str_squish(comm_name),
         comm_name=stringr::str_to_sentence(comm_name),
         comm_name=recode(comm_name,
                          "Atlantic tompcod"="Atlantic tomcod",
                          "Gafftopsail sea catfish"="Gafftopsail catfish",
                          "Inshore lizzardfish"="Inshore lizardfish",
                          "Sand seatrout"="Sand weakfish",
                          "Spotted seatrout"="Spotted weakfish",
                          "Skillet fish"="Skilletfish",
                          "Feathered blenny"="Feather blenny")) %>%
  # Fix a few taxonomies
  mutate(taxa1=ifelse(comm_name=="Northern white shrimp", "invertebrate", taxa1),
         taxa2=ifelse(comm_name=="Northern white shrimp", "crustacean", taxa2)) %>%
  # Format life stage
  rename(lifestage1=lifestage_origil,
         lifestage2=lifestage_mod,
         lifestage3=lifestage_mod_fil) %>%
  mutate(lifestage1=tolower(lifestage1),
         lifestage2=tolower(lifestage2),
         lifestage3=tolower(lifestage3),
         lifestage_info=gsub("_ ", "/", lifestage_info)) %>%
  # Format habitat
  mutate(habitat_struct=gsub("_ ", "/", habitat_struct),
         habitat_control=tolower(habitat_control)) %>%
  # Format habitat species
  mutate(habitat_species=gsub("_ ", ", ", habitat_species)) %>%
  # Remove less important columns
  # species_note=empty, coord=redundant
  select(-c(order, database_origin, data_extract, coord, species_note)) %>%
  # Rearrange columns
  # Site attributes, response attributes, predictor attributes
  select(country, region1, region2, state, location, site, lat_dd, long_dd, # site
         reference, study_id, # reference
         year, season1, season2, # year/season
         taxa1, taxa2, comm_name,  species, # taxa
         lifestage1, lifestage2, lifestage3, lifestage_info, size, unit, # lifestage
         sampling_type, sampling_gear, mesh_size, # sampling info
         # Habitat
         coastal_estuarine,
         # Environmental
         sal_regime:sal_unit, # salinity
         temp_mean:temp_range, # temperature
         do_mean:do_unit, # dissolved oxygen
         depth_mean:depth_standard, # depth
         everything())

# Inspect data
str(data)
freeR::complete(data)

# Is ID unique?
anyDuplicated(data$id) # must be 0

# Inspect site info
table(data$country)
table(data$region1)
table(data$region2)
table(data$state)
table(data$location)
table(data$site)
table(data$coord)

# Inspect reference info
sort(unique(data$reference))
sort(unique(data$study_id))

# Time of year
sort(unique(data$year))
table(data$season1)
table(data$season2)

# Taxonomic info
table(data$taxa1)
table(data$taxa2)
sort(unique(data$common_name))
sort(unique(data$species))

# Build taxa key
# Lots of work to do on this
taxa_key <- data %>%
  select(taxa1, taxa2, comm_name, species) %>%
  unique() %>%
  arrange(taxa1, taxa2, comm_name)

# Any duplicated taxa?
freeR::which_duplicated(taxa_key$species)
freeR::which_duplicated(taxa_key$comm_name)


# Check names
names_wrong <- freeR::check_names(taxa_key$species)
names_wrong_no_comma <- names_wrong[!grepl(",|spp", names_wrong)]
# freeR::suggest_names(names_wrong_no_comma)

# Life stage
sort(unique(data$lifestage1))
sort(unique(data$lifestage2))
sort(unique(data$lifestage3))
sort(unique(data$lifestage_info))
sort(unique(data$size))

# Sampling info
sort(unique(data$sampling_type))
sort(unique(data$sampling_gear))
sort(unique(data$mesh_size))

# Habitat info
table(data$coastal_estuarine)

# Salinity
sort(unique(data$sal_regime))
sort(unique(data$sal_unit))

# Dissolved oxygen
sort(unique(data$do_unit)) # standardizable?

# Depth
sort(unique(data$depth_standard)) # standardizable?

# Tidal zones
sort(unique(data$tidal_zone_cont))
sort(unique(data$tidal_zone_struct))
sort(unique(data$tidal_zone_struct_org))

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
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, color=region2)) +
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

