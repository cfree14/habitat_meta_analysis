
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
datadir <- "data"

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
  mutate(taxa2=gsub("_ |_", "/", taxa2)) %>%
  # Format common name
  rename(comm_name=common_name) %>%
  # Format life stage
  rename(lifestage1=lifestage_origil,
         lifestage2=lifestage_mod,
         lifestage3=lifestage_mod_fil) %>%
  mutate(lifestage1=tolower(lifestage1),
         lifestage2=tolower(lifestage2),
         lifestage3=tolower(lifestage3),
         lifestage_info=gsub("_ ", "/", lifestage_info)) %>%
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




# 2. Learn about data
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


# 3. Split data
################################################################################

# Training data
data_train_no_nas <- data %>%
  # Reduce to observations with a response variable
  filter(!is.na(lrr) & !is.na(sal_mean) & !is.na(depth_mean) & !is.na(temp_mean))

# Training data with NAs
data_train_nas <- data %>%
  # Simplify
  select(lrr, sal_mean, depth_mean, temp_mean) %>%
  # Reduce to observations with a response variable
  filter(!is.na(lrr))

# Impute NAs
data_train_imputed <- data_train_nas %>%
  rfImpute(lrr ~ ., .)

# Prediction data
data_pred <- data %>%
  filter(is.na(lrr))


# 4. Fit model
################################################################################

# Define tuning parameter grid
# mtry = Number of variables randomly sampled as candidate variables
fitGrid <- expand.grid(mtry=seq(4, 12, 4))

# Define tuning and training method
fitControl <- caret::trainControl(method="repeatedcv", number=10, repeats=10)

# Train RF model
rf_fit <- caret::train(lrr ~ sal_mean + temp_mean + depth_mean,
                       data=data_train_imputed,
                       method="rf",
                       metric="RMSE",
                       tuneGrid=fitGrid, trControl=fitControl, verbose=F)


# 5. Inspect model fit
################################################################################




# 6. Inspect variable importance
################################################################################

# Functions for extracting variable importance
# caret::varImp()
# randomForest::importance()
# randomForest::varImpPlot()

# Extract variable importance
var_imp_list <- caret::varImp(rf_fit)

# Format variable importance
var_imp_df <- var_imp_list[[1]] %>%
  # Add variable
  rownames_to_column(var="variable") %>%
  # Rename importance columns
  rename(importance=Overall) %>%
  # Arrange columns
  select(variable, importance) %>%
  # Arrange rows
  arrange(desc(importance))

# Plot variable importance
ggplot(var_imp_df, aes(x=importance, y=reorder(variable, importance))) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Importance", y="", title="Variable importance") +
  # Theme
  theme_bw()

# Random forest method
varImpPlot(rf_fit$finalModel)


# 7. Inspect marginal effects
################################################################################

# Functions for extracting marginal effects (partial dependence)
# pdp::partial() - listing more than 1 computes interaction
# edarf::partial_dependence() - this package is not maintained
# randomForest::margin()

# Extract marginal effects
variables_do <- c("sal_mean", "temp_mean", "depth_mean")
pdp_df_orig <- purrr::map_df(variables_do, function(x){

  # Compute marginal effects for predictor
  df <- pdp::partial(rf_fit, pred.var=x)

  # Format
  df1 <- df %>%
    # Convert to dataframe
    as.matrix() %>%
    as.data.frame() %>%
    # Rename columns
    setNames(c("value", "yhat")) %>%
    # Add variable
    mutate(variable=x) %>%
    # Arrange
    select(variable, value, yhat)


})

# Plot marginal effects
ggplot(pdp_df_orig, mapping=aes(x=value, y=yhat)) +
  facet_wrap(~variable, scales="free_x") +
  geom_line() +
  # Labels
  labs(x="Variable value", y="Marginal effect") +
  # Theme
  theme_bw()


# 8. Make predictions
################################################################################

# Predict LRRs for sites without LRRs
# (not currently predicting to sites with NA values)
?predict.randomForest
lrr_preds <- predict(rf_fit, newdata=data_pred)










