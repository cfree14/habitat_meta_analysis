
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
data <- readRDS(file.path(datadir, "lenfest_data.Rds"))

# A nice guide
# https://topepo.github.io/caret/variable-importance.html

# Steps
# 3. Split data
# 4. Fit model
# 5. Inspect model fit
# 6. Inspect variable importance
# 7. Inspect marginal effects
# 8. Make predictions


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










