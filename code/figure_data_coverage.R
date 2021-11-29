
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


# Build data
################################################################################

# Data coverage
data_coverage <- data %>%
  # Reduce to observations with LRR
  filter(!is.na(lrr)) %>%
  # Simplify
  select(id, temp_mean, sal_mean, do_mean, depth_mean) %>% # you have physical; habitat: cover/height/biomass/connectivity ()
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

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_blank(),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot coverage
g <- ggplot(data_coverage_ordered, aes(y=id, x=variable, fill=value)) +
  geom_tile() +
  # Labels
  labs(x="Predictor variable", y="Observation id",
       title="Predictor variable data availability") +
  # Legend
  scale_fill_manual(name="Data available?", values=c("grey95", "grey30")) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_data_coverage.png"),
       width=2.5, height=4.5, units="in", dpi=600)

