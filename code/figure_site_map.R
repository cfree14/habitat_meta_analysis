
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
data <- readRDS(file.path(datadir, "lenfest_data.Rds")) %>%
  # Order regions
  mutate(region2=factor(region2, levels=c("Northwest Pacific", "Southwest Pacific", "Southwestern Gulf of Mexico",
                                        "Northern Gulf of Mexico", "Northeastern Gulf of Mexico", "Southeast Atlantic", "Mid-Atlantic", "Northeast Atlantic"))) %>%
  # Unique sites
  group_by(region2, lat_dd, long_dd) %>%
  summarize(n=n()) %>% # not sure what n really is
  ungroup() %>%
  arrange(region2, desc(n))


# Plot data
################################################################################

# World
world <- rnaturalearth::ne_countries(scale="small", returnclas="sf")

# Map sites
range(data$lat_dd)
range(data$long_dd)

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_blank(),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.2,0.25),
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot sites
g <- ggplot() +
  # World
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2) +
  # Sites
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, fill=region2), pch=21, size=1.5, stroke=0.1) +
  # Labels
  labs(x="", y="") +
  scale_fill_discrete(name="") +
  # Crop
  coord_sf(xlim=c(-160,-50), ylim=c(15, 70)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_site_map.png"),
       width=4.5, height=3.3, units="in", dpi=600)


