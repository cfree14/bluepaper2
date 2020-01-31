
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(grid)
library(gridExtra)
library(tidyverse)
library(countrycode)

# Directories
dir_capture <- "/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines"
dir_plots <- "figures"

# Read data
wc_orig <- readRDS(file.path(dir_capture, "gaines_eez_level_results_approach1_2100.Rds"))
# tmsy <- readRDS(file.path(dir_capture, "gaines_territory_level_msy_time_series_by_type.Rds"))

# World
world <- rnaturalearth::ne_countries(scale="large", type = "countries", returnclass = "sf") %>% 
  mutate(iso3=countrycode(name_long, "country.name", "iso3c"))

# Projections
moll <- "+proj=moll"
wgs84 <-"+init=epsg:4326"

# Read EEZs
eezdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/eezs/processed"
eezs <- readRDS(file.path(eezdir, "world_eezs_v8.Rds"))
  
# EEZ conetorids
eezs_pts <- eezs %>% 
  sf::st_transform(moll) %>% 
  sf::st_centroid()


# Build data
################################################################################

# Build data
data <- wc_orig %>% 
  ungroup() %>% 
  # Subset relevant scenarios
  filter(scenario%in%c("Full Adaptation", "No Adaptation") & rcp %in% c("RCP26", "RCP85")) %>% 
  # Format scenario labels
  mutate(rcp=recode(rcp, 
                    "RCP26"="RCP 2.6",
                    "RCP85"="RCP 8.5")) %>% 
  # Add percent change in MSY and cap percent differences
  mutate(c_pdiff=ifelse(abs(c_pdiff)>100, sign(c_pdiff)*100, c_pdiff), # cap at 100
         # Cap percent difference in profits [-100%, 300%]
         p_pdiff=ifelse(p_pdiff < -100, -100, p_pdiff), 
         p_pdiff=ifelse(p_pdiff > 100, 100, p_pdiff),
         # Calculate percent difference in MSY: cap at [-100%, 100%]
         msy_pdiff=(msy_tot2-msy_tot1)/msy_tot1*100,
         msy_pdiff=ifelse(abs(msy_pdiff)>100, sign(msy_pdiff)*100, msy_pdiff)) %>% 
  # Select columns and reshape
  select(rcp, sovereign_iso3, country, eez, eez_id, msy_pdiff, scenario, c_pdiff) %>% 
  spread(key="scenario", value="c_pdiff") %>% 
  # Gather after spreading
  gather(key="metric", value="value", 6:8) %>% 
  # Rename metric
  mutate(metric=recode(metric, 
                       "No Adaptation"="c_pdiff_bau",
                       "Full Adaptation"="c_pdiff_full"),
         metric_label=recode(metric, 
                             "msy_pdiff"="Projected change in\nmaximum sustainable yield",
                             "c_pdiff_bau"="Projected change in\ncatch with BAU mgmt.",
                             "c_pdiff_full"="Projected change in\ncatch with adaptive mgmt."),
         metric_label=factor(metric_label,
                             levels=c("Projected change in\nmaximum sustainable yield",
                                      "Projected change in\ncatch with BAU mgmt.",
                                      "Projected change in\ncatch with adaptive mgmt.")))
  


# Plot data
################################################################################

# Add data to EEZs
data_sf_pt <- eezs_pts %>% 
  left_join(select(data, rcp, eez, metric, metric_label, value), by=c("eez")) %>% 
  filter(!is.na(value))

# Add data to EEZs
data_sf_poly <- eezs %>% 
  left_join(select(data, rcp, eez, metric, metric_label, value), by=c("eez")) %>% 
  filter(!is.na(value))

# My theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  strip.text = element_text(size=8),
                  legend.title=element_text(size=8),
                  legend.text = element_text(size=6),
                  panel.grid.major = element_line(size=0.2),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data_sf_poly) +
  # geom_sf(aes(color=value)) + # point version
  geom_sf(aes(fill=value), lwd=0.05, col="grey30") + # poly version
  facet_grid(metric_label ~ rcp) +
  geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,50),
                       labels=c("-100", "-50", "0", "50", ">100"),
                       name="Percent change\n(2100 relative to today)") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
#g

# Export figure
ggsave(g, filename=file.path(dir_plots, "Fig1_fisheries.png"), 
       width=6.5, height=6, units="in", dpi=600)

