
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
wc_orig <- readRDS(file.path(dir_capture, "gaines_territory_level_results_approach1.Rds"))
tmsy <- readRDS(file.path(dir_capture, "gaines_territory_level_msy_time_series_by_type.Rds"))

# World
world <- rnaturalearth::ne_countries(scale="large", type = "countries", returnclass = "sf") %>% 
  mutate(iso3=countrycode(name_long, "country.name", "iso3c"))


# Merge data
################################################################################

# Theme
my_theme <- theme(axis.title=element_blank(),
                  legend.position = "bottom")

# Calculate percent change in mean MSY from beginning to end
yrs1 <- 2012:2021; length(yrs1)
yrs2 <- 2091:2100; length(yrs2)
msy <- tmsy %>% 
  ungroup() %>% 
  # Sum MSY by EEZ and year
  group_by(rcp, sovereign, sovereign_iso3, year) %>%
  summarize(msy=sum(msy)) %>% 
  # Average over periods
  group_by(rcp, sovereign, sovereign_iso3) %>% 
  summarize(msy_avg1=mean(msy[year%in%yrs1]),
            msy_avg2=mean(msy[year%in%yrs2]),
            msy_perc=(msy_avg2-msy_avg1)/msy_avg1*100,
            msy_perc=pmin(300, msy_perc)) %>% 
  # Remove outrageous values
  filter(msy_perc <= 100 & msy_perc>=-100) %>% 
  # Reduce to scenarios of interest
  ungroup() %>% 
  filter(rcp%in%c("RCP45", "RCP85")) %>% 
  mutate(rcp=recode(rcp, "RCP45"="RCP 4.5", "RCP85"="RCP 8.5"))

# Add MSY to world
msy_sf <- world %>%
  left_join(msy, by=c("iso3"="sovereign_iso3")) %>% 
  filter(!is.na(msy_perc))

# Plot maps
g1 <- ggplot() +
  facet_wrap(~rcp, ncol=2) +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  geom_sf(data=msy_sf, aes(fill=msy_perc), lwd=0.05, col="grey30") +
  labs(title="Percent change in maximum sustainable yield (MSY)") +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,50),
                       name="Percent change in MSY\n(2100 relative to today)") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme
#g1

# Build data
data <- wc_orig %>% 
  ungroup() %>% 
  # Subset relevant scenarios
  filter(scenario%in%c("Full Adaptation", "No Adaptation") & rcp %in% c("RCP45", "RCP85")) %>% 
  # Format scenario labels
  mutate(rcp=recode(rcp, 
                    "RCP45"="RCP 4.5",
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
  select(rcp, scenario, sovereign_iso3, p_pdiff, c_pdiff) %>% 
  gather(key="outcome", value="perc", 4:5) %>% 
  mutate(outcome=recode(outcome, "p_pdiff"="Profits", "c_pdiff"="Catch")) %>% 
  # Reduce to just catch
  filter(outcome=="Catch")

# Add data to EEZ shapefile
out_sf <- world %>%
  left_join(data, by=c("iso3"="sovereign_iso3")) %>% 
  filter(!is.na(outcome) & !is.na(rcp))

# Plot all
g2 <- ggplot(out_sf) +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white") +
  geom_sf(aes(fill=perc), lwd=0.05, col="grey30") +
  geom_hline(aes(yintercept = 0), linetype="dotted", color="grey30", size=0.1) +
  facet_grid(scenario ~ rcp) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,50),
                       name="Percent change in catch\n(2100 relative to today)") +
  labs(title="Percent change in catch with adaptation") +
  # scale_fill_gradient2(low=RColorBrewer::brewer.pal(9, "RdBu")[1],
  #                      high=RColorBrewer::brewer.pal(9, "RdBu")[9],
  #                      mid=RColorBrewer::brewer.pal(9, "RdBu")[5],
  #                      midpoint=0,
  #                      limits=c(-100,250),
  #                      name="Percent change in catch\n(2100 relative to today)", na.value="grey80") +
  ylab("") +
  theme_bw() + theme(legend.position = "bottom")
#g2

# Merge plots and export
g <- grid.arrange(g1, g2, ncol=1, heights=c(1/3, 2/3))
ggsave(g, filename=file.path(dir_plots, "figure_capture_fisheries.png"), 
       width=6.5, height=8.5, units="in", dpi=600)

