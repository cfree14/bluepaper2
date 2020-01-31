
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(readxl)
library(tidyverse)

# Directories
indir <- "data/oecd/raw"
outdir <- "data/oecd/processed"
plotdir <- "data/oecd/figures"

# Read data
gva_orig <- read_excel(file.path(indir, "Figure8.2.xls"))
jobs_orig <- read_excel(file.path(indir, "Figure8.3.xls"))

# Format data
################################################################################

# Format GVA
gva <- gva_orig %>% 
  slice(13:nrow(.)) %>% 
  select(2:ncol(.)) %>% 
  setNames(c("sector", "gva2010", "gva2030")) %>% 
  gather(key="year", value="gva", 2:ncol(.)) %>% 
  mutate(year=gsub("gva", "", year) %>% as.numeric, 
         gva=as.numeric(gva))

# Format jobs
jobs <- jobs_orig %>% 
  slice(13:nrow(.)) %>% 
  select(2:ncol(.)) %>% 
  setNames(c("sector", "jobs2010", "jobs2030")) %>% 
  gather(key="year", value="jobs", 2:ncol(.)) %>% 
  mutate(year=gsub("jobs", "", year) %>% as.numeric,
         jobs=as.numeric(jobs))

# Merge data
eco_sectors <- c("Fish processing", "Industrial marine aquaculture", "Industrial capture fisheries", "Marine and coastal tourism")
data <- gva %>% 
  left_join(jobs) %>% 
  mutate(sector_type=ifelse(sector %in% eco_sectors, "Ecosystem-dependent", "Ecosystem-independent"))

# Plot
g <- ggplot(data, aes(x=year, y=gva/1e12, fill=sector)) +
  geom_bar(stat="identity") +
  labs(x="", y="Gross value added (USD trillions)") +
  theme_bw()
g

# Plot
g <- ggplot(data, aes(x=year, y=jobs/1e6, fill=sector)) +
  geom_bar(stat="identity") +
  labs(x="", y="Full-time jobs (millions)") +
  theme_bw()
g








