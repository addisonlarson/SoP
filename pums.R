library(tidycensus); library(ipumsr); library(tidyverse);
library(tigris); library(sf)
pmNJ <- pumas(34)
pmPA <- pumas(42)
pm <- rbind(pmNJ, pmPA)
pm <- st_as_sf(pm)
dvrpc <- read_sf("D:/alarson/SEPAPedCounting/data/dvrpctrcts.shp") %>%
  st_combine(.) %>%
  st_union(.) %>%
  st_transform(., 4269)
pmReg <- st_intersection(pm, dvrpc)
pmId <- as.numeric(pmReg$PUMACE10)

ddi <- read_ipums_ddi("D:/alarson/SuburbanizationPoverty/CensusData/usa_00002.xml")
pums <- read_ipums_micro(ddi)
pums %<>% subset(OWNERSHP == 2) %>%
  subset(PUMA %in% pmId)
