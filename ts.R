# do we have evidence to suggest that poverty has significantly increased
# in our neighborhoods of interest as compared to the rest of the region?
# problematic that I don't have MOEs but I'm running with this as a proof of concept
rm(list=ls())
library(sf); library(dplyr)
library(ggplot2); library(stringr)
setwd("D:/alarson/SuburbanizationPoverty/Outputs")
# region
pov90 <- st_read("./pov90.shp", stringsAsFactors = FALSE)
pov00 <- st_read("./pov00.shp", stringsAsFactors = FALSE)
pov10 <- st_read("./pov10.shp", stringsAsFactors = FALSE)
pov16 <- st_read("./pov16.shp", stringsAsFactors = FALSE)
# counties (must remove cities from them before overlay with region)
cPov90 <- st_read("./countyPov90.shp", stringsAsFactors = FALSE)
cPov00 <- st_read("./countyPov00.shp", stringsAsFactors = FALSE)
cPov10 <- st_read("./countyPov10.shp", stringsAsFactors = FALSE)
cPov16 <- st_read("./countyPov16.shp", stringsAsFactors = FALSE)
# contains city / burb / twp / borough info
clusters <- read.csv("D:/alarson/SuburbanizationPoverty/CensusData/TargetTracts.csv",
                     stringsAsFactors = FALSE)
# use this for all overlays
spClusters <- merge(cPov16, clusters, by = "GEOID")
spClusters <- spClusters[(spClusters$Type != "City"),]
spClusters <- spClusters[c(1)]
# remains (shps without city trcts with % residents 0-199% FPL > 1.5 SD county mean)
r90 <- st_intersection(cPov90, spClusters)
r00 <- st_intersection(cPov00, spClusters)
r10 <- st_intersection(cPov10, spClusters)
r16 <- st_intersection(cPov16, spClusters)
r90 <- distinct(r90, GISJOIN, .keep_all = TRUE)
r00 <- distinct(r00, GISJOIN, .keep_all = TRUE)
r10 <- distinct(r10, GISJOIN, .keep_all = TRUE)
r16 <- distinct(r16, GEOID, .keep_all = TRUE)
# remove features from pov90 etc. by GISJOIN
# regionwide tracts, including those with low levels of low-income residents
pov90 <- pov90[!(pov90$GISJOIN %in% r90$GISJOIN),]
pov00 <- pov00[!(pov00$GISJOIN %in% r00$GISJOIN),]
pov10 <- pov10[!(pov10$GISJOIN %in% r10$GISJOIN),]
pov16 <- pov16[!(pov16$GEOID %in% r16$GEOID),]
# just the urban tracts with % residents 0-199% FPL > 1.5 SD county mean
city90 <- cPov90[!(cPov90$GISJOIN %in% r90$GISJOIN),]
city00 <- cPov00[!(cPov00$GISJOIN %in% r00$GISJOIN),]
city10 <- cPov10[!(cPov10$GISJOIN %in% r10$GISJOIN),]
city16 <- cPov16[!(cPov16$GEOID %in% r16$GEOID),]
# until we can actually link these features....
# take a look at changing group means
region <- list(as.data.frame(pov90),
               as.data.frame(pov00),
               as.data.frame(pov10),
               as.data.frame(pov16))
regionMeans <- data.frame("Mean" = sapply(region, function(x) mean(x[,3])))
city <- list(as.data.frame(city90),
             as.data.frame(city00),
             as.data.frame(city10),
             as.data.frame(city16))
cityMeans <- data.frame("Mean" = sapply(city, function(x) mean(x[,4])))
cluster <- list(as.data.frame(cPov90),
                as.data.frame(cPov00),
                as.data.frame(cPov10),
                as.data.frame(cPov16))
clusterMeans <- data.frame("Mean" = sapply(cluster, function(x) mean(x[,4])))

regionMeans$Type <- "Region"; cityMeans$Type <- "Low-Income Urban"; clusterMeans$Type <- "Low-Income Suburban/Rural"
regionMeans$Year <- c(1990,2000,2010,2016); cityMeans$Year <- c(1990,2000,2010,2016); clusterMeans$Year <- c(1990,2000,2010,2016)
regionMeans$YearPrev <- c(NA, regionMeans[1:3,1])
cityMeans$YearPrev <- c(NA, cityMeans[1:3,1])
clusterMeans$YearPrev <- c(NA, clusterMeans[1:3,1])
regionMeans$YoY <- (regionMeans$Mean - regionMeans$YearPrev) / regionMeans$YearPrev * 100
cityMeans$YoY <- (cityMeans$Mean - cityMeans$YearPrev) / cityMeans$YearPrev * 100
clusterMeans$YoY <- (clusterMeans$Mean - clusterMeans$YearPrev) / clusterMeans$YearPrev * 100
regionMeans$BaseChg <- (regionMeans$Mean - regionMeans$Mean[1]) / regionMeans$Mean[1] * 100
cityMeans$BaseChg <- (cityMeans$Mean - cityMeans$Mean[1]) / cityMeans$Mean[1] * 100
clusterMeans$BaseChg <- (clusterMeans$Mean - clusterMeans$Mean[1]) / clusterMeans$Mean[1] * 100

dat <- rbind(regionMeans, cityMeans, clusterMeans)
# tiff("yoyByType.tiff", units = "in", width = 8, height = 6, res = 600, compression = "lzw")
ggplot(dat, aes(x = Year, y = YoY, group = Type)) +
  geom_line(aes(color = Type)) +
  geom_point(aes(color = Type)) +
  theme_minimal() +
  labs(title = "Low-income urban neighborhoods (probably) exhibit concentrating poverty",
       subtitle = "Probably, because I dropped the MOEs",
       y = "Percent change from previous year") +
  scale_x_continuous(limits = c(2000,2016))
# dev.off()
# tiff("meansByType.tiff", units = "in", width = 8, height = 6, res = 600, compression = "lzw")
ggplot(dat, aes(x = Year, y = Mean, group = Type)) +
  geom_line(aes(color = Type)) +
  geom_point(aes(color = Type)) +
  theme_minimal() +
  labs(title = "Percentage of low-income residents on the rise regionwide",
       subtitle = "Individuals making less than 200% FPL,\nor $48,500 for a family of four in 2016.",
       y = "Percentage low-income residents")
# dev.off()
# tiff("pctChgByType.tiff", units = "in", width = 8, height = 6, res = 600, compression = "lzw")
ggplot(dat, aes(x = Year, y = BaseChg, group = Type)) +
  geom_line(aes(color = Type)) +
  geom_point(aes(color = Type)) +
  theme_minimal() +
  labs(title = "Meaning of this graph unk",
       subtitle = "Individuals making less than 200% FPL,\nor $48,500 for a family of four in 2016.",
       y = "Percentage change in low-income residents")
# dev.off()

# now for the fun part...matching up tracts from 1990 to now
# use LTDB to standardize areas to 2010. Must have original GEOID
# requires STATA, so will do on my home PC later
setwd("D:/alarson/SuburbanizationPoverty/CensusData")
xwalk90 <- read.csv("crosswalk_1990_2010.csv", stringsAsFactors = FALSE)
xwalk00 <- read.csv("crosswalk_2000_2010.csv", stringsAsFactors = FALSE)
xwalk90$trtid90 <- as.character(xwalk90$trtid90)
xwalk90$trtid10 <- as.character(xwalk90$trtid10)
pov <- read.csv("nhgis0006_ts_nominal_tract.csv", stringsAsFactors = FALSE)
pov$xwalk <- paste(pov$STATEFP, pov$COUNTYFP, sep = "_")
keep <- subset(pov, xwalk %in% c("34_5", "34_7", "34_15",
                                 "34_21", "42_17", "42_29",
                                 "42_45", "42_91", "42_101"))
# e.g. 42101035300, SSCCCTTTTTT
keep$origGEOID <- paste0(keep$STATEFP, str_pad(keep$COUNTYFP, 3, "left", "0"), keep$TRACTA)
keep <- keep[c(1:4,52)]
pov90 <- merge(pov90, keep, by.x = "GISJOIN", by.y = "GJOIN1990")
pov00 <- merge(pov00, keep, by.x = "GISJOIN", by.y = "GJOIN2000")
pov90 <- as.data.frame(as_Spatial(pov90))
pov00 <- as.data.frame(as_Spatial(pov00))
write.csv(pov90, "D:/alarson/SuburbanizationPoverty/SoP/port_90.csv", row.names = FALSE)
write.csv(pov00, "D:/alarson/SuburbanizationPoverty/SoP/port_00.csv", row.names = FALSE)
