# do we have evidence to suggest that poverty has significantly increased
# in our neighborhoods of interest as compared to the rest of the region?
# problematic that I don't have MOEs but I'm running with this as a proof of concept
rm(list=ls())
library(sf); library(dplyr)
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
pov90 <- pov90[!(pov90$GISJOIN %in% r90$GISJOIN),]
pov00 <- pov00[!(pov00$GISJOIN %in% r00$GISJOIN),]
pov10 <- pov10[!(pov10$GISJOIN %in% r10$GISJOIN),]
pov16 <- pov16[!(pov16$GEOID %in% r16$GEOID),]
# until we can actually link these features....
# take a look at changing group means
region <- list(as.data.frame(pov90),
               as.data.frame(pov00),
               as.data.frame(pov10),
               as.data.frame(pov16))
regionMeans <- sapply(region, function(x) mean(rowSums(x[,2:3])))
cluster <- list(as.data.frame(cPov90),
                as.data.frame(cPov00),
                as.data.frame(cPov10),
                as.data.frame(cPov16))
clusterMeans <- sapply(cluster, function(x) mean(x[,6]))
# ALTERNATIVELY, not the region v. the suburban clusters,
# but the surburban clusters v. the city clusters

