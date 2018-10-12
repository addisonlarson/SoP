library(rgdal); library(spatialEco)
library(ape); library(ggplot2)
library(tidycensus); library(tidyverse)
library(stringr); library(tigris)
setwd("D:/alarson/SuburbanizationPoverty/CensusData")
trct90 <- readOGR(".", "US_tract_1990_conflated", stringsAsFactors = FALSE)
trct00 <- readOGR(".", "US_tract10_2000", stringsAsFactors = FALSE)
trct10 <- readOGR(".", "US_tract_2010", stringsAsFactors = FALSE) 
pov <- read.csv("nhgis0006_ts_nominal_tract.csv", stringsAsFactors = FALSE)
pov$xwalk <- paste(pov$STATEFP, pov$COUNTYFP, sep = "_")
keep <- subset(pov, xwalk %in% c("34_5", "34_7", "34_15",
                                 "34_21", "42_17", "42_29",
                                 "42_45", "42_91", "42_101"))
keep90 <- keep$GJOIN1990
keep00 <- keep$GJOIN2000
keep10 <- keep$GJOIN2012
trct90 <- trct90[c(4)]; trct00 <- trct00[c(15)]; trct10 <- trct10[c(13)]
pov <- subset(pov, NHGISCODE %in% keep$NHGISCODE)
pov <- pov[, -( grep("\\M$" , colnames(pov), perl = TRUE))]
# universe by year
pov$univ_90 <- rowSums(pov[seq(15, 39, 3)])
pov$univ_00 <- rowSums(pov[seq(16, 40, 3)])
pov$univ_10 <- rowSums(pov[seq(17, 41, 3)])
# bl 100% by year
pov$tot99_90 <- rowSums(pov[seq(15, 21, 3)])
pov$tot99_00 <- rowSums(pov[seq(16, 22, 3)])
pov$tot99_10 <- rowSums(pov[seq(17, 23, 3)])
# 100-199% by year
pov$tot199_90 <- rowSums(pov[seq(24, 35, 3)])
pov$tot199_00 <- rowSums(pov[seq(25, 37, 3)])
pov$tot199_10 <- rowSums(pov[seq(26, 38, 3)])
# 200%+ by year
pov$tot200_90 <- pov$C20AI1990
pov$tot200_00 <- pov$C20AI2000
pov$tot200_10 <- pov$C20AI125
# percentages
pov <- pov[c(1:4,42:54)]
pctList <- list()
for (i in 6:8){
  pct99 <- pov[i + 3] / pov [i] * 100
  pctList[[i - 5]] <- pct99
  names(pctList)[i - 5] <- colnames(pov)[i]
  pct199 <- pov[i + 6] / pov [i] * 100
  pctList[[i - 2]] <- pct199
  names(pctList)[i - 2] <- colnames(pov)[i + 3]
  pct200 <- pov[i + 9] / pov [i] * 100
  pctList[[i + 1]] <- pct200
  names(pctList)[i + 1] <- colnames(pov)[i + 6]
}
pct <- as.data.frame(pctList)
colnames(pct) <- gsub("tot", "pct", colnames(pct))
pct$GID <- pov$NHGISCODE
pct <- merge(pct, pov, by.x = "GID", by.y = "NHGISCODE")
pct <- pct[c(1:13)]

varNums <- str_pad(as.character(1:131), 3, "left", pad = 0)
myCall <- paste0("B17024_", varNums, "E")
collect <- get_acs(geography = "tract",
                    state = c(34,42),
                    year = 2016,
                    output = "wide",
                    variables = myCall)
# Clean up fields
collect <- collect[, -( grep("\\M$" , colnames(collect), perl = TRUE))]
# Get state/county info so we can split df by MSA
collect$st <- substr(collect$GEOID, 1, 2)
collect$cty <- substr(collect$GEOID, 3, 5)
collect$stcty <- paste0(collect$st, collect$cty)
# Subset out DVRPC counties
dvrpc <- c("34005", "34007", "34015", "34021",
           "42017", "42029", "42045", "42091", "42101")
collect <- subset(collect, stcty %in% dvrpc)
collect <- collect[,-which(names(collect) %in% c("NAME1",
                                                 "GEOID1",
                                                 "NAME2",
                                                 "GEOID2",
                                                 "NAME3",
                                                 "GEOID3",
                                                 "NAME4",
                                                 "GEOID4",
                                                 "NAME5",
                                                 "GEOID5"))]
# universe 2016
collect$univ_16 <- collect$B17024_001E
# bl 100%
collect$tot99_16 <- rowSums(collect[c(seq(5, 133, 13),
                                      seq(6, 133, 13),
                                      seq(7, 133, 13))])
# 100-199%
collect$tot199_16 <- rowSums(collect[c(seq(8, 133, 13),
                                       seq(9, 133, 13),
                                       seq(10, 133, 13),
                                       seq(11, 133, 13),
                                       seq(12, 133, 13))])
# 200%+
collect$tot200_16 <- rowSums(collect[c(seq(13, 133, 13),
                                       seq(14, 133, 13),
                                       seq(15, 133, 13),
                                       seq(16, 133, 13))])
collect$pct99_16 <- collect$tot99_16 / collect$univ_16 * 100
collect$pct199_16 <- collect$tot199_16 / collect$univ_16 * 100
collect$pct200_16 <- collect$tot200_16 / collect$univ_16 * 100
collect <- collect[c(1,141:143)]

sts <- c("NJ", "PA") 
combined <- rbind_tigris(
  lapply(sts, function(x) {
    tracts(x, cb = TRUE)
  })
)
trct16 <- merge(combined, collect, by = "GEOID")
trct16 <- trct16[c(1,10:12)]
trct16 <- sp.na.omit(trct16)
trct16 <- spTransform(trct16, CRS("+init=epsg:26918"))

trct90 <- merge(trct90, pct, by.x = "GISJOIN", by.y = "GJOIN1990")
trct90 <- trct90[c(1, seq(3,9,3))]
trct90 <- sp.na.omit(trct90)
trct90 <- spTransform(trct90, CRS("+init=epsg:26918"))
trct00 <- merge(trct00, pct, by = "GISJOIN", by.y = "GJOIN2000")
trct00 <- trct00[c(1, seq(4,10,3))]
trct00 <- sp.na.omit(trct00)
trct00 <- spTransform(trct00, CRS("+init=epsg:26918"))
trct10 <- merge(trct10, pct, by = "GISJOIN", by.y = "GJOIN2012")
trct10 <- trct10[c(1, seq(5,11,3))]
trct10 <- sp.na.omit(trct10)
trct10 <- spTransform(trct10, CRS("+init=epsg:26918"))

# Export for maps
setwd("D:/alarson/SuburbanizationPoverty/Outputs")
# writeOGR(trct90, ".", "pov90", driver = "ESRI Shapefile")
# writeOGR(trct00, ".", "pov00", driver = "ESRI Shapefile")
# writeOGR(trct10, ".", "pov10", driver = "ESRI Shapefile")
# writeOGR(trct16, ".", "pov16", driver = "ESRI Shapefile")

# check out spatial ac
# using IDW...should or shouldn't use link matrix???
trct90coords <- coordinates(trct90)
trct90dist <- as.matrix(dist(trct90coords))
trct90idist <- 1 / trct90dist
trct90idist[!is.finite(trct90idist)] <- 0
trct00coords <- coordinates(trct00)
trct00dist <- as.matrix(dist(trct00coords))
trct00idist <- 1 / trct00dist
trct00idist[!is.finite(trct00idist)] <- 0
trct10coords <- coordinates(trct10)
trct10dist <- as.matrix(dist(trct10coords))
trct10idist <- 1 / trct10dist
trct10idist[!is.finite(trct10idist)] <- 0
trct16coords <- coordinates(trct16)
trct16dist <- as.matrix(dist(trct16coords))
trct16idist <- 1 / trct16dist
trct16idist[!is.finite(trct16idist)] <- 0

time <- matrix(c(1990, 2000, 2010, 2016), nrow = 4, ncol = 1)
a1 <- Moran.I(trct90$pct99_90, trct90idist)
a2 <- Moran.I(trct00$pct99_00, trct00idist)
a3 <- Moran.I(trct10$pct99_10, trct10idist)
a4 <- Moran.I(trct16$pct99_16, trct16idist)
spatialAC <- matrix(c(as.numeric(a1[1]),
                      as.numeric(a2[1]),
                      as.numeric(a3[1]),
                      as.numeric(a4[1])), nrow = 4, ncol = 1)
type <- "FPL Below 100%"
a <- cbind(time, spatialAC, type)

b1 <- Moran.I(trct90$pct199_90, trct90idist)
b2 <- Moran.I(trct00$pct199_00, trct00idist)
b3 <- Moran.I(trct10$pct199_10, trct10idist)
b4 <- Moran.I(trct16$pct199_16, trct16idist)
spatialAC <- matrix(c(as.numeric(b1[1]),
                      as.numeric(b2[1]),
                      as.numeric(b3[1]),
                      as.numeric(b4[1])), nrow = 4, ncol = 1)
type <- "FPL 100-199%"
b <- cbind(time, spatialAC, type)

c1 <- Moran.I(trct90$pct200_90, trct90idist)
c2 <- Moran.I(trct00$pct200_00, trct00idist)
c3 <- Moran.I(trct10$pct200_10, trct10idist)
c4 <- Moran.I(trct16$pct200_16, trct16idist)
spatialAC <- matrix(c(as.numeric(c1[1]),
                      as.numeric(c2[1]),
                      as.numeric(c3[1]),
                      as.numeric(c4[1])), nrow = 4, ncol = 1)
type <- "FPL 200+%"
c <- cbind(time, spatialAC, type)

moran <- as.data.frame(rbind(a,b,c))
colnames(moran) <- c("Year", "SpatialAC", "Level")
moran$Level <- as.factor(moran$Level)
moran$SpatialAC <- as.numeric(as.character(moran$SpatialAC))

# tiff("betweentracts.tiff", units = "in", width = 6, height = 6, res = 600, compression = "lzw")
ggplot(data = moran, aes(x = Year,
                         y = SpatialAC,
                         group = Level)) +
  geom_line(aes(color = Level)) +
  geom_point(aes(color = Level)) + theme_minimal() +
  labs(y = "Spatial Autocorrelation (Moran's i) of Poverty",
       title = "Spatial clustering of poverty, 1990-2016",
       subtitle = "All observations exhibit statistically significant spatial clustering")
# dev.off()

# next
# convert to points; look at cluster presence / movement (mclust)
# PUMS / gini coefficient
library(mclust)
# determine what's going to "count" as poverty for mclust...
# start out with +2 SD
allBlPov <- c(trct90$pct99_90, trct00$pct99_00, trct10$pct99_10, trct16$pct99_16)
cutBlPov <- mean(allBlPov) + 2 * sd(allBlPov) # 39.988%

# clusters of persons below 100% FPL
blPov90 <- trct90[trct90$pct99_90 >= cutBlPov,]
blPov00 <- trct00[trct00$pct99_00 >= cutBlPov,]
blPov10 <- trct10[trct10$pct99_10 >= cutBlPov,]
blPov16 <- trct16[trct16$pct99_16 >= cutBlPov,]
blPov90c <- coordinates(blPov90)
blPov00c <- coordinates(blPov00)
blPov10c <- coordinates(blPov10)
blPov16c <- coordinates(blPov16)

bic99_90 <- mclustBIC(blPov90c)
plot(bic99_90); summary(bic99_90)
mod99_90 <- Mclust(blPov90c, x = bic99_90)
summary(mod99_90, parameters = TRUE)
plot(mod99_90, what = "density",
     ylim = c(4373907.3,4495325.8),
     xlim = c(402585.1,552345.6))
mod99_90$parameters$mean

bic99_00 <- mclustBIC(blPov00c)
plot(bic99_00); summary(bic99_00)
mod99_00 <- Mclust(blPov00c, x = bic99_00)
summary(mod99_00, parameters = TRUE)
plot(mod99_00, what = "density",
     ylim = c(4373907.3,4495325.8),
     xlim = c(402585.1,552345.6))
mod99_00$parameters$mean

bic99_10 <- mclustBIC(blPov10c)
plot(bic99_10); summary(bic99_10)
mod99_10 <- Mclust(blPov10c, x = bic99_10)
summary(mod99_10, parameters = TRUE)
plot(mod99_10, what = "density",
     ylim = c(4373907.3,4495325.8),
     xlim = c(402585.1,552345.6))
mod99_10$parameters$mean

bic99_16 <- mclustBIC(blPov16c)
plot(bic99_16); summary(bic99_16)
mod99_16 <- Mclust(blPov16c, x = bic99_16)
summary(mod99_16, parameters = TRUE)
plot(mod99_16, what = "density",
     ylim = c(4373907.3,4495325.8),
     xlim = c(402585.1,552345.6))
mod99_16$parameters$mean

# ought to export the selected points and the tract cluster means
# points:
writeOGR(blPov90, ".", "blPov90", "ESRI Shapefile")
writeOGR(blPov00, ".", "blPov00", "ESRI Shapefile")
writeOGR(blPov10, ".", "blPov10", "ESRI Shapefile")
writeOGR(blPov16, ".", "blPov16", "ESRI Shapefile")

# cluster means:
mod99_90coords <- matrix(c(mod99_90$parameters$mean),
                         nrow = 4, ncol = 2, byrow = TRUE)
mod99_90cent <- SpatialPointsDataFrame(mod99_90coords,
                                       data.frame("oid" = 1:4),
                                       proj4string = CRS("+init=epsg:26918"))
writeOGR(mod99_90cent, ".", "blPov90cent", "ESRI Shapefile")

mod99_00coords <- matrix(c(mod99_00$parameters$mean),
                         nrow = 4, ncol = 2, byrow = TRUE)
mod99_00cent <- SpatialPointsDataFrame(mod99_00coords,
                                       data.frame("oid" = 1:4),
                                       proj4string = CRS("+init=epsg:26918"))
writeOGR(mod99_00cent, ".", "blPov00cent", "ESRI Shapefile")

mod99_10coords <- matrix(c(mod99_10$parameters$mean),
                         nrow = 5, ncol = 2, byrow = TRUE)
mod99_10cent <- SpatialPointsDataFrame(mod99_10coords,
                                       data.frame("oid" = 1:5),
                                       proj4string = CRS("+init=epsg:26918"))
writeOGR(mod99_10cent, ".", "blPov10cent", "ESRI Shapefile")

mod99_16coords <- matrix(c(mod99_16$parameters$mean),
                         nrow = 7, ncol = 2, byrow = TRUE)
mod99_16cent <- SpatialPointsDataFrame(mod99_16coords,
                                       data.frame("oid" = 1:7),
                                       proj4string = CRS("+init=epsg:26918"))
writeOGR(mod99_16cent, ".", "blPov16cent", "ESRI Shapefile")
