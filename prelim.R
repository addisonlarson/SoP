library(rgdal); library(spatialEco); library(ape); library(ggplot2)
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
# setwd("D:/alarson/SuburbanizationPoverty/Outputs")
# writeOGR(trct90, ".", "pov90", driver = "ESRI Shapefile")
# writeOGR(trct00, ".", "pov00", driver = "ESRI Shapefile")
# writeOGR(trct10, ".", "pov10", driver = "ESRI Shapefile")

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

time <- matrix(c(1990, 2000, 2010), nrow = 3, ncol = 1)
a1 <- Moran.I(trct90$pct99_90, trct90idist)
a2 <- Moran.I(trct00$pct99_00, trct00idist)
a3 <- Moran.I(trct10$pct99_10, trct10idist)
spatialAC <- matrix(c(as.numeric(a1[1]),
                      as.numeric(a2[1]),
                      as.numeric(a3[1])), nrow = 3, ncol = 1)
type <- "FPL Below 100%"
a <- cbind(time, spatialAC, type)

b1 <- Moran.I(trct90$pct199_90, trct90idist)
b2 <- Moran.I(trct00$pct199_00, trct00idist)
b3 <- Moran.I(trct10$pct199_10, trct10idist)
spatialAC <- matrix(c(as.numeric(b1[1]),
                      as.numeric(b2[1]),
                      as.numeric(b3[1])), nrow = 3, ncol = 1)
type <- "FPL 100-199%"
b <- cbind(time, spatialAC, type)

c1 <- Moran.I(trct90$pct200_90, trct90idist)
c2 <- Moran.I(trct00$pct200_00, trct00idist)
c3 <- Moran.I(trct10$pct200_10, trct10idist)
spatialAC <- matrix(c(as.numeric(c1[1]),
                      as.numeric(c2[1]),
                      as.numeric(c3[1])), nrow = 3, ncol = 1)
type <- "FPL 200+%"
c <- cbind(time, spatialAC, type)

moran <- as.data.frame(rbind(a,b,c))
colnames(moran) <- c("Year", "SpatialAC", "Level")
moran$Level <- as.factor(moran$Level)

ggplot(data = moran, aes(x = Year, y = SpatialAC, color = Level)) +
  geom_point()
# fine for now

# next
# add data from 2016 api
# make pretty maps
# convert to points; look at cluster presence / movement (mclust)
# PUMS / gini coefficient
