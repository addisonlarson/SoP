# 7 clusters of interest from prior mapping
library(rgdal); library(tidycensus); library(stringr)
clusters <- read.csv("D:/alarson/SuburbanizationPoverty/CensusData/TargetTracts.csv")
setwd("D:/alarson/SuburbanizationPoverty/Outputs")
trct16 <- readOGR(".", "pov16", stringsAsFactors = FALSE)

# B01003, B03002, B19013. Compare cluster to county mean
varNums <- str_pad(as.character(1:21), 3, "left", pad = 0)
myCall <- paste0("B03002_", varNums, "E")
collect <- get_acs(geography = "tract",
                   state = c(42,34),
                   year = 2016,
                   output = "wide",
                   variables = c(
                     "B01003_001E",
                     "B19013_001E",
                     "B25071_001E",
                     myCall))
# Clean up fields
collect <- collect[, -( grep("\\M$" , colnames(collect), perl = TRUE))]
# Get state/county info so we can split df by MSA
collect$st <- substr(collect$GEOID, 1, 2)
collect$cty <- substr(collect$GEOID, 3, 5)
collect$stcty <- paste0(collect$st, collect$cty)
# Subset out DVRPC counties
dvrpc <- c("34015", "42017", "42029", "42091")
collect <- subset(collect, stcty %in% dvrpc)

# Link clusters to estimates
clusters$GEOID <- as.character(clusters$GEOID)
clusters <- merge(clusters, collect, by = "GEOID")
clusters <- split(clusters, clusters$PlaceName)
clusterRes <- data.frame()
for (i in 1:length(clusters)){
  caPlaceName <- as.character(clusters[[i]]$PlaceName[1])
  caStCty <- clusters[[i]]$stcty[1]
  caCtyName <- as.character(clusters[[i]]$CtyName[1])
  caMedInc <- weighted.mean(clusters[[i]]$B19013_001E,
                            clusters[[i]]$B01003_001E,
                            na.rm = TRUE)
  caHisp <- weighted.mean(clusters[[i]]$B03002_012E /
                            clusters[[i]]$B03002_001E * 100,
                          clusters[[i]]$B01003_001E,
                          na.rm = TRUE)
  caWht <- weighted.mean(clusters[[i]]$B03002_003E /
                           clusters[[i]]$B03002_001E * 100,
                         clusters[[i]]$B01003_001E,
                         na.rm = TRUE)
  caBlk <- weighted.mean((clusters[[i]]$B03002_004E + clusters[[i]]$B03002_014E) /
                           clusters[[i]]$B03002_001E * 100,
                         clusters[[i]]$B01003_001E,
                         na.rm = TRUE)
  caPop <- mean(clusters[[i]]$B01003_001E, na.rm = TRUE)
  myRow <- data.frame("placeName" = caPlaceName,
                      "stcty" = caStCty,
                      "countyName" = caCtyName,
                      "medInc" = caMedInc,
                      "hisp" = caHisp,
                      "wht" = caWht,
                      "blk" = caBlk,
                      "trctPop" = caPop)
  clusterRes <- rbind(clusterRes, myRow)
}

counties <- split(collect, as.factor(collect$stcty))
countyRes <- data.frame()
for (i in 1:length(counties)){
  caStCty <- counties[[i]]$stcty[1]
  caMedInc <- weighted.mean(counties[[i]]$B19013_001E,
                            counties[[i]]$B01003_001E,
                            na.rm = TRUE)
  caHisp <- weighted.mean(counties[[i]]$B03002_012E /
                            counties[[i]]$B03002_001E * 100,
                          counties[[i]]$B01003_001E,
                          na.rm = TRUE)
  caWht <- weighted.mean(counties[[i]]$B03002_003E /
                           counties[[i]]$B03002_001E * 100,
                         counties[[i]]$B01003_001E,
                         na.rm = TRUE)
  caBlk <- weighted.mean((counties[[i]]$B03002_004E + counties[[i]]$B03002_014E) /
                           counties[[i]]$B03002_001E * 100,
                         counties[[i]]$B01003_001E,
                         na.rm = TRUE)
  caPop <- mean(counties[[i]]$B01003_001E, na.rm = TRUE)
  myRow <- data.frame("stcty" = caStCty,
                      "medInc_c" = caMedInc,
                      "hisp_c" = caHisp,
                      "wht_c" = caWht,
                      "blk_c" = caBlk,
                      "trctPop_c" = caPop)
  countyRes <- rbind(countyRes, myRow)
}

clusterRes;countyRes
# some sort of paired visualization
