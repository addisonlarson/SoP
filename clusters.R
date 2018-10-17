library(rgdal); library(tidycensus)
library(stringr); library(ggplot2)
library(dplyr)
# identify clusters of interest through spatial overlays
setwd("D:/alarson/SuburbanizationPoverty/Outputs")
pov90 <- readOGR(".", "countyPov90", stringsAsFactors = FALSE)
pov00 <- readOGR(".", "countyPov00", stringsAsFactors = FALSE)
pov10 <- readOGR(".", "countyPov10", stringsAsFactors = FALSE)
pov16 <- readOGR(".", "countyPov16", stringsAsFactors = FALSE)
intersections <- rbind((as.data.frame(pov16[pov90,]))["GEOID"], # 2016 in 1990
                       (as.data.frame(pov16[pov00,]))["GEOID"], # 2016 in 2000
                       (as.data.frame(pov16[pov10,]))["GEOID"]) # 2016 in 2010
intersections <- distinct(intersections)
intersections <- merge(pov16, intersections, by = "GEOID")
# Join this with attributes
setwd("D:/alarson/SuburbanizationPoverty/CensusData")
attrib <- readOGR(".", "DVRPCBOUNDARY", stringsAsFactors = FALSE)

intersections <- intersections[c(1)]
attribIntersections <- over(intersections, attrib)
attribIntersections$GEOID <- intersections$GEOID
attribIntersections <- attribIntersections[c("GEOID",
                                             "CO_NAME",
                                             "MUN_NAME",
                                             "MUN_TYPE")]
colnames(attribIntersections) <- c("GEOID", "County", "PlaceName", "Type")
# write.csv(attribIntersections, "TargetTracts.csv", row.names = FALSE)

clusters <- read.csv("D:/alarson/SuburbanizationPoverty/CensusData/TargetTracts.csv",
                     stringsAsFactors = FALSE)
clusters <- subset(clusters, clusters$Type != "City")

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
                     "B14001_001E",
                     "B14001_002E",
                     myCall))
# Clean up fields
collect <- collect[, -( grep("\\M$" , colnames(collect), perl = TRUE))]
# Get state/county info so we can split df by MSA
collect$st <- substr(collect$GEOID, 1, 2)
collect$cty <- substr(collect$GEOID, 3, 5)
collect$stcty <- paste0(collect$st, collect$cty)
# Subset out DVRPC counties
dvrpc <- c("34005", "34007", "34015", "34021",
           "42017", "42029", "42045", "42091")
collect <- subset(collect, stcty %in% dvrpc)
# Drop low-population tracts
collect <- collect[collect$B01003_001E >= 1000,]
# Drop tracts with loooooooots of students
collect$pctStudent <- collect$B14001_002E / collect$B14001_001E * 100
students <- na.omit(collect$pctStudent); plot(density(students))
collect <- collect[collect$pctStudent < 50,] # too conservative?

# Link clusters to estimates
clusters$GEOID <- as.character(clusters$GEOID)
clusters <- merge(clusters, collect, by = "GEOID")
clusters <- split(clusters, clusters$County)
clusterRes <- data.frame()
for (i in 1:length(clusters)){
  caStCty <- clusters[[i]]$stcty[1]
  caCtyName <- as.character(clusters[[i]]$County[1])
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
  myRow <- data.frame("stcty" = caStCty,
                      "County" = caCtyName,
                      "MedianIncome_p" = caMedInc,
                      "PctHispanic_p" = caHisp,
                      "PctWhite_p" = caWht,
                      "PctBlack_p" = caBlk,
                      "MeanTrctPop_p" = caPop)
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
                      "MedianIncome_c" = caMedInc,
                      "PctHispanic_c" = caHisp,
                      "PctWhite_c" = caWht,
                      "PctBlack_c" = caBlk,
                      "MeanTrctPop_c" = caPop)
  countyRes <- rbind(countyRes, myRow)
}

# Merge then reshape
fullRes <- merge(clusterRes, countyRes, by = "stcty", all.x = TRUE)
fullRes <- fullRes[-c(1)]
fullResL <- reshape(fullRes, varying = c("MedianIncome_p",
                                         "MedianIncome_c",
                                         "PctHispanic_p",
                                         "PctHispanic_c",
                                         "PctWhite_p",
                                         "PctWhite_c",
                                         "PctBlack_p",
                                         "PctBlack_c",
                                         "MeanTrctPop_p",
                                         "MeanTrctPop_c"),
                    direction = "long", idvar = "County", sep = "_")
colnames(fullResL)[2] <- "Geography"
fullResL$Geography <- ifelse(fullResL$Geography == "p", "Cluster", "County")
fullResL$Geography <- as.factor(fullResL$Geography)

for (i in 3:7){
  res <- ggplot(fullResL, aes_string(fill = "Geography",
                                     y = names(fullResL)[i],
                                     x = "County")) +
    geom_bar(position = "dodge", stat = "Identity") +
    labs(title = paste("Comparison of Counties to Low-Income Tract Clusters,",
                       names(fullResL)[i], sep = " ")) +
    theme_minimal() + coord_flip()
  tiff(paste0("compare_", names(fullResL)[i], ".tiff"),
       units = "in", width = 8, height = 6,
       res = 600, compression = "lzw")
  plot(res)
  dev.off()
}
