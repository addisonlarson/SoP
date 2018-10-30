rm(list=ls())
library(sf); library(dplyr); library(mclust); library(ggplot2); library(car)
setwd("D:/alarson/SuburbanizationPoverty/Outputs")
ts <- st_read("./tsData.shp", stringsAsFactors = FALSE)
ts <- filter_all(ts, all_vars(. != -100 & . != 0))
plot(ts$pct199_16, ts$base90_16) # use model-based clustering to group
attrib <- ts %>% st_set_geometry(NULL)
mod <- densityMclust(attrib[c(2,8)])
summary(mod)
plot(mod, what = "density", type = "persp")
# Reorder factors. 1234567 to 1246735. CHECK THIS; IT IS WRONG
#       CHANGE        INCOME
# 1     High          High
# 2     Moderate      High
# 4     Slow          High
# 6     Moderate      Low
# 7     Slow          Low
# 3     Slow          Moderate
# 5     Slow          Highest
ts$origGroup <- mod$classification
ts$revisedGroup <- factor(recode(mod$classification,
                   "1 = 'High-High';
                   2 = 'Moderate-High';
                   4 = 'Slow-High';
                   6 = 'Moderate-Low';
                   7 = 'Slow-Low';
                   3 = 'Slow-Moderate';
                   5 = 'Slow-Highest'"),
                   levels = c("High-High",
                              "Moderate-High", 
                              "Slow-High",
                              "Moderate-Low",
                              "Slow-Low",
                              "Slow-Moderate",
                              "Slow-Highest"))
# st_write(ts, "./tsDataGroups.shp")
attrib$Group <- ts$revisedGroup
set.seed(999)
samp <- attrib %>%
  group_by(Group) %>%
  sample_n(15)
myCols <- c(rgb(101,129,102, maxColorValue = 255),
            rgb(152,182,153, maxColorValue = 255),
            rgb(200,231,201, maxColorValue = 255),
            rgb(56,99,134, maxColorValue = 255),
            rgb(79,157,193, maxColorValue = 255),
            rgb(170,216,224, maxColorValue = 255),
            rgb(230,230,230, maxColorValue = 255))
tiff("tractGroupings.tiff", units = "in", width = 7.5, height = 7.5, res = 600, compression = "lzw")
ggplot(attrib, aes(x = pct199_16, y = base90_16, color = Group)) +
  geom_point() + theme_minimal() +
  scale_color_manual(values = myCols) +
  labs(title = "Groupings of Census Tracts by Baseline and Percentage Change",
       subtitle = "First word describes rate of change; second describes tract income composition",
       y = "Percentage Change in Low-Income Residents, 1990-2016",
       x = "Percentage of Low-Income Residents, 2016")
dev.off()
