rm(list=ls())
library(sf); library(dplyr); library(mclust); library(ggplot2)
setwd("D:/alarson/SuburbanizationPoverty/Outputs")
ts <- st_read("./tsData.shp", stringsAsFactors = FALSE)
ts <- filter_all(ts, all_vars(. != -100 & . != 0))
plot(ts$pct199_16, ts$base90_16) # use model-based clustering to group
attrib <- ts %>% st_set_geometry(NULL)
mod <- densityMclust(attrib[c(2,8)])
summary(mod)
plot(mod, what = "density", type = "persp")
ts$group <- mod$classification
# st_write(ts, "./tsDataGroups.shp")
attrib$Group <- mod$classification
set.seed(999)
samp <- attrib %>%
  group_by(Group) %>%
  sample_n(15)
# write.csv(samp, "comparetomap.csv", row.names = FALSE)
attrib$Group <- as.factor(attrib$Group)
myCols <- c(rgb(123,117,84, maxColorValue = 255),
            rgb(23,24,59, maxColorValue = 255),
            rgb(161,22,146, maxColorValue = 255),
            rgb(114,190,118, maxColorValue = 255),
            rgb(255,180,154, maxColorValue = 255),
            rgb(169,175,209, maxColorValue = 255),
            rgb(255,79,121, maxColorValue = 255))
tiff("tractGroupings.tiff", units = "in", width = 7.5, height = 7.5, res = 600, compression = "lzw")
ggplot(attrib, aes(x = pct199_16, y = base90_16, color = Group)) +
  geom_point() + theme_minimal() +
  scale_color_manual(values = myCols) +
  labs(title = "Groupings of Census Tracts by Baseline and Percentage Change",
       y = "Percentage Change in Low-Income Residents, 1990-2016",
       x = "Percentage of Low-Income Residents, 2016")
dev.off()
