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
attrib$group <- mod$classification
set.seed(999)
samp <- attrib %>%
  group_by(group) %>%
  sample_n(15)
# write.csv(samp, "comparetomap.csv", row.names = FALSE)
attrib$group <- as.factor(attrib$group)
# tiff("tractGroupings.tiff", units = "in", width = 10, height = 7.5, res = 600, compression = "lzw")
ggplot(attrib, aes(x = pct199_16, y = base90_16, color = group)) + geom_point()
# dev.off()
