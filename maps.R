require(here); require(sf); require(dplyr); require(magrittr)
require(RColorBrewer); require(forcats); require(tidycensus)
options(stringsAsFactors = FALSE)
# 1. Upload req'd files
# 2. Export plots

# 1. Upload req'd files
mcd <- st_read(here("data", "./mcd.shp")) %>% # MCD boundary
  filter(DVRPC_REG == "Yes")
shp_a <- st_read(here("outputs", "./shp_a.shp")) # 1990 Census data
shp_b <- st_read(here("outputs", "./shp_b.shp")) # 2000 Census data
shp_c <- st_read(here("outputs", "./shp_c.shp")) # 2012 ACS data (2010 midpoint)
shp_d <- st_read(here("outputs", "./shp_d.shp")) # 2017 ACS data
cv_c <- st_read(here("outputs", "./cv_c.shp")) %>% # 2012 ACS data reliability
  mutate(cat_li = fct_relevel(cat_li, "High", "Medium", "Low"))
cv_d <- st_read(here("outputs", "./cv_d.shp")) %>% # 2017 ACS data reliability
  mutate(cat_li = fct_relevel(cat_li, "High", "Medium", "Low"),
         z_dif_sig = fct_relevel(z_dif_sig, "Yes", "No"),
         z_1dir_sig = fct_relevel(z_1dir_sig, "Yes", "No"))
cluster <- st_read(here("outputs", "./cluster.shp")) # Baseline x change clusters

# 2. Export plots
eq_int = c(-0.0001,0.2,0.4,0.6,0.8,1.0001)

# 1990 Census
png(here("figures", "li_a.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_a["li_a"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "Blues"),
     border = NA, main = "Pct. Low-Income Residents, 1990", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# 2000 Census
png(here("figures", "li_b.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_b["li_b"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "Blues"),
     border = NA, main = "Pct. Low-Income Residents, 2000", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# 2012 ACS
png(here("figures", "li_c.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_c["li_c"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "Blues"),
     border = NA, main = "Pct. Low-Income Residents, 2010", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# 2012 ACS data reliability
fill_blue <- rev(brewer.pal(4, "Blues"))[2:4]
png(here("figures", "cv_c.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(cv_c["cat_li"], pal = fill_blue,
     key.pos = 1, border = NA, main = "Reliability of Estimate, 2010", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# 2017 ACS
png(here("figures", "li_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["li_d"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "Blues"),
     border = NA, main = "Pct. Low-Income Residents, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# 2017 ACS data reliability
fill_blue <- rev(brewer.pal(4, "Blues"))[2:4]
png(here("figures", "cv_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(cv_d["cat_li"], pal = fill_blue,
     key.pos = 1, border = NA, main = "Reliability of Estimate, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# Statistically significant change ACS 2012-2017
fill_blue <- rev(brewer.pal(4, "Blues"))[3:4]
png(here("figures", "sig_chg_qual.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(cv_d["z_dif_sig"], pal = fill_blue,
     key.pos = 1, border = NA,
     main = expression(paste("Significant Change in Pct. Low-Income Residents, 2010-2017, ", alpha, " = 0.05")),
     reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# Statistically significant increase ACS 2012-2017
fill_blue <- rev(brewer.pal(4, "Blues"))[3:4]
png(here("figures", "stat_inc_qual.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(cv_d["z_1dir_sig"], pal = fill_blue,
     key.pos = 1, border = NA,
     main = expression(paste("Significant Increase in Pct. Low-Income Residents, 2010-2017, ", alpha, " = 0.05")),
     reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# 2017 ACS LI relative to county, ratio
png(here("figures", "li_rel_ratio_cty_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["rel_li"], breaks = "equal", nbreaks = 5, key.pos = 1,
     pal = brewer.pal(5, "Blues"),
     border = NA, main = "Ratio of Low-Income Residents Relative to County Mean, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# 2017 ACS LI relative to county, sd
fill_blue <- brewer.pal(5, "Blues")
png(here("figures", "li_rel_z_cty_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["z_cat"], pal = fill_blue,
     key.pos = 1, border = NA,
     main = expression("Low-Income Residents Relative to County Mean, "*italic(z)*"-Scores, 2017"),
     reset = FALSE)
plot(st_geometry(mcd), col = "white", add = TRUE)
dev.off()

# 2017 ACS LI relative to region, ratio
png(here("figures", "li_rel_ratio_reg_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["rel_li_reg"], breaks = "equal", nbreaks = 5, key.pos = 1,
     pal = brewer.pal(5, "Blues"),
     border = NA, main = "Ratio of Low-Income Residents Relative to Regional Mean, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# 2017 ACS LI relative to region, sd
fill_blue <- brewer.pal(5, "Blues")
png(here("figures", "li_rel_z_reg_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["z_cat_reg"], pal = fill_blue,
     key.pos = 1, border = NA,
     main = expression("Low-Income Residents Relative to Regional Mean, "*italic(z)*"-Scores, 2017"),
     reset = FALSE)
plot(st_geometry(mcd), col = "white", add = TRUE)
dev.off()

# Clusters
fill_blue <- brewer.pal(7, "Blues")
png(here("figures", "clusters.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(cluster["group"], pal = fill_blue,
     key.pos = 1, border = NA,
     main = "(DRAFT) Clusters by 2017 Pct. LI and 1990-2017 Pct. Chg. LI",
     reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
