library(here); library(sf); library(tidyverse); library(RColorBrewer)
options(stringsAsFactors = FALSE)
# 1. Upload req'd files
# 2. Export plots

# 1. Upload req'd files
mcd <- st_read(here("map_data", "mcd.shp")) %>%
  filter(DVRPC_REG == "Yes")
cv_c <- read_csv(here("process_data", "cv_inc_c.csv")) %>%
  mutate_at(vars(GEOID), as.character) %>%
  mutate_at(vars(cat_li_c), funs(fct_relevel(., "High", "Medium", "Low"))) %>%
  select(-li_c)
cv_d <- left_join(read_csv(here("process_data", "cv_hous_d.csv")),
                  read_csv(here("process_data", "cv_inc_d.csv"))) %>%
  left_join(., read_csv(here("process_data", "cv_nw_d.csv"))) %>%
  mutate_at(vars(GEOID), as.character) %>%
  mutate_at(vars(cat_hous_d), funs(fct_relevel(., "High", "Medium", "Low"))) %>%
  mutate_at(vars(cat_li_d), funs(fct_relevel(., "High", "Medium", "Low"))) %>%
  mutate_at(vars(cat_nw_d), funs(fct_relevel(., "High", "Medium", "Low"))) %>%
  mutate_at(vars(z_1dir_sig), funs(fct_relevel(., "Yes", "No"))) %>%
  select(-hous_d, -li_d, -nw_d)
shp_a <- st_read(here("process_data", "a_shp.shp"))
shp_b <- st_read(here("process_data", "b_shp.shp"))
shp_c <- st_read(here("process_data", "c_shp.shp")) %>%
  left_join(., cv_c, by = "GEOID")
shp_d <- st_read(here("process_data", "d_shp.shp")) %>%
  left_join(., cv_d, by = "GEOID")
delta <- st_read(here("process_data", "delta_shp.shp"))
level <- st_read(here("process_data", "level_shp.shp")) %>%
  filter(rr_li <= 40)
cluster <- st_read(here("outputs", "cluster.shp"))

# 2. Export plots
eq_int = c(-0.0001,0.2,0.4,0.6,0.8,1.0001)
# 2.1. INCOME
png(here("figures", "li_a.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_a["li_a"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "Blues"),
     border = NA, main = "Pct. Low-Income Residents, 1990", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "li_b.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_b["li_b"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "Blues"),
     border = NA, main = "Pct. Low-Income Residents, 2000", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "li_c.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_c["li_c"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "Blues"),
     border = NA, main = "Pct. Low-Income Residents, 2010", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
fill_blue <- rev(brewer.pal(4, "Blues"))[2:4]
png(here("figures", "cv_li_c.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_c["cat_li_c"], pal = fill_blue,
     key.pos = 1, border = NA, main = "Reliability of Estimate, 2010", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "li_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["li_d"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "Blues"),
     border = NA, main = "Pct. Low-Income Residents, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
fill_blue <- rev(brewer.pal(4, "Blues"))[2:4]
png(here("figures", "cv_li_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["cat_li_d"], pal = fill_blue,
     key.pos = 1, border = NA, main = "Reliability of Estimate, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
fill_blue <- rev(brewer.pal(4, "Blues"))[3:4]
png(here("figures", "stat_inc_qual.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(cv_d["z_1dir_sig"], pal = fill_blue,
     key.pos = 1, border = NA,
     main = expression(paste("Significant Increase in Pct. Low-Income Residents, 2010-2017, ", alpha, " = 0.05")),
     reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# 2017 relative to region
fill_blue <- brewer.pal(5, "Blues")
png(here("figures", "lvl_li_rr.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(level["rr_li"], breaks = "equal", nbreaks = 5, key.pos = 1,
     pal = fill_blue,
     border = NA, main = "Ratio of Low-Income Residents Relative to Regional Mean, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# 2017 relative to county
png(here("figures", "lvl_li_rc.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(level["rc_li"], breaks = "equal", nbreaks = 5, key.pos = 1,
     pal = fill_blue,
     border = NA, main = "Ratio of Low-Income Residents Relative to County Mean, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# Delta relative to region
png(here("figures", "del_li_rr.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(delta["rr_li"], breaks = "equal", nbreaks = 5, key.pos = 1,
     pal = fill_blue,
     border = NA, main = "Ratio of Change in Low-Income Residents Relative to Region, 1990-2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# Delta relative to county
png(here("figures", "del_li_rc.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(delta["rc_li"], breaks = "equal", nbreaks = 5, key.pos = 1,
     pal = fill_blue,
     border = NA, main = "Ratio of Change in Low-Income Residents Relative to County, 1990-2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()


# CONTINUE SAME APPROACH FOR OTHER VARIABLES


# Clusters
fill_blue <- brewer.pal(5, "Pastel1")
png(here("figures", "clusters.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(cluster["group"], pal = fill_blue,
     key.pos = 1, border = NA,
     main = "Clusters by 2017 Pct. LI and 1990-2017 Pct. Chg. LI",
     reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
