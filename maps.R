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
  left_join(., cv_d, by = "GEOID") %>%
  filter(hous_d < 1500)
delta <- st_read(here("process_data", "delta_shp.shp")) %>%
  filter(rr_nw <= 40)
level <- st_read(here("process_data", "level_shp.shp")) %>%
  filter(rr_li <= 40)
cluster <- st_read(here("outputs", "cluster.shp"))

# 2. Export plots
# Define breaks
eq_int = c(-0.0001,0.2,0.4,0.6,0.8,1.0001)
hous_a <- shp_a %>% st_set_geometry(NULL) %>% pull(hous_a)
hous_b <- shp_b %>% st_set_geometry(NULL) %>% pull(hous_b)
hous_c <- shp_c %>% st_set_geometry(NULL) %>% pull(hous_c)
hous_d <- shp_d %>% st_set_geometry(NULL) %>% pull(hous_d)
hous_all <- c(hous_a, hous_b, hous_c, hous_d) %>% na.omit(.)
eq_int_hous <- seq(min(hous_all), max(hous_all), length.out = 6)

# Define colors
continuous_5 <- brewer.pal(5, "Blues")
categorical_5 <- brewer.pal(5, "Pastel1")
reliability <- rev(brewer.pal(3, "Pastel1"))
significant <- rev(brewer.pal(3, "Pastel1")[-2])

# 2.1. INCOME
png(here("figures", "li_a.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_a["li_a"], breaks = eq_int, key.pos = 1, pal = continuous_5,
     border = NA, main = "Pct. Low-Income Residents, 1990", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "li_b.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_b["li_b"], breaks = eq_int, key.pos = 1, pal = continuous_5,
     border = NA, main = "Pct. Low-Income Residents, 2000", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "li_c.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_c["li_c"], breaks = eq_int, key.pos = 1, pal = continuous_5,
     border = NA, main = "Pct. Low-Income Residents, 2010", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "cv_li_c.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_c["cat_li_c"], pal = reliability,
     key.pos = 1, border = NA, main = "Reliability of Estimate, 2010", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "li_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["li_d"], breaks = eq_int, key.pos = 1, pal = continuous_5,
     border = NA, main = "Pct. Low-Income Residents, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "cv_li_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["cat_li_d"], pal = reliability, key.pos = 1, border = NA,
     main = "Reliability of Estimate, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "stat_inc_qual.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["z_1dir_sig"], pal = significant, key.pos = 1, border = NA,
     main = expression(paste("Significant Increase in Pct. Low-Income Residents, 2010-2017, ", alpha, " = 0.05")),
     reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# 2017 relative to region
png(here("figures", "lvl_li_rr.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(level["rr_li"], breaks = "equal", nbreaks = 5, key.pos = 1, pal = continuous_5,
     border = NA, main = "Ratio of Low-Income Residents Relative to Regional Mean, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# 2017 relative to county
png(here("figures", "lvl_li_rc.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(level["rc_li"], breaks = "equal", nbreaks = 5, key.pos = 1, pal = continuous_5,
     border = NA, main = "Ratio of Low-Income Residents Relative to County Mean, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# Delta relative to region
png(here("figures", "del_li_rr.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(delta["rr_li"], breaks = "equal", nbreaks = 5, key.pos = 1, pal = continuous_5,
     border = NA, main = "Ratio of Change in Low-Income Residents Relative to Region, 1990-2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# Delta relative to county
png(here("figures", "del_li_rc.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(delta["rc_li"], breaks = "equal", nbreaks = 5, key.pos = 1,pal = continuous_5,
     border = NA, main = "Ratio of Change in Low-Income Residents Relative to County, 1990-2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# 2.2. RACIAL AND ETHNIC MINORITY POPULATIONS
png(here("figures", "nw_a.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_a["nw_a"], breaks = eq_int, key.pos = 1, pal = continuous_5,
     border = NA, main = "Pct. Nonwhite Residents, 1990", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "nw_b.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_b["nw_b"], breaks = eq_int, key.pos = 1, pal = continuous_5,
     border = NA, main = "Pct. Nonwhite Residents, 2000", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "nw_c.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_c["nw_c"], breaks = eq_int, key.pos = 1, pal = continuous_5,
     border = NA, main = "Pct. Nonwhite Residents, 2010", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "nw_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["nw_d"], breaks = eq_int, key.pos = 1, pal = continuous_5,
     border = NA, main = "Pct. Nonwhite Residents, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "cv_nw_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["cat_nw_d"], pal = reliability,
     key.pos = 1, border = NA, main = "Reliability of Estimate, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# 2017 relative to region
png(here("figures", "lvl_nw_rr.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(level["rr_nw"], breaks = "equal", nbreaks = 5, key.pos = 1, pal = continuous_5,
     border = NA, main = "Ratio of Nonwhite Residents Relative to Regional Mean, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# 2017 relative to county
png(here("figures", "lvl_nw_rc.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(level["rc_nw"], breaks = "equal", nbreaks = 5, key.pos = 1, pal = continuous_5,
     border = NA, main = "Ratio of Nonwhite Residents Relative to County Mean, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# Delta relative to region
png(here("figures", "del_nw_rr.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(delta["rr_nw"], breaks = "equal", nbreaks = 5, key.pos = 1, pal = continuous_5,
     border = NA, main = "Ratio of Change in Nonwhite Residents Relative to Region, 1990-2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# Delta relative to county
png(here("figures", "del_nw_rc.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(delta["rc_nw"], breaks = "equal", nbreaks = 5, key.pos = 1, pal = continuous_5,
     border = NA, main = "Ratio of Change in Nonwhite Residents Relative to County, 1990-2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# 2.3. MEDIAN HOME VALUE
png(here("figures", "hous_a.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_a["hous_a"], breaks = eq_int_hous, key.pos = 1, pal = continuous_5,
     border = NA, main = "Median Home Value, 1990 (2017 Dollars)", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "hous_b.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_b["hous_b"], breaks = eq_int_hous, key.pos = 1, pal = continuous_5,
     border = NA, main = "Median Home Value, 2000 (2017 Dollars)", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "hous_c.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_c["hous_c"], breaks = eq_int_hous, key.pos = 1, pal = continuous_5,
     border = NA, main = "Median Home Value, 2010 (2017 Dollars)", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "hous_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["hous_d"], breaks = eq_int_hous, key.pos = 1, pal = continuous_5,
     border = NA, main = "Median Home Value, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
png(here("figures", "cv_hous_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["cat_hous_d"], pal = reliability, key.pos = 1, border = NA,
     main = "Reliability of Estimate, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# 2017 relative to region
png(here("figures", "lvl_hous_rr.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(level["rr_hous"], breaks = "equal", nbreaks = 5, key.pos = 1, pal = continuous_5,
     border = NA, main = "Ratio of Home Value Relative to Regional Mean, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# 2017 relative to county
png(here("figures", "lvl_hous_rc.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(level["rc_hous"], breaks = "equal", nbreaks = 5, key.pos = 1, pal = continuous_5,
     border = NA, main = "Ratio of Home Value Relative to County Mean, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# Delta relative to region
png(here("figures", "del_hous_rr.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(delta["rr_hous"], breaks = "equal", nbreaks = 5, key.pos = 1, pal = continuous_5,
     border = NA, main = "Ratio of Change in Home Value Relative to Region, 1990-2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
# Delta relative to county
png(here("figures", "del_hous_rc.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(delta["rc_hous"], breaks = "equal", nbreaks = 5, key.pos = 1, pal = continuous_5,
     border = NA, main = "Ratio of Change in Home Value Relative to County, 1990-2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# Clusters
png(here("figures", "clusters.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(cluster["group"], pal = categorical_5, key.pos = 1, border = NA,
     main = "Clusters by 2017 Pct. LI and 1990-2017 Pct. Chg. LI",
     reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()
