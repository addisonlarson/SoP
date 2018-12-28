require(here); require(sf); require(dplyr); require(magrittr)
require(RColorBrewer); require(forcats); require(tidycensus)
options(stringsAsFactors = FALSE)
# 1. Upload req'd files
# 2. Compute percentage low-income persons relative to county
# 3. Compute percentage low-income persons relative to region
# 4. Export plots

# 1. Upload req'd files
mcd <- st_read(here("data", "./mcd.shp")) %>% # MCD boundary
  filter(DVRPC_REG == "Yes")
shp_a <- st_read(here("outputs", "./shp_a.shp")) # 1990 Census data
shp_b <- st_read(here("outputs", "./shp_b.shp")) # 2000 Census data
shp_c <- st_read(here("outputs", "./shp_c.shp")) # 2012 ACS data (2010 midpoint)
shp_d <- st_read(here("outputs", "./shp_d.shp")) # 2017 ACS data
cv_d <- st_read(here("outputs", "./cv_d.shp")) %>% # 2017 ACS data reliability
  mutate(cat_li = fct_relevel(cat_li, "High", "Medium", "Low"))

# 2. Compute percentage low-income persons relative to county
cty_li <- get_acs(geography = "county",
        state = c(34,42),
        variables = c("S1701_C01_001E",
                      "S1701_C01_042E"),
        output = "wide") %>%
  mutate(xwalk = paste(substr(GEOID, 1, 2), as.numeric(substr(GEOID, 3, 5)), sep = "_")) %>%
  filter(xwalk %in% c("34_5", "34_7", "34_15",
                      "34_21", "42_17", "42_29",
                      "42_45", "42_91", "42_101")) %>%
  rename(tot199_d = S1701_C01_042E) %>%
  mutate(cty_li = tot199_d / S1701_C01_001E,
         cty = substr(GEOID, 3, 5)) %>%
  select(cty, cty_li)
# Ratio of tract li to county li
shp_d %<>% mutate(cty = substr(GEOID, 3, 5)) %>%
  left_join(., cty_li) %>%
  mutate(rel_li = li_d / cty_li)
# z-score of tract li to county li
cty_sd <- shp_d %>%
  group_by(cty) %>%
  summarize(cty_sd = sd(li_d, na.rm = TRUE)) %>%
  st_set_geometry(NULL)
shp_d %<>% left_join(., cty_sd) %>%
  mutate(z = (li_d - cty_li) / cty_sd,
         z_cat = cut(z, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)))

# 3. Compute percentage low-income persons relative to region
reg_li <- weighted.mean(shp_d$li_d, shp_d$univ_d)
reg_sd <- sd(shp_d$li_d, na.rm = TRUE)
shp_d %<>%
  mutate(rel_li_reg = li_d / reg_li,
         z_reg = (li_d - reg_li) / reg_sd,
         z_cat_reg = cut(z_reg, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)))

# 4. Export plots
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

# 2017 ACS
png(here("figures", "li_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["li_d"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "Blues"),
     border = NA, main = "Pct. Low-Income Residents, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "gray", add = TRUE)
dev.off()

# 2017 ACS data reliability
fill_blue <- rev(brewer.pal(3, "Blues"))
png(here("figures", "cv_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(cv_d["cat_li"], pal = fill_blue,
     key.pos = 1, border = NA, main = "Reliability of Estimate", reset = FALSE)
plot(st_geometry(mcd), col = "white", add = TRUE)
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
