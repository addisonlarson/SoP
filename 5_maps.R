require(here); require(sf); require(RColorBrewer); require(forcats)
# 1. Upload req'd files
# 2. Export plots

# 1. Upload req'd files
mcd <- st_read(here("data", "./mcd.shp")) %>% # MCD boundary
  filter(DVRPC_REG == "Yes")
shp_a <- st_read(here("outputs", "./shp_a.shp")) # 1990 Census data
shp_b <- st_read(here("outputs", "./shp_b.shp")) # 2000 Census data
shp_c <- st_read(here("outputs", "./shp_c.shp")) # 2012 ACS data (2010 midpoint)
shp_d <- st_read(here("outputs", "./shp_d.shp")) # 2017 ACS data
cv_d <- st_read(here("outputs", "./cv_d.shp")) %>% # 2017 ACS data reliability
  mutate(cat_li = fct_relevel(cat_li, "High", "Medium", "Low"))

# 2. Export plots
eq_int = c(-0.0001,0.2,0.4,0.6,0.8,1.0001)

# 1990 Census
png(here("figures", "li_a.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_a["li_a"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "PuBuGn"),
     border = NA, main = "Pct Low-Income Residents, 1990", reset = FALSE)
plot(st_geometry(mcd), col = "dimgray", add = TRUE)
dev.off()

# 2000 Census
png(here("figures", "li_b.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_b["li_b"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "PuBuGn"),
     border = NA, main = "Pct Low-Income Residents, 2000", reset = FALSE)
plot(st_geometry(mcd), col = "dimgray", add = TRUE)
dev.off()

# 2012 ACS
png(here("figures", "li_c.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_c["li_c"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "PuBuGn"),
     border = NA, main = "Pct Low-Income Residents, 2010", reset = FALSE)
plot(st_geometry(mcd), col = "dimgray", add = TRUE)
dev.off()

# 2017 ACS
png(here("figures", "li_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(shp_d["li_d"], breaks = eq_int, key.pos = 1,
     pal = brewer.pal(5, "PuBuGn"),
     border = NA, main = "Pct Low-Income Residents, 2017", reset = FALSE)
plot(st_geometry(mcd), col = "dimgray", add = TRUE)
dev.off()

# 2017 ACS data reliability
png(here("figures", "cv_d.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(cv_d["cat_li"], pal = c("#9EBC9F", "#E5EAEB", "#B9A394"),
     key.pos = 1, border = NA, main = "Reliability of Estimate", reset = FALSE)
plot(st_geometry(mcd), col = "dimgray", add = TRUE)
dev.off()
