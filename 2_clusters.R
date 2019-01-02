require(here); require(sf); require(tidyverse); require(haven); require(mclust)
# 1. Upload data
# 2. Compute 2017 baseline and 1990-2017 percentage change
# 3. Identify clusters
# 4. Export

# 1. Upload data
# Harmonized 1990 and 2000 census tract data
trct_a <- read_dta(here("data", "corresp_a.dta")) %>%
  rename(GEOID = trtid10)
trct_b <- read_dta(here("data", "corresp_b.dta")) %>%
  rename(GEOID = trtid10)
geo <- st_read(here("outputs", "./shp_c.shp")) %>%
  select(GEOID)
trct_a_h <- geo %>% left_join(., trct_a)
trct_b_h <- geo %>% left_join(., trct_b)
# 2017 data
trct_d <- st_read(here("outputs", "./shp_d.shp")) %>%
  select(GEOID, li_d)

# 2. Compute 2017 baseline and 1990-2017 percentage change
dat <- trct_a_h %>% st_set_geometry(NULL) %>% select(-univ_a)
cluster <- left_join(trct_d, dat) %>%
  mutate(change = (li_d - li_a) / li_a) %>%
  rename(baseline = li_d) %>%
  select(GEOID, baseline, change) %>%
  filter_all(., all_vars(. != -1)) %>%
  slice(-1365) # Remove single Inf value
cluster_df <- cluster %>% st_set_geometry(NULL)

# 3. Identify clusters
mod <- densityMclust(cluster_df[c(2,3)])
summary(mod)
plot(mod, what = "density", type = "persp")
cluster$group <- as.factor(mod$classification)

# 4. Export
st_write(trct_a_h, here("outputs", "shp_a_h.shp"))
st_write(trct_b_h, here("outputs", "shp_b_h.shp"))
st_write(cluster, here("outputs", "cluster.shp"))
