# The basic process:
# 1. Clean tabular data 1990, 2000, 2010, 2017
# 2. Join 1990, 2000, and 2010 data to contemporaneous shapefiles
#    (2017 includes spatial spatial data in download)
# 3. Prep tabular data to use with LTDB
# 4. Compute percentage low-income persons relative to county
# 5. Compute percentage low-income persons relative to region
# 6. Export shapefiles

require(here); require(sf); require(dplyr);
require(tidycensus); require(stringr); require(magrittr)
options(stringsAsFactors = FALSE)

# 1. Clean tabular data 1990, 2000, 2010, 2017
# 1990 - 2010 data comes from NHGIS
# Persons by Ratio of Income to Poverty Level in Previous Year, Code C20
dat <- read.csv(here("data", "nhgis0006_ts_nominal_tract.csv")) %>%
  select(-ends_with("M")) %>%
  mutate(xwalk = paste(STATEFP, COUNTYFP, sep = "_"),
         univ_a = rowSums(select(., seq(15, 39, 3))),
         univ_b = rowSums(select(., seq(16, 40, 3)))) %>%
  rename(tot200_a = C20AI1990,
         tot200_b = C20AI2000) %>%
  mutate(tot199_a = univ_a - tot200_a,
         tot199_b = univ_b - tot200_b,
         GEOID = paste0(STATEFP, str_pad(COUNTYFP, 3, "left", "0"), str_pad(TRACTA, 6, "left", "0"))) %>%
  filter(xwalk %in% c("34_5", "34_7", "34_15",
                      "34_21", "42_17", "42_29",
                      "42_45", "42_91", "42_101")) %>%
  select(1:4, GEOID, ends_with("_a"), ends_with("_b")) %>%
  mutate(li_a = tot199_a / univ_a,
         li_b = tot199_b / univ_b,
         hi_a = 1 - li_a,
         hi_b = 1 - li_b)

# Separate by year
dat_a <- dat %>% filter(GJOIN1990 != "")
dat_b <- dat %>% filter(GJOIN2000 != "")

# 2010 ACS data from American FactFinder (table not on API)
dat_c <- read.csv(here("data", "ACS_12_5YR_S1701_with_ann.csv")) %>%
  rename(univ_c = HC01_EST_VC01,
         tot199_c = HC01_EST_VC55,
         GEOID = GEO.id2) %>%
  mutate(tot200_c = univ_c - tot199_c,
         li_c = tot199_c / univ_c,
         hi_c = 1 - li_c) %>%
  select(GEOID, ends_with("_c")) %>%
  mutate_at(vars(matches("GEOID")), as.character)

# 2017 ACS data from Census API
trct_d <- get_acs(geography = "tract",
                  state = c(34,42),
                  variables = c("S1701_C01_001E",
                                "S1701_C01_042E"),
                  output = "wide",
                  geometry = TRUE) %>%
  mutate(xwalk = paste(substr(GEOID, 1, 2), as.numeric(substr(GEOID, 3, 5)), sep = "_")) %>%
  filter(xwalk %in% c("34_5", "34_7", "34_15",
                      "34_21", "42_17", "42_29",
                      "42_45", "42_91", "42_101")) %>%
  rename(univ_d = S1701_C01_001E,
         tot199_d = S1701_C01_042E) %>%
  mutate(tot200_d = univ_d - tot199_d,
         li_d = tot199_d / univ_d,
         hi_d = 1 - li_d) %>%
  select(GEOID, ends_with("_d")) %>%
  st_transform(., 26918)

# 2. Join 1990, 2000, and 2010 data to contemporaneous shapefiles
trct_a <- st_read(here("data", "./US_tract_1990_conflated.shp")) %>%
  select(GISJOIN) %>%
  left_join(., dat_a, by = c("GISJOIN" = "GJOIN1990")) %>%
  filter(GISJOIN %in% dat_a$GJOIN1990) %>%
  select(1:5, ends_with("_a")) %>%
  st_transform(., 26918)
trct_b <- st_read(here("data", "./US_tract10_2000.shp")) %>%
  select(GISJOIN) %>%
  filter(GISJOIN %in% dat_b$GJOIN2000) %>%
  left_join(., dat_b, by = c("GISJOIN" = "GJOIN2000")) %>%
  select(1:5, ends_with("_b")) %>%
  st_transform(., 26918)
trct_c <- trct_d %>%
  select(GEOID) %>%
  left_join(., dat_c, by = "GEOID")

# 4. Prep tabular data to use with LTDB
orig_a <- trct_a %>%
  select(5:10) %>%
  st_set_geometry(NULL)
orig_b <- trct_b %>%
  select(5:10) %>%
  st_set_geometry(NULL)
write.csv(orig_a, here("outputs", "orig_a.csv"), row.names = FALSE)
write.csv(orig_b, here("outputs", "orig_b.csv"), row.names = FALSE)

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
trct_d %<>% mutate(cty = substr(GEOID, 3, 5)) %>%
  left_join(., cty_li) %>%
  mutate(rel_li = li_d / cty_li)
# z-score of tract li to county li
cty_sd <- trct_d %>%
  group_by(cty) %>%
  summarize(cty_sd = sd(li_d, na.rm = TRUE)) %>%
  st_set_geometry(NULL)
trct_d %<>% left_join(., cty_sd) %>%
  mutate(z = (li_d - cty_li) / cty_sd,
         z_cat = cut(z, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)))

# 3. Compute percentage low-income persons relative to region
reg_li <- weighted.mean(trct_d$li_d, trct_d$univ_d)
reg_sd <- sd(trct_d$li_d, na.rm = TRUE)
trct_d %<>%
  mutate(rel_li_reg = li_d / reg_li,
         z_reg = (li_d - reg_li) / reg_sd,
         z_cat_reg = cut(z_reg, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)))

# 3. Export shapefiles
st_write(trct_a, here("outputs", "shp_a.shp"))
st_write(trct_b, here("outputs", "shp_b.shp"))
st_write(trct_c, here("outputs", "shp_c.shp"))
st_write(trct_d, here("outputs", "shp_d.shp"))
