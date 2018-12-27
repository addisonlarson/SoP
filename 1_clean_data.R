# The basic process:
# 1. Clean tabular data 1990, 2000, 2010, 2017
# 2. Join 1990, 2000, and 2010 data to contemporaneous shapefiles
#    (2017 includes spatial spatial data in download)
# 3. Export shapefiles
# 4. Prep tabular data to use with LTDB
require(here); require(sf); require(dplyr); require(tidycensus); require(stringr)
options(stringsAsFactors = FALSE)

# 1. Clean tabular data 1990, 2000, 2010, 2017
# 1990 - 2010 data comes from NHGIS
# Persons by Ratio of Income to Poverty Level in Previous Year, Code C20
dat <- read.csv(here("data", "nhgis0006_ts_nominal_tract.csv")) %>%
  select(-ends_with("M")) %>%
  mutate(xwalk = paste(STATEFP, COUNTYFP, sep = "_"),
         univ_a = rowSums(select(., seq(15, 39, 3))),
         univ_b = rowSums(select(., seq(16, 40, 3))),
         univ_c = rowSums(select(., seq(17, 41, 3)))) %>%
  rename(tot200_a = C20AI1990,
         tot200_b = C20AI2000,
         tot200_c = C20AI125) %>%
  mutate(tot199_a = univ_a - tot200_a,
         tot199_b = univ_b - tot200_b,
         tot199_c = univ_c - tot200_c,
         GEOID = paste0(STATEFP, str_pad(COUNTYFP, 3, "left", "0"), TRACTA)) %>%
  filter(xwalk %in% c("34_5", "34_7", "34_15",
                      "34_21", "42_17", "42_29",
                      "42_45", "42_91", "42_101")) %>%
  select(1:4, GEOID, ends_with("_a"), ends_with("_b"), ends_with("_c")) %>%
  mutate(li_a = tot199_a / univ_a,
         li_b = tot199_b / univ_b,
         li_c = tot199_c / univ_c,
         hi_a = 1 - li_a,
         hi_b = 1 - li_b,
         hi_c = 1 - li_c)

# Separate by year
dat_a <- dat %>% filter(GJOIN1990 != "")
dat_b <- dat %>% filter(GJOIN2000 != "")
dat_c <- dat %>% filter(GJOIN2012 != "")

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
trct_c <- st_read(here("data", "./US_tract_2010.shp")) %>%
  select(GISJOIN) %>%
  left_join(., dat_c, by = c("GISJOIN" = "GJOIN2012")) %>%
  filter(GISJOIN %in% dat_c$GJOIN2000) %>%
  select(1:5, ends_with("_c")) %>%
  st_transform(., 26918)
  
# 3. Export shapefiles
st_write(trct_a, here("outputs", "shp_a.shp"))
st_write(trct_b, here("outputs", "shp_b.shp"))
st_write(trct_c, here("outputs", "shp_c.shp"))
st_write(trct_d, here("outputs", "shp_d.shp"))

# 4. Prep tabular data to use with LTDB
orig_a <- trct_a %>%
  select(5:10) %>%
  st_set_geometry(NULL)
orig_b <- trct_b %>%
  select(5:10) %>%
  st_set_geometry(NULL)
write.csv(orig_a, here("outputs", "orig_a.csv"), row.names = FALSE)
write.csv(orig_b, here("outputs", "orig_b.csv"), row.names = FALSE)
