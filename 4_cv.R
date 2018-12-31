require(here); require(sf); require(dplyr)
require(tidycensus); require(stringr)
options(stringsAsFactors = FALSE)

# 2010 ACS data from American FactFinder
dat <- read.csv(here("data", "ACS_12_5YR_S1701_with_ann.csv")) %>%
  rename(GEOID = GEO.id2) %>%
  mutate(li = HC01_EST_VC55 / HC01_EST_VC01,
         moe_li = moe_prop(HC01_EST_VC55, HC01_EST_VC01, HC01_MOE_VC55, HC01_MOE_VC01),
         cv_li = (moe_li / 1.645) / li,
         cat_li = cut(cv_li, breaks = c(-Inf, 0.12, 0.4, Inf), labels = c("High","Medium","Low"))) %>%
  select(GEOID, ends_with("li")) %>%
  mutate_at(vars(matches("GEOID")), as.character)

# Attach geometry
cv_c <- st_read(here("outputs", "./shp_c.shp")) %>%
  select(GEOID) %>%
  left_join(., dat)

# 2017 ACS data from Census API
cv_d <- get_acs(geography = "tract",
                  state = c(34,42),
                  variables = c("S1701_C01_001E",
                                "S1701_C01_042E"),
                  output = "wide",
                  geometry = TRUE) %>%
  mutate(xwalk = paste(substr(GEOID, 1, 2), as.numeric(substr(GEOID, 3, 5)), sep = "_")) %>%
  filter(xwalk %in% c("34_5", "34_7", "34_15",
                      "34_21", "42_17", "42_29",
                      "42_45", "42_91", "42_101")) %>%
  mutate(li = S1701_C01_042E / S1701_C01_001E,
         moe_li = moe_prop(S1701_C01_042E, S1701_C01_001E, S1701_C01_042M, S1701_C01_001M),
         cv_li = (moe_li / 1.645) / li,
         cat_li = cut(cv_li, breaks = c(-Inf, 0.12, 0.4, Inf), labels = c("High","Medium","Low"))) %>%
  select(GEOID, ends_with("li")) %>%
  st_transform(., 26918)

# Export
st_write(cv_c, here("outputs", "cv_c.shp"))
st_write(cv_d, here("outputs", "cv_d.shp"))
