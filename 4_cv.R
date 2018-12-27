require(here); require(sf); require(dplyr); require(tidycensus); require(stringr)
options(stringsAsFactors = FALSE)

# There's a problem with the 2010 data. Also use factor classifications for CV reliability.

# 2010 data from NHGIS
cv_c <- read.csv(here("data", "nhgis0006_ts_nominal_tract.csv")) %>%
  mutate(xwalk = paste(STATEFP, COUNTYFP, sep = "_"),
         GEOID = paste0(STATEFP, str_pad(COUNTYFP, 3, "left", "0"), TRACTA)) %>%
  filter(xwalk %in% c("34_5", "34_7", "34_15",
                      "34_21", "42_17", "42_29",
                      "42_45", "42_91", "42_101")) %>%
  filter(GJOIN2012 != "") %>%
  select(1:4, GEOID, contains("125"))
# Universe estimate and MOE
cv_c$univ <- cv_c %>%
  select(ends_with("125")) %>%
  mutate(v = rowSums(select_all(.))) %>%
  pull(v)
cv_c$moe_univ <- cv_c %>%
  select(ends_with("M")) %>%
  mutate_all(funs(. ^ 2)) %>%
  mutate(v = sqrt(rowSums(select_all(.)))) %>%
  pull(v)
# Below 200% FPL estimate and MOE
cv_c$tot199 <- cv_c %>%
  select(ends_with("125"), -C20AI125) %>%
  mutate(v = rowSums(select_all(.))) %>%
  pull(v)
cv_c$moe_tot199 <- cv_c %>%
  select(ends_with("M"), -C20AI125M) %>%
  mutate_all(funs(. ^ 2)) %>%
  mutate(v = sqrt(rowSums(select_all(.)))) %>%
  pull(v)
# Clean data and compute final proportions
cv_c <- cv_c %>%
  mutate(li = tot199 / univ,
         moe_li = moe_prop(tot199, univ, moe_tot199, moe_univ),
         cv_li = (moe_li / 1.645) / li) %>%
  select(1:5, ends_with("li"))
# Merge with shapefile
cv_c <- st_read(here("data", "./US_tract_2010.shp")) %>%
  select(GISJOIN) %>%
  left_join(., cv_c, by = c("GISJOIN" = "GJOIN2012")) %>%
  filter(GISJOIN %in% cv_c$GJOIN2000) %>%
  select(1:5, ends_with("_li")) %>%
  st_transform(., 26918)

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
         cv_li = (moe_li / 1.645) / li) %>%
  select(GEOID, ends_with("li")) %>%
  st_transform(., 26918)

plot(cv_d["cv_li"])
