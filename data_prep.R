library(here); library(sf); library(tidyverse); library(haven)
library(tidycensus); library(tigris); library(quantmod)
options(stringsAsFactors = FALSE)
options(tigris_use_cache = TRUE, tigris_class = "sf")
source("functions.R")
area <- c("34005", "34007", "34015", "34021",
          "42017", "42029", "42045", "42091", "42101")
# INCOME
# 1990 and 2000: NHGIS Code C20, Persons by Ratio of Income to Poverty Level in Previous Year
# NOTE: Contemporaneous tracts must be harmonized using LTDB
d <- read_csv(here("raw_data", "nhgis0006_ts_nominal_tract.csv")) %>%
  select(-NAME1990, -NAME2000) %>%
  mutate(GEOID = paste0(STATEFP, COUNTYFP, str_pad(TRACTA, 6, "left", "0")),
         stcty = paste0(STATEFP, COUNTYFP),
         univ_a = select(., ends_with("1990"), -GJOIN1990) %>% rowSums(., na.rm = TRUE),
         univ_b = select(., ends_with("2000"), -GJOIN2000) %>% rowSums(., na.rm = TRUE),
         tot199_a = univ_a - C20AI1990,
         tot199_b = univ_b - C20AI2000,
         li_a = tot199_a / univ_a,
         li_b = tot199_b / univ_b) %>%
  filter(stcty %in% area) %>%
  select(GEOID, GJOIN1990, GJOIN2000, ends_with("_a"), ends_with("b"))
# Separate by year
inc_a <- d %>% filter(GJOIN1990 != "") %>%
  select(GEOID, GJOIN1990, ends_with("a"))
inc_b <- d %>% filter(GJOIN2000 != "") %>%
  select(GEOID, GJOIN2000, ends_with("b"))
# 2010: ACS 5-Year S1701 2008-2012 Midpoint
d_nj <- get_acs(geography = "tract", state = 34,
             variables = c("S1701_C01_001", "S1701_C01_038"),
             year = 2012, cache_table = TRUE)
d_pa <- get_acs(geography = "tract", state = 42,
                variables = c("S1701_C01_001", "S1701_C01_038"),
                year = 2012)
inc_c <- bind_rows(d_nj, d_pa) %>%
  mutate(stcty = str_sub(GEOID, 1, 5)) %>%
  filter(stcty %in% area) %>%
  group_by(GEOID) %>%
  summarize(univ_c = estimate[1],
            tot199_c = estimate[2],
            li_c = estimate[2] / estimate[1])
# 2017: ACS 5-Year S1701
inc_d <- get_acs(geography = "tract", state = c(34,42),
                 variables = c("S1701_C01_001", "S1701_C01_042")) %>%
  mutate(stcty = str_sub(GEOID, 1, 5)) %>%
  filter(stcty %in% area) %>%
  group_by(GEOID) %>%
  summarize(univ_d = estimate[1],
            tot199_d = estimate[2],
            li_d = estimate[2] / estimate[1])

# RACE AND ETHNICITY
# % Residents NOT White Non-Hispanic
# 1990 and 2000: NHGIS Code CW7, Persons by Hispanic or Latino Origin by Race
# NOTE: Tracts aggregated from contemporaneous block to 2010 boundaries
d <- read_csv(here("raw_data", "nhgis0009_ts_geog2010_tract.csv")) %>%
  select(STATEA, COUNTYA, TRACTA, ends_with("1990"), ends_with("2000")) %>%
  mutate(univ_a = select(., ends_with("1990")) %>% rowSums(., na.rm = TRUE),
         univ_b = select(., ends_with("2000")) %>% rowSums(., na.rm = TRUE),
         totnw_a = univ_a - CW7AA1990,
         totnw_b = univ_b - CW7AA1990,
         nw_a = totnw_a / univ_a,
         nw_b = totnw_b / univ_b,
         stcty = paste0(STATEA, COUNTYA),
         GEOID = paste0(STATEA, COUNTYA, TRACTA)) %>%
  filter(stcty %in% area) %>%
  select(GEOID, totnw_a, nw_a, totnw_b, nw_b)
# Separate by year
rac_a <- d %>% select(GEOID, ends_with("a"))
rac_b <- d %>% select(GEOID, ends_with("b"))
# 2010: ACS 5-Year B03002 2008-2012 Midpoint
rac_nj <- get_acs(geography = "tract", state = 34,
                  variables = c("B03002_001", "B03002_003"),
                  year = 2012, cache_table = TRUE)
rac_pa <- get_acs(geography = "tract", state = 42,
                  variables = c("B03002_001", "B03002_003"),
                  year = 2012)
rac_c <- bind_rows(rac_nj, rac_pa) %>%
  mutate(stcty = str_sub(GEOID, 1, 5)) %>%
  filter(stcty %in% area) %>%
  group_by(GEOID) %>%
  summarize(totnw_c = estimate[1] - estimate[2],
            nw_c = (estimate[1] - estimate[2]) / estimate[1])
# 2017: ACS 5-Year B03002
rac_d <- get_acs(geography = "tract", state = c(34,42),
                 variables = c("B03002_001", "B03002_003")) %>%
  mutate(stcty = str_sub(GEOID, 1, 5)) %>%
  filter(stcty %in% area) %>%
  group_by(GEOID) %>%
  summarize(totnw_d = estimate[1] - estimate[2],
            nw_d = (estimate[1] - estimate[2]) / estimate[1])

# MEDIAN HOME VALUE
# CPI, 2017 Base
loadSymbols("CPIAUCSL", src = "FRED")
avg_cpi <- apply.yearly(CPIAUCSL, mean)
inflation <- avg_cpi / as.numeric(avg_cpi["2017"])
# 1990: NHGIS Code DS120, Specified Owner-Occupied Housing Units
# NOTE: Contemporaneous tracts must be harmonized using LTDB
hous_a <- read_csv(here("raw_data", "nhgis0010_ds120_1990_tract.csv")) %>%
  mutate(stcty = paste0(STATEA, COUNTYA),
         hous_a = EST001 / 1000 / as.numeric(inflation["1990-12-01"])) %>%
  filter(stcty %in% area) %>%
  select(GISJOIN, hous_a)
# 2000: NHGIS Code DS151, Specified Owner-Occupied Housing Units
# NOTE: Contemporaneous tracts must be harmonized using LTDB
hous_b <- read_csv(here("raw_data", "nhgis0010_ds151_2000_tract.csv")) %>%
  mutate(stcty = paste0(STATEA, COUNTYA),
         hous_b = GB7001 / 1000 / as.numeric(inflation["2000-12-01"])) %>%
  filter(stcty %in% area) %>%
  select(GISJOIN, hous_b)
# 2010: ACS 5-Year B25077 2008-2012 Midpoint
hous_nj <- get_acs(geography = "tract", state = 34,
                     table = "B25077",
                     year = 2012, cache_table = TRUE)
hous_pa <- get_acs(geography = "tract", state = 42,
                     table = "B25077",
                     year = 2012)
hous_c <- bind_rows(hous_nj, hous_pa) %>%
  mutate(stcty = str_sub(GEOID, 1, 5),
         hous_c = estimate / 1000 / as.numeric(inflation["2010-12-01"])) %>%
  filter(stcty %in% area) %>%
  select(GEOID, hous_c)
# 2017: ACS 5-Year B25077
hous_d <- get_acs(geography = "tract", state = c(34,42),
                  table = "B25077") %>%
  mutate(stcty = str_sub(GEOID, 1, 5)) %>%
  filter(stcty %in% area) %>%
  mutate(hous_d = estimate / 1000) %>%
  select(GEOID, hous_d)

# LAND VALUE
# https://www.fhfa.gov/PolicyProgramsResearch/Research/Pages/wp1901.aspx
# Not available for NJ; not enough data in general
land <- read_csv(here("raw_data", "panel_census_tracts.csv")) %>%
  mutate(stcty = str_sub(`Census Tract`, 1, 5)) %>%
  filter(stcty %in% area & Year == 2017)

# Export data for LTDB
orig_a <- inner_join(inc_a, hous_a, by = c("GJOIN1990" = "GISJOIN")) %>%
  select(-GJOIN1990)
orig_b <- inner_join(inc_b, hous_b, by = c("GJOIN2000" = "GISJOIN")) %>%
  select(-GJOIN2000)
write_dta(orig_a, here("process_data", "orig_a.dta"))
write_dta(orig_b, here("process_data", "orig_b.dta"))

# Import data from LTDB
corresp_a <- read_dta(here("process_data", "corresp_a.dta")) %>%
  zap_formats(.) %>% labelled::remove_labels(.)
corresp_a_2 <- read_dta(here("process_data", "corresp_a_2.dta")) %>%
  zap_formats(.) %>% labelled::remove_labels(.) %>%
  select(-univ_a)
corresp_a <- corresp_a %>% left_join(., corresp_a_2)
corresp_b <- read_dta(here("process_data", "corresp_b.dta")) %>%
  zap_formats(.) %>% labelled::remove_labels(.)
corresp_b_2 <- read_dta(here("process_data", "corresp_b_2.dta")) %>%
  zap_formats(.) %>% labelled::remove_labels(.) %>%
  select(-univ_b)
corresp_b <- corresp_b %>% left_join(., corresp_b_2)

# Join as tabular
a <- inner_join(rac_a, corresp_a, by = c("GEOID" = "trtid10")) %>%
  select(GEOID, sort(current_vars()))
b <- inner_join(rac_b, corresp_b, by = c("GEOID" = "trtid10")) %>%
  select(GEOID, sort(current_vars()))
c <- inner_join(hous_c, inc_c) %>%
  inner_join(., rac_c) %>%
  select(GEOID, sort(current_vars()))
d <- inner_join(hous_d, inc_d) %>%
  inner_join(., rac_d) %>%
  select(GEOID, sort(current_vars()))

# Get census tracts
st <- str_sub(area, 1, 2)
cty <- str_sub(area, 3, 5)
trct <- map2(st, cty, ~{tracts(state = .x, county = .y, cb = TRUE, year = 2017)}) %>%
  rbind_tigris() %>%
  st_transform(., 26918) %>%
  select(GEOID, ALAND, AWATER)

# Join as shapefile
a_shp <- left_join(trct, a)
b_shp <- left_join(trct, b)
c_shp <- left_join(trct, c)
d_shp <- left_join(trct, d)

# Export all
write_csv_here(a); write_csv_here(b); write_csv_here(c); write_csv_here(d)
write_shp_here(a_shp); write_shp_here(b_shp); write_shp_here(c_shp); write_shp_here(d_shp)
