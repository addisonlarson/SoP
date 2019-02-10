library(here); library(tidyverse); library(tidycensus); library(sf); library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")
source("functions.R")
area <- c("34005", "34007", "34015", "34021",
          "42017", "42029", "42045", "42091", "42101")
# Get census tracts
st <- str_sub(area, 1, 2)
cty <- str_sub(area, 3, 5)
trct <- map2(st, cty, ~{tracts(state = .x, county = .y, cb = TRUE, year = 2017)}) %>%
  rbind_tigris() %>%
  st_transform(., 26918) %>%
  select(GEOID)

# Percentage of low-income residents
# 2010: ACS 5-Year S1701 2008-2012 Midpoint
d_nj <- get_acs(geography = "tract", state = 34, output = "wide",
                variables = c("S1701_C01_001", "S1701_C01_038"),
                year = 2012)
d_pa <- get_acs(geography = "tract", state = 42, output = "wide",
                variables = c("S1701_C01_001", "S1701_C01_038"),
                year = 2012)
cv_inc_c <- bind_rows(d_nj, d_pa) %>%
  mutate(stcty = str_sub(GEOID, 1, 5),
         li_c = S1701_C01_038E / S1701_C01_001E,
         moe_li_c = moe_prop(S1701_C01_038E, S1701_C01_001E, S1701_C01_038M, S1701_C01_001M),
         cv_li_c = (moe_li_c / 1.645) / li_c,
         cat_li_c = cut(cv_li_c, breaks = c(-Inf, 0.12, 0.4, Inf), labels = c("High","Medium","Low"))) %>%
  filter(stcty %in% area) %>%
  select(GEOID, ends_with("_c"))
# 2017: ACS 5-Year S1701
cv_inc_d <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                    variables = c("S1701_C01_001", "S1701_C01_042")) %>%
  mutate(stcty = str_sub(GEOID, 1, 5),
         li_d = S1701_C01_042E / S1701_C01_001E,
         moe_li_d = moe_prop(S1701_C01_042E, S1701_C01_001E, S1701_C01_042M, S1701_C01_001M),
         cv_li_d = (moe_li_d / 1.645) / li_d,
         cat_li_d = cut(cv_li_d, breaks = c(-Inf, 0.12, 0.4, Inf), labels = c("High","Medium","Low"))) %>%
  filter(stcty %in% area) %>%
  select(GEOID, ends_with("_d"))
# Remove non-finite results
cv_inc_c <- cv_inc_c[is.finite(rowSums(cv_inc_c[,2:4])),]
cv_inc_d <- cv_inc_d[is.finite(rowSums(cv_inc_d[,2:4])),]
# Test for statistically significant increase
num_c <- cv_inc_c$li_c
denom_c <- (cv_inc_c$moe_li_c / 1.645) ^ 2
num_d <- cv_inc_d$li_d
denom_d <- (cv_inc_d$moe_li_d / 1.645) ^ 2
cv_inc_d <- cv_inc_d %>%
  mutate(z_1dir = (num_d - num_c) / sqrt(denom_d + denom_c),
         z_1dir_sig = ifelse(z_1dir > 1.645, "Yes", "No"))
# Export
write_csv_here(cv_inc_c); write_csv_here(cv_inc_d)
cv_inc_c_shp <- left_join(trct, cv_inc_c)
write_shp_here(cv_inc_c_shp)
cv_inc_d_shp <- left_join(trct, cv_inc_d)
write_shp_here(cv_inc_d_shp)

# Percentage of nonwhite residents
# Variance replicate tables not available for ACS 5-Year B03002 2008-2012 Midpoint
# 2017: ACS 5-Year B03002
# Download files
nj_url <- "https://www2.census.gov/programs-surveys/acs/replicate_estimates/2017/data/5-year/140/B03002_34.csv.gz"
pa_url <- "https://www2.census.gov/programs-surveys/acs/replicate_estimates/2017/data/5-year/140/B03002_42.csv.gz"
nj_temp <- tempfile()
download.file(nj_url, nj_temp)
nj_var_rep <- read_csv(gzfile(nj_temp))
pa_temp <- tempfile()
download.file(pa_url, pa_temp)
pa_var_rep <- read_csv(gzfile(pa_temp))
# Merge and subset for DVRPC region
var_rep <- bind_rows(nj_var_rep, pa_var_rep) %>%
  mutate_at(vars(GEOID), funs(str_sub(., 8, 18))) %>%
  filter(str_sub(GEOID, 1, 5) %in% area) %>%
  select(-TBLID, -NAME, -TITLE, -moe, -CME, -SE) %>%
  filter(ORDER %in% 4:12)
# Sum up subfields
num <- var_rep %>% 
  group_by(GEOID) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  select(-GEOID)
estim <- num %>% select(estimate)
individual_replicate <- num %>% select(-estimate)
# Grab GEOIDs to append to results
GEOID <- var_rep %>% select(GEOID) %>% distinct(.) %>% pull(.)
sqdiff_fun <- function(v, e) (v-e)^2
sqdiff <- mapply(sqdiff_fun, individual_replicate, estim) 
sum_sqdiff <- rowSums(sqdiff)
variance <- 0.05 * sum_sqdiff
moe <- round(sqrt(variance) * 1.645, 0)
temp <- cbind(GEOID, moe) %>%
  as_tibble(.) %>%
  mutate_at(vars(moe), as.numeric)

cv_nw_d <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                   variables = c("B03002_001", "B03002_003")) %>%
  left_join(., temp) %>%
  mutate(stcty = str_sub(GEOID, 1, 5),
         nw_d = (B03002_001E - B03002_003E) / B03002_001E,
         moe_nw_d = moe_prop(B03002_003E, B03002_001E, moe, B03002_001M),
         cv_nw_d = (moe_nw_d / 1.645) / nw_d,
         cat_nw_d = cut(cv_nw_d, breaks = c(-Inf, 0.12, 0.4, Inf), labels = c("High","Medium","Low"))) %>%
  filter(stcty %in% area) %>%
  select(GEOID, ends_with("_d"))
# Export
write_csv_here(cv_nw_d)
cv_nw_d_shp <- left_join(trct, cv_nw_d)
write_shp_here(cv_nw_d_shp)

# Median home value
# Statistical testing not possible with medians
# 2017: ACS 5-Year B25077
cv_hous_d <- get_acs(geography = "tract", state = c(34,42), table = "B25077") %>%
  mutate(stcty = str_sub(GEOID, 1, 5),
         hous_d = estimate / 1000,
         moe_hous_d = moe / 1000,
         cv_hous_d = (moe_hous_d / 1.645) / hous_d,
         cat_hous_d = cut(cv_hous_d, breaks = c(-Inf, 0.12, 0.4, Inf), labels = c("High","Medium","Low"))) %>%
  filter(stcty %in% area) %>%
  select(GEOID, ends_with("_d"))
# Export
write_csv_here(cv_hous_d)
cv_hous_d_shp <- left_join(trct, cv_hous_d)
write_shp_here(cv_hous_d_shp)
