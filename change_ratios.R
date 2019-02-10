library(here); library(tidyverse); library(sf); library(tigris)
options(stringsAsFactors = FALSE)
theme_set(theme_minimal())
options(tigris_use_cache = TRUE, tigris_class = "sf")
source("functions.R")

area <- c("34005", "34007", "34015", "34021",
          "42017", "42029", "42045", "42091", "42101")
st <- str_sub(area, 1, 2)
cty <- str_sub(area, 3, 5)
trct <- map2(st, cty, ~{tracts(state = .x, county = .y, cb = TRUE, year = 2017)}) %>%
  rbind_tigris() %>%
  st_transform(., 26918) %>%
  select(GEOID)

a <- read_csv(here("process_data", "a.csv"))
b <- read_csv(here("process_data", "b.csv"))
c <- read_csv(here("process_data", "c.csv"))
d <- read_csv(here("process_data", "d.csv"))

# Tracts with popuation below 100 in 2017 excluded
merg <- left_join(a, b) %>%
  left_join(., c) %>%
  left_join(., d) %>%
  filter(univ_d >= 100) %>%
  mutate(stcty = str_sub(GEOID, 1, 5))

# CHANGE
# Change relative to regional mean 1990 - 2017
# rdel = "regional delta"
rdel <- merg %>%
  summarize(rdel_hous = (sum(hous_d) - sum(hous_a)) / sum(hous_a),
            rdel_li = (sum(tot199_d) - sum(tot199_a)) / sum(tot199_a),
            rdel_nw = (sum(totnw_d) - sum(totnw_a)) / sum(totnw_a),
            rdel_pop = (sum(univ_d) - sum(univ_a)) / sum(univ_a))
# Change relative to county mean 1990 - 2017
# cdel = "county delta"
cdel <- merg %>%
  group_by(stcty) %>%
  summarize(cdel_hous = (sum(hous_d) - sum(hous_a)) / sum(hous_a),
            cdel_li = (sum(tot199_d) - sum(tot199_a)) / sum(tot199_a),
            cdel_nw = (sum(totnw_d) - sum(totnw_a)) / sum(totnw_a),
            cdel_pop = (sum(univ_d) - sum(univ_a)) / sum(univ_a))
# d = "delta" 1990 - 2017
# Can't divide by 0 so putting 1
merg <- merg %>% mutate(tot199_a = replace(tot199_a, tot199_a == 0, 1),
                d_hous = (hous_d - hous_a) / hous_a,
                d_li = (tot199_d - tot199_a) / tot199_a,
                d_nw = (totnw_d - totnw_a) / totnw_a,
                d_pop = (univ_d - univ_a) / univ_a)
# rr = "relative to region"
# value of rr is x times faster or slower than regional change
merg <- merg %>% mutate(rr_hous = d_hous / rdel$rdel_hous,
                        rr_li = d_li / rdel$rdel_li,
                        rr_nw = d_nw / rdel$rdel_nw,
                        rr_pop = d_pop / rdel$rdel_pop)
# rc = "relative to county"
# value of rc is x times faster or slower than county change
merg <- merg %>% left_join(., cdel, by = "stcty") %>%
  mutate(rc_hous = d_hous / cdel_hous,
         rc_li = d_li / cdel_li,
         rc_nw = d_nw / cdel_nw,
         rc_pop = d_pop / cdel_pop)
# Export
delta <- merg %>% select(GEOID, starts_with("d"), starts_with("rc"), starts_with("rr")) %>%
  mutate_at(vars(GEOID), as.character)
write_csv_here(delta)
delta_shp <- left_join(trct, delta)
write_shp_here(delta_shp)

# CURRENT STATUS
# Change relative to regional mean 1990 - 2017
# rlvl = "regional level"
rlvl <- merg %>%
  summarize(rlvl_hous = mean(hous_d),
            rlvl_li = mean(li_d),
            rlvl_nw = mean(nw_d),
            rlvl_pop = mean(univ_d))
# Change relative to county mean 1990 - 2017
# clvl = "county level"
clvl <- merg %>%
  group_by(stcty) %>%
  summarize(clvl_hous = mean(hous_d),
            clvl_li = mean(li_d),
            clvl_nw = mean(nw_d),
            clvl_pop = mean(univ_d))
# rr = "relative to region"
# value of rr is x ratio of tract to regional level
merg <- merg %>% mutate(rr_hous = d_hous / rlvl$rlvl_hous,
                        rr_li = d_li / rlvl$rlvl_li,
                        rr_nw = d_nw / rlvl$rlvl_nw,
                        rr_pop = d_pop / rlvl$rlvl_pop)
# rc = "relative to county"
# value of rr is x ratio of tract to county level
merg <- merg %>% left_join(., clvl, by = "stcty") %>%
  mutate(rc_hous = d_hous / clvl_hous,
         rc_li = d_li / clvl_li,
         rc_nw = d_nw / clvl_nw,
         rc_pop = d_pop / clvl_pop)
# Export
level <- merg %>% select(GEOID, ends_with("d"), starts_with("rc"), starts_with("rr")) %>%
  mutate_at(vars(GEOID), as.character)
write_csv_here(level)
level_shp <- left_join(trct, level)
write_shp_here(level_shp)
