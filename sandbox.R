library(here); library(tidyverse); library(sf)
options(stringsAsFactors = FALSE)
theme_set(theme_minimal())
write_csv_here <- function(i){
  export_name <- deparse(substitute(i))
  write_csv(i, here("outputs", paste0(export_name, ".csv")))
}

# Let's look at population change over time.
shp_a <- st_read(here("outputs", "./shp_a_h.shp")) %>%
  select(GEOID, univ_a, li_a) %>%
  mutate(tot199_a = univ_a * li_a) %>%
  st_set_geometry(NULL)
shp_b <- st_read(here("outputs", "./shp_b_h.shp")) %>%
  select(GEOID, univ_b, li_b) %>%
  mutate(tot199_b = univ_b * li_b) %>%
  st_set_geometry(NULL)
shp_c <- st_read(here("outputs", "./shp_c.shp")) %>%
  select(GEOID, univ_c, li_c, tot199_c) %>%
  st_set_geometry(NULL)
shp_d <- st_read(here("outputs", "./shp_d.shp")) %>%
  select(GEOID, univ_d, li_d, tot199_d)

# NOTE: Census tracts with population less than 100 in 2017 excluded
merg_wide_sf <- left_join(shp_d, shp_c) %>%
  left_join(., shp_b) %>%
  left_join(., shp_a) %>%
  filter(univ_d >= 100)

# Compute population change relative to regional mean 1990 - 2017
# Compare li change relative to regional mean 1990 - 2017
# del = "delta"
merg_wide_sf <- merg_wide_sf %>%
  mutate(tot199_a = replace(tot199_a, tot199_a == 0, 1),
         del_li = (tot199_d - tot199_a) / tot199_a,
         del_pop = (univ_d - univ_a) / univ_a)
reg_del_li <- merg_wide_sf %>%
  st_set_geometry(NULL) %>%
  summarize((sum(tot199_d) - sum(tot199_a)) / sum(tot199_a)) %>%
  pull(.)
reg_del_pop <- merg_wide_sf %>%
  st_set_geometry(NULL) %>%
  summarize((sum(univ_d) - sum(univ_a)) / sum(univ_a)) %>%
  pull(.)
merg_wide_sf <- merg_wide_sf %>%
  mutate(rel_del_li = del_li - reg_del_li,
         rel_del_pop = del_pop - reg_del_pop)
# rel_del_li is x percent faster or slower than the regional change
plot(merg_wide_sf$rel_del_pop, merg_wide_sf$rel_del_li)
abline(h = 0, col = "blue"); abline(v = 0, col = "blue")

# Number LI residents has grown 33% in our region 1990-2017.
# Meanwhile, population has grown 10% in the same time period.

# Let's just break this into quadrants.
# What places grew faster than the regional mean and have had a drop in LI residents?
# aka gentrification nation?
merg_wide_sf <- merg_wide_sf %>%
  mutate(pop_li_chg = case_when(rel_del_li >= 0 & rel_del_pop >= 0 ~ "Increase LI / increase overall",
                                rel_del_li < 0 & rel_del_pop >= 0 ~ "Decrease LI / increase overall",
                                rel_del_li >= 0 & rel_del_pop < 0 ~ "Increase LI / decrease overall",
                                rel_del_li < 0 & rel_del_pop < 0 ~ "Decrease LI / decrease overall"))
plot(merg_wide_sf["pop_li_chg"])

# Compute population change relative to county mean 1990 - 2017
# Compare li change relative to county mean 1990 - 2017
merg_wide_sf <- merg_wide_sf %>%
  mutate(stcty = str_sub(GEOID, 1, 5))
cty_del_li <- merg_wide_sf %>%
  st_set_geometry(NULL) %>%
  group_by(stcty) %>%
  summarize(c_del_li = (sum(tot199_d) - sum(tot199_a)) / sum(tot199_a))
cty_del_pop <- merg_wide_sf %>%
  st_set_geometry(NULL) %>%
  group_by(stcty) %>%
  summarize(c_del_pop = (sum(univ_d) - sum(univ_a)) / sum(univ_a))
merg_wide_sf <- left_join(merg_wide_sf, cty_del_li) %>%
  left_join(., cty_del_pop)
merg_wide_sf <- merg_wide_sf %>%
  mutate(c_rel_del_li = del_li - c_del_li,
         c_rel_del_pop = del_pop - c_del_pop) %>%
  select(GEOID, del_li, del_pop, rel_del_li, rel_del_pop,
         pop_li_chg, c_del_li, c_del_pop, c_rel_del_li,
         c_rel_del_pop)
# c_rel_del_li is x percent faster or slower than the county change

merg_wide_sf <- merg_wide_sf %>%
  mutate(c_pop_li_chg = case_when(c_rel_del_li >= 0 & c_rel_del_pop >= 0 ~ "Increase LI / increase overall",
                                  c_rel_del_li < 0 & c_rel_del_pop >= 0 ~ "Decrease LI / increase overall",
                                  c_rel_del_li >= 0 & c_rel_del_pop < 0 ~ "Increase LI / decrease overall",
                                  c_rel_del_li < 0 & c_rel_del_pop < 0 ~ "Decrease LI / decrease overall"))

plot(merg_wide_sf["c_pop_li_chg"])
plot(st_geometry(merg_wide_sf), reset = FALSE, border = "gray",
     main = "Increasing overall population and decreasing low-income population
     relative to county mean, 1990-2017")
plot(st_geometry(merg_wide_sf %>%
                   filter(c_pop_li_chg == "Decrease LI / increase overall")),
     col = "gainsboro", add = TRUE)
plot(st_geometry(merg_wide_sf), reset = FALSE, border = "gray",
     main = "Decreasing overall population and increasing low-income population
     relative to county mean, 1990-2017")
plot(st_geometry(merg_wide_sf %>%
                   filter(c_pop_li_chg == "Increase LI / decrease overall")),
     col = "gainsboro", add = TRUE)
# One could make an interesting "Gapminder"-style chart,
# where we see the 1990, 2000, 2010, 2017 values move dynamically
# But this is not conducive to print reports

# Drastically shorten field names for export
names(merg_wide_sf) <- str_replace(names(merg_wide_sf), "del", "d")
names(merg_wide_sf) <- str_replace(names(merg_wide_sf), "rel", "r")
names(merg_wide_sf) <- str_replace(names(merg_wide_sf), "pop", "p")

st_write(merg_wide_sf, here("outputs", "population_income_change.shp"))

# Just keeping reshape methods for reference
merg_wide <- merg_wide_sf %>% st_set_geometry(NULL)
merg_long <- reshape(merg_wide, idvar = "GEOID", direction = "long", sep = "_",
                     varying = c("univ_a", "univ_b", "univ_c", "univ_d",
                                 "li_a", "li_b", "li_c", "li_d",
                                 "tot199_a", "tot199_b", "tot199_c", "tot199_d"),
                     timevar = "Year") %>%
  mutate_at(vars(Year), funs(case_when(. == "a" ~ 1990,
                                       . == "b" ~ 2000,
                                       . == "c" ~ 2010,
                                       . == "d" ~ 2017)))

# Race / ethnicity
library(tidycensus)
area <- c("34005", "34007", "34015", "34021",
          "42017", "42029", "42045", "42091", "42101")
rac_eth_17 <- get_acs(geography = "tract", state = c(34,42),
                      variables = c("B03002_001", "B03002_003"),
                      cache_table = TRUE) %>%
  mutate(stcty = str_sub(GEOID, 1, 5)) %>%
  filter(stcty %in% area) %>%
  select(GEOID, variable, estimate)
rac_eth_10_nj <- get_acs(geography = "tract", state = 34,
                         variables = c("B03002_001", "B03002_003"),
                         year = 2012, cache_table = TRUE)
rac_eth_10_pa <- get_acs(geography = "tract", state = 42,
                         variables = c("B03002_001", "B03002_003"),
                         year = 2012)
rac_eth_10 <- bind_rows(rac_eth_10_nj, rac_eth_10_pa) %>%
  mutate(stcty = str_sub(GEOID, 1, 5)) %>%
  filter(stcty %in% area) %>%
  select(GEOID, variable, estimate)

rac_eth_d <- rac_eth_17 %>%
  group_by(GEOID) %>%
  summarize(totnw_d = estimate[1] - estimate[2],
            nw_d = (estimate[1] - estimate[2]) / estimate[1])

rac_eth_c <- rac_eth_10 %>%
  group_by(GEOID) %>%
  summarize(totnw_c = estimate[1] - estimate[2],
            nw_c = (estimate[1] - estimate[2]) / estimate[1])

rac_eth_d <- read_csv(here("outputs", "rac_eth_d.csv")) %>%
  mutate_at(vars(GEOID), as.character)
rac_eth_c <- read_csv(here("outputs", "rac_eth_c.csv"))

# Sigh let's work with the hard data now
# Thankfully aggregated from orig block to 2010 tract
rac_eth <- read_csv(here("data", "nhgis0009_ts_geog2010_tract.csv")) %>%
  select(STATEA, COUNTYA, TRACTA, ends_with("1990"), ends_with("2000")) %>%
  mutate(univ_a = select(., ends_with("1990")) %>% rowSums(., na.rm = TRUE),
         univ_b = select(., ends_with("2000")) %>% rowSums(., na.rm = TRUE),
         totnw_a = univ_a - CW7AA1990,
         totnw_b = univ_b - CW7AA1990,
         nw_a = totnw_a / univ_a,
         nw_b = totnw_b / univ_b,
         xwalk = paste(STATEA, COUNTYA, sep = "_"),
         GEOID = paste0(STATEA, COUNTYA, TRACTA)) %>%
  filter(xwalk %in% c("34_005", "34_007", "34_015",
                      "34_021", "42_017", "42_029",
                      "42_045", "42_091", "42_101")) %>%
  select(GEOID, totnw_a, nw_a, totnw_b, nw_b) %>%
  as.data.frame(.)

# Reshape
rac_eth_long <- reshape(rac_eth, idvar = "GEOID", direction = "long", sep = "_",
                        varying = c("totnw_a", "totnw_b", "nw_a", "nw_b"),
                        timevar = "Year") %>%
  mutate_at(vars(Year), funs(case_when(. == "a" ~ 1990,
                                       . == "b" ~ 2000)))
rac_eth_a <- rac_eth_long %>% filter(Year == 1990) %>% select(-Year)
rac_eth_b <- rac_eth_long %>% filter(Year == 2000) %>% select(-Year)

write_csv_here(rac_eth_a)
write_csv_here(rac_eth_b)
write_csv_here(rac_eth_c)
write_csv_here(rac_eth_d)

# Cool. Now let's look at what's happening
merg_wide_sf <- merg_wide_sf %>%
  left_join(., rac_eth_a) %>%
  left_join(., rac_eth_d) %>%
  mutate(del_rac = (totnw_d - totnw) / totnw,
         stcty = str_sub(GEOID, 1, 5))
cty_del_rac <- merg_wide_sf %>%
  st_set_geometry(NULL) %>%
  group_by(stcty) %>%
  summarize(c_del_rac = (sum(totnw_d) - sum(totnw)) / sum(totnw))
merg_wide_sf <- merg_wide_sf %>%
  left_join(., cty_del_rac) %>%
  mutate(c_rel_del_rac = del_rac - c_del_rac)

# Remove outlier
merg_wide_sf <- merg_wide_sf %>% filter(c_r_d_li < 50)
# Too sleepy to continue. c_rel_del_rac x c_r_d_li x c_r_d_p?
plot(merg_wide_sf$c_r_d_li, merg_wide_sf$c_r_d_p)
plot(merg_wide_sf$c_r_d_li, merg_wide_sf$c_rel_del_rac)
plot(merg_wide_sf$c_r_d_p, merg_wide_sf$c_rel_del_rac)
cor(merg_wide_sf$c_r_d_li, merg_wide_sf$c_r_d_p) # Decent positive cor here
# p.s. I obviously don't care much about whether the trend is linear
cor(merg_wide_sf$c_r_d_li, merg_wide_sf$c_rel_del_rac)
cor(merg_wide_sf$c_r_d_p, merg_wide_sf$c_rel_del_rac)

# NEW DATA!
# Price of residential land for census tracts! https://www.fhfa.gov/PolicyProgramsResearch/Research/Pages/wp1901.aspx
# Median home value for 1990 and 2000. Must be adjusted for inflation and harmonized.
# Must download 2010 and 2017 from B25077.
# BLS adjusted series ID CUUR0000SA0


# Would be cool to reorganize this thing.
# first, data download and processing. import data = big files
# process data = processed input files, sharable to github
# export data = results (be careful about what goes in here)
library(quantmod)
getSymbols("CPIAUCSL", src = "FRED")
avg.cpi <- apply.yearly(CPIAUCSL, mean)
cf <- avg.cpi/as.numeric(avg.cpi["2017"]) # using 2017 as the base year
# AMAZING. Save this so we can use later
write.zoo(cf, here("data", "cpi_2017.csv"), sep=",")


hous_d <- get_acs(geography = "tract", state = c(34,42),
                  table = "B25077") %>%
  mutate(stcty = str_sub(GEOID, 1, 5)) %>%
  filter(stcty %in% area) %>%
  mutate(hous_d = estimate / 1000) %>%
  select(GEOID, hous_d)
hous_c_nj <- get_acs(geography = "tract", state = 34,
                     table = "B25077",
                     year = 2012, cache_table = TRUE)
hous_c_pa <- get_acs(geography = "tract", state = 42,
                     table = "B25077",
                     year = 2012)
hous_c <- bind_rows(hous_c_nj, hous_c_pa) %>%
  mutate(stcty = str_sub(GEOID, 1, 5),
         hous_c = estimate / 1000 * as.numeric(cf["2010-12-01"])) %>%
  filter(stcty %in% area) %>%
  select(GEOID, hous_c)
write_csv_here(hous_c)
write_csv_here(hous_d)

hous_c <- read_csv(here("outputs", "hous_c.csv")) %>%
  mutate_at(vars(GEOID), as.character)

# Moving on to 1990 and 2000
# ???????????
hous_a <- read_csv(here("data", "nhgis0010_ds120_1990_tract.csv")) %>%
  mutate(GEOID = paste0(STATEA, str_pad(COUNTYA, 3, "left", "0"), str_pad(TRACTA, 6, "left", "0")),
         stcty = paste0(STATEA, COUNTYA),
         hous_a = EST001 / 1000 * as.numeric(cf["1990-12-01"])) %>%
  filter(stcty %in% area) %>%
  select(GEOID, hous_a)
# Should append univ to this for LTDB
pop_a <- st_read(here("outputs", "./shp_a.shp")) %>%
  select(GEOID, univ_a) %>%
  st_set_geometry(NULL) %>%
  mutate_at(vars(GEOID), as.character)
hous_a <- left_join(hous_a, pop_a, by = "GEOID")
write_csv_here(hous_a)

hous_b <- read_csv(here("data", "nhgis0010_ds151_2000_tract.csv")) %>%
  mutate_at(vars(TRACTA), funs(str_pad(., 6, side = "left", pad = "0"))) %>%
  mutate(GEOID = paste0(STATEA, TRACTA, COUNTYA),
         stcty = paste0(STATEA, COUNTYA),
         hous_b = GB7001 / 1000 * as.numeric(cf["2000-12-01"])) %>%
  filter(stcty %in% area) %>%
  select(GEOID, hous_b)
# Should append univ to this for LTDB
pop_b <- st_read(here("outputs", "./shp_b.shp")) %>%
  select(GEOID, univ_b) %>%
  st_set_geometry(NULL)
hous_b <- left_join(hous_b, pop_a)
write_csv_here(hous_b)

# export those as dta
library(here); library(haven)



# 4. Compute percentage low-income persons relative to county
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

# 5. Compute percentage low-income persons relative to region
reg_li <- weighted.mean(trct_d$li_d, trct_d$univ_d)
reg_sd <- sd(trct_d$li_d, na.rm = TRUE)
trct_d %<>%
  mutate(rel_li_reg = li_d / reg_li,
         z_reg = (li_d - reg_li) / reg_sd,
         z_cat_reg = cut(z_reg, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)))

