library(here); library(tidyverse); library(sf)
options(stringsAsFactors = FALSE)
theme_set(theme_minimal())

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

# NEW DATA!
# Race / ethnicity for 1990 and 2000 aggregated from block to 2010 tract boundaries (no harmonization required)
# Must download B03002 for 2010 and 2017.
# Price of residential land for census tracts! https://www.fhfa.gov/PolicyProgramsResearch/Research/Pages/wp1901.aspx
# Median home value for 1990 and 2000. Must be adjusted for inflation and harmonized.
# Must download 2010 and 2017 from B25077.
