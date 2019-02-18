library(here); library(tidyverse); library(sf); library(tigris)
options(stringsAsFactors = FALSE)
theme_set(theme_minimal())
options(tigris_use_cache = TRUE, tigris_class = "sf")
source("functions.R")

area <- c("34005", "34007", "34015", "34021",
          "42017", "42029", "42045", "42091", "42101")
st <- str_sub(area, 1, 2)
cty <- str_sub(area, 3, 5)
trct <- map2(st, cty, ~{tracts(state = .x,
                               county = .y,
                               year = 2017)}) %>%
  rbind_tigris() %>%
  st_transform(., 26918) %>%
  select(GEOID)

# Can't divide by 0 so 0 becomes 0.1
a <- read_csv(here("process_data", "a.csv")) %>%
  mutate_at(vars(li_a, nw_a), funs(. * 100)) %>%
  mutate_all(funs(ifelse(. == 0, 0.1, .)))
d <- read_csv(here("process_data", "d.csv")) %>%
  mutate_at(vars(li_d, nw_d), funs(. * 100))

# Tracts with popuation below 100 in 2017 excluded
slicer <- d %>% filter(univ_d < 100) %>%
  select(GEOID) %>% pull(.)
a <- a %>% filter(!(GEOID %in% slicer))
d <- d %>% filter(!(GEOID %in% slicer))

# Absolute change
# 2017 - 1990 (Works okay for % vals)
diffs_fun <- function(i, j, ...) (j - i)
diffs_ovr <- map2_dfr(a, d, diffs_fun)

# Absolute change relative to region
diffs_reg_mean <- map_dfr(diffs_ovr, mean)
diffs_reg <- map2_dfr(diffs_reg_mean, diffs_ovr, diffs_fun)

# Absolute change relative to county
diffs_ovr$GEOID <- a$GEOID
diffs_co_mean <- diffs_ovr %>%
  mutate(stcty = str_sub(GEOID, 1, 5)) %>%
  group_by(stcty) %>%
  summarize_all(mean) %>%
  select(-GEOID)
names(diffs_co_mean) <- str_replace(names(diffs_co_mean), "_a", "_cm")
diffs_ovr <- diffs_ovr %>% mutate(stcty = str_sub(GEOID, 1, 5))
diffs_co <- left_join(diffs_ovr, diffs_co_mean) %>%
  mutate(hous = hous_a - hous_cm,
         li = li_a - li_cm,
         nw = nw_a - nw_cm,
         tot199 = tot199_a - tot199_cm,
         totnw = totnw_a - totnw_cm,
         univ = univ_a - univ_cm) %>%
  select(hous, li, nw, tot199, totnw, univ)

# Clean these up
diffs_ovr <- diffs_ovr %>% select(-stcty, -GEOID)
names(diffs_ovr) <- str_replace(names(diffs_ovr), "_a", "")
names(diffs_reg) <- str_replace(names(diffs_reg), "_a", "")
diffs_co <- diffs_co %>% select(-GEOID)
diffs_reg <- diffs_reg %>% select(-GEOID)

# Relative change
# (2017 - 1990) / 1990 (Works better for most things?)
ratios_fun <- function(i, j, ...) (j - i) / i * 100
ratios_ovr <- map2_dfr(a, d, ratios_fun)

# Percentage change relative to region
ratios_reg_mean <- map_dfr(ratios_ovr, mean)
ratios_reg <- map2_dfr(ratios_reg_mean, ratios_ovr, diffs_fun)

# Percentage change relative to county
ratios_ovr$GEOID <- a$GEOID
ratios_co_mean <- ratios_ovr %>%
  mutate(stcty = str_sub(GEOID, 1, 5)) %>%
  group_by(stcty) %>%
  summarize_all(mean) %>%
  select(-GEOID)
names(ratios_co_mean) <- str_replace(names(ratios_co_mean), "_a", "_cm")
ratios_ovr <- ratios_ovr %>% mutate(stcty = str_sub(GEOID, 1, 5))
ratios_co <- left_join(ratios_ovr, ratios_co_mean) %>%
  mutate(hous = hous_a - hous_cm,
         li = li_a - li_cm,
         nw = nw_a - nw_cm,
         tot199 = tot199_a - tot199_cm,
         totnw = totnw_a - totnw_cm,
         univ = univ_a - univ_cm) %>%
  select(hous, li, nw, tot199, totnw, univ)

# Clean these up
ratios_ovr <- ratios_ovr %>% select(-stcty, -GEOID)
names(ratios_ovr) <- str_replace(names(ratios_ovr), "_a", "")
names(ratios_reg) <- str_replace(names(ratios_reg), "_a", "")
ratios_ovr <- ratios_ovr %>% select(-GEOID)
ratios_reg <- ratios_reg %>% select(-GEOID)

# Export
rm(list = setdiff(ls(), c("diffs_ovr",
                          "diffs_co",
                          "diffs_reg",
                          "ratios_ovr",
                          "ratios_co",
                          "ratios_reg")))
for (i in ls()){
  write_csv(get(i), here("process_data", paste0(i, ".csv")))
}
