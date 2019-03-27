library(here); library(tidyverse); library(sf); library(tigris); library(corrplot)
options(stringsAsFactors = FALSE)
theme_set(theme_minimal())
options(tigris_use_cache = TRUE, tigris_class = "sf")
source("functions.R")

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
diffs_ovr <- diffs_ovr %>% select(-stcty)
names(diffs_ovr) <- str_replace(names(diffs_ovr), "_a", "")
names(diffs_reg) <- str_replace(names(diffs_reg), "_a", "")
diffs_co$GEOID <- a$GEOID
diffs_reg$GEOID <- a$GEOID

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
ratios_ovr <- ratios_ovr %>% select(-stcty)
names(ratios_ovr) <- str_replace(names(ratios_ovr), "_a", "")
names(ratios_reg) <- str_replace(names(ratios_reg), "_a", "")
ratios_co$GEOID <- a$GEOID
ratios_reg$GEOID <- a$GEOID

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

# `hous`: percentage change in median home value for the tract
# `tot199`: percentage change in low-income residents
# `univ`: percentage change in population
ratios_ovr %>%
  select(GEOID, hous, tot199, univ) %>%
  mutate(cty = str_sub(GEOID, 1, 5)) %>%
  group_by(cty) %>%
  summarize_if(is.numeric, funs(mean(.))) %>%
  write_csv(., here("outputs", "change_by_county.csv"))
s <- ratios_ovr %>%
  select(GEOID, hous, tot199, univ) %>%
  mutate_at(vars(GEOID), as.character) %>%
  mutate(cty = str_sub(GEOID, 1, 5)) %>%
  left_join(trct, .)
ratios_bycty <- split(s, s$cty)
for(i in 1:length(ratios_bycty)){
  cty <- names(ratios_bycty)[i]
  shp <- as_tibble((ratios_bycty)[i])
  st_write(shp, here("outputs", paste0("change", cty, ".shp")))
}

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

# Correlations
diffs_co <- diffs_co %>%
  select(GEOID, hous, li, nw, univ) %>%
  drop_na(.)
diffs_reg <- diffs_reg %>%
  select(GEOID, hous, li, nw, univ) %>%
  drop_na(.)
diffs_co_cor <- cor(diffs_co[2:5])
corrplot.mixed(diffs_co_cor)
diffs_reg_cor <- cor(diffs_reg[2:5])
corrplot.mixed(diffs_reg_cor)



left_join(trct, diffs_co) %>%
  st_write(., here("process_data", "abs_rel_co.shp"), delete_dsn = TRUE)
left_join(trct, diffs_reg) %>%
  st_write(., here("process_data", "abs_rel_reg.shp"), delete_dsn = TRUE)
left_join(trct, ratios_co) %>%
  st_write(., here("process_data", "ratio_rel_co.shp"), delete_dsn = TRUE)
left_join(trct, ratios_reg) %>%
  st_write(., here("process_data", "ratio_rel_reg.shp"), delete_dsn = TRUE)
