library(here); library(tidyverse); library(tidycensus); library(sf)
source("functions.R")
area <- c("34005", "34007", "34015", "34021",
          "42017", "42029", "42045", "42091", "42101")

# Percentage of low-income residents
# 2010: ACS 5-Year S1701 2008-2012 Midpoint
d_nj <- get_acs(geography = "tract", state = 34, output = "wide",
                variables = c("S1701_C01_001", "S1701_C01_038"),
                year = 2012)
d_pa <- get_acs(geography = "tract", state = 42, output = "wide",
                variables = c("S1701_C01_001", "S1701_C01_038"),
                year = 2012)
cv_c <- bind_rows(d_nj, d_pa) %>%
  mutate(stcty = str_sub(GEOID, 1, 5),
         li_c = S1701_C01_038E / S1701_C01_001E,
         moe_li_c = moe_prop(S1701_C01_038E, S1701_C01_001E, S1701_C01_038M, S1701_C01_001M),
         cv_li_c = (moe_li_c / 1.645) / li_c,
         cat_li_c = cut(cv_li_c, breaks = c(-Inf, 0.12, 0.4, Inf), labels = c("High","Medium","Low"))) %>%
  filter(stcty %in% area) %>%
  select(GEOID, ends_with("_c"))
# 2017: ACS 5-Year S1701
cv_d <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                 variables = c("S1701_C01_001", "S1701_C01_042")) %>%
  mutate(stcty = str_sub(GEOID, 1, 5),
         li_d = S1701_C01_042E / S1701_C01_001E,
         moe_li_d = moe_prop(S1701_C01_042E, S1701_C01_001E, S1701_C01_042M, S1701_C01_001M),
         cv_li_d = (moe_li_d / 1.645) / li_d,
         cat_li_d = cut(cv_li_d, breaks = c(-Inf, 0.12, 0.4, Inf), labels = c("High","Medium","Low"))) %>%
  filter(stcty %in% area) %>%
  select(GEOID, ends_with("_d"))
# Remove non-finite results
cv_c <- cv_c[is.finite(rowSums(cv_c[,2:4])),]
cv_d <- cv_d[is.finite(rowSums(cv_d[,2:4])),]
# Test for statistically significant increase
num_c <- cv_c$li_c
denom_c <- (cv_c$moe_li_c / 1.645) ^ 2
num_d <- cv_d$li_d
denom_d <- (cv_d$moe_li_d / 1.645) ^ 2
cv_d <- cv_d %>%
  mutate(z_1dir = (num_d - num_c) / sqrt(denom_d + denom_c),
         z_1dir_sig = ifelse(z_1dir > 1.645, "Yes", "No"))
# Export
write_csv_here(cv_c); write_csv_here(cv_d)
