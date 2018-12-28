require(here); require(sf); require(dplyr)
require(tidycensus); require(stringr)
options(stringsAsFactors = FALSE)

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

st_write(cv_d, here("outputs", "cv_d.shp"))
