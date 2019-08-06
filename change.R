library(tidyverse); library(here); library(tigris); library(sf)
options(tigris_class = "sf")

stcty <- c("34005", "34007", "34015", "34021", "42017",
           "42029", "42045", "42091", "42101")

merg_90 <- read_csv(here("final", "merg_90.csv")) %>%
  mutate_at(vars(geoid), as.character)
merg_17 <- read_csv(here("final", "merg_17.csv")) %>%
  mutate_at(vars(geoid), as.character)

change <- bind_rows(merg_90, merg_17) %>%
  group_by(geoid) %>%
  summarize(pop_d = pop[2] - pop[1],
            hu_d = hu[2] - hu[1],
            own_d = own[2] - own[1],
            vhu_d = vhu[2] - vhu[1],
            rm_d = rm[2] - rm[1],
            em_d = em[2] - em[1],
            unemp_d = unemp[2] - unemp[1],
            mhi_d = mhi[2] - mhi[1],
            pov199_d = pov199[2] - pov199[1],
            pov99_d = pov99[2] - pov99[1],
            mhv_d = mhv[2] - mhv[1])

res <- NULL
for (i in 1:length(stcty)){
  indiv <- tracts(state = str_sub(stcty, 1, 2)[i],
                  county = str_sub(stcty, 3, 5)[i]) %>%
    select(GEOID)
  res <- rbind(res, indiv)
}

change <- inner_join(res, change, by = c("GEOID" = "geoid")) %>%
  st_transform(., 26918) %>%
  mutate_if(is.numeric, round, 3)

st_write(change, here("final", "change.shp"), delete_dsn = TRUE)
