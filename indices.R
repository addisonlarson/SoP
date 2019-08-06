library(tidyverse); library(here)

merg_90 <- read_csv(here("final", "merg_90.csv")) %>%
  mutate_at(vars(geoid), as.character)
merg_00 <- read_csv(here("final", "merg_00.csv")) %>%
  mutate_at(vars(geoid), as.character)
merg_10 <- read_csv(here("final", "merg_10.csv")) %>%
  mutate_at(vars(geoid), as.character)
merg_17 <- read_csv(here("final", "merg_17.csv")) %>%
  mutate_at(vars(geoid), as.character)

sum <- function(i, ..., na.rm = TRUE) {
  base::sum(i, ..., na.rm = na.rm)
}

ts <- bind_rows(merg_90, merg_00) %>%
  bind_rows(., merg_10) %>%
  bind_rows(., merg_17) %>%
  mutate_at(vars(pop), funs(. * 1000)) %>%
  mutate(rm_cnt = rm * pop / 100,
         nonrm_cnt = pop - rm_cnt,
         em_cnt = em * pop / 100,
         nonem_cnt = pop - em_cnt,
         pov199_cnt = pov199 * pop / 100,
         nonpov199_cnt = pop - pov199_cnt,
         pov99_cnt = pov99 * pop / 100,
         nonpov99_cnt = pop - pov99_cnt,
         unemp_cnt = unemp * pop / 100,
         nonunemp_cnt = pop - unemp_cnt)
ts_reg <- ts %>% group_by(year) %>%
  summarize(rm_denom = sum(rm_cnt),
            nonrm_denom = sum(nonrm_cnt),
            em_denom = sum(em_cnt),
            nonem_denom = sum(nonem_cnt),
            pov199_denom = sum(pov199_cnt),
            nonpov199_denom = sum(nonpov199_cnt),
            pov99_denom = sum(pov99_cnt),
            nonpov99_denom = sum(nonpov99_cnt),
            unemp_denom = sum(unemp_cnt),
            nonunemp_denom = sum(nonunemp_cnt),
            pop_denom = sum(pop),
            mhi_denom = mean(mhi, na.rm = TRUE))
rm_idx <- left_join(ts, ts_reg) %>%
  mutate(comp = abs(nonrm_cnt / nonrm_denom - rm_cnt / rm_denom)) %>%
  group_by(year) %>%
  summarize(idx = round((sum(comp) / 2), 3))
em_idx <- left_join(ts, ts_reg) %>%
  mutate(comp = abs(nonem_cnt / nonem_denom - em_cnt / em_denom)) %>%
  group_by(year) %>%
  summarize(idx = round((sum(comp) / 2), 3))
pov199_idx <- left_join(ts, ts_reg) %>%
  mutate(comp = abs(nonpov199_cnt / nonpov199_denom - pov199_cnt / pov199_denom)) %>%
  group_by(year) %>%
  summarize(idx = round((sum(comp) / 2), 3))
pov99_idx <- left_join(ts, ts_reg) %>%
  mutate(comp = abs(nonpov99_cnt / nonpov99_denom - pov99_cnt / pov99_denom)) %>%
  group_by(year) %>%
  summarize(idx = round((sum(comp) / 2), 3))
unemp_idx <- left_join(ts, ts_reg) %>%
  mutate(comp = abs(nonunemp_cnt / nonunemp_denom - unemp_cnt / unemp_denom)) %>%
  group_by(year) %>%
  summarize(idx = round((sum(comp) / 2), 3))
mhi_idx <- left_join(ts, ts_reg) %>%
  mutate(pop_share = pop / pop_denom,
         mhi_share = mhi / mhi_denom,
         lnmhi_share = log(mhi_share)) %>%
  group_by(year) %>%
  summarize(idx = round((sum(pop_share * mhi_share * lnmhi_share)), 3))

# Export
write_csv(rm_idx, here("final", "racial_dissim_idx.csv"))
write_csv(em_idx, here("final", "ethnic_dissim_idx.csv"))
write_csv(pov199_idx, here("final", "low_income_dissim_idx.csv"))
write_csv(pov99_idx, here("final", "poverty_dissim_idx.csv"))
write_csv(unemp_idx, here("final", "unemployment_dissim_idx.csv"))
write_csv(mhi_idx, here("final", "income_theil_idx.csv"))
