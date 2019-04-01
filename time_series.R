library(tidyverse); library(here); library(extrafont)

merg_90 <- read_csv(here("final", "merg_90.csv")) %>%
  mutate_at(vars(geoid), as.character)
merg_00 <- read_csv(here("final", "merg_00.csv")) %>%
  mutate_at(vars(geoid), as.character)
merg_10 <- read_csv(here("final", "merg_10.csv")) %>%
  mutate_at(vars(geoid), as.character)
merg_17 <- read_csv(here("final", "merg_17.csv")) %>%
  mutate_at(vars(geoid), as.character)

ts <- bind_rows(merg_90, merg_00) %>%
  bind_rows(., merg_10) %>%
  bind_rows(., merg_17)

ts_cty <- ts %>% mutate(County = case_when(str_sub(geoid, 3, 5) == "005" ~ "Burlington",
                                           str_sub(geoid, 3, 5) == "007" ~ "Camden",
                                           str_sub(geoid, 3, 5) == "015" ~ "Gloucester",
                                           str_sub(geoid, 3, 5) == "021" ~ "Mercer",
                                           str_sub(geoid, 3, 5) == "017" ~ "Bucks",
                                           str_sub(geoid, 3, 5) == "029" ~ "Chester",
                                           str_sub(geoid, 3, 5) == "045" ~ "Delaware",
                                           str_sub(geoid, 3, 5) == "091" ~ "Montgomery",
                                           str_sub(geoid, 3, 5) == "101" ~ "Philadelphia")) %>%
  group_by(County, year) %>%
  summarize(hu = sum(hu, pop, na.rm = TRUE),
            mhi = weighted.mean(mhi, pop, na.rm = TRUE),
            mhv = weighted.mean(mhv, pop, na.rm = TRUE),
            own = weighted.mean(own, pop, na.rm = TRUE),
            pov199 = weighted.mean(pov199, pop, na.rm = TRUE),
            pov99 = weighted.mean(pov99, pop, na.rm = TRUE),
            rm = weighted.mean(rm, pop, na.rm = TRUE),
            em = weighted.mean(em, pop, na.rm = TRUE),
            unemp = weighted.mean(unemp, pop, na.rm = TRUE),
            vhu = weighted.mean(vhu, pop, na.rm = TRUE),
            pop = sum(pop, na.rm = TRUE))
ts_4cty <- ts %>% mutate(Subregion = case_when(str_sub(geoid, 3, 5) %in% c("005", "007", "015", "021") ~ "NJ Suburban Counties",
                                            str_sub(geoid, 3, 5) %in% c("017", "029", "045", "091") ~ "PA Suburban Counties",
                                            str_sub(geoid, 3, 5) == "101" ~ "Philadelphia")) %>%
  group_by(Subregion, year) %>%
  summarize(hu = sum(hu, pop, na.rm = TRUE),
            mhi = weighted.mean(mhi, pop, na.rm = TRUE),
            mhv = weighted.mean(mhv, pop, na.rm = TRUE),
            own = weighted.mean(own, pop, na.rm = TRUE),
            pov199 = weighted.mean(pov199, pop, na.rm = TRUE),
            pov99 = weighted.mean(pov99, pop, na.rm = TRUE),
            rm = weighted.mean(rm, pop, na.rm = TRUE),
            em = weighted.mean(em, pop, na.rm = TRUE),
            unemp = weighted.mean(unemp, pop, na.rm = TRUE),
            vhu = weighted.mean(vhu, pop, na.rm = TRUE),
            pop = sum(pop, na.rm = TRUE))
ts_reg <- ts %>%
  group_by(year) %>%
  summarize(hu = sum(hu, pop, na.rm = TRUE),
            mhi = weighted.mean(mhi, pop, na.rm = TRUE),
            mhv = weighted.mean(mhv, pop, na.rm = TRUE),
            own = weighted.mean(own, pop, na.rm = TRUE),
            pov199 = weighted.mean(pov199, pop, na.rm = TRUE),
            pov99 = weighted.mean(pov99, pop, na.rm = TRUE),
            rm = weighted.mean(rm, pop, na.rm = TRUE),
            em = weighted.mean(em, pop, na.rm = TRUE),
            unemp = weighted.mean(unemp, pop, na.rm = TRUE),
            vhu = weighted.mean(vhu, pop, na.rm = TRUE),
            pop = sum(pop, na.rm = TRUE),
            County = "Region")
ts_cty <- bind_rows(ts_cty, ts_reg)

# By Subregion
ggplot(ts_4cty, aes(x = year, y = pop)) + geom_line(aes(color = Subregion)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Change in population 1990-2017",
       x = "Year",
       y = "Total population (1000s)")
ggsave(here("figs", "d_pop_4cty.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_4cty, aes(x = year, y = hu)) + geom_line(aes(color = Subregion)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Change in housing units 1990-2017",
       x = "Year",
       y = "Total housing units (1000s)")
ggsave(here("figs", "d_hu_4cty.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_4cty, aes(x = year, y = mhi)) + geom_line(aes(color = Subregion)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Median household income 1990-2017",
       x = "Year",
       y = "Median household income ($1000s), 2017 dollars")
ggsave(here("figs", "d_mhi_4cty.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_4cty, aes(x = year, y = mhv)) + geom_line(aes(color = Subregion)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Median home value 1990-2017",
       x = "Year",
       y = "Median home value ($1000s), 2017 dollars")
ggsave(here("figs", "d_mhv_4cty.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_4cty, aes(x = year, y = own)) + geom_line(aes(color = Subregion)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Homeownership 1990-2017",
       x = "Year",
       y = "Percentage owner-occupied housing units")
ggsave(here("figs", "d_own_4cty.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_4cty, aes(x = year, y = pov199)) + geom_line(aes(color = Subregion)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Low-income residents 1990-2017",
       x = "Year",
       y = "Percentage residents with incomes below 199% FPL")
ggsave(here("figs", "d_pov199_4cty.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_4cty, aes(x = year, y = pov99)) + geom_line(aes(color = Subregion)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Residents in poverty 1990-2017",
       x = "Year",
       y = "Percentage residents with incomes below 100% FPL")
ggsave(here("figs", "d_pov99_4cty.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_4cty, aes(x = year, y = rm)) + geom_line(aes(color = Subregion)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Racial minority residents 1990-2017",
       x = "Year",
       y = "Percentage nonwhite residents")
ggsave(here("figs", "d_rm_4cty.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_4cty, aes(x = year, y = em)) + geom_line(aes(color = Subregion)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Ethnic minority residents 1990-2017",
       x = "Year",
       y = "Percentage ethnic minority residents")
ggsave(here("figs", "d_em_4cty.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_4cty, aes(x = year, y = unemp)) + geom_line(aes(color = Subregion)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Unemployment 1990-2017",
       x = "Year",
       y = "Percentage unemployed residents in the labor force")
ggsave(here("figs", "d_unemp_4cty.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_4cty, aes(x = year, y = vhu)) + geom_line(aes(color = Subregion)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Vacant housing units 1990-2017",
       x = "Year",
       y = "Percentage vacant housing units")
ggsave(here("figs", "d_vhu_4cty.png"), width = 7, height = 5, units = "in", dpi = 400)

# By County
ggplot(ts_cty %>% filter(County != "Region"), aes(x = year, y = pop)) + geom_line(aes(color = County)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  # scale_color_manual(values = c("red")) +
  labs(title = "Change in population 1990-2017",
       x = "Year",
       y = "Total population (1000s)")
ggsave(here("figs", "d_pop.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_cty %>% filter(County != "Region"), aes(x = year, y = hu)) + geom_line(aes(color = County)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Change in housing units 1990-2017",
       x = "Year",
       y = "Total housing units (1000s)")
ggsave(here("figs", "d_hu.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_cty, aes(x = year, y = mhi)) + geom_line(aes(color = County)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Median household income 1990-2017",
       x = "Year",
       y = "Median household income ($1000s), 2017 dollars")
ggsave(here("figs", "d_mhi.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_cty, aes(x = year, y = mhv)) + geom_line(aes(color = County)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Median home value 1990-2017",
       x = "Year",
       y = "Median home value ($1000s), 2017 dollars")
ggsave(here("figs", "d_mhv.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_cty, aes(x = year, y = own)) + geom_line(aes(color = County)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Homeownership 1990-2017",
       x = "Year",
       y = "Percentage owner-occupied housing units")
ggsave(here("figs", "d_own.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_cty, aes(x = year, y = pov199)) + geom_line(aes(color = County)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Low-income residents 1990-2017",
       x = "Year",
       y = "Percentage residents with incomes below 199% FPL")
ggsave(here("figs", "d_pov199.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_cty, aes(x = year, y = pov99)) + geom_line(aes(color = County)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Residents in poverty 1990-2017",
       x = "Year",
       y = "Percentage residents with incomes below 100% FPL")
ggsave(here("figs", "d_pov99.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_cty, aes(x = year, y = rm)) + geom_line(aes(color = County)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Racial minority residents 1990-2017",
       x = "Year",
       y = "Percentage nonwhite residents")
ggsave(here("figs", "d_rm.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_cty, aes(x = year, y = em)) + geom_line(aes(color = County)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Ethnic minority residents 1990-2017",
       x = "Year",
       y = "Percentage ethnic minority residents")
ggsave(here("figs", "d_em.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_cty, aes(x = year, y = unemp)) + geom_line(aes(color = County)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Unemployment 1990-2017",
       x = "Year",
       y = "Percentage unemployed residents in the labor force")
ggsave(here("figs", "d_unemp.png"), width = 7, height = 5, units = "in", dpi = 400)
ggplot(ts_cty, aes(x = year, y = vhu)) + geom_line(aes(color = County)) +
  scale_color_viridis_d(option = "inferno", alpha = 0.75) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Vacant housing units 1990-2017",
       x = "Year",
       y = "Percentage vacant housing units")
ggsave(here("figs", "d_vhu.png"), width = 7, height = 5, units = "in", dpi = 400)
