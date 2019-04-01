library(here); library(sf); library(tidyverse)
library(tigris); library(units); library(extrafont)
options(tigris_class = "sf")
stcty <- c("34005", "34007", "34015", "34021", "42017",
           "42029", "42045", "42091", "42101")

dens <- c(-Inf, 10, 35, 60, Inf)
dens_labs <- c("0-10%", "11-35%", "36-60%", "61-100%")

merg_90 <- read_csv(here("final", "merg_90.csv")) %>%
  mutate_at(vars(geoid), as.character) %>%
  mutate(li_cat = cut(pov199, breaks = dens, labels = dens_labs))
merg_00 <- read_csv(here("final", "merg_00.csv")) %>%
  mutate_at(vars(geoid), as.character) %>%
  mutate(li_cat = cut(pov199, breaks = dens, labels = dens_labs))
merg_10 <- read_csv(here("final", "merg_10.csv")) %>%
  mutate_at(vars(geoid), as.character) %>%
  mutate(li_cat = cut(pov199, breaks = dens, labels = dens_labs))
merg_17 <- read_csv(here("final", "merg_17.csv")) %>%
  mutate_at(vars(geoid), as.character) %>%
  mutate(li_cat = cut(pov199, breaks = dens, labels = dens_labs))

res <- NULL
for (i in 1:length(stcty)){
  indiv <- tracts(state = str_sub(stcty, 1, 2)[i],
                  county = str_sub(stcty, 3, 5)[i]) %>%
    select(GEOID)
  res <- rbind(res, indiv)
}
res <- res %>% st_transform(26918)

# Define city center location
city_hall <- st_sfc(st_point(c(-75.163600, 39.953366)), crs = 4326) %>%
  st_transform(., 26918)

# Compute distances
cent <- st_centroid(res)
res$dist <- st_distance(city_hall, cent) %>% set_units(., "mi") %>% as.numeric(.)

# Low-income x distance + reclass distance
dens <- c(-Inf, 5, 15, 30, Inf)
dens_labs <- c("0-5", "6-15", "16-30", "31-50")

res_90 <- inner_join(res, merg_90, by = c("GEOID" = "geoid")) %>%
  mutate(dist_cat = cut(dist, breaks = dens, labels = dens_labs)) %>%
  st_set_geometry(NULL) %>%
  group_by(dist_cat, li_cat) %>%
  tally(.) %>%
  drop_na(.) %>%
  group_by(li_cat) %>%
  mutate(freq = n / sum(n))
res_00 <- inner_join(res, merg_00, by = c("GEOID" = "geoid")) %>%
  mutate(dist_cat = cut(dist, breaks = dens, labels = dens_labs)) %>%
  st_set_geometry(NULL) %>%
  group_by(dist_cat, li_cat) %>%
  tally(.) %>%
  drop_na(.) %>%
  group_by(li_cat) %>%
  mutate(freq = n / sum(n))
res_10 <- inner_join(res, merg_10, by = c("GEOID" = "geoid")) %>%
  mutate(dist_cat = cut(dist, breaks = dens, labels = dens_labs)) %>%
  st_set_geometry(NULL) %>%
  group_by(dist_cat, li_cat) %>%
  tally(.) %>%
  drop_na(.) %>%
  group_by(li_cat) %>%
  mutate(freq = n / sum(n))
res_17 <- inner_join(res, merg_17, by = c("GEOID" = "geoid")) %>%
  mutate(dist_cat = cut(dist, breaks = dens, labels = dens_labs)) %>%
  st_set_geometry(NULL) %>%
  group_by(dist_cat, li_cat) %>%
  tally(.) %>%
  drop_na(.) %>%
  group_by(li_cat) %>%
  mutate(freq = n / sum(n))

ggplot(res_90, aes(fill = li_cat, y = freq, x = dist_cat)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Tract distance to City Hall by percentage low-income residents, 1990",
       x = "Distance to City Hall (mi.)",
       y = "Percentage of census tracts by income group",
       fill = "Pct. low-income") +
  scale_fill_manual(values = c("#F5932F", "#E36A30", "#BA4649", "#962859"))
ggsave(here("figs", "lixdr_90.png"), width = 7, height = 5, units = "in", dpi = 400)

ggplot(res_00, aes(fill = li_cat, y = freq, x = dist_cat)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Tract distance to City Hall by percentage low-income residents, 2000",
       x = "Distance to City Hall (mi.)",
       y = "Percentage of census tracts by income group",
       fill = "Pct. low-income") +
  scale_fill_manual(values = c("#F5932F", "#E36A30", "#BA4649", "#962859"))
ggsave(here("figs", "lixdr_00.png"), width = 7, height = 5, units = "in", dpi = 400)

ggplot(res_10, aes(fill = li_cat, y = freq, x = dist_cat)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Tract distance to City Hall by percentage low-income residents, 2010",
       x = "Distance to City Hall (mi.)",
       y = "Percentage of census tracts by income group",
       fill = "Pct. low-income") +
  scale_fill_manual(values = c("#F5932F", "#E36A30", "#BA4649", "#962859"))
ggsave(here("figs", "lixdr_10.png"), width = 7, height = 5, units = "in", dpi = 400)

ggplot(res_17, aes(fill = li_cat, y = freq, x = dist_cat)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(text = element_text(family = "Segoe UI", color = "#666666")) +
  labs(title = "Tract distance to City Hall by percentage low-income residents, 2017",
       x = "Distance to City Hall (mi.)",
       y = "Percentage of census tracts by income group",
       fill = "Pct. low-income") +
  scale_fill_manual(values = c("#F5932F", "#E36A30", "#BA4649", "#962859"))
ggsave(here("figs", "lixdr_17.png"), width = 7, height = 5, units = "in", dpi = 400)
