require(here); require(sf); require(dplyr); require(RColorBrewer)
require(units); require(ggplot2); require(reshape2); require(magrittr)
theme_set(theme_minimal())
# Upload required files + reclass percentage low-income
dens <- c(-Inf, 0.1, 0.35, 0.6, Inf)
dens_labs <- c("0-10%","11-35%","36-60%", "61-100%")

shp_a <- st_read(here("outputs", "./shp_a.shp")) %>%
  mutate(li_cat_a = cut(li_a, breaks = dens,
                        labels = dens_labs))
shp_b <- st_read(here("outputs", "./shp_b.shp")) %>%
  mutate(li_cat_b = cut(li_b, breaks = dens,
                        labels = dens_labs))
shp_c <- st_read(here("outputs", "./shp_c.shp")) %>%
  mutate(li_cat_c = cut(li_c, breaks = dens,
                        labels = dens_labs))
shp_d <- st_read(here("outputs", "./shp_d.shp")) %>%
  mutate(li_cat_d = cut(li_d, breaks = dens,
                        labels = dens_labs))

# Define city center location
city_hall <- st_sfc(st_point(c(-75.163600, 39.953366)), crs = 4326) %>%
  st_transform(., 26918)

# Compute distances
cent_a <- st_centroid(shp_a)
dist_a <- st_distance(city_hall, cent_a) %>% set_units(., "mi") %>% as.numeric(.)
cent_b <- st_centroid(shp_b)
dist_b <- st_distance(city_hall, cent_b) %>% set_units(., "mi") %>% as.numeric(.)
cent_c <- st_centroid(shp_c)
dist_c <- st_distance(city_hall, cent_c) %>% set_units(., "mi") %>% as.numeric(.)
cent_d <- st_centroid(shp_d)
dist_d <- st_distance(city_hall, cent_d) %>% set_units(., "mi") %>% as.numeric(.)

# Low-income x distance + reclass distance
dens <- c(-Inf, 5, 15, 30, Inf)
dens_labs <- c("0-5","6-15","16-30", "31-50")

lixd_a <- tibble(pct_cat = shp_a$li_cat_a, pct = shp_a$li_a, dist = dist_a) %>%
  mutate(dist_cat = cut(dist, breaks = dens, labels = dens_labs))
lixd_b <- tibble(pct_cat = shp_b$li_cat_b, pct = shp_b$li_b, dist = dist_b) %>%
  mutate(dist_cat = cut(dist, breaks = dens, labels = dens_labs))
lixd_c <- tibble(pct_cat = shp_c$li_cat_c, pct = shp_c$li_c, dist = dist_c)%>%
  mutate(dist_cat = cut(dist, breaks = dens, labels = dens_labs))
lixd_d <- tibble(pct_cat = shp_d$li_cat_d, pct = shp_d$li_d, dist = dist_d)%>%
  mutate(dist_cat = cut(dist, breaks = dens, labels = dens_labs))

# Reshape data for plotting on single chart
lixd_a %<>% mutate(yr = 1990)
lixd_b %<>% mutate(yr = 2000)
lixd_c %<>% mutate(yr = 2010)
lixd_d %<>% mutate(yr = 2017)
lixd_full <- bind_rows(lixd_a, lixd_b) %>%
  bind_rows(., lixd_c) %>%
  bind_rows(., lixd_d) %>%
  mutate_at(vars(matches("yr")), as.factor)

# Reshape data for grouping by percentage low-income and distance from Center City
lixdr_a <- lixd_a %>% select(pct_cat, dist_cat) %>%
  count(pct_cat, dist_cat) %>%
  na.omit(.) %>%
  group_by(pct_cat) %>%
  mutate(freq = n /sum(n)) %>%
  select(-n)
lixdr_b <- lixd_b %>% select(pct_cat, dist_cat) %>%
  count(pct_cat, dist_cat) %>%
  na.omit(.) %>%
  group_by(pct_cat) %>%
  mutate(freq = n /sum(n)) %>%
  select(-n)
lixdr_c <- lixd_c %>% select(pct_cat, dist_cat) %>%
  count(pct_cat, dist_cat) %>%
  na.omit(.) %>%
  group_by(pct_cat) %>%
  mutate(freq = n /sum(n)) %>%
  select(-n)
lixdr_d <- lixd_d %>% select(pct_cat, dist_cat) %>%
  count(pct_cat, dist_cat) %>%
  na.omit(.) %>%
  group_by(pct_cat) %>%
  mutate(freq = n /sum(n)) %>%
  select(-n)

# Continuous
dot_blue <- brewer.pal(4, "Blues")[2]
line_blue <- brewer.pal(4, "Blues")[3]

ggplot(lixd_a, aes(x = dist, y = pct)) + geom_point(color = dot_blue) +
  geom_smooth(color = line_blue) +
  labs(title = "Tract Distance to City Hall by Percentage Low-Income Residents, 1990",
       x = "Distance to City Hall (mi.)",
       y = "Percentage Low-Income Residents")
ggsave(here("figures", "lixd_a.png"), dpi = 500)

ggplot(lixd_b, aes(x = dist, y = pct)) + geom_point(color = dot_blue) +
  geom_smooth(color = line_blue) +
  labs(title = "Tract Distance to City Hall by Percentage Low-Income Residents, 2000",
       x = "Distance to City Hall (mi.)",
       y = "Percentage Low-Income Residents")
ggsave(here("figures", "lixd_b.png"), dpi = 500)

ggplot(lixd_c, aes(x = dist, y = pct)) + geom_point(color = dot_blue) +
  geom_smooth(color = line_blue) +
  labs(title = "Tract Distance to City Hall by Percentage Low-Income Residents, 2010",
       x = "Distance to City Hall (mi.)",
       y = "Percentage Low-Income Residents")
ggsave(here("figures", "lixd_c.png"), dpi = 500)

ggplot(lixd_d, aes(x = dist, y = pct)) + geom_point(color = dot_blue) +
  geom_smooth(color = line_blue) +
  labs(title = "Tract Distance to City Hall by Percentage Low-Income Residents, 2017",
       x = "Distance to City Hall (mi.)",
       y = "Percentage Low-Income Residents")
ggsave(here("figures", "lixd_d.png"), dpi = 500)

ggplot(lixd_full, aes(x = dist, y = pct, color = yr)) + geom_smooth(se = FALSE) +
  scale_color_brewer(palette = "Blues") +
  labs(title = "Tract Distance to City Hall by Percentage Low-Income Residents",
       x = "Distance to City Hall (mi.)",
       y = "Percentage Low-Income Residents",
       color = "Year")
ggsave(here("figures", "lixd_full.png"), dpi = 500)

# Categorical
ggplot(lixdr_a, aes(fill = pct_cat, y = freq, x = dist_cat)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  labs(title = "Tract Distance to City Hall by Percentage Low-Income Residents, 1990",
       x = "Distance to City Hall (mi.)",
       y = "Percentage of Census Tracts by Income Group",
       fill = "Pct. Low-Income") +
  scale_fill_brewer(palette = "Blues")
ggsave(here("figures", "lixdr_a.png"), dpi = 500)

ggplot(lixdr_b, aes(fill = pct_cat, y = freq, x = dist_cat)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  labs(title = "Tract Distance to City Hall by Percentage Low-Income Residents, 2000",
       x = "Distance to City Hall (mi.)",
       y = "Percentage of Census Tracts by Income Group",
       fill = "Pct. Low-Income") +
  scale_fill_brewer(palette = "Blues")
ggsave(here("figures", "lixdr_b.png"), dpi = 500)

ggplot(lixdr_c, aes(fill = pct_cat, y = freq, x = dist_cat)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  labs(title = "Tract Distance to City Hall by Percentage Low-Income Residents, 2010",
       x = "Distance to City Hall (mi.)",
       y = "Percentage of Census Tracts by Income Group",
       fill = "Pct. Low-Income") +
  scale_fill_brewer(palette = "Blues")
ggsave(here("figures", "lixdr_c.png"), dpi = 500)

ggplot(lixdr_d, aes(fill = pct_cat, y = freq, x = dist_cat)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  labs(title = "Tract Distance to City Hall by Percentage Low-Income Residents, 2017",
       x = "Distance to City Hall (mi.)",
       y = "Percentage of Census Tracts by Income Group",
       fill = "Pct. Low-Income") +
  scale_fill_brewer(palette = "Blues")
ggsave(here("figures", "lixdr_d.png"), dpi = 500)
