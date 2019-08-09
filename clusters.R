library(here); library(tidyverse); library(sf)
library(mclust); library(tidycensus)

slicer <- c("42045980000", "42017980000", "42101980800",
            "42101980300", "42101980500", "42101980400",
            "42101980900", "42101980700", "42101980600",
            "42101005000", "34021002400")

# 1. Load data
merg_90 <- read_csv(here("final", "merg_90.csv")) %>%
  select(geoid, pov199) %>%
  rename(baseline = pov199) %>%
  mutate_at(vars(geoid), as.character)

# 2. Compute and append MOE
moe <- get_acs(geography = "tract",
               year = 2017,
               variables = c("B05010_001", # Denom for blpov
                             "B05010_002", # 0-99%
                             "B05010_010"), # 100-199%
               state = c(34, 42),
               output = "wide") %>%
  filter(str_sub(GEOID, 1, 5) %in% c("34005", "34007", "34015", "34021", "42017",
                                     "42029", "42045", "42091", "42101")) %>%
  mutate(pov199 = (B05010_002E + B05010_010E) / B05010_001E * 100) %>%
  filter(!(GEOID %in% slicer)) %>%
  mutate(se_a = sqrt(B05010_002M^2 + B05010_010M^2),
         se_b = B05010_001M,
         p = (B05010_002E + B05010_010E) / B05010_001E,
         under_root = sqrt(se_a^2 - p^2 * se_b^2),
         negative_root_exception = ifelse(!is.finite(under_root), sqrt(se_a^2 + p^2 * se_b^2), under_root),
         interim_moe = 100 * negative_root_exception / B05010_001E,
         final_moe = ifelse(p == 1, se_a / B05010_001E * 100, interim_moe),
         final_se = final_moe / 1.645) %>%
  select(GEOID, pov199, final_se)

dat <- inner_join(merg_90, moe, by = c("geoid" = "GEOID")) %>%
  mutate(change = (pov199 - baseline) / baseline * 100) %>%
  drop_na(.) %>%
  select(geoid, baseline, pov199, final_se, change) %>%
  filter(is.finite(change))

# 3. Identify clusters
# 3a. Clusters of estimates (not considering MOEs)
mod <- Mclust(dat %>% select(baseline, change))
summary(mod, parameters = TRUE)
plot(mod, what = "BIC"); plot(mod, what = "density")
group <- as.factor(mod$classification)

# 3b.
probs <- function(q){rnorm(1, mean = dat$pov199, sd = dat$final_se)}
# sim = simulated; sel_sim = selected simulations; res = results
sim <- matrix(nrow = nrow(dat), ncol = 1000) # ncol = n simulations
sim_assign <- matrix(nrow = nrow(dat), ncol = 1000) # ncol = n simulations
sel_sim <- matrix(nrow = nrow(dat), ncol = 2)
res <- matrix(nrow = ncol(sim), ncol = 2)
for(i in 1:ncol(sim)){
  sim[,i] <- apply(dat, 1, probs) # simulated 2017
  sel_sim[,1] <- dat$baseline
  sel_sim[,2] <- (sim[,i] - dat$baseline) / dat$baseline * 100 # simulated change
  mod <- Mclust(sel_sim, verbose = FALSE)
  sim_assign[,i] <- mod$classification
  res[i,1] <- mod$bic; res[i,2] <- which.max(mod$BIC)
}
write_csv(as.data.frame(sim_assign), here("final", "cluster_assignments.csv"))
write_csv(as.data.frame(res), here("final", "cluster_simulations.csv"))

# 3c. Analyze output from 1000 simulations of likely outcomes given MOE
# Sample output table to identify best model number
# Table shows  are possible models
mod$BIC
res <- read_csv(here("final", "cluster_simulations.csv")) %>%
  rename(BIC = V1, model_name = V2)
res %>% count(model_name) %>% arrange(desc(n))
# The original model selected (no. 86, VVE, 5)...is a fluke.
# no. 120, VVV, 3 is most common result when accounting for MOE.

# 3d. Re-run analysis
dat_2 <- dat %>%
  select(baseline, change)
mod <- Mclust(dat_2, G = 3, modelNames = "VVV")
summary(mod, parameters = TRUE)
plot(mod, what = "density")

dat <- dat %>%
  mutate(group = as.factor(mod$classification))

# 4. Export
write_csv(dat, here("final", "cluster.csv"))

ggplot(dat, aes(x = baseline, y = change)) +
  geom_point(aes(color = group))
