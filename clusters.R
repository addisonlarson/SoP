library(here); library(tidyverse); library(sf)
library(mclust); library(tidycensus)

# 1. Load data
merg_90 <- read_csv(here("final", "merg_90.csv")) %>%
  select(geoid, pov199) %>%
  rename(pov199_90 = pov199) %>%
  mutate_at(vars(geoid), as.character)
merg_17 <- read_csv(here("final", "merg_17.csv")) %>%
  select(geoid, pov199) %>%
  rename(baseline = pov199) %>%
  mutate_at(vars(geoid), as.character)

# 2. Compute and append MOE
slicer <- c("42045980000", "42017980000", "42101980800",
            "42101980300", "42101980500", "42101980400",
            "42101980900", "42101980700", "42101980600",
            "42101005000", "34021002400")
moe <- get_acs(geography = "tract",
               year = 2017,
               variables = c("B05010_001", # Denom for blpov
                             "B05010_002", # 0-99%
                             "B05010_010", # 100-199%
                             "B05010_018"), # 200+ %
               state = c(34, 42),
               output = "wide") %>%
  filter(str_sub(GEOID, 1, 5) %in% c("34005", "34007", "34015", "34021", "42017",
                                     "42029", "42045", "42091", "42101")) %>%
  mutate(pov199 = (B05010_002E + B05010_010E) / B05010_001E * 100) %>%
  filter(!(GEOID %in% slicer)) %>%
  rename_all(tolower) %>%
  mutate(moe_numerator = sqrt(b05010_002m^2 + b05010_010m^2),
         se_a = sqrt(b05010_002m^2 + b05010_010m^2),
         se_b = b05010_001m,
         p = (b05010_002e + b05010_010e) / b05010_001e,
         under_root = sqrt(se_a^2 + p^2 * se_b^2),
         interim_se = 100 * under_root / b05010_001e,
         final_se = ifelse(p == 1, se_a / b05010_001e * 100, interim_se),
         final_moe = final_se / 1.645) %>%
  select(geoid, final_moe)

dat <- inner_join(merg_90, merg_17) %>%
  inner_join(., moe) %>%
  mutate(change = baseline - pov199_90) %>%
  drop_na(.) %>%
  select(geoid, baseline, pov199_90, change, final_moe)

# 3. Identify clusters
# 3a. Clusters of estimates (not considering MOEs)
mod <- Mclust(dat %>% select(baseline, change))
summary(mod, parameters = TRUE)
plot(mod, what = "BIC"); plot(mod, what = "density", type = "persp")
group <- as.factor(mod$classification)

# 3b.
probs <- function(q){rnorm(1, mean = dat$baseline, sd = dat$final_moe)}
# sim = simulated; sel_sim = selected simulations; res = results
sim <- matrix(nrow = nrow(dat), ncol = 1000) # ncol = n simulations
sim_assign <- matrix(nrow = nrow(dat), ncol = 1000) # ncol = n simulations
sel_sim <- matrix(nrow = nrow(dat), ncol = 2)
res <- matrix(nrow = ncol(sim), ncol = 2)
for(i in 1:ncol(sim)){
  sim[,i] <- apply(dat, 1, probs) # simulated baseline
  sel_sim[,1] <- sim[,i]
  sel_sim[,2] <- (sim[,i] - dat$pov199_90) # simulated change
  mod <- Mclust(sel_sim, verbose = FALSE)
  sim_assign[,i] <- mod$classification
  res[i,1] <- mod$bic; res[i,2] <- which.max(mod$BIC)
}
write_csv(as.data.frame(sim_assign), here("final", "cluster_assignments.csv"))
write_csv(as.data.frame(res), here("final", "cluster_simulations.csv"))

# 3c. Analyze output from 1000 simulations of likely outcomes given MOE
mod$BIC # Sample output table to identify best model number
res <- read_csv(here("outputs", "cluster_simulations.csv")) %>%
  rename(BIC = V1, model_name = V2)
res %>% count(model_name) %>% arrange(desc(n))
# The original model selected (no. 52, VVI, 7)...is a fluke.
# VVV, 5 is most common result when accounting for MOE.

# 3d. Redo model according to results of simulation
cluster_df <- cluster_df %>%
  rename(baseline = li_d) %>%
  select(baseline, change)
mod <- Mclust(cluster_df, G = 5, modelNames = "VVV")
summary(mod, parameters = TRUE)
plot(mod, what = "density", type = "persp")

cluster <- cluster %>%
  mutate(group = as.factor(mod$classification)) %>%
  select(-moe_d)

# 3e. Pretty sure this will get you same answer
# Drop cluster_assignments columns where model != VVV,5 (122)
res_assignments <- read_csv(here("outputs", "cluster_assignments.csv")) %>%
  select(which(res$model_name == 122))

# 4. Export
st_write(cluster, here("outputs", "cluster.shp"))
