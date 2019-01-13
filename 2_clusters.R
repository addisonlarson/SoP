require(here); require(sf); require(tidyverse); require(haven); require(mclust)
# 1. Upload data
# 2. Compute 2017 baseline and 1990-2017 percentage change
# 3. Identify clusters
# 4. Export

# 1. Upload data
# Harmonized 1990 and 2000 census tract data
trct_a <- read_dta(here("data", "corresp_a.dta")) %>%
  rename(GEOID = trtid10)
trct_b <- read_dta(here("data", "corresp_b.dta")) %>%
  rename(GEOID = trtid10)
geo <- st_read(here("outputs", "./shp_c.shp")) %>%
  select(GEOID)
trct_a_h <- geo %>% left_join(., trct_a)
trct_b_h <- geo %>% left_join(., trct_b)
# 2017 data
cv_d <- st_read(here("outputs", "./cv_d.shp")) %>%
  pull(moe_li)
trct_d <- st_read(here("outputs", "./shp_d.shp")) %>%
  select(GEOID, li_d) %>%
  mutate(moe_d = cv_d)

# 2. Compute 2017 baseline and 1990-2017 percentage change
dat <- trct_a_h %>% st_set_geometry(NULL) %>% select(-univ_a)
cluster <- left_join(trct_d, dat) %>%
  mutate(change = (li_d - li_a) / li_a) %>%
  rename(baseline = li_d) %>%
  select(GEOID, baseline, change, moe_d) %>%
  filter_all(., all_vars(. != -1)) %>%
  slice(-1365) # Remove single Inf value
cluster_df <- cluster %>% st_set_geometry(NULL)

# 3. Identify clusters
# 3a. Clusters of estimates
mod <- Mclust(cluster_df[c(2,3)])
summary(mod, parameters = TRUE)
plot(mod, what = "BIC"); plot(mod, what = "density", type = "persp")
cluster$group <- as.factor(mod$classification)

# 3b. Simulate percentages using MOE
# Compute clusters across several iterations
# Determine best clustering scheme when accounting for MOE
# Once find best model, use Mclust to append group classification
# WARNING: This takes several hours to run
cluster_df <- left_join(trct_d, dat) %>%
  mutate(change = (li_d - li_a) / li_a) %>%
  filter_all(., all_vars(. != -1)) %>%
  slice(-1365) %>%
  st_set_geometry(NULL)

probs <- function(i){rnorm(1, mean = cluster_df[,2], sd = cluster_df[,3] / 1.645)}
# sim = simulated; sel_sim = selected simulations; res = results
sim <- matrix(nrow = 1364, ncol = 1000)
sim_assign <- matrix(nrow = 1364, ncol = 1000)
sel_sim <- matrix(nrow = 1364, ncol = 2)
res <- matrix(nrow = ncol(sim), ncol = 2)
for(i in 1:ncol(sim)){
  sim[,i] <- apply(cluster_df, 1, probs) # simulated baseline
  sel_sim[,1] <- sim[,i]
  sel_sim[,2] <- (sim[,i] - cluster_df[,4]) / cluster_df[,4] # simulated change
  mod <- Mclust(sel_sim, verbose = FALSE)
  sim_assign[,i] <- mod$classification
  res[i,1] <- mod$bic; res[i,2] <- which.max(mod$BIC)
}
write.csv(sim_assign, here("outputs", "cluster_assignments.csv"), row.names = FALSE)
write.csv(res, here("outputs", "cluster_simulations.csv"), row.names = FALSE)

# 3c. Analyze output from 1000 simulations of likely outcomes given MOE
mod$BIC # Sample output table to identify best model number
res <- read.csv(here("outputs", "cluster_simulations.csv")) %>%
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
res_assignments <- read.csv(here("outputs", "cluster_assignments.csv")) %>%
  select(which(res$model_name == 122))

# 4. Export
st_write(trct_a_h, here("outputs", "shp_a_h.shp"))
st_write(trct_b_h, here("outputs", "shp_b_h.shp"))
st_write(cluster, here("outputs", "cluster.shp"))
