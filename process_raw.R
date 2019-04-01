library(tidyverse); library(haven); library(tidycensus); library(here)
# Make sure to slice low-population tracts
slicer <- c("42045980000", "42017980000", "42101980800",
            "42101980300", "42101980500", "42101980400",
            "42101980900", "42101980700", "42101980600",
            "42101005000", "34021002400")

# Block-Standardized Data
bl <- read_csv(here("dev", "raw", "nhgis0011_ts_geog2010_tract.csv")) %>%
  mutate(geoid = paste0(str_sub(GISJOIN, 2, 3), str_sub(GISJOIN, 5, 7), str_sub(GISJOIN, 9, 14)),
         stcty = paste0(str_sub(GISJOIN, 2, 3), str_sub(GISJOIN, 5, 7))) %>%
  filter(stcty %in% c("34005", "34007", "34015", "34021", "42017", "42029", "42045", "42091", "42101")) %>%
  filter(!(geoid %in% slicer))
bl_90 <- bl %>%
  replace_na(list(CM1AA1990 = 0, CM1AB1990 = 0,
                  CM1AC1990 = 0, CM1AD1990 = 0,
                  CM1AE1990 = 0, CM1AF1990 = 0,
                  CM1AG1990 = 0)) %>%
  mutate(year = 1990,
         pop = CL8AA1990 / 1000,
         hu = CM7AA1990 / 1000,
         own = (CN1AA1990) / (CN1AA1990 + CN1AB1990) * 100,
         vhu = (CM7AA1990 - (CN1AA1990 + CN1AB1990)) / CM7AA1990 * 100,
         rm_u = CM1AA1990 + CM1AB1990 + CM1AC1990 + CM1AD1990 +
           CM1AE1990 + CM1AF1990 + CM1AG1990,
         rm = (rm_u - CM1AA1990) / rm_u * 100,
         em_u = CY8AA1990 + CY8AB1990 + CY8AC1990 + CY8AD1990 + CY8AE1990 + CY8AF1990,
         em = CY8AF1990 / em_u * 100) %>%
  select(geoid, year, pop, hu, own, vhu, rm, em)
bl_00 <- bl %>%
  replace_na(list(CM1AA2000 = 0, CM1AB2000 = 0,
                  CM1AC2000 = 0, CM1AD2000 = 0,
                  CM1AE2000 = 0, CM1AF2000 = 0,
                  CM1AG2000 = 0)) %>%
  mutate(year = 2000,
         pop = CL8AA2000 / 1000,
         hu = CM7AA2000 / 1000,
         own = (CN1AA2000) / (CN1AA2000 + CN1AB2000) * 100,
         vhu = (CM7AA2000 - (CN1AA2000 + CN1AB2000)) / CM7AA2000 * 100,
         rm_u = CM1AA2000 + CM1AB2000 + CM1AC2000 + CM1AD2000 +
           CM1AE2000 + CM1AF2000 + CM1AG2000,
         rm = (rm_u - CM1AA2000) / rm_u * 100,
         em_u = CY8AA2000 + CY8AB2000 + CY8AC2000 + CY8AD2000 + CY8AE2000 + CY8AF2000,
         em = CY8AF2000 / em_u * 100) %>%
  select(geoid, year, pop, hu, own, vhu, rm, em)
bl_10 <- bl %>%
  replace_na(list(CM1AA2010 = 0, CM1AB2010 = 0,
                  CM1AC2010 = 0, CM1AD2010 = 0,
                  CM1AE2010 = 0, CM1AF2010 = 0,
                  CM1AG2010 = 0)) %>%
  mutate(year = 2010,
         pop = CL8AA2010 / 1000,
         hu = CM7AA2010 / 1000,
         own = (CN1AA2010) / (CN1AA2010 + CN1AB2010) * 100,
         vhu = (CM7AA2010 - (CN1AA2010 + CN1AB2010)) / CM7AA2010 * 100,
         rm_u = CM1AA2010 + CM1AB2010 + CM1AC2010 + CM1AD2010 +
           CM1AE2010 + CM1AF2010 + CM1AG2010,
         rm = (rm_u - CM1AA2010) / rm_u * 100,
         em_u = CY8AA2010 + CY8AB2010 + CY8AC2010 + CY8AD2010 + CY8AE2010 + CY8AF2010,
         em = CY8AF2010 / em_u * 100) %>%
  select(geoid, year, pop, hu, own, vhu, rm, em)
write_csv(bl_90, here("interim", "bl_std_90.csv"))
write_csv(bl_00, here("interim", "bl_std_00.csv"))
write_csv(bl_10, here("interim", "bl_std_10.csv"))
# Input Data for LTDB
raw_90a <- read_csv(here("raw", "nhgis0011_ds120_1990_tract.csv"))
gr <- str_sub(raw_90a$GISJOIN, 2, 3)
o <- str_sub(raw_90a$GISJOIN, 5, 7)
ss <- str_sub(raw_90a$GISJOIN, 9, -1) %>%
  str_pad(., 6, side = "right", pad = "0")
gross <- paste0(gr, o, ss)
raw_90a <- raw_90a %>%
  rename(mhv = EST001) %>%
  mutate(year = 1990,
         geoid = gross) %>%
  select(geoid, year, mhv)
raw_90b <- read_csv(here("raw", "nhgis0011_ds123_1990_tract.csv"))
gr <- str_sub(raw_90b$GISJOIN, 2, 3)
o <- str_sub(raw_90b$GISJOIN, 5, 7)
ss <- str_sub(raw_90b$GISJOIN, 9, -1) %>%
  str_pad(., 6, side = "right", pad = "0")
gross <- paste0(gr, o, ss)
raw_90b <- raw_90b %>%
  mutate(unemp_u = E4I002 + E4I003 + E4I006 + E4I007,
         unemp = (E4I003 + E4I007) / unemp_u * 100,
         mhi = E4U001,
         pov_u = E1C001 + E1C002 + E1C003 + E1C004 + E1C005 +
           E1C006 + E1C007 + E1C008 + E1C009,
         pov199 = (pov_u - E1C009) / pov_u * 100,
         pov99 = (E1C001 + E1C002 + E1C003) / pov_u * 100,
         geoid = gross) %>%
  select(geoid, unemp, mhi, pov199, pov99)
raw_90c <- read_csv(here("raw", "nhgis0012_ds120_1990_tract.csv"))
gr <- str_sub(raw_90c$GISJOIN, 2, 3)
o <- str_sub(raw_90c$GISJOIN, 5, 7)
ss <- str_sub(raw_90c$GISJOIN, 9, -1) %>%
  str_pad(., 6, side = "right", pad = "0")
gross <- paste0(gr, o, ss)
raw_90c <- raw_90c %>%
  mutate(pop = ET1001,
         geoid = gross) %>%
  select(geoid, pop)
raw_00a <- read_csv(here("raw", "nhgis0011_ds151_2000_tract.csv")) %>%
  mutate(unemp_u = GLR001 + GLR002 + GLR003 + GLR004,
         unemp = (GLR002 + GLR004) / unemp_u * 100,
         mhi = GMY001,
         pov_u = GN8001 + GN8002 + GN8003 + GN8004 + GN8005 + GN8006 + GN8007 + GN8008 + GN8009,
         pov199 = (pov_u - GN8009) / pov_u * 100,
         pov99 = (GN8001 + GN8002 + GN8003) / pov_u * 100,
         mhv = GB7001,
         year = 2000,
         geoid = paste0(str_sub(GISJOIN, 2, 3), str_sub(GISJOIN, 5, 7), str_sub(GISJOIN, 9, 14))) %>%
  select(geoid, unemp, mhi, pov199, pov99, mhv, year)
raw_00b <- read_csv(here("raw", "nhgis0012_ds146_2000_tract.csv")) %>%
  mutate(geoid = paste0(str_sub(GISJOIN, 2, 3), str_sub(GISJOIN, 5, 7), str_sub(GISJOIN, 9, 14)),
         pop = FL5001) %>%
  select(geoid, pop)
# Merge, adjust for inflation, rescale dollar units
ltdb_90 <- left_join(raw_90a, raw_90b, by = "geoid") %>%
  left_join(., raw_90c, by = "geoid") %>%
  filter(str_sub(geoid, 1, 5) %in% c("34005", "34007", "34015", "34021",
                                     "42017", "42029", "42045", "42091", "42101")) %>%
  filter(!(geoid %in% slicer)) %>%
  mutate_at(vars(mhv), funs(. / 1000 * 1.84)) %>%
  mutate_at(vars(mhi), funs(. / 1000 * 1.84)) %>%
  mutate_at(vars(geoid), as.character)
ltdb_00 <- left_join(raw_00a, raw_00b, by  = "geoid") %>%
  filter(str_sub(geoid, 1, 5) %in% c("34005", "34007", "34015", "34021",
                                     "42017", "42029", "42045", "42091", "42101")) %>%
  filter(!(geoid %in% slicer)) %>%
  mutate_at(vars(mhv), funs(. / 1000 * 1.42)) %>%
  mutate_at(vars(mhi), funs(. / 1000 * 1.42)) %>%
  mutate_at(vars(geoid), as.character)
write_dta(ltdb_90, here("interim", "ltdb_90.dta"))
write_dta(ltdb_00, here("interim", "ltdb_00.dta"))

# USE LTDB TO HARMONIZE DATA WITH 2010 TRACT BOUNDARIES

ltdb_90_unemp <- read_dta(here("interim", "unemp_90.dta")) %>%
  select(trtid10, unemp) %>%
  rename(geoid = trtid10)
ltdb_90_mhi <- read_dta(here("interim", "mhi_90.dta")) %>%
  select(trtid10, mhi) %>%
  rename(geoid = trtid10)
ltdb_90_pov199 <- read_dta(here("interim", "pov199_90.dta")) %>%
  select(trtid10, pov199) %>%
  rename(geoid = trtid10)
ltdb_90_pov99 <- read_dta(here("interim", "pov99_90.dta")) %>%
  select(trtid10, pov99) %>%
  rename(geoid = trtid10)
ltdb_90_mhv <- read_dta(here("interim", "mhv_90.dta")) %>%
  select(trtid10, mhv) %>%
  rename(geoid = trtid10)
ltdb_90_res <- full_join(ltdb_90_unemp, ltdb_90_mhi) %>%
  full_join(., ltdb_90_pov199) %>%
  full_join(., ltdb_90_pov99) %>%
  full_join(., ltdb_90_mhv)
ltdb_00_unemp <- read_dta(here("interim", "unemp_00.dta")) %>%
  select(trtid10, unemp) %>%
  rename(geoid = trtid10)
ltdb_00_mhi <- read_dta(here("interim", "mhi_00.dta")) %>%
  select(trtid10, mhi) %>%
  rename(geoid = trtid10)
ltdb_00_pov199 <- read_dta(here("interim", "pov199_00.dta")) %>%
  select(trtid10, pov199) %>%
  rename(geoid = trtid10)
ltdb_00_pov99 <- read_dta(here("interim", "pov99_00.dta")) %>%
  select(trtid10, pov99) %>%
  rename(geoid = trtid10)
ltdb_00_mhv <- read_dta(here("interim", "mhv_00.dta")) %>%
  select(trtid10, mhv) %>%
  rename(geoid = trtid10)
ltdb_00_res <- full_join(ltdb_00_unemp, ltdb_00_mhi) %>%
  full_join(., ltdb_00_pov199) %>%
  full_join(., ltdb_00_pov99) %>%
  full_join(., ltdb_00_mhv)

merg_90 <- left_join(bl_90, ltdb_90_res)
merg_00 <- left_join(bl_00, ltdb_00_res)

acs_10_nj <- get_acs(geography = "tract",
                  year = 2010,
                  variables = c("B25077_001",
                                "B05010_001",
                                "B05010_002", # 0-99%
                                "B05010_010", # 100-199%
                                "B05010_018", # 200+ %
                                "B19013_001",
                                "B17005_004", # Males blpov in labor force
                                "B17005_009", # Males abpov in labor force
                                "B17005_015", # Females blpov in labor force
                                "B17005_020", # Females abpov in labor force
                                "B17005_006", # Males blpov in labor force unemp
                                "B17005_011", # Males abpov in labor force unemp
                                "B17005_017", # Females blpov in labor force
                                "B17005_022"), # Females abpov in labor force
                  state = 34,
                  output = "wide") %>%
  filter(str_sub(GEOID, 1, 5) %in% c("34005", "34007", "34015", "34021", "42017",
                                     "42029", "42045", "42091", "42101")) %>%
  mutate(pov199 = (B05010_001E - B05010_018E) / B05010_001E * 100,
         pov99 = B05010_002E / B05010_001E * 100,
         mhv = B25077_001E * 1.12 / 1000,
         mhi = B19013_001E * 1.12 / 1000,
         unemp = (B17005_006E + B17005_011E + B17005_017E + B17005_022E) /
           (B17005_004E + B17005_009E + B17005_015E + B17005_020E) * 100) %>%
  select(GEOID, pov199, pov99, mhv, mhi, unemp)
acs_10_pa <- get_acs(geography = "tract",
                  year = 2010,
                  variables = c("B25077_001",
                                "B05010_001",
                                "B05010_002", # 0-99%
                                "B05010_010", # 100-199%
                                "B05010_018", # 200+ %
                                "B19013_001",
                                "B17005_004", # Males blpov in labor force
                                "B17005_009", # Males abpov in labor force
                                "B17005_015", # Females blpov in labor force
                                "B17005_020", # Females abpov in labor force
                                "B17005_006", # Males blpov in labor force unemp
                                "B17005_011", # Males abpov in labor force unemp
                                "B17005_017", # Females blpov in labor force
                                "B17005_022"), # Females abpov in labor force
                  state = 42,
                  output = "wide") %>%
  filter(str_sub(GEOID, 1, 5) %in% c("34005", "34007", "34015", "34021", "42017",
                                     "42029", "42045", "42091", "42101")) %>%
  mutate(pov199 = (B05010_001E - B05010_018E) / B05010_001E * 100,
         pov99 = B05010_002E / B05010_001E * 100,
         mhv = B25077_001E * 1.12 / 1000,
         mhi = B19013_001E * 1.12 / 1000,
         unemp = (B17005_006E + B17005_011E + B17005_017E + B17005_022E) /
           (B17005_004E + B17005_009E + B17005_015E + B17005_020E) * 100) %>%
  select(GEOID, pov199, pov99, mhv, mhi, unemp)
acs_10 <- bind_rows(acs_10_pa, acs_10_nj)
merg_10 <- left_join(bl_10, acs_10, by = c("geoid" = "GEOID"))

acs_17 <- get_acs(geography = "tract",
                  variables = c("B01003_001", # Population
                                "B25002_001", # Total housing units *and* denom for vhu
                                "B25003_001", # Denom for own
                                "B25003_002", # Num for own
                                "B25077_001", # mhv
                                "B25002_003", # Num for vhu
                                "B02001_001", # Denom for rm
                                "B02001_002", # Num (sort of) for rm
                                "B03002_001", # Denom for em
                                "B03002_012", # Num for em
                                "B05010_001", # Denom for blpov
                                "B05010_002", # 0-99%
                                "B05010_010", # 100-199%
                                "B05010_018", # 200+ %
                                "B19013_001", # mhi
                                "B17005_004", # Males blpov in labor force
                                "B17005_009", # Males abpov in labor force
                                "B17005_015", # Females blpov in labor force
                                "B17005_020", # Females abpov in labor force
                                "B17005_006", # Males blpov in labor force unemp
                                "B17005_011", # Males abpov in labor force unemp
                                "B17005_017", # Females blpov in labor force
                                "B17005_022"), # Females abpov in labor force
                  state = c(34, 42),
                  output = "wide") %>%
  filter(str_sub(GEOID, 1, 5) %in% c("34005", "34007", "34015", "34021", "42017",
                                     "42029", "42045", "42091", "42101")) %>%
  mutate(pop = B01003_001E / 1000,
         hu = B25002_001E / 1000,
         own = B25003_002E / B25003_001E * 100,
         mhv = B25077_001E / 1000,
         vhu = B25002_003E / B25002_001E * 100,
         rm = (B02001_001E - B02001_002E) / B02001_001E * 100,
         em = B03002_012E / B03002_001E * 100,
         pov199 = (B05010_001E - B05010_018E) / B05010_001E * 100,
         pov99 = B05010_002E / B05010_001E * 100,
         mhi = B19013_001E / 1000,
         unemp = (B17005_006E + B17005_011E + B17005_017E + B17005_022E) /
           (B17005_004E + B17005_009E + B17005_015E + B17005_020E) * 100,
         year = 2017) %>%
  filter(!(GEOID %in% slicer)) %>%
  select(GEOID, pop, hu, own, mhv, vhu, rm, em, pov199, pov99, mhi, unemp, year) %>%
  rename_all(tolower)

write_csv(merg_90, here("final", "merg_90.csv"))
write_csv(merg_00, here("final", "merg_00.csv"))
write_csv(merg_10, here("final", "merg_10.csv"))
write_csv(acs_17, here("final", "merg_17.csv"))
