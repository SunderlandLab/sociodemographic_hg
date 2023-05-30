## Purpose: Calculate US Census estimate + margin of error (MOE).
# date created: 2/1/23
# date updated: 2/10/23

## ==================================================================================================================================================
## Load data - default = 2010 data.
## ------------
# FISH
fish_df00 <- LoadFISH()
fish_df0  <- LoadEJdf(fish_df00)

fish_df0  <- fish_df0 %>% group_by(GISJOIN) %>% mutate(FISH = sum(AIAN, ASIAN, HIPI, MULTI))

# --------------------------------------------------------------------------------------------------------------------
# INCOME
income_df00 <- LoadINCOME(nhgisYR = nhgis2010_bg, nhgiscode_est = nhgis2010_EST, nhgiscode_moe = nhgis2010_MOE)
income_df0  <- LoadEJdf(income_df00)

income_df0  <- income_df0 %>% group_by(GISJOIN) %>% mutate(INCOME20 = sum(L10k, L15k, L20k))

# --------------------------------------------------------------------------------------------------------------------
# POVERTY
poverty_df00 <- LoadPOVERTY()
poverty_df0  <- LoadEJdf(poverty_df00)

poverty_df0  <- poverty_df0 %>% group_by(GISJOIN) %>%
  mutate(POVERTY200 = sum(R0.50, R1.00, R1.25, R1.50, R1.85, R2.00))

# --------------------------------------------------------------------------------------------------------------------
# NOENG
noeng_df00 <- LoadNOENG()
noeng_df0  <- LoadEJdf(noeng_df00)

noeng_df0  <- noeng_df0 %>% group_by(GISJOIN) %>% 
  mutate(NOENG = sum(Sp, Euro, Azn, Other))

# --------------------------------------------------------------------------------------------------------------------
# NOHISCH
nohisch_df00 <- LoadNOHISCH()
nohisch_df0  <- LoadEJdf(nohisch_df00)

nohisch_df0  <- nohisch_df0 %>% group_by(GISJOIN) %>% 
  mutate(NOHISCH = sum(edu1, edu2, edu3, edu4, edu5,
                     edu6, edu7, edu8, edu9,
                     edu10, edu11, edu12, edu13, 
                     edu14, edu15, edu16))

## ==================================================================================================================================================
## Calculate MOE
# ----------------
# FISH
fish_df <- CalcMOE(fish_df0, 'FISH', 'RACE_Tot')
# head(fish_df)
# summary(fish_df)

income_df <- CalcMOE(income_df0, 'INCOME20', 'INCOME_Tot')
# head(income_df)
# summary(income_df)

poverty_df <- CalcMOE(poverty_df0, 'POVERTY200', 'POVERTY_Tot')
# head(poverty_df)
# summary(poverty_df)

noeng_df <- CalcMOE(noeng_df0, 'NOENG', 'NOENG_Tot')
# head(noeng_df)
# summary(noeng_df)

nohisch_df <- CalcMOE(nohisch_df0, 'NOHISCH', 'NOHISCH_Tot')
# head(nohisch_df)
# summary(nohisch_df)
