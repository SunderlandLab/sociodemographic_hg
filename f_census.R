## Functions to load US Census estimate & MOE values.
# date created: 2/1/23
# date updated: 2/21/23
# ====================================================================================================================
## Specific to ea variable.
LoadBLACK <- function(nhgisYR = nhgis2010_bg, nhgiscode_est = nhgis2010_EST, nhgiscode_moe = nhgis2010_MOE) {
  ej_df <- data.frame(
    # Census BG
    GISJOIN = nhgisYR$GISJOIN,
    # Total
    RACE_Tot     = nhgisYR[[paste0(nhgiscode_est$raceC, '001')]],
    RACE_Tot.moe = nhgisYR[[paste0(nhgiscode_moe$raceC, '001')]] * (1.960/1.645), # convert to 95% CI
    # Central Estimate
    BLACK  = nhgisYR[[paste0(nhgiscode_est$raceC, '003')]],
    # MOE
    BLACK.moe  = nhgisYR[[paste0(nhgiscode_moe$raceC, '003')]] * (1.960/1.645)) # convert to 95% CI
  
  return(ej_df)
}

# ----------------------------------------------------------------------------------------
LoadFISH <- function(nhgisYR = nhgis2010_bg, nhgiscode_est = nhgis2010_EST, nhgiscode_moe = nhgis2010_MOE) {
  ej_df <- data.frame(
    # Census BG
    GISJOIN = nhgisYR$GISJOIN,
    # Total
    RACE_Tot     = nhgisYR[[paste0(nhgiscode_est$raceC, '001')]],
    RACE_Tot.moe = nhgisYR[[paste0(nhgiscode_moe$raceC, '001')]] * (1.960/1.645), # convert to 95% CI
    # Central Estimate
    AIAN  = nhgisYR[[paste0(nhgiscode_est$raceC, '004')]],
    ASIAN = nhgisYR[[paste0(nhgiscode_est$raceC, '005')]],
    HIPI  = nhgisYR[[paste0(nhgiscode_est$raceC, '006')]],
    OTHER = nhgisYR[[paste0(nhgiscode_est$raceC, '007')]],
    MULTI = nhgisYR[[paste0(nhgiscode_est$raceC, '008')]],
    # MOE
    AIAN.moe  = nhgisYR[[paste0(nhgiscode_moe$raceC, '004')]] * (1.960/1.645), # convert to 95% CI
    ASIAN.moe = nhgisYR[[paste0(nhgiscode_moe$raceC, '005')]] * (1.960/1.645), # convert to 95% CI
    HIPI.moe  = nhgisYR[[paste0(nhgiscode_moe$raceC, '006')]] * (1.960/1.645), # convert to 95% CI
    OTHER.moe = nhgisYR[[paste0(nhgiscode_moe$raceC, '007')]] * (1.960/1.645), # convert to 95% CI
    MULTI.moe = nhgisYR[[paste0(nhgiscode_moe$raceC, '008')]] * (1.960/1.645)) # convert to 95% CI
  
  return(ej_df)
}

# ----------------------------------------------------------------------------------------
LoadINCOME <- function(nhgisYR = nhgis2010_bg, nhgiscode_est = nhgis2010_EST, nhgiscode_moe = nhgis2010_MOE) {
  ej_df <- data.frame(
    # Census BG
    GISJOIN = nhgisYR$GISJOIN,
    # Total
    INCOME_Tot     = nhgisYR[[paste0(nhgiscode_est$income20C, '001')]],
    INCOME_Tot.moe = nhgisYR[[paste0(nhgiscode_moe$income20C, '001')]] * (1.960/1.645), # convert to 95% CI
    # Central Estimate
    L10k = nhgisYR[[paste0(nhgiscode_est$income20C, '002')]],
    L15k = nhgisYR[[paste0(nhgiscode_est$income20C, '003')]],
    L20k = nhgisYR[[paste0(nhgiscode_est$income20C, '004')]],
    # MOE
    L10k.moe = nhgisYR[[paste0(nhgiscode_moe$income20C, '002')]] * (1.960/1.645), # convert to 95% CI
    L15k.moe = nhgisYR[[paste0(nhgiscode_moe$income20C, '003')]] * (1.960/1.645), # convert to 95% CI
    L20k.moe = nhgisYR[[paste0(nhgiscode_moe$income20C, '004')]] * (1.960/1.645)) # convert to 95% CI
  
  return(ej_df)
}

# ----------------------------------------------------------------------------------------
LoadPOVERTY <- function(nhgisYR = nhgis2010_bg, nhgiscode_est = nhgis2010_EST, nhgiscode_moe = nhgis2010_MOE) {
  ej_df <- data.frame(
    # Census BG
    GISJOIN = nhgisYR$GISJOIN,
    # Total
    POVERTY_Tot     = nhgisYR[[paste0(nhgiscode_est$povC, '001')]],
    POVERTY_Tot.moe = nhgisYR[[paste0(nhgiscode_moe$povC, '001')]] * (1.960/1.645), # convert to 95% CI
    # Central Estimate
    R0.50 = nhgisYR[[paste0(nhgiscode_est$povC, '002')]],
    R1.00 = nhgisYR[[paste0(nhgiscode_est$povC, '003')]],
    R1.25 = nhgisYR[[paste0(nhgiscode_est$povC, '004')]],
    R1.50 = nhgisYR[[paste0(nhgiscode_est$povC, '005')]],
    R1.85 = nhgisYR[[paste0(nhgiscode_est$povC, '006')]],
    R2.00 = nhgisYR[[paste0(nhgiscode_est$povC, '007')]],
    # MOE
    R0.50.moe = nhgisYR[[paste0(nhgiscode_moe$povC, '002')]] * (1.960/1.645), # convert to 95% CI
    R1.00.moe = nhgisYR[[paste0(nhgiscode_moe$povC, '003')]] * (1.960/1.645), # convert to 95% CI
    R1.25.moe = nhgisYR[[paste0(nhgiscode_moe$povC, '004')]] * (1.960/1.645), # convert to 95% CI
    R1.50.moe = nhgisYR[[paste0(nhgiscode_moe$povC, '005')]] * (1.960/1.645), # convert to 95% CI
    R1.85.moe = nhgisYR[[paste0(nhgiscode_moe$povC, '006')]] * (1.960/1.645), # convert to 95% CI
    R2.00.moe = nhgisYR[[paste0(nhgiscode_moe$povC, '007')]] * (1.960/1.645)) # convert to 95% CI
  
  return(ej_df)
}

# ----------------------------------------------------------------------------------------
LoadNOENG <- function(nhgisYR = nhgis2010_bg, nhgiscode_est = nhgis2010_EST, nhgiscode_moe = nhgis2010_MOE) {
  ej_df <- data.frame(
    # Census BG
    GISJOIN = nhgisYR$GISJOIN,
    # Total
    NOENG_Tot     = nhgisYR[[paste0(nhgiscode_est$engC, '001')]],
    NOENG_Tot.moe = nhgisYR[[paste0(nhgiscode_moe$engC, '001')]] * (1.960/1.645), # convert to 95% CI
    # Central Estimate
    Sp    = nhgisYR[[paste0(nhgiscode_est$engC, '004')]],
    Euro  = nhgisYR[[paste0(nhgiscode_est$engC, '007')]],
    Azn   = nhgisYR[[paste0(nhgiscode_est$engC, '010')]],
    Other = nhgisYR[[paste0(nhgiscode_est$engC, '013')]],
    # MOE
    Sp.moe    = nhgisYR[[paste0(nhgiscode_moe$engC, '004')]] * (1.960/1.645), # convert to 95% CI
    Euro.moe  = nhgisYR[[paste0(nhgiscode_moe$engC, '007')]] * (1.960/1.645), # convert to 95% CI
    Azn.moe   = nhgisYR[[paste0(nhgiscode_moe$engC, '010')]] * (1.960/1.645), # convert to 95% CI
    Other.moe = nhgisYR[[paste0(nhgiscode_moe$engC, '013')]] * (1.960/1.645)) # convert to 95% CI
  
  return(ej_df)
}

# ----------------------------------------------------------------------------------------
LoadNOHISCH <- function(nhgisYR = nhgis2010_bg, nhgiscode_est = nhgis2010_EST, nhgiscode_moe = nhgis2010_MOE) {
  # if 2020
  if (nhgiscode_est$eduC == "AMRZE" | nhgiscode_est$eduC == "AMRZM") {
    ej_df <- data.frame(
      GISJOIN = nhgisYR$GISJOIN,
      # Total
      NOHISCH_Tot     = nhgisYR[[paste0(nhgiscode_est$eduC, '001')]],
      NOHISCH_Tot.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '001')]] * (1.960/1.645), # convert to 95% CI
      # Central Estimate
      edu1 = nhgisYR[[paste0(nhgiscode_est$eduC, '002')]],
      edu2 = nhgisYR[[paste0(nhgiscode_est$eduC, '003')]],
      edu3 = nhgisYR[[paste0(nhgiscode_est$eduC, '004')]],
      
      edu4 = nhgisYR[[paste0(nhgiscode_est$eduC, '005')]],
      edu5 = nhgisYR[[paste0(nhgiscode_est$eduC, '006')]],
      edu6 = nhgisYR[[paste0(nhgiscode_est$eduC, '007')]],
      edu7 = nhgisYR[[paste0(nhgiscode_est$eduC, '008')]],
      edu8 = nhgisYR[[paste0(nhgiscode_est$eduC, '009')]],
      
      edu9  = nhgisYR[[paste0(nhgiscode_est$eduC, '010')]],
      edu10 = nhgisYR[[paste0(nhgiscode_est$eduC, '011')]],
      edu11 = nhgisYR[[paste0(nhgiscode_est$eduC, '012')]],
      
      edu12 = nhgisYR[[paste0(nhgiscode_est$eduC, '013')]],
      edu13 = nhgisYR[[paste0(nhgiscode_est$eduC, '014')]],
      edu14 = nhgisYR[[paste0(nhgiscode_est$eduC, '015')]],
      edu15 = nhgisYR[[paste0(nhgiscode_est$eduC, '016')]],
      
      # MOE
      edu1.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '002')]] * (1.960/1.645), # convert to 95% CI
      edu2.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '003')]] * (1.960/1.645), # convert to 95% CI
      edu3.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '004')]] * (1.960/1.645), # convert to 95% CI
      
      edu4.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '005')]] * (1.960/1.645), # convert to 95% CI
      edu5.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '006')]] * (1.960/1.645), # convert to 95% CI
      edu6.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '007')]] * (1.960/1.645), # convert to 95% CI
      edu7.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '008')]] * (1.960/1.645), # convert to 95% CI
      edu8.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '009')]] * (1.960/1.645), # convert to 95% CI
      
      edu9.moe  = nhgisYR[[paste0(nhgiscode_moe$eduC, '010')]] * (1.960/1.645), # convert to 95% CI
      edu10.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '011')]] * (1.960/1.645), # convert to 95% CI
      edu11.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '012')]] * (1.960/1.645), # convert to 95% CI
      
      edu12.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '013')]] * (1.960/1.645), # convert to 95% CI
      edu13.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '014')]] * (1.960/1.645), # convert to 95% CI
      edu14.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '015')]] * (1.960/1.645), # convert to 95% CI
      edu15.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '016')]] * (1.960/1.645)  # convert to 95% CI
    )
    # if 2010
  } else{
    # Census BG
    ej_df <- data.frame(
      GISJOIN = nhgisYR$GISJOIN,
      # Total
      NOHISCH_Tot     = nhgisYR[[paste0(nhgiscode_est$eduC, '001')]],
      NOHISCH_Tot.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '001')]] * (1.960/1.645), # convert to 95% CI
      # Central Estimate
      edu1 = nhgisYR[[paste0(nhgiscode_est$eduC, '003')]],
      edu2 = nhgisYR[[paste0(nhgiscode_est$eduC, '004')]],
      edu3 = nhgisYR[[paste0(nhgiscode_est$eduC, '005')]],
      edu4 = nhgisYR[[paste0(nhgiscode_est$eduC, '006')]],
      edu5 = nhgisYR[[paste0(nhgiscode_est$eduC, '007')]],
      edu6 = nhgisYR[[paste0(nhgiscode_est$eduC, '008')]],
      edu7 = nhgisYR[[paste0(nhgiscode_est$eduC, '009')]],
      edu8 = nhgisYR[[paste0(nhgiscode_est$eduC, '010')]],
      
      edu9  = nhgisYR[[paste0(nhgiscode_est$eduC, '020')]],
      edu10 = nhgisYR[[paste0(nhgiscode_est$eduC, '021')]],
      edu11 = nhgisYR[[paste0(nhgiscode_est$eduC, '022')]],
      edu12 = nhgisYR[[paste0(nhgiscode_est$eduC, '023')]],
      edu13 = nhgisYR[[paste0(nhgiscode_est$eduC, '024')]],
      edu14 = nhgisYR[[paste0(nhgiscode_est$eduC, '025')]],
      edu15 = nhgisYR[[paste0(nhgiscode_est$eduC, '026')]],
      edu16 = nhgisYR[[paste0(nhgiscode_est$eduC, '027')]],
      
      # MOE
      edu1.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '003')]] * (1.960/1.645), # convert to 95% CI
      edu2.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '004')]] * (1.960/1.645), # convert to 95% CI
      edu3.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '005')]] * (1.960/1.645), # convert to 95% CI
      edu4.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '006')]] * (1.960/1.645), # convert to 95% CI
      edu5.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '007')]] * (1.960/1.645), # convert to 95% CI
      edu6.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '008')]] * (1.960/1.645), # convert to 95% CI
      edu7.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '009')]] * (1.960/1.645), # convert to 95% CI
      edu8.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '010')]] * (1.960/1.645), # convert to 95% CI
      
      edu9.moe  = nhgisYR[[paste0(nhgiscode_moe$eduC, '020')]] * (1.960/1.645), # convert to 95% CI
      edu10.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '021')]] * (1.960/1.645), # convert to 95% CI
      edu11.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '022')]] * (1.960/1.645), # convert to 95% CI
      edu12.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '023')]] * (1.960/1.645), # convert to 95% CI
      edu13.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '024')]] * (1.960/1.645), # convert to 95% CI
      edu14.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '025')]] * (1.960/1.645), # convert to 95% CI
      edu15.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '026')]] * (1.960/1.645), # convert to 95% CI
      edu16.moe = nhgisYR[[paste0(nhgiscode_moe$eduC, '027')]] * (1.960/1.645)  # convert to 95% CI
    )
  }
  return(ej_df)
}