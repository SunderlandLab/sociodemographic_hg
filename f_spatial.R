## Spatial modeling functions to load NHGIS data from 2010 & 2020.
# date created: 5/26/22
# date updated: 6/26/23
# ================================================================================================================
## Apply spatial regression model using normal distribution
Spmodel <- function(data_shp) {
  # Create neighborhood
  deltaNb_lsw <- data_shp %>%
    spdep::poly2nb(queen = T) %>%
    spdep::nb2listw(zero.policy = T)
  
  ## Create spatial lag eqn
  Hg_slm <- spatialreg::lagsarlm(
    deltaHg ~ FISH + INCOME20 + NOHISCH + NOENG,
    data  = data_shp,
    listw = deltaNb_lsw,
    zero.policy = TRUE,
    na.action   = na.omit)
  
  return(Hg_slm)
}

# ---------------------------------------------------------------------------------------
## Apply spatial regression model using log distribution
Spmodel_log <- function(data_shp) {
  # Create neighborhood
  deltaNb_lsw <- data_shp %>%
    spdep::poly2nb(queen = T) %>%
    spdep::nb2listw(zero.policy = T)
  
  ## Create spatial lag eqn
  logHg_slm <- spatialreg::lagsarlm(
    logHg ~ logFISH + logINCOME20 + logNOHISCH + logNOENG,
    data  = data_shp,
    listw = deltaNb_lsw,
    zero.policy = TRUE,
    na.action   = na.omit)
  
  return(list(slm = logHg_slm, 
              Nb = deltaNb_lsw))
}

# ---------------------------------------------------------------------------------------
## Use spatial regression model to predict data
PredictSp <- function(sp_mod, new_dat){
  deltaNb_lsw <- new_dat %>%
    spdep::poly2nb(queen = T) %>%
    spdep::nb2listw(zero.policy = T)  
  
  predict_dat <- predict(sp_mod, newdata = new_dat, listw = deltaNb_lsw)
  
  return(predict_dat)
}

# ---------------------------------------------------------------------------------------
## Calculate Moran's I locally & save
COType <- function(vulnerable_shp, varName, titleText, cols=1:5) {
  deltaNb_lsw <- vulnerable_shp %>%
    spdep::poly2nb(queen = T) %>%
    spdep::nb2listw(zero.policy = T)
  
  varVals <- vulnerable_shp[[varName]] %>% as.character() %>% as.numeric()
  lisaRslt <- spdep::localmoran(varVals, deltaNb_lsw, 
                                zero.policy = TRUE, na.action = na.exclude)
  significanceLevel <- 0.05; # 95% confidence
  meanVal <- mean(varVals, na.rm=TRUE);
  
  lisaRslt %<>% tibble::as_tibble() %>%
    magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr(z > 0)")) %>%
    dplyr::mutate(coType = dplyr::case_when(
      `Pr(z > 0)` > 0.05 ~ "Insignificant",
      `Pr(z > 0)` <= 0.05 & Ii >= 0 & varVals >= meanVal ~ "HH",
      `Pr(z > 0)` <= 0.05 & Ii >= 0 & varVals < meanVal ~ "LL",
      `Pr(z > 0)` <= 0.05 & Ii < 0 & varVals >= meanVal ~ "HL",
      `Pr(z > 0)` <= 0.05 & Ii < 0 & varVals < meanVal ~ "LH"
    ))
  
  # Now add this coType to original sf data
  vulnerable_shp[['coType']] <- lisaRslt$coType %>% tidyr::replace_na("Insignificant")
  
  return(vulnerable_shp)
}

# ---------------------------------------------------------------------------------------
## Plot LISA as a function.
plotCOType <- function(vulnerable_shp, varName, titleText, cols=1:5) {
  deltaNb_lsw <- vulnerable_shp %>%
    spdep::poly2nb(c('deltaHg'), queen = T) %>%
    spdep::nb2listw(zero.policy = T)
  
  varVals <- vulnerable_shp[[varName]] %>% as.character() %>% as.numeric()
  lisaRslt <- spdep::localmoran(varVals, deltaNb_lsw, 
                                zero.policy = TRUE, na.action = na.exclude)
  significanceLevel <- 0.05; # 95% confidence
  meanVal <- mean(varVals, na.rm=TRUE);
  
  lisaRslt %<>% tibble::as_tibble() %>%
    magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr(z > 0)")) %>%
    dplyr::mutate(coType = dplyr::case_when(
      `Pr(z > 0)` > 0.05 ~ "Insignificant",
      `Pr(z > 0)` <= 0.05 & Ii >= 0 & varVals >= meanVal ~ "HH",
      `Pr(z > 0)` <= 0.05 & Ii >= 0 & varVals < meanVal ~ "LL",
      `Pr(z > 0)` <= 0.05 & Ii < 0 & varVals >= meanVal ~ "HL",
      `Pr(z > 0)` <= 0.05 & Ii < 0 & varVals < meanVal ~ "LH"
    ))
  
  # Now add this coType to original sf data
  vulnerable_shp[['coType']] <- lisaRslt$coType %>% tidyr::replace_na("Insignificant")
  
  ggplot(vulnerable_shp) +
    geom_sf(aes(fill=coType),color = 'lightgrey') +
    scale_fill_manual(values = c('HH' = 'red','HL' = 'brown', 
                                 'Insignificant' = 'NA',
                                 'LH' = 'blue', 'LL' = 'cyan')[cols], name='Clusters & \nOutliers') +
    labs(title = titleText) +
    theme_bw()
}

# ---------------------------------------------------------------------------------------
## Create table summarizing results of spatial regression model
TabSLM <- function(log_slm){
  slm_sum <- summary(log_slm)
  
  coefs <- slm_sum$Coef[,c(1,4)]
  
  rho <- slm_sum$Wald1$estimate
  rho.pval <- slm_sum$Wald1$p.value
  
  slm_tab <- rbind(coefs, c(rho, rho.pval))
  rownames(slm_tab)[nrow(slm_tab)] <- 'rho'
  
  return(slm_tab)
}