## MATS misc functions.
# date created: 2/1/23
# date updated: 2/17/23
# ====================================================================================================================
## Save df specific for EJ var of interest.
LoadEJdf <- function(ej_df){
  ej_df2 <- left_join(bg10_est[, c('GISJOIN', 'POP')],
                      bg10_moe[, c('GISJOIN', 'POP')],
                      suffix = c("", ".moe"),
                      by = 'GISJOIN')
  ej_df3 <- left_join(ej_df2, ej_df, by = "GISJOIN") %>% data.frame()
  
  return(ej_df3)
}

# --------------------------------------------------------------------------------------------------------------------
## Multiply columns
Multiply <- function(x, y, na_rm = T){apply(data.frame(x, y), 1, prod, na.rm = na_rm)}

# --------------------------------------------------------------------------------------------------------------------
## Calculate moe_sum on a data.frame
moe_sum_ej <- function(df,n){
  # Select appropriate cols
  df_est <- df[1:n]
  df_moe <- df[(n+1):(2*n)]
  
  # Sum MOE
  mosum <- moe_sum(moe = df_moe, estimate = df_est, na.rm = T)
  
  return(mosum)
}

# --------------------------------------------------------------------------------------------------------------------
## Calc MOE per BG
CalcMOE0 <- function(ej_df0, ej.factor, ej.tot) {
  # Step 1c: Calculate sum of all AIAN, ASIAN, HIPI, MULTI in census BG = high fish consumers = FISH.
  # MOE
  N      <- (ncol(ej_df0) - 6)/2
  ej_df1 <- ej_df0[,6:(6+2*N-1)]
  print(head(ej_df1))

  A_num.moe <- apply(ej_df1, 1, moe_sum_ej, n = N)
  
  # Step 1d: Calculate FISH as proportion per census BG.
  A <- ej_df0[[ej.factor]] / ej_df0[[ej.tot]]

  A.moe <-
    moe_prop(
      num = ej_df0[[ej.factor]], denom = ej_df0[[ej.tot]],
      moe_num = A_num.moe, moe_denom = ej_df0[[paste0(ej.tot, '.moe')]]
    )

  ej_df <- cbind(ej_df0[, c('GISJOIN', 'POP', 'POP.moe')],
                 Central = A,
                 Central.MOE = A.moe)

  return(ej_df)
}

# --------------------------------------------------------------------------------------------------------------------
## Use above CalcMOE0 function but rename cols more easily.
CalcMOE <- function(ej_df0, ej.factor, ej.tot){
  ej_df <- CalcMOE0(ej_df0, ej.factor, ej.tot)
  colnames(ej_df) <- c('GISJOIN', 'POP', 'POP.moe', ej.factor, paste0(ej.factor, '.moe'))
  # print(c(ej.factor, paste0(ej.factor, '.moe')))
  return(ej_df)
}

# --------------------------------------------------------------------------------------------------------------------
## Calculate percentage in buffer
CalcCI <- function(ej_df, ej.factor, b.uff, buff.sz){
  # Ignore census BG with NA for given EJ factor
  ej_df <- ej_df[which(is.na(ej_df[[ej.factor]]) == F), ]
  
  # Estimate
  B_ejppl <- Multiply(ej_df[[ej.factor]], ej_df[['POP']])
  B_num   <- sum(B_ejppl, na.rm = T)
  B_denom <- sum(ej_df[['POP']], na.rm = T)
  
  B <- B_num / B_denom

  # -------------------------------------------------------
  # MOE
  B_ejppl.moe <- moe_product(est1 = ej_df[[ej.factor]], est2 = ej_df[['POP']], 
                             moe1 = ej_df[[paste0(ej.factor, '.moe')]], moe2 = ej_df[['POP.moe']])
  B_num.moe   <- moe_sum(moe = B_ejppl.moe, estimate = B_ejppl, na.rm = T)
  B_denom.moe <- moe_sum(moe = ej_df[['POP.moe']], estimate = ej_df[['POP']], na.rm = T)
  
  B.moe <- moe_prop(num = B_num, denom = B_denom, 
                    moe_num = B_num.moe, moe_denom = B_denom.moe)
  
  # Save as df
  B_df <- data.frame(Labels = c('EJ.factor', 'Buffer', 'Distance', 'Central', 'MOE', 'Lower', 'Upper', 'PPL', 'PPL.moe'),
                     Vals   = c(  ej.factor,    b.uff,    buff.sz,         B, B.moe, B-B.moe, B+B.moe, B_num, B_num.moe))
  # Clean up
  B_df  <- B_df %>% t() %>% data.frame()
  colnames(B_df) <- B_df[1,] %>% as.character()
  B_df <- B_df[-1,]
  B_df[,3:ncol(B_df)] <- as.numeric(B_df[,3:ncol(B_df)])
  
  return(B_df)
}

# --------------------------------------------------------------------------------------------------------------------
## Compare inside vs outside buffer
BufferTab <- function(ej_df, ej.factor, buff_bg = clip5, buff.sz = "5") {
  # Which census BG are in vs outside buffer
  inbuff  <- ej_df[which(ej_df$GISJOIN %in% buff_bg$GISJOIN),]
  outbuff <- ej_df[which(!ej_df$GISJOIN %in% buff_bg$GISJOIN),]
  
  # Calculate FISH % in & outside buff
  ej_in  <- CalcCI(inbuff, ej.factor, 'In', buff.sz)
  ej_out <- CalcCI(outbuff, ej.factor, 'Out', buff.sz)
  
  # Combine In & Out
  ej_buff <- rbind(ej_in, ej_out)
  
  return(ej_buff)
}

# --------------------------------------------------------------------------------------------------------------------
## Plot CI comparing buffers
PlotBuff <-
  function(ej.factor,
           allbuff,
           ej_df,
           in.out = "In",
           pointsize = 2,
           errorsize = 0.5) {
    # Filter buffer
    buff <- filter(allbuff, EJ.factor == ej.factor &
                     substr(Buffer, 1, 2) == in.out)
    
    # US mean
    us_CI <- CalcCI(ej_df, ej.factor, b.uff = 'US', buff.sz = 'US')
    
    # Plot buffer
    ggplot(data = buff) +
      # US Average
      geom_hline(yintercept = us_CI$Central) +
      # US Average CI
      annotate('rect', alpha = 0.7, fill = 'lightgrey',
               ymin = us_CI$Lower,
               ymax = us_CI$Upper,
               xmin = -Inf, # 0.5,
               xmax = Inf #124
      ) +
      # Error bars
      geom_errorbar(
        aes(ymin = Lower, ymax = Upper, x = Distance),
        width = 0,
        # height = 0,
        size = errorsize,
        color = "darkblue",
        position = position_dodge(width = 0.9),
      ) +
      # Central Estimate
      geom_point(aes(x = Distance, y = Central),
                 color = "blue",
                 size = pointsize,
      ) +
      # Aesthetics
      scale_x_continuous(breaks = c(1,5,15,25,50,100)) +
      coord_trans(x = "log10") +
      labs(title = ej.factor, x = 'Distance (km)', y = 'Proportion') +
      theme_bw()
    
  }

# --------------------------------------------------------------------------------------------------------------
## Plot box plots for various buffer distances.
PlotVulnerable <- function(EJ.factor, 
                           inbuff5 = inbuff5, inbuff10 = inbuff10, inbuff50 = inbuff50, inbuff100 = inbuff100, 
                           outbuff5 = outbuff5, outbuff10 = outbuff10, outbuff50 = outbuff50, outbuff100 = outbuff100) {
  ej_plt <- ggplot() +
    geom_boxplot(aes(y = get(EJ.factor), x = 'US'),
                 data = us_vulnerable,
                 color = 'darkred') +
    # Inside buffer
    geom_boxplot(aes(y = get(EJ.factor), x = 'In 10km'), data = inbuff10) +
    geom_boxplot(aes(y = get(EJ.factor), x = 'In 50km'), data = inbuff50) +
    geom_boxplot(aes(y = get(EJ.factor), x = 'In 100km'), data = inbuff100) +
    # Outside buffer
    geom_boxplot(aes(y = get(EJ.factor), x = 'Out 10km'), data = outbuff10) +
    geom_boxplot(aes(y = get(EJ.factor), x = 'Out 50km'), data = outbuff50) +
    geom_boxplot(aes(y = get(EJ.factor), x = 'Out 100km'), data = outbuff100) +
    
    labs(y = EJ.factor, x = "") +
    theme_minimal()
  
  return(ej_plt)
}

# --------------------------------------------------------------------------------------------------------------
## Perform significance tests.
# t-test
Ttest_tab <- function(EJ.factor, 
                      inbuff5 = inbuff5, inbuff10 = inbuff10, inbuff50 = inbuff50, inbuff100 = inbuff100, 
                      outbuff5 = outbuff5, outbuff10 = outbuff10, outbuff50 = outbuff50, outbuff100 = outbuff100) {
  
  # Calculate population
  inbuff5$EJw   <- inbuff5[[EJ.factor]]
  inbuff10$EJw  <- inbuff10[[EJ.factor]]
  inbuff50$EJw  <- inbuff50[[EJ.factor]]
  inbuff100$EJw <- inbuff100[[EJ.factor]]
  
  outbuff5$EJw   <- outbuff5[[EJ.factor]]
  outbuff10$EJw  <- outbuff10[[EJ.factor]]
  outbuff50$EJw  <- outbuff50[[EJ.factor]]
  outbuff100$EJw <- outbuff100[[EJ.factor]]
  
  us_vulnerable$EJw   <- us_vulnerable[[EJ.factor]]
  
  # Remove infinite values 
  inbuff5   <- filter(inbuff5, EJw > 0)
  inbuff10  <- filter(inbuff10, EJw > 0)
  inbuff50  <- filter(inbuff50, EJw > 0)
  inbuff100 <- filter(inbuff100, EJw > 0)
  
  outbuff5   <- filter(outbuff5, EJw > 0)
  outbuff10  <- filter(outbuff10, EJw > 0)
  outbuff50  <- filter(outbuff50, EJw > 0)
  outbuff100 <- filter(outbuff100, EJw > 0)
  
  us_vulnerable <- filter(us_vulnerable, EJ.factor != 0)
  
  # Inside Buffer vs US
  us5   <- t.test(inbuff5[[EJ.factor]], us_vulnerable[[EJ.factor]])
  us10  <- t.test(inbuff10[[EJ.factor]], us_vulnerable[[EJ.factor]])
  us50  <- t.test(inbuff50[[EJ.factor]], us_vulnerable[[EJ.factor]])
  us100 <- t.test(inbuff100[[EJ.factor]], us_vulnerable[[EJ.factor]])
  
  # Inside vs Outside Buffer
  out5   <- t.test(log(inbuff5[[EJ.factor]]), log(outbuff5[[EJ.factor]]))
  out10  <- t.test(log(inbuff10[[EJ.factor]]), log(outbuff10[[EJ.factor]]))
  out50  <- t.test(log(inbuff50[[EJ.factor]]), log(outbuff50[[EJ.factor]]))
  out100 <- t.test(log(inbuff100[[EJ.factor]]), log(outbuff100[[EJ.factor]]))
  
  t_ls <- list(us5 = us5, us10 = us10, us50 = us50, us100 = us100, 
               out5 = out5, out10 = out10, out50 = out50, out100 = out100)
  
  # sig_df <- data.frame(US = rep(NA3)Buffer = rep(NA,3))
  sig_df <-
    data.frame(
      Significant = rep(NA, 8),
      row.names = c('US 5km', 'US 10km', 'US 50km', 'US 100km', 
                    'Buffer 5km', 'Buffer 10km', 'Buffer 50km', 'Buffer 100km')
    )
  
  # Make table indicating whether difference is significant
  for (i in 1:length(t_ls)) {
    if (t_ls[[i]]$p.value < 0.05) {
      sig_df[i, 1] <- 1
    } else{
      sig_df[i, 1] <- 0
    }
  }
  
  print(sig_df)
  
  return(list(sig_df = sig_df, t_ls = t_ls))
}

# --------------------------------------------------------------------------------------------------------------
## Wilcoxon test
Wtest_tab <- function(EJ.factor, 
                      inbuff5 = inbuff5, inbuff10 = inbuff10, inbuff50 = inbuff50, inbuff100 = inbuff100, 
                      outbuff5 = outbuff5, outbuff10 = outbuff10, outbuff50 = outbuff50, outbuff100 = outbuff100) {
  # Inside Buffer vs US
  us5   <- wilcox.test(inbuff5[[EJ.factor]], us_vulnerable[[EJ.factor]])
  us10  <- wilcox.test(inbuff10[[EJ.factor]], us_vulnerable[[EJ.factor]])
  us50  <- wilcox.test(inbuff50[[EJ.factor]], us_vulnerable[[EJ.factor]])
  us100 <- wilcox.test(inbuff100[[EJ.factor]], us_vulnerable[[EJ.factor]])
  
  # Inside vs Outside Buffer
  out5   <- wilcox.test(inbuff5[[EJ.factor]], outbuff5[[EJ.factor]])
  out10  <- wilcox.test(inbuff10[[EJ.factor]], outbuff10[[EJ.factor]])
  out50  <- wilcox.test(inbuff50[[EJ.factor]], outbuff50[[EJ.factor]])
  out100 <- wilcox.test(inbuff100[[EJ.factor]], outbuff100[[EJ.factor]])
  
  t_ls <- list(us5 = us5, us10 = us10, us50 = us50, us100 = us100, 
               out5 = out5, out10 = out10, out50 = out50, out100 = out100)
  
  sig_df <-
    data.frame(
      Significant = rep(NA, 8),
      row.names = c('US 5km', 'US 10km', 'US 50km', 'US 100km', 
                    'Buffer 5km', 'Buffer 10km', 'Buffer 50km', 'Buffer 100km')
    )
  
  # Make table indicating whether difference is significant
  for (i in 1:length(t_ls)) {
    # print(t_ls[[i]]$p.value)
    if (t_ls[[i]]$p.value < 0.05) {
      sig_df[i, 1] <- 1
    } else{
      sig_df[i, 1] <- 0
    }
  }
  
  print(sig_df)
  
  return(list(sig_df = sig_df, t_ls = t_ls))
}


# --------------------------------------------------------------------------------------------------------------
## Function to knit into table.
KnitTab <- function(df, caption = "", digits = 3) {
  knitr::kable(format(df, digits = digits),
               caption = caption,
               digits = digits)
}

# --------------------------------------------------------------------------------------------------------------
## Re-order column
Orderby <- function(df, col.name, de.creasing = T){
  return(df[order(df[[col.name]], decreasing = de.creasing),])
}

# --------------------------------------------------------------------------------------------------------------
## Faster plotting of polygons (shp)
st_plot <- function(shp){
  shp %>% st_geometry() %>% plot()
}

# --------------------------------------------------------------------------------------------------------------
## For Q1 & Q2, compare proportions of subgroups for buffers of multiple sizes
Compare_EGU_Buffers <- function(buff.sz, us_egu = egu_pt7801, bg_shp = bg10_shp) {
  # Find buffer
  egu_buff <- st_buffer(us_egu, as.numeric(paste0(buff.sz, 'e3'))) %>% st_transform(st_crs(usa_shp0))
  
  # Find intersection of buffer with census BG
  egu_bg <- st_intersection(egu_buff, bg_shp)
  
  # Calculate buffer proportions inside & outside
  fish_buff    <- BufferTab(fish_df, 'FISH', egu_bg, buff.sz)
  income_buff  <- BufferTab(income_df, 'INCOME20', egu_bg, buff.sz)
  poverty_buff <- BufferTab(poverty_df, 'POVERTY200', egu_bg, buff.sz)
  nohisch_buff <- BufferTab(nohisch_df, 'NOHISCH', egu_bg, buff.sz)
  noeng_buff   <- BufferTab(noeng_df, 'NOENG', egu_bg, buff.sz)
  
  # Combine
  all_buff <- rbind(fish_buff, income_buff, poverty_buff, 
                    nohisch_buff, noeng_buff)
  
  return(all_buff)
}

# --------------------------------------------------------------------------------------------------------------
## Manually calculate confidence interval
confidence.interval <- function(col_of_vals, conf.int = 0.95){
  sample.mean <- mean(col_of_vals, na.rm = T)
  
  sample.n <- length(col_of_vals)
  sample.sd <- sd(col_of_vals, na.rm = T)
  sample.se <- sample.sd/sqrt(sample.n)
  
  alpha = 1-conf.int
  degrees.freedom = sample.n - 1
  t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
  
  margin.error <- t.score * sample.se
  
  lower.bound <- sample.mean - margin.error
  upper.bound <- sample.mean + margin.error
  
  ci_df <- col_of_valsa.frame(Stat = c('Mean', 'Lower', 'Upper'), Value = c(sample.mean, lower.bound, upper.bound))
  
  return(ci_df)
}
