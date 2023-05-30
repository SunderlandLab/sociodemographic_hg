## Functions to load NHGIS data from 2010 & 2020.
# date created: 3/9/22
# date updated: 2/10/23
# ================================================================================================================
Load_NHGISdf <- function(nhgisYR,
                         nhgiscode_ls, # list of defined NHGIS codes
                         year) {
## =======================================================================================
     
  if (year == 2010) {
    ## -------------------------------------------------------------------
    ## POPULATION
    popYR <-
      data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))
    
    popYR <- popYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      STATE = nhgisYR$STATE,
      POP   = nhgisYR[[paste0(nhgiscode_ls$popC, '001')]]
    )
    
    # Remove empty col
    popYR <- popYR[, 2:ncol(popYR)]
    
    ## -------------------------------------------------------------------
    ## RACE
    # Create empty data.frame
    raceYR <-
      data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))
    
    # Save race cols
    raceYR <- raceYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      WHITE = nhgisYR[[paste0(nhgiscode_ls$raceC, '002')]],
      BLACK = nhgisYR[[paste0(nhgiscode_ls$raceC, '003')]],
      AIAN  = nhgisYR[[paste0(nhgiscode_ls$raceC, '004')]],
      ASIAN = nhgisYR[[paste0(nhgiscode_ls$raceC, '005')]],
      HIPI  = nhgisYR[[paste0(nhgiscode_ls$raceC, '006')]],
      OTHER = nhgisYR[[paste0(nhgiscode_ls$raceC, '007')]],
      MULTI = nhgisYR[[paste0(nhgiscode_ls$raceC, '008')]],
      
      RACE.TOT = nhgisYR[[paste0(nhgiscode_ls$raceC, '001')]]
    )
    
    # Remove empty col
    raceYR <- raceYR[, 2:ncol(raceYR)]
    
    ## -------------------------------------------------------------------
    # HISPANIC
    # Create empty data.frame
    hispanicYR <-
      data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))
    
    # Save cols
    hispanicYR <- hispanicYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      
      HISPANICYES   = nhgisYR[[paste0(nhgiscode_ls$hispC, '012')]],
      HISPANICWHITE = nhgisYR[[paste0(nhgiscode_ls$hispC, '013')]],
      HISPANICBLACK = nhgisYR[[paste0(nhgiscode_ls$hispC, '014')]],
      
      WHITEONLY = nhgisYR[[paste0(nhgiscode_ls$hispC, '003')]],
      BLACKONLY = nhgisYR[[paste0(nhgiscode_ls$hispC, '004')]],
      AIANONLY  = nhgisYR[[paste0(nhgiscode_ls$hispC, '005')]],
      ASIANONLY = nhgisYR[[paste0(nhgiscode_ls$hispC, '006')]],
      HIPIONLY  = nhgisYR[[paste0(nhgiscode_ls$hispC, '007')]],
      OTHERONLY = nhgisYR[[paste0(nhgiscode_ls$hispC, '008')]],
      MULTIONLY = nhgisYR[[paste0(nhgiscode_ls$hispC, '009')]],
      
      HISPANIC.TOT = nhgisYR[[paste0(nhgiscode_ls$hispC, '001')]]
    )
    
    # Remove empty col
    hispanicYR <- hispanicYR[, 2:ncol(hispanicYR)]
    
    # Calculate subgroups
    hispanicYR <- hispanicYR %>% mutate(
      NONWHITEHISPANIC = hispanicYR$HISPANICYES - hispanicYR$HISPANICWHITE,
      NONBLACKHISPANIC = hispanicYR$HISPANICYES - hispanicYR$HISPANICBLACK,
      POC = hispanicYR$HISPANICYES + hispanicYR$BLACKONLY + hispanicYR$AIANONLY + hispanicYR$ASIANONLY + hispanicYR$HIPIONLY +
        hispanicYR$OTHERONLY + hispanicYR$MULTIONLY
    )
    
    ## -------------------------------------------------------------------
    ## LANGUAGE - % limited English speaking household
    # Create empty data.frame
    engYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))
    
    # Save cols
    engYR <- engYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      NOENG = (nhgisYR[[paste0(nhgiscode_ls$engC, '004')]] +
                 nhgisYR[[paste0(nhgiscode_ls$engC, '007')]] +
                 nhgisYR[[paste0(nhgiscode_ls$engC, '010')]] +
                 nhgisYR[[paste0(nhgiscode_ls$engC, '013')]]),
      
      
      NOENG.TOT = nhgisYR[[paste0(nhgiscode_ls$engC, '001')]]
    )
    
    # Remove empty col
    engYR <- engYR[, 2:ncol(engYR)]
    
    ## -------------------------------------------------------------------
    ## EDUCATION
    # Create empty data.frame
    eduYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))
    
    # Save cols
    eduYR <- eduYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      MIDSCH  = (nhgisYR[[paste0(nhgiscode_ls$eduC, '007')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '024')]] +
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '008')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '025')]] +
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '009')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '026')]] +
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '010')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '027')]] +
                   
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '011')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '028')]] +
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '012')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '029')]] +
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '013')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '030')]] +
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '014')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '031')]] +
                   
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '015')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '032')]] +
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '016')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '033')]] +
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '017')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '034')]] +
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '018')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '035')]] ),
      
      HISCH  = (nhgisYR[[paste0(nhgiscode_ls$eduC, '011')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '028')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '012')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '029')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '013')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '030')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '014')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '031')]] +
                  
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '015')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '032')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '016')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '033')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '017')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '034')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '018')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '035')]] ),
      
      COLLEGE = (nhgisYR[[paste0(nhgiscode_ls$eduC, '015')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '032')]] +
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '016')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '033')]] +
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '017')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '034')]] +
                   nhgisYR[[paste0(nhgiscode_ls$eduC, '018')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '035')]] ),
      
      GRAD   = (nhgisYR[[paste0(nhgiscode_ls$eduC, '016')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '033')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '017')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '034')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '018')]] + nhgisYR[[paste0(nhgiscode_ls$eduC, '035')]] ),
      
      EDU.TOT = nhgisYR[[paste0(nhgiscode_ls$eduC, '001')]]
    )
    
    # Remove empty col
    eduYR <- eduYR[, 2:ncol(eduYR)]
    
    ## -------------------------------------------------------------------
    ## UNEMPLOYMENT
    # Create empty data.frame
    workYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))
    
    # Save cols
    workYR <- workYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      UNEMP   = (nhgisYR[[paste0(nhgiscode_ls$unempC, '025')]] + 
                   nhgisYR[[paste0(nhgiscode_ls$unempC, '049')]]),
      
      UNEMP.TOT = nhgisYR[[paste0(nhgiscode_ls$unempC, '001')]]
    )
    
    # Remove empty col
    workYR <- workYR[, 2:ncol(workYR)]
    
    ## -------------------------------------------------------------------
    ## POVERTY
    # Create empty data.frame
    povertyYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))
    
    # Save cols
    povertyYR <- povertyYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      # Under 100% poverty level
      POVERTY100 = (nhgisYR[[paste0(nhgiscode_ls$povC, '002')]] + nhgisYR[[paste0(nhgiscode_ls$povC, '003')]]),
      # Under 200% poverty level (low-income in EJscreen)
      POVERTY200 = (
        nhgisYR[[paste0(nhgiscode_ls$povC, '002')]] + nhgisYR[[paste0(nhgiscode_ls$povC, '003')]] +
          nhgisYR[[paste0(nhgiscode_ls$povC, '004')]] + nhgisYR[[paste0(nhgiscode_ls$povC, '005')]] +
          nhgisYR[[paste0(nhgiscode_ls$povC, '006')]] + nhgisYR[[paste0(nhgiscode_ls$povC, '007')]]
      ),
      
      POVERTY.TOT = nhgisYR[[paste0(nhgiscode_ls$povC, '001')]]
    )
    
    # Remove empty col
    povertyYR <- povertyYR[, 2:ncol(povertyYR)]
    
    ## -------------------------------------------------------------------
    ## INCOME - Median household income
    # Create empty data.frame
    income20YR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))
    
    # Save cols
    income20YR <- income20YR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      # Per capita income20
      INCOME20  = (nhgisYR[[paste0(nhgiscode_ls$income20C, '002')]] + 
                     nhgisYR[[paste0(nhgiscode_ls$income20C, '003')]] + nhgisYR[[paste0(nhgiscode_ls$income20C, '004')]]),
      
      INCOME20.TOT = nhgisYR[[paste0(nhgiscode_ls$income20C, '001')]]
    )
    
    # Remove empty col
    income20YR <- income20YR[, 2:ncol(income20YR)]
    
    ## -------------------------------------------------------------------
    ## % RENTING
    # Create empty data.frame
    rentYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))
    
    # Save cols
    rentYR <- rentYR %>% mutate(
      GISJOIN  = nhgisYR$GISJOIN,
      RENT = nhgisYR[[paste0(nhgiscode_ls$rentC, '003')]],
      
      RENT.TOT = nhgisYR[[paste0(nhgiscode_ls$rentC, '001')]]
    )
    
    # Remove empty col
    rentYR <- rentYR[, 2:ncol(rentYR)]
    
    ## Create comprehensive data.frame
    #  ---------------------------------
    nhgisYR_df <- popYR %>%
      left_join(raceYR, by = 'GISJOIN') %>%
      left_join(hispanicYR, by = 'GISJOIN') %>%
      left_join(engYR, by = 'GISJOIN') %>%
      left_join(eduYR, by = 'GISJOIN') %>%
      # left_join(incomeYR, by = 'GISJOIN') %>%
      left_join(income20YR, by = 'GISJOIN') %>%
      left_join(povertyYR[c('GISJOIN', 'POVERTY200', 'POVERTY.TOT')], by = 'GISJOIN') %>%
      left_join(workYR, by = 'GISJOIN') %>%
      left_join(rentYR, by = 'GISJOIN') #%>%
      # left_join(housevalYR, by = 'GISJOIN') %>%
      # left_join(houseageYR, by = 'GISJOIN')  
    
  }

  ## =======================================================================================

  else if (year > 2010) {
    ## -------------------------------------------------------------------
    ## POPULATION
    popYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))

    popYR <- popYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      STATE = nhgisYR$STATE,
      POP   = nhgisYR[[paste0(nhgiscode_ls$popC, '001')]]
    )

    # Remove empty col
    popYR <- popYR[, 2:ncol(popYR)]

    ## -------------------------------------------------------------------
    ## RACE
    # Create empty data.frame
    raceYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))

    # Save race cols
    raceYR <- raceYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      WHITE = nhgisYR[[paste0(nhgiscode_ls$raceC, '002')]] / nhgisYR[[paste0(nhgiscode_ls$raceC, '001')]],
      BLACK = nhgisYR[[paste0(nhgiscode_ls$raceC, '003')]] / nhgisYR[[paste0(nhgiscode_ls$raceC, '001')]],
      AIAN  = nhgisYR[[paste0(nhgiscode_ls$raceC, '004')]] / nhgisYR[[paste0(nhgiscode_ls$raceC, '001')]],
      ASIAN = nhgisYR[[paste0(nhgiscode_ls$raceC, '005')]] / nhgisYR[[paste0(nhgiscode_ls$raceC, '001')]],
      HIPI  = nhgisYR[[paste0(nhgiscode_ls$raceC, '006')]] / nhgisYR[[paste0(nhgiscode_ls$raceC, '001')]],
      OTHER = nhgisYR[[paste0(nhgiscode_ls$raceC, '007')]] / nhgisYR[[paste0(nhgiscode_ls$raceC, '001')]],
      MULTI = nhgisYR[[paste0(nhgiscode_ls$raceC, '008')]] / nhgisYR[[paste0(nhgiscode_ls$raceC, '001')]],
    )

    # Remove empty col
    raceYR <- raceYR[, 2:ncol(raceYR)]

    ## -------------------------------------------------------------------
    # HISPANIC
    # Create empty data.frame
    hispanicYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))

    # Save cols
    hispanicYR <- hispanicYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,

      HISPANICTOTAL = nhgisYR[[paste0(nhgiscode_ls$hispC, '012')]] / nhgisYR[[paste0(nhgiscode_ls$hispC, '001')]],
      HISPANICWHITE = nhgisYR[[paste0(nhgiscode_ls$hispC, '013')]] / nhgisYR[[paste0(nhgiscode_ls$hispC, '001')]],
      HISPANICBLACK = nhgisYR[[paste0(nhgiscode_ls$hispC, '014')]] / nhgisYR[[paste0(nhgiscode_ls$hispC, '001')]],

      WHITEONLY = nhgisYR[[paste0(nhgiscode_ls$hispC, '003')]] / nhgisYR[[paste0(nhgiscode_ls$hispC, '001')]],
      BLACKONLY = nhgisYR[[paste0(nhgiscode_ls$hispC, '004')]] / nhgisYR[[paste0(nhgiscode_ls$hispC, '001')]],
      AIANONLY  = nhgisYR[[paste0(nhgiscode_ls$hispC, '005')]] / nhgisYR[[paste0(nhgiscode_ls$hispC, '001')]],
      ASIANONLY = nhgisYR[[paste0(nhgiscode_ls$hispC, '006')]] / nhgisYR[[paste0(nhgiscode_ls$hispC, '001')]],
      HIPIONLY  = nhgisYR[[paste0(nhgiscode_ls$hispC, '007')]] / nhgisYR[[paste0(nhgiscode_ls$hispC, '001')]],
      OTHERONLY = nhgisYR[[paste0(nhgiscode_ls$hispC, '008')]] / nhgisYR[[paste0(nhgiscode_ls$hispC, '001')]],
      MULTIONLY = nhgisYR[[paste0(nhgiscode_ls$hispC, '009')]] / nhgisYR[[paste0(nhgiscode_ls$hispC, '001')]]

    )

    # Remove empty col
    hispanicYR <- hispanicYR[, 2:ncol(hispanicYR)]

    # Calculate subgroups
    hispanicYR <- hispanicYR %>% mutate(
      NONWHITEHISPANIC = hispanicYR$HISPANICTOTAL - hispanicYR$HISPANICWHITE,
      NONBLACKHISPANIC = hispanicYR$HISPANICTOTAL - hispanicYR$HISPANICBLACK,
      POC = hispanicYR$HISPANICTOTAL + hispanicYR$BLACKONLY + hispanicYR$AIANONLY + hispanicYR$ASIANONLY + hispanicYR$HIPIONLY +
        hispanicYR$OTHERONLY + hispanicYR$MULTIONLY
    )

    ## -------------------------------------------------------------------
    ## LANGUAGE - % limited English speaking household
    # Create empty data.frame
    engYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))

    # Save cols
    engYR <- engYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      NOENG = (nhgisYR[[paste0(nhgiscode_ls$engC, '004')]] +
                 nhgisYR[[paste0(nhgiscode_ls$engC, '007')]] +
                 nhgisYR[[paste0(nhgiscode_ls$engC, '010')]] +
                 nhgisYR[[paste0(nhgiscode_ls$engC, '013')]]) / nhgisYR[[paste0(nhgiscode_ls$engC, '001')]]
    )

    # Remove empty col
    engYR <- engYR[, 2:ncol(engYR)]

    ## -------------------------------------------------------------------
    ## EDUCATION
    # Create empty data.frame
    eduYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))

    # Save cols
    eduYR <- eduYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,

      NOHISCH  = (nhgisYR[[paste0(nhgiscode_ls$eduC, '002')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '003')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '004')]] +

                  nhgisYR[[paste0(nhgiscode_ls$eduC, '005')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '006')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '007')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '008')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '009')]] +

                  nhgisYR[[paste0(nhgiscode_ls$eduC, '010')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '011')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '012')]] +

                  nhgisYR[[paste0(nhgiscode_ls$eduC, '013')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '014')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '015')]] +
                  nhgisYR[[paste0(nhgiscode_ls$eduC, '016')]]
                ) / nhgisYR[[paste0(nhgiscode_ls$eduC, '001')]],
    )

    # Remove empty col
    eduYR <- eduYR[, 2:ncol(eduYR)]

    ## -------------------------------------------------------------------
    ## UNEMPLOYMENT
    # Create empty data.frame
    workYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))

    # Save cols
    workYR <- workYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      UNEMP   = (nhgisYR[[paste0(nhgiscode_ls$unempC, '005')]] + nhgisYR[[paste0(nhgiscode_ls$unempC, '007')]]) / nhgisYR[[paste0(nhgiscode_ls$unempC, '001')]]
    )

    # Remove empty col
    workYR <- workYR[, 2:ncol(workYR)]

    ## -------------------------------------------------------------------
    ## POVERTY
    # Create empty data.frame
    povertyYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))

    # Save cols
    povertyYR <- povertyYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      # Under 100% poverty level
      POVERTY100 = (nhgisYR[[paste0(nhgiscode_ls$povC, '002')]] + nhgisYR[[paste0(nhgiscode_ls$povC, '003')]]) /
        nhgisYR[[paste0(nhgiscode_ls$povC, '001')]],
      # Under 200% poverty level (low-income in EJscreen)
      POVERTY200 = (
        nhgisYR[[paste0(nhgiscode_ls$povC, '002')]] + nhgisYR[[paste0(nhgiscode_ls$povC, '003')]] +
          nhgisYR[[paste0(nhgiscode_ls$povC, '004')]] + nhgisYR[[paste0(nhgiscode_ls$povC, '005')]] +
          nhgisYR[[paste0(nhgiscode_ls$povC, '006')]] + nhgisYR[[paste0(nhgiscode_ls$povC, '007')]]
      )
      / nhgisYR[[paste0(nhgiscode_ls$povC, '001')]]
    )

    # Remove empty col
    povertyYR <- povertyYR[, 2:ncol(povertyYR)]

    ## -------------------------------------------------------------------
    ## INCOME - Median household income
    # Create empty data.frame
    incomeYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))

    # Save cols
    incomeYR <- incomeYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      # Per capita income
      INCOME  = nhgisYR[[paste0(nhgiscode_ls$incomeC, '001')]]
    )

    # Remove empty col
    incomeYR <- incomeYR[, 2:ncol(incomeYR)]

    ## -------------------------------------------------------------------
    ## INCOME - Median household income
    # Create empty data.frame
    income20YR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))

    # Save cols
    income20YR <- income20YR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      # Per capita income20
      INCOME20  = (nhgisYR[[paste0(nhgiscode_ls$income20C, '002')]] +
                     nhgisYR[[paste0(nhgiscode_ls$income20C, '003')]] + nhgisYR[[paste0(nhgiscode_ls$income20C, '004')]]) /
        nhgisYR[[paste0(nhgiscode_ls$income20C, '001')]]
    )

    # Remove empty col
    income20YR <- income20YR[, 2:ncol(income20YR)]

    ## -------------------------------------------------------------------
    ## % RENTING
    # Create empty data.frame
    rentYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))

    # Save cols
    rentYR <- rentYR %>% mutate(
      GISJOIN  = nhgisYR$GISJOIN,
      RENT = nhgisYR[[paste0(nhgiscode_ls$rentC, '003')]] / nhgisYR[[paste0(nhgiscode_ls$rentC, '001')]]
    )

    # Remove empty col
    rentYR <- rentYR[, 2:ncol(rentYR)]

    ## -------------------------------------------------------------------
    ## HOUSE VALUE - Median housing value for owner-occupied users
    # Create empty data.frame
    housevalYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))

    # Save cols
    housevalYR <- housevalYR %>% mutate(
      GISJOIN  = nhgisYR$GISJOIN,
      HOUSEVAL = nhgisYR[[paste0(nhgiscode_ls$housevalC, '001')]]
    )

    # Remove empty col
    housevalYR <- housevalYR[, 2:ncol(housevalYR)]

    ## -------------------------------------------------------------------
    ## HOUSE AGE
    # Create empty data.frame
    houseageYR <- data.frame(matrix(nrow = nrow(nhgisYR), ncol = 1))

    # Save cols
    houseageYR <- houseageYR %>% mutate(
      GISJOIN = nhgisYR$GISJOIN,
      HOUSEAGE = ((nhgisYR[[paste0(nhgiscode_ls$houseageC, '002')]] * (year - 2014)) +
                    (nhgisYR[[paste0(nhgiscode_ls$houseageC, '003')]] * (year - 2010)) +
                    (nhgisYR[[paste0(nhgiscode_ls$houseageC, '004')]] * (year - 2000)) +
                    (nhgisYR[[paste0(nhgiscode_ls$houseageC, '005')]] * (year - 1990)) +
                    (nhgisYR[[paste0(nhgiscode_ls$houseageC, '006')]] * (year - 1980)) +
                    (nhgisYR[[paste0(nhgiscode_ls$houseageC, '007')]] * (year - 1970)) +
                    (nhgisYR[[paste0(nhgiscode_ls$houseageC, '008')]] * (year - 1960)) +
                    (nhgisYR[[paste0(nhgiscode_ls$houseageC, '009')]] * (year - 1950)) +
                    (nhgisYR[[paste0(nhgiscode_ls$houseageC, '010')]] * (year - 1940)) +
                    (nhgisYR[[paste0(nhgiscode_ls$houseageC, '011')]] * (year - 1900))    # assume 1900 is earliest year
      ) / nhgisYR[[paste0(nhgiscode_ls$houseageC, '001')]]
    )

    # Integer
    houseageYR$HOUSEAGE <- as.integer(houseageYR$HOUSEAGE)

    # Remove empty col
    houseageYR <- houseageYR[, 2:ncol(houseageYR)]

    ## Create comprehensive data.frame
    #  ---------------------------------
    nhgisYR_df <- popYR %>%
      left_join(raceYR, by = 'GISJOIN') %>%
      left_join(hispanicYR, by = 'GISJOIN') %>%
      left_join(engYR, by = 'GISJOIN') %>%
      left_join(eduYR, by = 'GISJOIN') %>%
      left_join(incomeYR, by = 'GISJOIN') %>%
      left_join(income20YR, by = 'GISJOIN') %>%
      left_join(povertyYR[c('GISJOIN', 'POVERTY200')], by = 'GISJOIN') %>%
      left_join(workYR, by = 'GISJOIN') %>%
      # left_join(uninsYR, by = 'GISJOIN') %>%
      left_join(rentYR, by = 'GISJOIN') %>%
      left_join(housevalYR, by = 'GISJOIN') %>%
      left_join(houseageYR, by = 'GISJOIN')

  }
  
  ## =======================================================================================
  
  return(nhgisYR_df)
}
