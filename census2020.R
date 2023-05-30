## Load census data from NHGIS.
# date created: 1/6/23
# date updated: 2/5/23
# =========================================================================================================
# Load function.
source('f_LoadNHGIS.R')

# Load data - change to your directory.
nhgis2010_bg <- read.csv('data/nhgis0055_ds176_20105_blck_grp.csv')
nhgis2020_bg <- read.csv('data/nhgis0039_ds249_20205_blck_grp.csv')

# =========================================================================================================
## 2010
# Load NHGIS codes.
nhgis2010_EST <- list(popC       = "JMAE",
                       raceC     = "JMBE",
                       hispC     = "JMJE",
                       eduC      = "JN9E",
                       engC      = "JOAE",
                       povC      = "JOCE",
                       incomeC   = "JQBE",
                       income20C = "JOHE",
                       unempC    = "JQ4E",
                       rentC     = "JRKE",
                       houseageC = "JSDE",
                       housevalC = "JTIE")

nhgis2010_MOE <- list(popC      = "JMAM",
                      raceC     = "JMBM",
                      hispC     = "JMJM",
                      eduC      = "JN9M",
                      engC      = "JOAM",
                      povC      = "JOCM",
                      incomeC   = "JQBM",
                      income20C = "JOHM",
                      unempC    = "JQ4M",
                      rentC     = "JRKM",
                      houseageC = "JSDM",
                      housevalC = "JTIM")

# 2010 bg
bg10_est <- Load_NHGISdf(nhgisYR = nhgis2010_bg, nhgis2010_EST, 2010)
bg10_moe <- Load_NHGISdf(nhgisYR = nhgis2010_bg, nhgis2010_MOE, 2010)

# head(bg10_est)
# head(bg10_moe)
# 
# summary(bg10_est)
# summary(bg10_moe)


# =========================================================================================================
## 2020
# Load NHGIS codes.
nhgis2020_EST <- list(popC       = "AMPVE",
                       raceC     = "AMPWE",
                       hispC     = "AMP3E",
                       eduC      = "AMRZE",
                       engC      = "AMZLE",
                       povC      = "AMZME",
                       incomeC   = "AMR8E",
                       income20C = "AMR7E",
                       unempC    = "AMT9E",
                       rentC     = "AMUFE",
                       houseageC = "AMU7E",
                       housevalC = "AMWBE")

nhgis2020_MOE <- list(popC      = "AMPVM",
                      raceC     = "AMPWM",
                      hispC     = "AMP3M",
                      eduC      = "AMRZM",
                      engC      = "AMZLM",
                      povC      = "AMZMM",
                      incomeC   = "AMR8M",
                      income20C = "AMR7M",
                      unempC    = "AMT9M",
                      rentC     = "AMUFM",
                      houseageC = "AMU7M",
                      housevalC = "AMWBM")

# 2020 bg
bg20_df  <- Load_NHGISdf(nhgisYR = nhgis2020_bg, nhgis2020_EST, 2020)
# Add hi fish consuming pop & no high school
bg20_df <- bg20_df %>% group_by(GISJOIN) %>% mutate(FISH = sum(ASIAN, HIPI, AIAN, MULTI, na.rm = T))

bg20_moe <- Load_NHGISdf(nhgisYR = nhgis2020_bg, nhgis2020_MOE, 2020)

# Save interested vars
us_vulnerable20     <- bg20_df %>% dplyr::select(GISJOIN, STATE, POP, FISH, POVERTY200, INCOME20, NOHISCH, NOENG) %>% data.frame()
summary(us_vulnerable20)

