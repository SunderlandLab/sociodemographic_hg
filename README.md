# Modeling sociodemographic factors near coal-fired power plants
## Introduction
Source code for the paper:
Sociodemographic Disparities in Mercury Exposure from
U.S. Coal-Fired Power Plants
***[DOI: TBD](LINK TO PAPER TBD)***

## Authors
* [Mona Q. Dai](https://scholar.harvard.edu/monadai)
* Benjamin Geyman
* Xindi Hu
* Colin Thackray
* [Elsie M. Sunderland](https://bgc.seas.harvard.edu/)

##  Order and purpose of scripts
### Main Scripts
- 1_PP_vulnerable_buffer.Rmd: Explore whether the proportion of potentially vulnerable groups within    
                              buffers surrounding power plants change at differing buffer sizes.
                              
- 2_PP_active_retire.Rmd    : Explore whether the proportion of potentially vulnerable groups within the                               buffers surrounding power plants that are still active differ from those                                that have been retired since 2010.

- 3_Hg_deposition.Rmd       : Evaluate spatial clustering of Hg deposition & sociodemographic factors                                 across the US.


### Supporting Scripts
- census2020.R: Load & organize 2010 & 2020 US Census data from NHGIS.

- LoadCensus.R: Organize US Census data into vulnerable subgroups for central
                estimates & margin of error (MOE) vlaues.


### Functions
- f_census.R   : Group US Census data by vulnerable subgroups.

- f_LoadNHGIS.R: Load US Census data from NHGIS.

- f_spatial.R  : Apply spatial regression models.

- f_Misc.R     : Other functions to read & clean data, calculate statistics 
                 (margin of error [MOE], confidence interval [CI], etc.), 
                 plotting, statistical significance tests


### Necessary packages
- tidyverse
- tidycensus
- foreign
- cowplot
- spatstat
- sf
- terra
- rsample 
- lmtest
- raster
- ncdf4
- stars
- spatialreg
- spdep


### Data
All data files can be found on Harvard Dataverse
(https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/9X5GEH).

Download all files to the "data" directory to run the above scripts. 

US Census data files downloaded from NHGIS (https://www.nhgis.org/):
[tag: nhgis]
- nhgis0055_ds176_20105_blck_grp.csv, _codebook.txt : US Census block group sociodemographic factors & 
                                                      codebook for 2006-2010.
- nhgis0039_ds249_20205_blck_grp.csv, _codebook.txt : US Census block group sociodemographic factors & 
                                                      codebook for 2016-2020.
- US_blck_grp_2010.shp : US Census block group polygons for 2006-2010.
- US_blck_grp_2020.shp : US Census block group polygons for 2016-2020.
- States_shapefile.shp : US state polygons.

Simulated Hg deposition data from GEOS-Chem:
[tag: deposition]
- Total_2010.nc : Hg deposition from all sources for 2010.
- Total_2020.nc : Hg deposition from all sources for 2020.
- EGU_2010.nc   : Hg deposition from US electric generating units (EGUs) only in 2010.
- EGU_2020.nc   : Hg deposition from US electric generating units (EGUs) only in 2020.

Power plant & US census related data files:
[tag: plant_census]
- facility_gisjoin5.dbf        : List of overlapping 2010 coal-fired power plant IDs & 
                                 US Census block group IDs within 5km.
- facility2020_retire_0123.csv : List of coal-fired power plants & retirement status.
- facility2020_pt.shp          : Locations of coal-fired power plants as points.
- vulnerable20_tot.RDS         : Counts of 2020 vulnerable subgroups by census block group.
- us_vulnerable2020.shp        : Proportion of 2020 vulnerable subgroups by census block group as polygons.
  


