###############################################################################################
## CHECK DAM AGRICULTURE ON THE MESA VERDE CUESTA
## KELSEY M. REESE
## SUBMITTED FOR REVIEW
## YEAR VOL(NUM): PGS-PGS
###############################################################################################

## MASTER ##

setwd('~/Reese_2020/')

dir.create('./output/spatial-products/paleocar',recursive=T,showWarnings=F)
dir.create('./output/results/',recursive=T,showWarnings=F)
dir.create('./output/figures/',recursive=T,showWarnings=F)

## FUNCTIONS AND ENVIRONMENT
base::source('./FUNCTIONS/polygonUTM_NAD83.R')
base::source('./script/Reese-environment.R')
base::source('./FUNCTIONS/paleocar.R')
base::source('./FUNCTIONS/costRaster.R')

## RESULTS
base::source('./script/Reese-results.R')

## PLOTS
base::source('./script/Reese-figures.R')

###############################################################################################
###############################################################################################