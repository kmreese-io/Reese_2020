###############################################################################################
## CHECK DAM AGRICULTURE ON THE MESA VERDE CUESTA
## KELSEY M. REESE
## SUBMITTED FOR REVIEW
## YEAR VOL(NUM): PGS-PGS
###############################################################################################

## RESULTS ##

## Year of check dam estimate
year <- 889 + i

## Percent of total estimated momentary households that could be supported for one year using only check dam agriculture
percent.hh.checkdam.supported <- (total.n.households.supported/est.momentary.population[i]) * 100
percent.hh.checkdam.supported[percent.hh.checkdam.supported > 100] <- 100

## Compiling and exporting the results
results <- base::cbind(year,est.momentary.population[i],max.n.checkdams,n.checkdams.niche,total.n.households.supported,percent.hh.checkdam.supported)
utils::write.table(results,'./output/results/results-households-supported.csv',append=T,col.names=F,row.names=F,sep=',')

## Compiling and exporting information for Table 1
modeling.phase <- c(10:19)
period.start <- c(880,920,980,1020,1060,1100,1140,1180,1225,1260)
period.end <- c(920,980,1020,1060,1100,1140,1180,1225,1260,1280)

known.households <- c(sum(hh.coordinates[which(hh.coordinates$ph10 >= 1 ),]$ph10),
                      sum(hh.coordinates[which(hh.coordinates$ph11 >= 1 ),]$ph11),
                      sum(hh.coordinates[which(hh.coordinates$ph12 >= 1 ),]$ph12),
                      sum(hh.coordinates[which(hh.coordinates$ph13 >= 1 ),]$ph13),
                      sum(hh.coordinates[which(hh.coordinates$ph14 >= 1 ),]$ph14),
                      sum(hh.coordinates[which(hh.coordinates$ph15 >= 1 ),]$ph15),
                      sum(hh.coordinates[which(hh.coordinates$ph16 >= 1 ),]$ph16),
                      sum(hh.coordinates[which(hh.coordinates$ph17 >= 1 ),]$ph17),
                      sum(hh.coordinates[which(hh.coordinates$ph18 >= 1 ),]$ph18),
                      sum(hh.coordinates[which(hh.coordinates$ph19 >= 1 ),]$ph19))

known.momentized.households <- c(round(sum(hh.coordinates[which(hh.coordinates$ph10 >= 1 ),]$ph10) * (18/40)),
                                 round(sum(hh.coordinates[which(hh.coordinates$ph11 >= 1 ),]$ph11) * (18/60)),
                                 round(sum(hh.coordinates[which(hh.coordinates$ph12 >= 1 ),]$ph12) * (18/40)),
                                 round(sum(hh.coordinates[which(hh.coordinates$ph13 >= 1 ),]$ph13) * (21/40)),
                                 round(sum(hh.coordinates[which(hh.coordinates$ph14 >= 1 ),]$ph14) * (21/40)),
                                 round(sum(hh.coordinates[which(hh.coordinates$ph15 >= 1 ),]$ph15) * (40/40)),
                                 round(sum(hh.coordinates[which(hh.coordinates$ph16 >= 1 ),]$ph16) * (40/40)),
                                 round(sum(hh.coordinates[which(hh.coordinates$ph17 >= 1 ),]$ph17) * (45/45)),
                                 round(sum(hh.coordinates[which(hh.coordinates$ph18 >= 1 ),]$ph18) * (35/35)),
                                 round(sum(hh.coordinates[which(hh.coordinates$ph19 >= 1 ),]$ph19) * (20/20)))

estimated.momentized.cuesta.households <- c(round(sum(hh.coordinates[which(hh.coordinates$ph10 >= 1 ),]$ph10) * (18/40) * surveyed.area.proportion),
                                            round(sum(hh.coordinates[which(hh.coordinates$ph11 >= 1 ),]$ph11) * (18/60) * surveyed.area.proportion),
                                            round(sum(hh.coordinates[which(hh.coordinates$ph12 >= 1 ),]$ph12) * (18/40) * surveyed.area.proportion),
                                            round(sum(hh.coordinates[which(hh.coordinates$ph13 >= 1 ),]$ph13) * (21/40) * surveyed.area.proportion),
                                            round(sum(hh.coordinates[which(hh.coordinates$ph14 >= 1 ),]$ph14) * (21/40) * surveyed.area.proportion),
                                            round(sum(hh.coordinates[which(hh.coordinates$ph15 >= 1 ),]$ph15) * (40/40) * surveyed.area.proportion),
                                            round(sum(hh.coordinates[which(hh.coordinates$ph16 >= 1 ),]$ph16) * (40/40) * surveyed.area.proportion),
                                            round(sum(hh.coordinates[which(hh.coordinates$ph17 >= 1 ),]$ph17) * (45/45) * surveyed.area.proportion),
                                            round(sum(hh.coordinates[which(hh.coordinates$ph18 >= 1 ),]$ph18) * (35/35) * surveyed.area.proportion),
                                            round(sum(hh.coordinates[which(hh.coordinates$ph19 >= 1 ),]$ph19) * (20/20) * surveyed.area.proportion))

estimated.total.cuesta.population <- c(round(estimated.momentized.cuesta.households * mean(c(3.3,6)))) # people per household are estimated from Kohler 2012 (3.3) and the average of the range given by Lightfoot 1994 of 5--7 people (6)

table.1 <- base::cbind(modeling.phase,period.start,period.end,known.households,known.momentized.households,estimated.momentized.cuesta.households,estimated.total.cuesta.population)
utils::write.csv(table.1,'./output/results/results-table-1.csv',row.names=F)

###############################################################################################
###############################################################################################