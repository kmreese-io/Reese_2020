###############################################################################################
## CHECK DAM AGRICULTURE ON THE MESA VERDE CUESTA
## KELSEY M. REESE
## SUBMITTED FOR REVIEW
## YEAR VOL(NUM): PGS-PGS
###############################################################################################

## ANALYSIS ##

## Fill depressions in DEM and calculate streams
rgrass7::execGRASS('r.fill.dir',flags='overwrite',parameters=list(input='grass.dem',output='mv.dem.fill.dir',direction='mv.dem.direction'))
rgrass7::execGRASS('r.watershed',flags='overwrite',parameters=list(elevation='mv.dem.fill.dir',accumulation='mv.dem.accumulation'))
rgrass7::execGRASS('r.stream.extract',flags='overwrite',parameters=list(elevation='mv.dem.fill.dir',threshold=2,stream_raster='mv.dem.stream.raster',stream_vector='mv.dem.stream.vector'))

## Export stream results into R environment, crop to study area, and save
mv.stream.raster <- rgrass7::readRAST('mv.dem.stream.raster')
mv.channels <- raster::crop(mv.stream.raster,mv.uplift)
rgdal::writeOGR(mv.channels,'./output/spatial-products',layer='channel-networks-total',driver='ESRI Shapefile')

## Load stream results calculated above, and create slope raster based on DEM
mv.channels <- rgdal::readOGR('./output/spatial-products',layer='channel-networks-total')
mv.slope.degrees <- raster::terrain(mv.dem,opt='slope',unit='degrees',neighbors=8)
mv.channel.list <- lapply(c(1:99998),FUN=function(x,...) { mv.channels[which(mv.channels$mv_dm__ == x ),] } )
mv.channel.slope <- lapply(c(1:length(mv.channel.list)),FUN=function(x,...) { try( raster::extract(mv.slope.degrees,mv.channel.list[[x]]) ) } )
mv.channel.mean <- lapply(c(1:length(mv.channel.slope)),FUN=function(x,...) { try( mean(unlist(mv.channel.slope[x])) ) } )
mv.channel.union <- lapply(c(1:length(mv.channel.list)),FUN=function(x,...) { raster::union(mv.channel.list[[x]]) } )

condition <- sapply(c(1:length(mv.channel.mean)), function(x) mv.channel.mean[x][[1]] > mv.checkdam.slope.min & mv.channel.mean[x][[1]] < mv.checkdam.slope.max )
slope.range <- mv.channel.union[condition]
channel.mean <- mv.channel.mean[condition]
channel.mean <- as.data.frame(unlist(channel.mean))
mv.checkdam.polygons <- unlist(unlist(slope.range))

mv.checkdam.lines <- sapply(c(1:length(mv.checkdam.polygons)),function(x) as(mv.checkdam.polygons[[x]],'SpatialLinesDataFrame') )
mv.checkdam.df.setup <- lapply(c(1:length(mv.checkdam.lines)),function(x) aggregate(mv.checkdam.lines[[x]],dissolve=T) )
mv.checkdam.line.length <- lapply(c(1:length(mv.checkdam.df.setup)),function(x) rgeos::gLength(mv.checkdam.df.setup[[x]],byid=T) )
line.length <- as.data.frame(unlist(mv.checkdam.line.length))

for(i in 1:length(mv.checkdam.df.setup)) {
  mv.checkdam.df.setup[[i]]$slope.average <- channel.mean[i,]
}

for(i in 1:length(mv.checkdam.df.setup)) {
  mv.checkdam.df.setup[[i]]$channel.length <- line.length[i,]
}

mv.checkdam.sldf <- do.call(rbind,mv.checkdam.df.setup)
rgdal::writeOGR(mv.checkdam.sldf,'./output/spatial-products',layer='channel-networks-sloperange',driver='ESRI Shapefile',overwrite=T)
mv.channels <- rgdal::readOGR('./output/spatial-products',layer='channel-networks-sloperange')
mv.checkdam.channels <- raster::mask(mv.dem,mv.channels)
raster::writeRaster(mv.checkdam.channels,'./output/spatial-products/mv_checkdam_channels',overwrite=T)

## Load raster with all potential check dam cells
mv.channels <- rgdal::readOGR('./output/spatial-products',layer='channel-networks-sloperange')
projection(mv.channels) <- master.projection
mv.checkdam.channels <- raster::raster('./output/spatial-products/cuesta-channel-networks-sloperange')
mv.checkdam.channels.longlat <- raster::projectRaster(mv.checkdam.channels,crs=longlat.projection)

## Total area of potential check dam cells that meet slope requirements
max.n.checkdams <- sum(round(mv.channels$chnnl_l / mv.checkdam.length))
mv.checkdam.m2 <- max.n.checkdams * (mv.checkdam.length * mv.checkdam.width)
mv.checkdam.km2 <- mv.checkdam.m2 / 1000000

###############################################################################
## Paleoproductivity

growing.niche <- paleocar(template = extent(mv.checkdam.channels.longlat),
                          label = 'paleoproductivity',
                          raw.dir = './output/spatial-products/paleocar/raw',
                          extraction.dir = paste0('./output/spatial-products/paleocar/extractions/'),
                          prcp_threshold = 350,
                          gdd_threshold = 1800,
                          years = 1:2000,
                          force.redo = F)

###############################################################################
## Paleoproductivity results for AD 880--1280

## Estimating the total population on the cuesta given known population numbers in surveyed areas
mv.uplift.boundary <- rgeos::gIntersection(mv.uplift,mv.boundary)
projection(mv.uplift.boundary) <- master.projection

surveyed.area.proportion <- area(mv.uplift) / area(mv.uplift.boundary)

known.population <- utils::read.csv('../DATABASE/TABLES/Reese_et_al_2019-AA.csv')
pop.890_920 <- round(known.population[which(known.population$modeling.phase == 10),]$momentary.households * surveyed.area.proportion)
pop.921_980 <- round(known.population[which(known.population$modeling.phase == 11),]$momentary.households * surveyed.area.proportion)
pop.981_1020 <- round(known.population[which(known.population$modeling.phase == 12),]$momentary.households * surveyed.area.proportion)
pop.1021_1060 <- round(known.population[which(known.population$modeling.phase == 13),]$momentary.households * surveyed.area.proportion)
pop.1061_1100 <- round(known.population[which(known.population$modeling.phase == 14),]$momentary.households * surveyed.area.proportion)
pop.1101_1140 <- round(known.population[which(known.population$modeling.phase == 15),]$momentary.households * surveyed.area.proportion)
pop.1141_1180 <- round(known.population[which(known.population$modeling.phase == 16),]$momentary.households * surveyed.area.proportion)
pop.1181_1225 <- round(known.population[which(known.population$modeling.phase == 17),]$momentary.households * surveyed.area.proportion)
pop.1226_1260 <- round(known.population[which(known.population$modeling.phase == 18),]$momentary.households * surveyed.area.proportion)
pop.1261_1285 <- round(known.population[which(known.population$modeling.phase == 19),]$momentary.households * surveyed.area.proportion)

est.momentary.population <- c(base::rep(pop.890_920,31),
                              base::rep(pop.921_980,60),
                              base::rep(pop.981_1020,40),
                              base::rep(pop.1021_1060,40),
                              base::rep(pop.1061_1100,40),
                              base::rep(pop.1101_1140,40),
                              base::rep(pop.1141_1180,40),
                              base::rep(pop.1181_1225,45),
                              base::rep(pop.1226_1260,35),
                              base::rep(pop.1261_1285,25))

## Calculate the efficacy of check dams by year and percentage of population supported
study.years.niche <- growing.niche[[890:1285]]
study.years.niche[study.years.niche < 1] <- NA
niche.projection <- raster::projectRaster(study.years.niche[[1:396]],crs=master.projection,method='ngb')
foreach::registerDoSEQ()
niche.list <- spatial.tools::brickstack_to_raster_list(niche.projection)
projection(mv.channels) <- master.projection
projection(mv.uplift.boundary) <- master.projection
channels.mv.uplift.boundary <- raster::intersect(mv.channels,mv.uplift.boundary)

for(i in c(1:length(niche.list))) {
  niche.polygon <- try(raster::rasterToPolygons(niche.list[[i]],fun=function(x){x==1}))
  niche.channels <- try(raster::intersect(channels.mv.uplift.boundary,niche.polygon))
  productive.channel.length <- try(rgeos::gLength(niche.channels))
  n.checkdams.niche <- try(round(productive.channel.length / mv.checkdam.length))
  checkdam.totals.m2 <- try(n.checkdams.niche * (mv.checkdam.length * mv.checkdam.width))
  checkdam.totals.km2 <- try(checkdam.totals.m2 / 1000000)
  total.n.households.supported <- try(round(checkdam.totals.km2 / (0.020 * 4.65) ))
  base::source('./script/Reese-results.R')
  rm(niche.resample,niche.extent,niche.mv.uplift,niche.polygon,niche.channels,productive.channel.length,n.checkdams.niche,checkdam.totals.m2,checkdam.totals.km2,total.n.households.supported)
  gc()
}

###############################################################################
## Cost-distance investment of each known household to check dams

mv.channels <- rgdal::readOGR('./output/spatial-products',layer='channel-networks-sloperange')
max.n.checkdams <- sum(round(mv.channels$chnnl_l / mv.checkdam.length))
mv.checkdam.m2 <- max.n.checkdams * (mv.checkdam.length * mv.checkdam.width)
mv.checkdam.km2 <- mv.checkdam.m2 / 1000000

checkdamProximity <- function(x,...) {
  mv.checkdam.points <- as(mv.channels,'SpatialPointsDataFrame')
  mv.uplift.boundary <- rgeos::gIntersection(mv.uplift,mv.boundary)
  projection(mv.checkdam.points) <- master.projection
  projection(mv.uplift.boundary) <- master.projection
  mv.checkdam.points.masked <- mv.checkdam.points[mv.uplift.boundary,]
  n.points.per.checkdam <- length(mv.checkdam.points) / max.n.checkdams
  # n.points <- round((1991 / n.points.per.checkdam) * 0.6086956522) # total storage amount reported (Burns 1983; Parsons 1936)
  n.points <- round((1991 / n.points.per.checkdam) * (0.6086956522 / 2) ) # conservative estimate of total storage amount reported (Burns 1983; Parsons 1936)
  
  for(i in 1:momentary.households) {
    euclidean.distance <- as.matrix(raster::pointDistance(ph.sample[i,],as.matrix(coordinates(mv.checkdam.points.masked)),lonlat=F))
    euclidean.distance.coords <- cbind(euclidean.distance,coordinates(mv.checkdam.points.masked))
    euclidean.distance.sorted <- as.matrix(euclidean.distance.coords[order(euclidean.distance.coords[,1]),])
    mv.checkdams.needed <- as.matrix(euclidean.distance.sorted[1:n.points,2:3])
    checkdam.annual.productivity <- raster::extract(niche.projection[[years]],mv.checkdams.needed)
    checkdam.annual.percent <- as.matrix(apply(checkdam.annual.productivity,2,function(x) {sum(x,na.rm=T) / n.points}))
    mv.checkdam.points.productivity <- base::cbind(i,phase.number,t(checkdam.annual.percent[,1]))
    write.table(mv.checkdam.points.productivity,file='./output/results/checkdam-productivity-individual.csv',append=T,row.names=F,col.names=F,sep=',')
    checkdams.unique <- unique(mv.checkdams.needed[,1:2])
    site.to.checkdam <- as.matrix(raster::pointDistance(ph.sample[i,],checkdams.unique[1,],lonlat=F))
    checkdam.distances <- matrix(NA,nrow=nrow(checkdams.unique)-1,ncol=1)
    
    for(j in 1:(nrow(checkdams.unique)-1)) {
      remaining.checkdams.distance <- as.matrix(raster::pointDistance(checkdams.unique[j,],checkdams.unique[-(1:j),],lonlat=F))
      remaining.checkdams.coords <- cbind(remaining.checkdams.distance,checkdams.unique[-(1:j),])
      remaining.checkdams.sorted <- as.matrix(remaining.checkdams.coords[order(remaining.checkdams.coords[,1]),])
      checkdam.distances[j,1] <- remaining.checkdams.sorted[1,1]
    }
    
    checkdam.to.site <- raster::pointDistance(checkdams.unique[nrow(checkdams.unique),],ph.sample[i,],lonlat=F)
    mv.checkdams.distance.total <- base::sum(base::sum(site.to.checkdam,checkdam.distances[,1],checkdam.to.site)) / 1000
    on.path.cost.minutes <- ((mv.checkdams.distance.total / 5.036742) * 60)
    off.path.cost.minutes <- ((mv.checkdams.distance.total / 3.022045) * 60)
    results <- cbind(i,phase.number,momentary.households,mv.checkdams.distance.total,on.path.cost.minutes,off.path.cost.minutes)
    write.table(results,file='./output/results/checkdam-cost-distances.csv',append=T,row.names=F,col.names=F,sep=',')
    gc()
    mv.checkdam.points.masked <- euclidean.distance.sorted[-1:-n.points,-1]
  }
}

# A.D. 890--920
years <- (890-889):(920-889)
row.number <- 6
phase.number <- 10
phase.households <- hh.coordinates[which(hh.coordinates$ph10 >= 1),]
all.households <- phase.households[rep(seq_len(dim(phase.households)[1]),phase.households$ph10),]
momentary.households <- round((summary.information$use.life[row.number] / summary.information$phase.length[row.number]) * nrow(all.households))
ph.sample <- coordinates(all.households[sample(nrow(all.households),momentary.households,replace=F),])
ph.sample <- as.matrix(ph.sample[order(ph.sample[,1],decreasing=T),])
replicate(summary.information$n.iterations[row.number],checkdamProximity())

# A.D. 921--980
years <- (921-889):(980-889)
row.number <- 7
phase.number <- 11
phase.households <- hh.coordinates[which(hh.coordinates$ph11 >= 1),]
all.households <- phase.households[rep(seq_len(dim(phase.households)[1]),phase.households$ph11),]
momentary.households <- round((summary.information$use.life[row.number] / summary.information$phase.length[row.number]) * nrow(all.households))
ph.sample <- coordinates(all.households[sample(nrow(all.households),momentary.households,replace=F),])
ph.sample <- as.matrix(ph.sample[order(ph.sample[,1],decreasing=T),])
replicate(summary.information$n.iterations[row.number],checkdamProximity())

# A.D. 981--1020
years <- (981-889):(1020-889)
row.number <- 8
phase.number <- 12
phase.households <- hh.coordinates[which(hh.coordinates$ph12 >= 1),]
all.households <- phase.households[rep(seq_len(dim(phase.households)[1]),phase.households$ph12),]
momentary.households <- round((summary.information$use.life[row.number] / summary.information$phase.length[row.number]) * nrow(all.households))
ph.sample <- coordinates(all.households[sample(nrow(all.households),momentary.households,replace=F),])
ph.sample <- as.matrix(ph.sample[order(ph.sample[,1],decreasing=T),])
replicate(summary.information$n.iterations[row.number],checkdamProximity())

# A.D. 1021--1060
years <- (1021-889):(1060-889)
row.number <- 9
phase.number <- 13
phase.households <- hh.coordinates[which(hh.coordinates$ph13 >= 1),]
all.households <- phase.households[rep(seq_len(dim(phase.households)[1]),phase.households$ph13),]
momentary.households <- round((summary.information$use.life[row.number] / summary.information$phase.length[row.number]) * nrow(all.households))
ph.sample <- coordinates(all.households[sample(nrow(all.households),momentary.households,replace=F),])
ph.sample <- as.matrix(ph.sample[order(ph.sample[,1],decreasing=T),])
replicate(summary.information$n.iterations[row.number],checkdamProximity())

# A.D. 1061--1100
years <- (1061-889):(1100-889)
row.number <- 10
phase.number <- 14
phase.households <- hh.coordinates[which(hh.coordinates$ph14 >= 1),]
all.households <- phase.households[rep(seq_len(dim(phase.households)[1]),phase.households$ph14),]
momentary.households <- round((summary.information$use.life[row.number] / summary.information$phase.length[row.number]) * nrow(all.households))
ph.sample <- coordinates(all.households[sample(nrow(all.households),momentary.households,replace=F),])
ph.sample <- as.matrix(ph.sample[order(ph.sample[,1],decreasing=T),])
replicate(summary.information$n.iterations[row.number],checkdamProximity())

# A.D. 1101--1140
years <- (1101-889):(1140-889)
row.number <- 11
phase.number <- 15
phase.households <- hh.coordinates[which(hh.coordinates$ph15 >= 1),]
all.households <- phase.households[rep(seq_len(dim(phase.households)[1]),phase.households$ph15),]
momentary.households <- round((summary.information$use.life[row.number] / summary.information$phase.length[row.number]) * nrow(all.households))
ph.sample <- coordinates(all.households[sample(nrow(all.households),momentary.households,replace=F),])
ph.sample <- as.matrix(ph.sample[order(ph.sample[,1],decreasing=T),])
replicate(summary.information$n.iterations[row.number],checkdamProximity())

# A.D. 1141--1180
years <- (1141-889):(1180-889)
row.number <- 12
phase.number <- 16
phase.households <- hh.coordinates[which(hh.coordinates$ph16 >= 1),]
all.households <- phase.households[rep(seq_len(dim(phase.households)[1]),phase.households$ph16),]
momentary.households <- round((summary.information$use.life[row.number] / summary.information$phase.length[row.number]) * nrow(all.households))
ph.sample <- coordinates(all.households[sample(nrow(all.households),momentary.households,replace=F),])
ph.sample <- as.matrix(ph.sample[order(ph.sample[,1],decreasing=T),])
replicate(summary.information$n.iterations[row.number],checkdamProximity())

# A.D. 1181--1225
years <- (1181-889):(1225-889)
row.number <- 13
phase.number <- 17
phase.households <- hh.coordinates[which(hh.coordinates$ph17 >= 1),]
all.households <- phase.households[rep(seq_len(dim(phase.households)[1]),phase.households$ph17),]
momentary.households <- round((summary.information$use.life[row.number] / summary.information$phase.length[row.number]) * nrow(all.households))
ph.sample <- coordinates(all.households[sample(nrow(all.households),momentary.households,replace=F),])
ph.sample <- as.matrix(ph.sample[order(ph.sample[,1],decreasing=T),])
replicate(summary.information$n.iterations[row.number],checkdamProximity())

# A.D. 1226--1260
years <- (1226-889):(1260-889)
row.number <- 14
phase.number <- 18
phase.households <- hh.coordinates[which(hh.coordinates$ph18 >= 1),]
all.households <- phase.households[rep(seq_len(dim(phase.households)[1]),phase.households$ph18),]
momentary.households <- round((summary.information$use.life[row.number] / summary.information$phase.length[row.number]) * nrow(all.households))
ph.sample <- coordinates(all.households[sample(nrow(all.households),momentary.households,replace=F),])
ph.sample <- as.matrix(ph.sample[order(ph.sample[,1],decreasing=T),])
replicate(summary.information$n.iterations[row.number],checkdamProximity())

# A.D. 1261--1285
years <- (1261-889):(1285-889)
row.number <- 15
phase.number <- 19
phase.households <- hh.coordinates[which(hh.coordinates$ph19 >= 1),]
all.households <- phase.households[rep(seq_len(dim(phase.households)[1]),phase.households$ph19),]
momentary.households <- round((summary.information$use.life[row.number] / summary.information$phase.length[row.number]) * nrow(all.households))
ph.sample <- coordinates(all.households[sample(nrow(all.households),momentary.households,replace=F),])
ph.sample <- as.matrix(ph.sample[order(ph.sample[,1],decreasing=T),])
replicate(summary.information$n.iterations[row.number],checkdamProximity())

cost.distance <- read.csv('./output/results/checkdam-cost-distances.csv',header=F)

###############################################################################
## Average cost-distance for on- and off-path travel to entire check dam networks

# A.D. 895--920
years <- 890:920
phase.number <- 10
phase.cost.distance <- cost.distance[which(cost.distance[,2] == phase.number ),]
avg.on.path.costdistance <- mean(phase.cost.distance[,5])
avg.off.path.costdistance <- mean(phase.cost.distance[,6])
path.distances <- cbind(years,rep(avg.on.path.costdistance,length(years)),rep(avg.off.path.costdistance,length(years)))

write.table(path.distances,file='./output/results/checkdam-cost-travel-time.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 921--980
years <- 921:980
phase.number <- 11
phase.cost.distance <- cost.distance[which(cost.distance[,2] == phase.number ),]
avg.on.path.costdistance <- mean(phase.cost.distance[,5])
avg.off.path.costdistance <- mean(phase.cost.distance[,6])
path.distances <- cbind(years,rep(avg.on.path.costdistance,length(years)),rep(avg.off.path.costdistance,length(years)))

write.table(path.distances,file='./output/results/checkdam-cost-travel-time.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 981--1020
years <- 981:1020
phase.number <- 12
phase.cost.distance <- cost.distance[which(cost.distance[,2] == phase.number ),]
avg.on.path.costdistance <- mean(phase.cost.distance[,5])
avg.off.path.costdistance <- mean(phase.cost.distance[,6])
path.distances <- cbind(years,rep(avg.on.path.costdistance,length(years)),rep(avg.off.path.costdistance,length(years)))

write.table(path.distances,file='./output/results/checkdam-cost-travel-time.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1021--1060
years <- 1021:1060
phase.number <- 13
phase.cost.distance <- cost.distance[which(cost.distance[,2] == phase.number ),]
avg.on.path.costdistance <- mean(phase.cost.distance[,5])
avg.off.path.costdistance <- mean(phase.cost.distance[,6])
path.distances <- cbind(years,rep(avg.on.path.costdistance,length(years)),rep(avg.off.path.costdistance,length(years)))

write.table(path.distances,file='./output/results/checkdam-cost-travel-time.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1061--1100
years <- 1061:1100
phase.number <- 14
phase.cost.distance <- cost.distance[which(cost.distance[,2] == phase.number ),]
avg.on.path.costdistance <- mean(phase.cost.distance[,5])
avg.off.path.costdistance <- mean(phase.cost.distance[,6])
path.distances <- cbind(years,rep(avg.on.path.costdistance,length(years)),rep(avg.off.path.costdistance,length(years)))

write.table(path.distances,file='./output/results/checkdam-cost-travel-time.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1101--1140
years <- 1101:1140
phase.number <- 15
phase.cost.distance <- cost.distance[which(cost.distance[,2] == phase.number ),]
avg.on.path.costdistance <- mean(phase.cost.distance[,5])
avg.off.path.costdistance <- mean(phase.cost.distance[,6])
path.distances <- cbind(years,rep(avg.on.path.costdistance,length(years)),rep(avg.off.path.costdistance,length(years)))

write.table(path.distances,file='./output/results/checkdam-cost-travel-time.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1141--1180
years <- 1141:1180
phase.number <- 16
phase.cost.distance <- cost.distance[which(cost.distance[,2] == phase.number ),]
avg.on.path.costdistance <- mean(phase.cost.distance[,5])
avg.off.path.costdistance <- mean(phase.cost.distance[,6])
path.distances <- cbind(years,rep(avg.on.path.costdistance,length(years)),rep(avg.off.path.costdistance,length(years)))

write.table(path.distances,file='./output/results/checkdam-cost-travel-time.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1181--1225
years <- 1181:1225
phase.number <- 17
phase.cost.distance <- cost.distance[which(cost.distance[,2] == phase.number ),]
avg.on.path.costdistance <- mean(phase.cost.distance[,5])
avg.off.path.costdistance <- mean(phase.cost.distance[,6])
path.distances <- cbind(years,rep(avg.on.path.costdistance,length(years)),rep(avg.off.path.costdistance,length(years)))

write.table(path.distances,file='./output/results/checkdam-cost-travel-time.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1226--1260
years <- 1226:1260
phase.number <- 18
phase.cost.distance <- cost.distance[which(cost.distance[,2] == phase.number ),]
avg.on.path.costdistance <- mean(phase.cost.distance[,5])
avg.off.path.costdistance <- mean(phase.cost.distance[,6])
path.distances <- cbind(years,rep(avg.on.path.costdistance,length(years)),rep(avg.off.path.costdistance,length(years)))

write.table(path.distances,file='./output/results/checkdam-cost-travel-time.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1261--1285
years <- 1261:1285
phase.number <- 19
phase.cost.distance <- cost.distance[which(cost.distance[,2] == phase.number ),]
avg.on.path.costdistance <- mean(phase.cost.distance[,5])
avg.off.path.costdistance <- mean(phase.cost.distance[,6])
path.distances <- cbind(years,rep(avg.on.path.costdistance,length(years)),rep(avg.off.path.costdistance,length(years)))

write.table(path.distances,file='./output/results/checkdam-cost-travel-time.csv',append=T,row.names=F,col.names=F,sep=',')

###############################################################################
## Paleo-productivity of each check dam used in cost-distance analysis

checkdam.network.productivity <- as.matrix(read.csv('./output/results/checkdam-productivity-individual.csv',col.names=paste0('V',seq_len(62)),header=F,fill=T))

# A.D. 890--920
years <- 890:920
phase.number <- 10
phase.network.productivity <- checkdam.network.productivity[which(checkdam.network.productivity[,2] == phase.number ),]
avg.percent.network.productivity <- cbind(years,as.matrix((colMeans(phase.network.productivity[,3:(length(years)+2)])*100),ncol=1))

write.table(avg.percent.network.productivity,file='./output/results/checkdam-productivity-network.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 921--980
years <- 921:980
phase.number <- 11
phase.network.productivity <- checkdam.network.productivity[which(checkdam.network.productivity[,2] == phase.number ),]
avg.percent.network.productivity <- cbind(years,as.matrix((colMeans(phase.network.productivity[,3:(length(years)+2)])*100),ncol=1))

write.table(avg.percent.network.productivity,file='./output/results/checkdam-productivity-network.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 981--1020
years <- 981:1020
phase.number <- 12
phase.network.productivity <- checkdam.network.productivity[which(checkdam.network.productivity[,2] == phase.number ),]
avg.percent.network.productivity <- cbind(years,as.matrix((colMeans(phase.network.productivity[,3:(length(years)+2)])*100),ncol=1))

write.table(avg.percent.network.productivity,file='./output/results/checkdam-productivity-network.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1021--1060
years <- 1021:1060
phase.number <- 13
phase.network.productivity <- checkdam.network.productivity[which(checkdam.network.productivity[,2] == phase.number ),]
avg.percent.network.productivity <- cbind(years,as.matrix((colMeans(phase.network.productivity[,3:(length(years)+2)])*100),ncol=1))

write.table(avg.percent.network.productivity,file='./output/results/checkdam-productivity-network.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1061--1100
years <- 1061:1100
phase.number <- 14
phase.network.productivity <- checkdam.network.productivity[which(checkdam.network.productivity[,2] == phase.number ),]
avg.percent.network.productivity <- cbind(years,as.matrix((colMeans(phase.network.productivity[,3:(length(years)+2)])*100),ncol=1))

write.table(avg.percent.network.productivity,file='./output/results/checkdam-productivity-network.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1101--1140
years <- 1101:1140
phase.number <- 15
phase.network.productivity <- checkdam.network.productivity[which(checkdam.network.productivity[,2] == phase.number ),]
avg.percent.network.productivity <- cbind(years,as.matrix((colMeans(phase.network.productivity[,3:(length(years)+2)])*100),ncol=1))

write.table(avg.percent.network.productivity,file='./output/results/checkdam-productivity-network.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1141--1180
years <- 1141:1180
phase.number <- 16
phase.network.productivity <- checkdam.network.productivity[which(checkdam.network.productivity[,2] == phase.number ),]
avg.percent.network.productivity <- cbind(years,as.matrix((colMeans(phase.network.productivity[,3:(length(years)+2)])*100),ncol=1))

write.table(avg.percent.network.productivity,file='./output/results/checkdam-productivity-network.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1181--1225
years <- 1181:1225
phase.number <- 17
phase.network.productivity <- checkdam.network.productivity[which(checkdam.network.productivity[,2] == phase.number ),]
avg.percent.network.productivity <- cbind(years,as.matrix((colMeans(phase.network.productivity[,3:(length(years)+2)])*100),ncol=1))

write.table(avg.percent.network.productivity,file='./output/results/checkdam-productivity-network.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1226--1260
years <- 1226:1260
phase.number <- 18
phase.network.productivity <- checkdam.network.productivity[which(checkdam.network.productivity[,2] == phase.number ),]
avg.percent.network.productivity <- cbind(years,as.matrix((colMeans(phase.network.productivity[,3:(length(years)+2)])*100),ncol=1))

write.table(avg.percent.network.productivity,file='./output/results/checkdam-productivity-network.csv',append=T,row.names=F,col.names=F,sep=',')

# A.D. 1261--1285
years <- 1261:1285
phase.number <- 19
phase.network.productivity <- checkdam.network.productivity[which(checkdam.network.productivity[,2] == phase.number ),]
avg.percent.network.productivity <- cbind(years,as.matrix((colMeans(phase.network.productivity[,3:(length(years)+2)])*100),ncol=1))

write.table(avg.percent.network.productivity,file='./output/results/checkdam-productivity-network.csv',append=T,row.names=F,col.names=F,sep=',')

###############################################################################################
###############################################################################################