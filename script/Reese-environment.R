###############################################################################################
## CHECK DAM AGRICULTURE ON THE MESA VERDE CUESTA
## KELSEY M. REESE
## SUBMITTED FOR REVIEW
## YEAR VOL(NUM): PGS-PGS
###############################################################################################

## ENVIRONMENT ##

packages <-c('sp','gstat','rgdal','rgeos','raster','scales','FedData','rgrass7','paleocar','magrittr','topmodel','gdistance','grDevices','RColorBrewer','TeachingDemos','gdalUtils','utils','foreach','magrittr','ncdf4','spatialEco','spatial.tools','foreach','smoother')
for(p in packages) if(p %in% rownames(installed.packages()) == F) { install.packages(p) }
for(p in packages) suppressPackageStartupMessages(library(p,quietly=T,character.only=T))

## Environment universals
master.projection <- sp::CRS('+proj=utm +datum=NAD83 +zone=12')
longlat.projection <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84')
colors <- grDevices::colorRampPalette(c('black','white'))
heatcolors <- RColorBrewer::brewer.pal(11,'Spectral')

## Check dam characteristics, compiled from published literature (Hayes 1964; Rohn 1963, 1977; Stewart 1940a; Stewart and Donnelly 1943)
checkdam.characteristics <- read.csv('../DATABASE/TABLES/checkdam-characteristics.csv')
slope <- checkdam.characteristics[which(checkdam.characteristics$CHARACTERISTIC == 'grade' ),]
length <- checkdam.characteristics[which(checkdam.characteristics$CHARACTERISTIC == 'breadth' ),]
width <- checkdam.characteristics[which(checkdam.characteristics$CHARACTERISTIC == 'width' ),]

## Minimum and maximum reported check dam lengths and widths, converted from yard to meters
length.min.mean <- mean(length$VALUE_MIN,na.rm=T) / 1.0936
length.max.mean <- mean(length$VALUE_MAX,na.rm=T) / 1.0936
width.min.mean <- mean(width$VALUE_MIN,na.rm=T) / 1.0936
width.max.mean <- mean(width$VALUE_MAX,na.rm=T) / 1.0936

## The range of slopes are 1 standard deviation from the mean for reported check dams and are converted from percent to degrees
slope.degrees <- atan(slope$VALUE_MAX /100) * 180/pi
slope.degrees.mean <- mean(slope.degrees)
slope.degrees.sd <- sd(slope.degrees)

## Values used in analysis
mv.checkdam.slope.min <- slope.degrees.mean - slope.degrees.sd
mv.checkdam.slope.max <- slope.degrees.mean + slope.degrees.sd
mv.checkdam.length <- mean(c(length.min.mean,length.max.mean))
mv.checkdam.width <- mean(c(width.min.mean,width.max.mean))

## DEM data
mv.dem <- raster::raster('../DATABASE/SPATIAL/DEM/REGIONS/MESA-VERDE-UPLIFT/mv_dem')
mv.dem.longlat <- raster::projectRaster(mv.dem,crs=longlat.projection)
mv.extent.longlat <- raster::extent(xmin(mv.dem.longlat),xmax(mv.dem.longlat),ymin(mv.dem.longlat),ymax(mv.dem.longlat))
mv.slope.degrees <- raster::raster('../DATABASE/SPATIAL/DEM/REGIONS/MESA-VERDE-UPLIFT/mv_slope_degrees')
mv.aspect.degrees <- raster::raster('../DATABASE/SPATIAL/DEM/REGIONS/MESA-VERDE-UPLIFT/mv_aspect_degrees')
mv.hillshade <- raster::raster('../DATABASE/SPATIAL/DEM/REGIONS/MESA-VERDE-UPLIFT/mv_hillshade')

## SHAPEFILE data
mv.uplift <- rgdal::readOGR('../DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/boundaries-regional',layer='mv-uplift')
mv.boundary <- rgdal::readOGR('../DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/boundaries-national-parks',layer='mv-np-boundary')
state.boundaries <- rgdal::readOGR('../DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/states-united-states',layer='us-state-boundaries')
mv.uplift.longlat <- sp::spTransform(mv.uplift,longlat.projection)
mv.study.extent <- spTransform(polygonUTM_NAD83(UTM_North=ymax(mv.uplift)+750,UTM_South=ymin(mv.uplift)-750,UTM_East=xmin(mv.uplift)-750,UTM_West=xmax(mv.uplift)+750,UTM_Zone=12),master.projection)

## CULTURAL data
summary.information <- utils::read.csv('../DATABASE/TABLES/db_mv-population-overview.csv')
hh.information <- utils::read.csv('../DATABASE/TABLES/database-vepii-n-uplift-habitations.csv')
hh.coordinates <- base::matrix(NA,nrow=nrow(hh.information),ncol=2)
hh.coordinates[,1] <- hh.information$UTMEast
hh.coordinates[,2] <- hh.information$UTMNorth
hh.coordinates <- sp::SpatialPointsDataFrame(coords=hh.coordinates,hh.information,proj4string=master.projection)
mv.uplift.boundary <- rgeos::gIntersection(mv.uplift,mv.boundary)
projection(mv.uplift.boundary) <- master.projection
hh.coordinates <- hh.coordinates[mv.uplift.boundary,]
mv.extent <- raster::extent(xmin(mv.uplift)-750,xmax(mv.uplift)+750,ymin(mv.uplift)-750,ymax(mv.uplift)+750)

## Recorded checkdam data on the Mesa Verde cuesta within Mesa Verde National Park
vepii.site.attributes <- utils::read.csv('../DATABASE/TABLES/vepii-database/vepii-site-attributes.csv')
vepii.site.features <- utils::read.csv('../DATABASE/TABLES/vepii-database/vepii-site-features.csv')

vepii.cultural.features <- tibble::as_tibble(base::merge(vepii.site.attributes,vepii.site.features,by='Site.ID'))
cultural.features <- vepii.cultural.features[,c('Site.ID','Check.Dam')]

cuesta.sites <- utils::read.csv('../DATABASE/TABLES/database-vepii-n-uplift.csv')
colnames(cuesta.sites)[1] <- 'Site.ID'

cuesta.architectural.attributes <- base::merge(cuesta.sites,cultural.features,by='Site.ID')

hh.coordinates <- base::matrix(NA,nrow=nrow(cuesta.architectural.attributes),ncol=2)
hh.coordinates[,1] <- cuesta.architectural.attributes$UTMEast
hh.coordinates[,2] <- cuesta.architectural.attributes$UTMNorth
hh.coordinates <- sp::SpatialPointsDataFrame(coords=hh.coordinates,cuesta.architectural.attributes,proj4string=master.projection)

mv.uplift <- rgdal::readOGR('../DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/boundaries-regional',layer='mv-uplift')
mv.boundary <- rgdal::readOGR('../DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/boundaries-national-parks',layer='mv-np-boundary')

mv.uplift.boundary <- rgeos::gIntersection(mv.uplift,mv.boundary)
raster::projection(mv.uplift.boundary) <- master.projection
hh.coordinates <- hh.coordinates[mv.uplift.boundary,]

n.recorded.checkdams <- sum(hh.coordinates$Check.Dam,na.rm=T)

## COST raster
# cost.extent <- raster::crop(mv.dem,raster::extent(hh.coordinates)+1000)
# cost.raster <- costRaster4(cost.extent)
# base::saveRDS(cost.raster,'.output/spatial-products/cost-raster-4.rds')
cost.raster <- base::readRDS('./output/spatial-products/cost-raster-4.rds')

## GRASS environment
gisBase <- base::system('grass76 --config path',intern=T)
rgrass7::initGRASS(gisBase=gisBase,gisDbase='./output/spatial-products/',location='grass',mapset='PERMANENT',override=T)
rname <- base::paste('../DATABASE/SPATIAL/DEM/REGIONS/MESA-VERDE-UPLIFT','dem-mesa-verde-uplift.grd',sep='/')
rgrass7::execGRASS('r.in.gdal',flags='o',parameters=list(input=rname,output='grass.dem'))
rgrass7::execGRASS('g.region',parameters=list(raster='grass.dem')) 
rgrass7::execGRASS('g.proj',flags='c',epsg=26912)

###############################################################################################
###############################################################################################
