## COST RASTER ##
## KELSEY M. REESE via gdistance package

require(gdistance)

costRaster4 <- function(x,...) {
  n.directions <- 4
  heightDiff <- function(x){x[2] - x[1]}
  hd <- transition(x,heightDiff,directions=n.directions,symm=F)
  slope <- geoCorrection(hd)
  adj <- adjacent(x,cells=1:ncell(x),pairs=T,directions=n.directions)
  speed <- slope
  speed[adj] <- ((6 * exp(-3.5 * abs(slope[adj] + 0.05))) * 1000)
  cost.raster <- geoCorrection(speed)
  return(cost.raster)
}

costRaster8 <- function(x,...) {
  n.directions <- 8
  heightDiff <- function(x){x[2] - x[1]}
  hd <- transition(x,heightDiff,directions=n.directions,symm=F)
  slope <- geoCorrection(hd)
  adj <- adjacent(x,cells=1:ncell(x),pairs=T,directions=n.directions)
  speed <- slope
  speed[adj] <- ((6 * exp(-3.5 * abs(slope[adj] + 0.05))) * 1000)
  cost.raster <- geoCorrection(speed)
}

costRaster16 <- function(x,...) {
  n.directions <- 16
  heightDiff <- function(x){x[2] - x[1]}
  hd <- transition(x,heightDiff,directions=n.directions,symm=F)
  slope <- geoCorrection(hd)
  adj <- adjacent(x,cells=1:ncell(x),pairs=T,directions=n.directions)
  speed <- slope
  speed[adj] <- ((6 * exp(-3.5 * abs(slope[adj] + 0.05))) * 1000)
  cost.raster <- geoCorrection(speed)
}
