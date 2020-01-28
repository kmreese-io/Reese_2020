###############################################################################################
## CHECK DAM AGRICULTURE ON THE MESA VERDE CUESTA
## KELSEY M. REESE
## SUBMITTED FOR REVIEW
## YEAR VOL(NUM): PGS-PGS
###############################################################################################

## FIGURES ##

plot.extent <- extent(xmin(mv.dem)-750,xmax(mv.dem)+750,ymin(mv.dem)-750,ymax(mv.dem)+750)
mv.study.extent <- spTransform(polygonUTM_NAD83(UTM_North=ymax(mv.uplift)+750,UTM_South=ymin(mv.uplift)-750,UTM_East=xmin(mv.uplift)-750,UTM_West=xmax(mv.uplift)+750,UTM_Zone=12),master.projection)
rivers <- readOGR('../DATABASE/SPATIAL/SHAPEFILES/HYDROGRAPHY/rivers-us',layer='us-rivers')
mv.channels <- rgdal::readOGR('./output/spatial-products',layer='channel-networks-sloperange')
mv.channels.all <- rgdal::readOGR('./output/spatial-products',layer='channel-networks-total')
mv.slope.degrees <- raster::terrain(mv.dem,opt='slope',unit='degrees',neighbors=8)
mv.slope.degrees.uplift <- raster::mask(mv.slope.degrees,mv.uplift)
mv.slope.degrees.uplift.focal <- raster::focal(mv.slope.degrees.uplift,w=matrix(1,15,15),mean)

####################################################################################################
## FIGURE 1: Overview of Mesa Verde landform, with boundary; Four Corners inset, with study area bounding box

pdf('./output/figures/Figure 1.pdf')
par(bg=NA,mai=c(0.05,0.05,0.05,0.05))

## Base plot
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(mv.uplift)-750,xmax(mv.uplift)+750),ylim=c(ymin(mv.uplift)-750,ymax(mv.uplift)+750),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
plot(mv.boundary,col=alpha('darkgreen',alpha=0.4),border=NA,add=T)

## Landform outline and study area inset
lines(mv.uplift,col='black')
subplot(plot(state.boundaries,xlim=c(550000,900000),ylim=c(3700000,4650000),col='gray75',border='gray40'),x=xmin(mv.uplift)-750,y=ymax(mv.uplift)+750,size=c(1.5,1.5),vadj=1,hadj=0)
subplot(plot(state.boundaries[which(state.boundaries$STATE_NAME == 'Utah'),],border='black',col='gray75'))
subplot(plot(state.boundaries[which(state.boundaries$STATE_NAME == 'Colorado'),],border='black',col='gray75'))
subplot(plot(state.boundaries[which(state.boundaries$STATE_NAME == 'New Mexico'),],border='black',col='gray75'))
subplot(plot(state.boundaries[which(state.boundaries$STATE_NAME == 'Arizona'),],border='black',col='gray75'))

subplot(plot(state.boundaries,xlim=c(550000,900000),ylim=c(3700000,4650000),col='gray75',border='gray40'),x=xmin(mv.uplift)-750,y=ymax(mv.uplift)+750,size=c(1.5,1.5),vadj=1,hadj=0)
subplot(plot(mv.study.extent,xlim=c(550000,900000),ylim=c(3700000,4650000)),x=xmin(mv.uplift)-750,y=ymax(mv.uplift)+750,size=c(1.5,1.5),vadj=1,hadj=0)
subplot(box(),x=xmin(mv.uplift)-750,y=ymax(mv.uplift)+750,size=c(1.5,1.5),vadj=1,hadj=0)
text(704750,4131250,'UT',cex=0.7,col='gray40')
text(708500,4131250,'CO',cex=0.7,col='gray40')
text(704650,4127900,'AZ',cex=0.7,col='gray40')
text(708600,4127900,'NM',cex=0.7,col='gray40')

## Labels
text(726000,4126000,'Mesa Verde National Park',col='darkgreen')

## Scale
segments(723000,4104000,733000,4104000,lwd=1.5)
segments(723000,4103750,723000,4104250,lwd=1.5)
segments(728000,4103750,728000,4104250,lwd=1.5)
segments(733000,4103750,733000,4104250,lwd=1.5)
text(723250,4103950,'0',pos=2,cex=0.7,lwd=1.5)
text(732750,4103950,'10 km',pos=4,cex=0.7,xpd=T)

## North arrow
segments(735250,4103750,735250,4108750,lwd=1.5)
segments(735250,4108750,735000,4108000,lwd=1.5)
text(735250,4106250,'N')

box()

dev.off()

####################################################################################################
## FIGURE 2: Steps in potential check dam placement

pdf('./output/figures/Figure 2.pdf')
par(mfrow=c(2,2),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

## DEM landscape
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(mv.uplift)-750,xmax(mv.uplift)+750),ylim=c(ymin(mv.uplift)-750,ymax(mv.uplift)+750),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
lines(mv.uplift,col='black')
# Legend
legend('topleft',legend=c('Extent of Mesa Verde cuesta'),lty=c(1),col=c('black'),pt.cex=0.7,cex=0.7,y.intersp=0.9,bty='o',box.lwd=0.75,box.col='black',bg=alpha('gray80',alpha=0.7))
# Scale
segments(721500,4104000,731500,4104000,xpd=T)
segments(721500,4103750,721500,4104250,xpd=T)
segments(726500,4103750,726500,4104250,xpd=T)
segments(731500,4103750,731500,4104250,xpd=T)
text(721750,4103950,'0',pos=2,cex=0.7,xpd=T)
text(731000,4103950,'10 km',pos=4,cex=0.7,xpd=T)
# North arrow
segments(735250,4103750,735250,4108750)
segments(735250,4108750,735000,4108000)
text(735250,4106250,'N')
# Plot label
text(xmin(mv.uplift)+750,ymin(mv.uplift)+750,'a',cex=2)
box()

## All modelled Mesa Verde channels
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(mv.uplift)-750,xmax(mv.uplift)+750),ylim=c(ymin(mv.uplift)-750,ymax(mv.uplift)+750),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
lines(mv.channels.all,col='chocolate4',lwd=0.10,legend=F)
# Legend
legend('topleft',legend=c('Stream networks'),lty=c(1),col=c('chocolate4'),pt.cex=0.7,cex=0.7,y.intersp=0.9,bty='o',box.lwd=0.75,box.col='black',bg=alpha('gray80',alpha=0.7))
# Scale
segments(721500,4104000,731500,4104000,xpd=T)
segments(721500,4103750,721500,4104250,xpd=T)
segments(726500,4103750,726500,4104250,xpd=T)
segments(731500,4103750,731500,4104250,xpd=T)
text(721750,4103950,'0',pos=2,cex=0.7,xpd=T)
text(731000,4103950,'10 km',pos=4,cex=0.7,xpd=T)
# North arrow
segments(735250,4103750,735250,4108750)
segments(735250,4108750,735000,4108000)
text(735250,4106250,'N')
# Plot label
text(xmin(mv.uplift)+750,ymin(mv.uplift)+750,'b',cex=2)
box()

## Cells with slopes appropriate for check dams
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(mv.uplift)-750,xmax(mv.uplift)+750),ylim=c(ymin(mv.uplift)-750,ymax(mv.uplift)+750),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
plot(mv.slope.degrees.uplift.focal,col=rev(heatcolors),add=T,legend=F)
# Legend
legend('topleft',legend=c('Minimum slope of terrain','Maximum slope of terrain'),pch=c(NA,NA),pt.bg=c(rev(heatcolors)),pt.cex=2,cex=0.7,y.intersp=1,bty='o',box.lwd=0.75,box.col='black',bg=alpha('gray80',alpha=0.7))
plot(mv.slope.degrees.uplift,legend.only=T,col=heatcolors,box.col=NA,legend.width=0.6,smallplot=c(0.0375,0.0745,0.8875,0.9505),legend.shrink=0.5,axis.args=list(labels=F,tck=F,cex.axis=0.1))
# Scale
segments(721500,4104000,731500,4104000,xpd=T)
segments(721500,4103750,721500,4104250,xpd=T)
segments(726500,4103750,726500,4104250,xpd=T)
segments(731500,4103750,731500,4104250,xpd=T)
text(721750,4103950,'0',pos=2,cex=0.7,xpd=T)
text(731000,4103950,'10 km',pos=4,cex=0.7,xpd=T)
# North arrow
segments(735250,4103750,735250,4108750)
segments(735250,4108750,735000,4108000)
text(735250,4106250,'N')
# Plot label
text(xmin(mv.uplift)+750,ymin(mv.uplift)+750,'c',cex=2)
box()

## All potential check dam channels
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(mv.uplift)-750,xmax(mv.uplift)+750),ylim=c(ymin(mv.uplift)-750,ymax(mv.uplift)+750),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
plot(mv.channels,col='black',add=T,lwd=0.10,legend=F)
# Legend
legend('topleft',legend=c('Potential check dam networks'),lty=c(1),col=c('black'),pt.cex=0.7,cex=0.7,y.intersp=0.9,bty='o',box.lwd=0.75,box.col='black',bg=alpha('gray80',alpha=0.7))
# Scale
segments(721500,4104000,731500,4104000,xpd=T)
segments(721500,4103750,721500,4104250,xpd=T)
segments(726500,4103750,726500,4104250,xpd=T)
segments(731500,4103750,731500,4104250,xpd=T)
text(721750,4103950,'0',pos=2,cex=0.7,xpd=T)
text(731000,4103950,'10 km',pos=4,cex=0.7,xpd=T)
# North arrow
segments(735250,4103750,735250,4108750)
segments(735250,4108750,735000,4108000)
text(735250,4106250,'N')
# Plot label
text(xmin(mv.uplift)+750,ymin(mv.uplift)+750,'d',cex=2)
box()

dev.off()

####################################################################################################
## FIGURE 3: Steps in potential check dam efficacy

growing.niche.example <- raster::projectRaster(growing.niche[[1245]],crs=master.projection,method='ngb')
growing.niche.example[growing.niche.example < 1] <- 0
niche.polygon <- raster::rasterToPolygons(growing.niche.example,fun=function(x){x==1})
projection(niche.polygon) <- master.projection
projection(mv.channels) <- master.projection
niche.channel.example <- raster::intersect(mv.channels,niche.polygon)

pdf('./output/figures/Figure 3.pdf')
par(mfrow=c(2,2),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

## All potential check dam channels
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(mv.uplift)-750,xmax(mv.uplift)+750),ylim=c(ymin(mv.uplift)-750,ymax(mv.uplift)+750),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
plot(mv.channels,col='black',add=T,lwd=0.10,legend=F)
# Legend
legend('topleft',legend=c('Potential check dam networks'),lty=c(1),col=c('black'),pt.cex=0.7,cex=0.7,y.intersp=0.9,bty='o',box.lwd=0.75,box.col='black',bg=alpha('gray80',alpha=0.7))
# Scale
segments(721500,4104000,731500,4104000,xpd=T)
segments(721500,4103750,721500,4104250,xpd=T)
segments(726500,4103750,726500,4104250,xpd=T)
segments(731500,4103750,731500,4104250,xpd=T)
text(721750,4103950,'0',pos=2,cex=0.7,xpd=T)
text(731000,4103950,'10 km',pos=4,cex=0.7,xpd=T)
# North arrow
segments(735250,4103750,735250,4108750)
segments(735250,4108750,735000,4108000)
text(735250,4106250,'N')
# Plot label
text(xmin(mv.uplift)+750,ymin(mv.uplift)+750,'a',cex=2)
box()

## Maize growing niche example from AD 1245
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(mv.uplift)-750,xmax(mv.uplift)+750),ylim=c(ymin(mv.uplift)-750,ymax(mv.uplift)+750),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
plot(growing.niche.example,zlim=c(0,1),col=c(alpha('darkred',alpha=0.40),alpha('darkgreen',alpha=0.40)),add=T,legend=F)
# Legend
legend('topleft',legend=c('Area within maize growing niche','Area outside maize growing niche'),lty=c(1,1),col=c(alpha('darkgreen',alpha=0.4),alpha('darkred',alpha=0.4)),lwd=c(4,4),cex=0.7,y.intersp=0.9,bty='o',box.lwd=0.75,box.col='black',bg=alpha('gray80',alpha=0.7))
# Scale
segments(721500,4104000,731500,4104000,xpd=T)
segments(721500,4103750,721500,4104250,xpd=T)
segments(726500,4103750,726500,4104250,xpd=T)
segments(731500,4103750,731500,4104250,xpd=T)
text(721750,4103950,'0',pos=2,cex=0.7,xpd=T)
text(731000,4103950,'10 km',pos=4,cex=0.7,xpd=T)
# North arrow
segments(735250,4103750,735250,4108750)
segments(735250,4108750,735000,4108000)
text(735250,4106250,'N')
# Plot label
text(xmin(mv.uplift)+750,ymin(mv.uplift)+750,'b',cex=2)
box()

## Maize growing niche example from AD 1245 with all potential check dam channels
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(mv.uplift)-750,xmax(mv.uplift)+750),ylim=c(ymin(mv.uplift)-750,ymax(mv.uplift)+750),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
plot(growing.niche.example,zlim=c(0,1),col=c(alpha('darkred',alpha=0.40),alpha('darkgreen',alpha=0.40)),add=T,legend=F)
plot(mv.channels,col='black',add=T,lwd=0.10,legend=F)
# Legend
legend('topleft',legend=c('Potential check dam networks','Area within maize growing niche','Area outside maize growing niche'),lty=c(1,1,1),col=c('black',alpha('darkgreen',alpha=0.4),alpha('darkred',alpha=0.4)),lwd=c(1,4,4),cex=0.7,y.intersp=0.9,bty='o',box.lwd=0.75,box.col='black',bg=alpha('gray80',alpha=0.7))
# Scale
segments(721500,4104000,731500,4104000,xpd=T)
segments(721500,4103750,721500,4104250,xpd=T)
segments(726500,4103750,726500,4104250,xpd=T)
segments(731500,4103750,731500,4104250,xpd=T)
text(721750,4103950,'0',pos=2,cex=0.7,xpd=T)
text(731000,4103950,'10 km',pos=4,cex=0.7,xpd=T)
# North arrow
segments(735250,4103750,735250,4108750)
segments(735250,4108750,735000,4108000)
text(735250,4106250,'N')
# Plot label
text(xmin(mv.uplift)+750,ymin(mv.uplift)+750,'c',cex=2)
box()

## All potential check dam channels clipped to AD 1245 niche
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(mv.uplift)-750,xmax(mv.uplift)+750),ylim=c(ymin(mv.uplift)-750,ymax(mv.uplift)+750),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
plot(niche.channel.example,col='darkgreen',add=T,lwd=0.10,legend=F)
# Legend
legend('topleft',legend=c('Productive check dam networks, AD 1245'),lty=c(1),col=c('darkgreen'),pt.cex=0.7,cex=0.7,y.intersp=0.9,bty='o',box.lwd=0.75,box.col='black',bg=alpha('gray80',alpha=0.7))
# Scale
segments(721500,4104000,731500,4104000,xpd=T)
segments(721500,4103750,721500,4104250,xpd=T)
segments(726500,4103750,726500,4104250,xpd=T)
segments(731500,4103750,731500,4104250,xpd=T)
text(721750,4103950,'0',pos=2,cex=0.7,xpd=T)
text(731000,4103950,'10 km',pos=4,cex=0.7,xpd=T)
# North arrow
segments(735250,4103750,735250,4108750)
segments(735250,4108750,735000,4108000)
text(735250,4106250,'N')
# Plot label
text(xmin(mv.uplift)+750,ymin(mv.uplift)+750,'d',cex=2)
box()

dev.off()

####################################################################################################
## FIGURE 4: Summary of results

results <- read.csv('./output/results/results-households-supported.csv',col.names=c('year','est.momentary.population','max.n.checkdams','n.checkdams.niche','total.n.households.supported','percent.hh.checkdam.supported'))
total.growing.km2 <- (results$n.checkdams.niche * (mv.checkdam.length * mv.checkdam.width)) / 1000000
smoothed.area <- smoother::smth.gaussian(total.growing.km2,window=21,tails=T)
smoothed.percent <- smoother::smth.gaussian(results$percent.hh.checkdam.supported,window=21,tails=T)

pdf('./output/figures/Figure 4.pdf',height=5,width=9)
par(mfrow=c(1,1),mai=c(0.5,0.5,0.10,0.5),oma=c(0.5,0.5,0.5,0.5))

plot(results$year,total.growing.km2,xlim=c(885,1290),ylim=c(min(total.growing.km2),max(total.growing.km2)),axes=F,type='l',lwd=1,xlab='',ylab='',col=alpha('green',alpha=0.4))
abline(v=c(880,920,980,1020,1060,1100,1140,1180,1225,1260,1280),lty=2,col='gray')

lines(results$year,smoothed.area,lwd=2,col='darkgreen')

axis(2,at=seq(0,max(total.growing.km2),5),tick=T,labels=F,col='darkgreen')
text(x=862.5+5,y=seq(0,max(total.growing.km2),5),as.character(seq(0,max(total.growing.km2),5)),xpd=T,srt=0,pos=2,cex=0.75,col='darkgreen')
mtext('Total Area of Productive Check Dams (km2)',2,line=1.75,col='darkgreen')

par(new=T)
plot(results$year,results$percent.hh.checkdam.supported,xlim=c(885,1290),ylim=c(0,100),axes=F,type='l',lwd=1,xlab='',ylab='',col=alpha('blue',alpha=0.4))

lines(results$year,smoothed.percent,lwd=2,col='darkblue')

axis(4,at=seq(0,100,25),tick=T,labels=F,col='darkblue')
text(x=1297.5+10,y=seq(0,100,25),as.character(seq(0,100,25)),xpd=T,srt=0,pos=4,cex=0.75,col='darkblue')
mtext('Supported Households (%)',4,line=1.75,col='darkblue',srt=90)

axis(1,at=seq(880,1280,50),tick=T,labels=F)
text(x=seq(880,1280,50),y=-8.5,as.character(seq(880,1280,50)),xpd=T,cex=0.75)
mtext('Years (AD)',1,line=1.5)

dev.off()

####################################################################################################
## FIGURE 5: Steps to calculate cost-distance from example site to closest 1,979 check dams (equivalent) and back

row.number <- 14
phase.number <- 18
phase.households <- hh.coordinates[which(hh.coordinates$ph18 >= 1),]
all.households <- phase.households[rep(seq_len(dim(phase.households)[1]),phase.households$ph18),]

sample.site <- all.households[which(all.households$Sitename == 'FAR VIEW TOWER' ),]

# sample.point <- coordinates(all.households[sample(nrow(all.households),1,replace=F),])
sample.point <- coordinates(sample.site)

mv.checkdam.points <- as(mv.channels,'SpatialPointsDataFrame')
mv.uplift.boundary <- rgeos::gIntersection(mv.uplift,mv.boundary)
projection(mv.checkdam.points) <- master.projection
projection(mv.uplift.boundary) <- master.projection
mv.checkdam.points.masked <- mv.checkdam.points[mv.uplift.boundary,]
n.points.per.checkdam <- length(mv.checkdam.points) / max.n.checkdams
n.points <- round((1991 / n.points.per.checkdam) * (0.6086956522 / 2) )

zoom.extent <- raster::extent(sample.point[,1]-550,sample.point[,1]+550,sample.point[,2]-550,sample.point[,2]+550)
mv.channels.zoom <- raster::crop(mv.channels,zoom.extent)
mv.checkdam.points.masked.zoom <- raster::crop(mv.checkdam.points.masked,zoom.extent)
mv.dem.zoom <- raster::crop(mv.dem,zoom.extent)
mv.slope.zoom <- raster::terrain(mv.dem.zoom,opt='slope')
mv.aspect.zoom <- raster::terrain(mv.dem.zoom,opt='aspect')
mv.hillshade.zoom <- raster::hillShade(mv.slope.zoom,mv.aspect.zoom)

zoom.extent.plot <- raster::extent(sample.point[,1]-500,sample.point[,1]+500,sample.point[,2]-500,sample.point[,2]+500)
extent.example <- polygonUTM_NAD83(xmin(zoom.extent.plot),xmax(zoom.extent.plot),ymin(zoom.extent.plot),ymax(zoom.extent.plot),12)

euclidean.distance <- as.matrix(raster::pointDistance(sample.point[1,],as.matrix(coordinates(mv.checkdam.points.masked)),lonlat=F))
euclidean.distance.coords <- cbind(euclidean.distance,coordinates(mv.checkdam.points.masked))
euclidean.distance.sorted <- as.matrix(euclidean.distance.coords[order(euclidean.distance.coords[,1]),])
mv.checkdams.needed <- as.matrix(euclidean.distance.sorted[1:n.points,2:3])

pdf('./output/figures/Figure 5.pdf')
par(mfrow=c(2,2),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

# Overview of AD 1225--1260 sites, example site, and zoomed bounding box for example
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(mv.uplift)-750,xmax(mv.uplift)+750),ylim=c(ymin(mv.uplift)-750,ymax(mv.uplift)+750),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
plot(zoom.extent.plot,col='red',add=T,axes=F,legend=F)
points(coordinates(all.households),bg='black',col=NA,pch=21,cex=0.5)
points(sample.point[,1],sample.point[,2],bg='white',col='black',pch=21,cex=0.75)
# Legend
legend('topleft',legend=c('Example household','Contemporaneous households','Example area'),pch=c(21,21,0),pt.bg=c('white','black',NA),col=c('black',NA,'red'),pt.cex=0.7,cex=0.7,y.intersp=0.9,bty='o',box.lwd=0.75,box.col='black',bg=alpha('gray80',alpha=0.7))
# Scale
segments(721500,4104000,731500,4104000,xpd=T)
segments(721500,4103750,721500,4104250,xpd=T)
segments(726500,4103750,726500,4104250,xpd=T)
segments(731500,4103750,731500,4104250,xpd=T)
text(721750,4103950,'0',pos=2,cex=0.7,xpd=T)
text(731000,4103950,'10 km',pos=4,cex=0.7,xpd=T)
# North arrow
segments(735250,4103750,735250,4108750)
segments(735250,4108750,735000,4108000)
text(735250,4106250,'N')
# Plot label
text(xmin(mv.uplift)+750,ymin(mv.uplift)+750,'a',cex=2)
box()

# Close-up of example site with all potential check dam networks
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(zoom.extent.plot),xmax(zoom.extent.plot)),ylim=c(ymin(zoom.extent.plot),ymax(zoom.extent.plot)),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade.zoom,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem.zoom,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
plot(mv.channels.zoom,col='black',fill='black',lwd=0.10,add=T,axes=F,legend=F)
points(sample.point[,1],sample.point[,2],bg='white',col='black',pch=21,cex=0.75)
# Legend
legend('topleft',legend=c('Example household','Potential check dam networks'),pch=c(21,0),pt.bg=c('white','black'),pt.cex=0.7,cex=0.7,y.intersp=0.9,bty='o',box.lwd=0.75,box.col='black',bg=alpha('gray80',alpha=0.7))
# Scale
segments(721887.4-145,4123826+35,721887.4-145-250,4123826+35,xpd=T)
segments(721887.4-145,4123826+35+7.5,721887.4-145,4123826+35-7.5,xpd=T)
segments(721887.4-145-125,4123826+35+7.5,721887.4-145-125,4123826+35-7.5,xpd=T)
segments(721887.4-145-250,4123826+35+7.5,721887.4-145-250,4123826+35-7.5,xpd=T)
text(721887.4-145-250+10,4123826+35,'0',pos=2,cex=0.7,xpd=T)
text(721887.4-145-10,4123826+35,'250 m',pos=4,cex=0.7,xpd=T)
# North arrow
segments(721887.4-25,4123826+36-10,721887.4-25,4123826+36.5+150)
segments(721887.4-25,4123826+38+150,721887.4-32.5,4123826+36.5+150-25)
text(721887.4-25,4124000-70,'N')
# Plot label
text(sample.point[,1]-450,sample.point[,2]-450,'b',cex=2)
box()

# Euclidean lines between sample site and all required check dams
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(zoom.extent.plot),xmax(zoom.extent.plot)),ylim=c(ymin(zoom.extent.plot),ymax(zoom.extent.plot)),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade.zoom,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem.zoom,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
plot(mv.channels.zoom,col='black',fill='black',lwd=0.10,add=T,axes=F,legend=F)
segments(sample.point[,1],sample.point[,2],mv.checkdams.needed[,1],mv.checkdams.needed[,2],col='gray30')
points(sample.point[,1],sample.point[,2],bg='white',col='black',pch=21,cex=0.75)
# Legend
legend('topleft',legend=c('Example household','Potential check dam networks','Closest check dams for storage'),pch=c(21,0,124),pt.bg=c('white','black','gray30'),pt.cex=0.7,cex=0.7,y.intersp=0.9,bty='o',box.lwd=0.75,box.col='black',bg=alpha('gray80',alpha=0.7))
# Scale
segments(721887.4-145,4123826+35,721887.4-145-250,4123826+35,xpd=T)
segments(721887.4-145,4123826+35+7.5,721887.4-145,4123826+35-7.5,xpd=T)
segments(721887.4-145-125,4123826+35+7.5,721887.4-145-125,4123826+35-7.5,xpd=T)
segments(721887.4-145-250,4123826+35+7.5,721887.4-145-250,4123826+35-7.5,xpd=T)
text(721887.4-145-250+10,4123826+35,'0',pos=2,cex=0.7,xpd=T)
text(721887.4-145-10,4123826+35,'250 m',pos=4,cex=0.7,xpd=T)
# North arrow
segments(721887.4-25,4123826+36-10,721887.4-25,4123826+36.5+150)
segments(721887.4-25,4123826+38+150,721887.4-32.5,4123826+36.5+150-25)
text(721887.4-25,4124000-70,'N')
# Plot label
text(sample.point[,1]-450,sample.point[,2]-450,'c',cex=2)
box()

# Least-cost roundtrip pathway from example site to all required check dams
plot(1,type="n",xlab='',ylab='',xlim=c(xmin(zoom.extent.plot),xmax(zoom.extent.plot)),ylim=c(ymin(zoom.extent.plot),ymax(zoom.extent.plot)),xaxs="i",yaxs="i",axes=F,main='')
plot(mv.hillshade.zoom,col=colors(10000),legend=F,add=T,axes=F)
plot(mv.dem.zoom,col=alpha(colors(10000),alpha=0.40),add=T,axes=F,legend=F)
plot(mv.channels.zoom,col='black',fill='black',lwd=0.10,add=T,axes=F,legend=F)
segments(sample.point[,1],sample.point[,2],mv.checkdams.needed[1,1],mv.checkdams.needed[1,2],col='gray30',lwd=2)

euclidean.distance <- as.matrix(raster::pointDistance(sample.point[1,],as.matrix(coordinates(mv.checkdam.points.masked)),lonlat=F))
euclidean.distance.coords <- cbind(euclidean.distance,coordinates(mv.checkdam.points.masked))
euclidean.distance.sorted <- as.matrix(euclidean.distance.coords[order(euclidean.distance.coords[,1]),])
mv.checkdams.needed <- as.matrix(euclidean.distance.sorted[1:n.points,2:3])

example.checkdam.coordinates <- unique(mv.checkdams.needed[,1:2])

for(i in 1:nrow(example.checkdam.coordinates)) {
  remaining.checkdams.distance <- as.matrix(raster::pointDistance(example.checkdam.coordinates[i,],example.checkdam.coordinates[-(1:i),],lonlat=F))
  remaining.checkdams.coords <- cbind(remaining.checkdams.distance,example.checkdam.coordinates[-(1:i),])
  remaining.checkdams.sorted <- as.matrix(remaining.checkdams.coords[order(remaining.checkdams.coords[,1]),])
  segments(example.checkdam.coordinates[i,1],example.checkdam.coordinates[i,2],remaining.checkdams.sorted[1,2],remaining.checkdams.sorted[1,3],col='gray30',lwd=2)
}

segments(sample.point[,1],sample.point[,2],example.checkdam.coordinates[nrow(example.checkdam.coordinates),1],example.checkdam.coordinates[nrow(example.checkdam.coordinates),2],col='gray30',lwd=2)

points(sample.point[,1],sample.point[,2],bg='white',col='black',pch=21,cex=0.75)
# Legend
legend('topleft',legend=c('Example household','Potential check dam networks','Path through check dams for storage'),pch=c(21,0,124),pt.bg=c('white','black','gray30'),pt.cex=0.7,cex=0.7,y.intersp=0.9,bty='o',box.lwd=0.75,box.col='black',bg=alpha('gray80',alpha=0.7))
# Scale
segments(721887.4-145,4123826+35,721887.4-145-250,4123826+35,xpd=T)
segments(721887.4-145,4123826+35+7.5,721887.4-145,4123826+35-7.5,xpd=T)
segments(721887.4-145-125,4123826+35+7.5,721887.4-145-125,4123826+35-7.5,xpd=T)
segments(721887.4-145-250,4123826+35+7.5,721887.4-145-250,4123826+35-7.5,xpd=T)
text(721887.4-145-250+10,4123826+35,'0',pos=2,cex=0.7,xpd=T)
text(721887.4-145-10,4123826+35,'250 m',pos=4,cex=0.7,xpd=T)
# North arrow
segments(721887.4-25,4123826+36-10,721887.4-25,4123826+36.5+150)
segments(721887.4-25,4123826+38+150,721887.4-32.5,4123826+36.5+150-25)
text(721887.4-25,4124000-70,'N')
# Plot label
text(sample.point[,1]-450,sample.point[,2]-450,'d',cex=2)
box()

dev.off()

####################################################################################################
## FIGURE 6: Productivity of check dams for storage and average cost-distances to check dam networks through time

travel.time <- read.csv('./output/results/checkdam-cost-travel-time.csv',header=F)
network <- read.csv('./output/results/checkdam-productivity-network.csv',header=F)
smoothed.network.percent <- smoother::smth.gaussian(network[,2],window=21,tails=T)

network.percent <- as.matrix(network[,2]/100,ncol=1)
# annual.storage.percent <- 0.6086956522 # total storage amount reported (Burns 1983; Parsons 1936)
annual.storage.percent <- 0.6086956522 / 2 # conservative estimate of total storage amount reported (Burns 1983; Parsons 1936)
years.into.storage <- as.matrix(network.percent*annual.storage.percent)
amount.in.storage <- matrix(NA,ncol=2,nrow=nrow(network)+10)
spoilage.calories <- years.into.storage-years.into.storage*0.1
spoilage.calories <- rbind(spoilage.calories,0,0,0,0,0,0,0,0,0,0)
for(i in 1:10) {
  storage.amount <- sum(spoilage.calories[1:i,])
  amount.in.storage[i,1] <- i+889
  amount.in.storage[i,2] <- storage.amount
}
for(i in 11:nrow(network)) {
  storage.amount <- sum(spoilage.calories[i:(i-9),])
  amount.in.storage[i,1] <- i+889
  amount.in.storage[i,2] <- storage.amount
}
for(i in 397:nrow(amount.in.storage)) {
  storage.amount <- sum(spoilage.calories[i:(i-9),])
  amount.in.storage[i,1] <- i+889
  amount.in.storage[i,2] <- storage.amount
}

amount.in.storage.limited <- amount.in.storage[11:396,]


pdf('./output/figures/Figure 6.pdf',height=5,width=9)
par(mfrow=c(1,1),mai=c(0.5,0.5,0.10,0.5),oma=c(0.5,0.5,0.5,0.5))

plot(network[,1],network[,2],xlim=c(885,1290),ylim=c(0,100),axes=F,type='l',lwd=1,xlab='',ylab='',col=alpha('blue',alpha=0.4))
abline(v=c(880,920,980,1020,1060,1100,1140,1180,1225,1260,1280),lty=2,col='gray')

abline(v=amount.in.storage.limited[which(amount.in.storage.limited[,2] < 2 ),][,1],col=alpha('red',alpha=0.5),lwd=2)
abline(v=amount.in.storage.limited[which(amount.in.storage.limited[,2] > 4 ),][,1],col='darkgreen',lwd=2)

lines(network[,1],smoothed.network.percent,lwd=2,col='darkblue')

axis(1,at=seq(880,1280,50),tick=T,labels=F)
text(x=seq(880,1280,50),y=-8.5,as.character(seq(880,1280,50)),xpd=T,cex=0.75)
mtext('Years (AD)',1,line=1.5)

axis(4,at=seq(0,100,25),tick=T,labels=F,col='darkblue')
text(x=1297.5+10,y=seq(0,100,25),as.character(seq(0,100,25)),xpd=T,srt=0,pos=4,cex=0.75,col='darkblue')
mtext('Annual Household Storage Need Fulfilled (%)',4,line=1.75,col='darkblue')

par(new=T)

plot(travel.time[,1],travel.time[,2],xlim=c(885,1290),ylim=c(25,325),axes=F,type='l',lwd=2,xlab='',ylab='',col='gray30')
lines(travel.time[,1],travel.time[,3],col='gray30',lwd=2)
polygon(c(travel.time[,1],rev(travel.time[,1])),c(travel.time[,3],rev(travel.time[,2])),col=alpha('gray40',alpha=0.4),border=NA)

axis(2,at=seq(25,325,100),tick=T,labels=F,col='gray30')
text(x=862.5+5,y=seq(25,325,100),as.character(seq(25,325,100)),xpd=T,srt=0,pos=2,cex=0.75,col='gray30')
mtext('Avg. Travel Time through Check Dam Network (minutes)',2,line=1.75,col='gray30')

dev.off()

####################################################################################################
####################################################################################################