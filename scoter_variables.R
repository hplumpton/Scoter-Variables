library(rgdal)
library(maps)
library(raster)
library(rgeos)
library(maptools)


#bathymetry

bathy=raster("Layers/etopo1 bathymetry.tif")

scoters=read.csv("ObsData2.csv",header=TRUE)
scoters <-na.omit(scoters)
#image(bathy)

coordinates(scoters)<-c("longitude_dd","latitude_dd") 
#define x&y coordinates
proj4string(scoters)<-CRS("+proj=longlat +datum=WGS84") 
#assigning a projection
#plot(scoters, add=TRUE)
sco2=spTransform(scoters,CRS(proj4string(bathy))) 
#assign same projection as bathy

sco3=SpatialPoints(sco2) 
#extract function didn't like SpatialPointsDataFrame
sco2$bathy=extract(bathy,sco3)
sco2$bathy2=scale(sco2$bathy)
sco2$bathy2=as.numeric(sco2$bathy)


#substrate

substrate=rgdal::readOGR("Layers/substrate/conmapsg.shp")
#plot(substrate)
#plot(scoters,add=TRUE)

substrate<-spTransform(substrate,CRS(proj4string(bathy)))
sco2$substrate=extract(substrate,sco2)
#sco2$substrate=over(sco2,substrate)


#ocean floor slope

slope<-terrain(bathy, opt=c('slope'), unit='degrees')
scotslope=SpatialPoints(sco2)
proj4string(scotslope)<-CRS("+proj=longlat +datum=WGS84")

sco2$slope=extract(slope,scotslope)
sco2$slope2=scale(sco2$slope)
sco2$slope2=as.numeric(scale(sco2$slope))


#distance to shore

shoreline=rgdal::readOGR("Layers/shoreline/GSHHS_shp/i/GSHHS_i_L1.shp")
seshoreline <- crop(shoreline, extent(-82, -72, 28, 39))
seshoreline<-spTransform(seshoreline,CRS(proj4string(bathy)))

#plot(seshoreline)
#plot(scoters,add=TRUE)
dist<-distanceFromPoints(seshoreline,scoters)
proj4string(dist)<-CRS("+proj=longlat +datum=WGS84")

sco2$dist=extract(dist,sco2)
sco2$dist2=scale(sco2$dist)
sco2$dist2=as.numeric(scale(sco2$dist)) 
#You'll need to add as.numeric here too


#sediment mobility
#624 NA's probably because they were closer to the shoreline than layer was
#chose to drop variable

#North Atlantic Oscillation
sco2$NAO2=scale(sco2$NAO)
sco2$NAO2=as.numeric(sco2$NAO2)

#Bivalve Distribution
scot<-as.data.frame(scoters,header=TRUE)
scosc<-subset(scot,latitude_dd>=32.0 & latitude_dd<33.75,select=SurveyId:NAO)
coordinates(scosc)<-c("longitude_dd","latitude_dd")
sconc<-subset(scot,latitude_dd>=33.75,select=SurveyId:NAO)
coordinates(sconc)<-c("longitude_dd","latitude_dd")
scoga<-subset(scot,latitude_dd>=30.7 & latitude_dd<32.0,select=SurveyId:NAO)
coordinates(scoga)<-c("longitude_dd","latitude_dd")
scofl<-subset(scot,latitude_dd<30.7,select=SurveyId:NAO)
coordinates(scofl)<-c("longitude_dd","latitude_dd")

#south carolina
bival1=rgdal::readOGR("Layers/SCarolina/Layers/invertebrates.shp")
bival1<-spTransform(bival1,CRS(proj4string(bathy)))
#plot(bival1)
proj4string(scosc)<-CRS("+proj=longlat +datum=WGS84")
scosc=spTransform(scosc,CRS(proj4string(bathy)))
#plot(scosc,add=TRUE)
scosc$bival=extract(bival1,scosc)

#north carolina
bival2=rgdal::readOGR("Layers/NCarolina/LAYER FILES/invert.shp")
bival2<-spTransform(bival2,CRS(proj4string(bathy)))
#plot(bival2)
proj4string(sconc)<-CRS("+proj=longlat +datum=WGS84")
sconc=spTransform(sconc,CRS(proj4string(bathy)))
#plot(sconc,add=TRUE)
sconc$bival=extract(bival2,sconc)

#florida
bival3=rgdal::readOGR("Layers/Florida/Florida/layer/inverts.shp")
bival3<-spTransform(bival3,CRS(proj4string(bathy)))
#plot(bival3)
proj4string(scofl)<-CRS("+proj=longlat +datum=WGS84")
scofl=spTransform(scofl,CRS(proj4string(bathy)))
#plot(scofl,add=TRUE)
#scofl$bival=extract(bival3,scofl)
scofl$bival=over(scofl,bival3)

#georgia
bival4=rgdal::readOGR("Layers/Georgia/LAYER_FILES/invert.shp")
bival4<-spTransform(bival4,CRS(proj4string(bathy)))
#plot(bival4)
proj4string(scoga)<-CRS("+proj=longlat +datum=WGS84")
scoga=spTransform(scoga,CRS(proj4string(bathy)))
#plot(scoga,add=TRUE)
scoga$bival=extract(bival4,scoga)


scosc$bival=as.factor(scosc@data$bival$RARNUM)
sconc$bival=as.factor(sconc@data$bival$RARNUM)
scoga$bival=as.factor(scoga@data$bival$RARNUM)
scofl$bival=as.factor(scofl@data$bival$RARNUM)
scobival<-rbind(scosc,sconc,scoga,scofl)

sco2<-cbind(sco2,scobival[18])
#summary(sco2$bival)

#Marine Ecoregions
eco=rgdal::readOGR("Layers/MEOW/meow_ecos.shp")
ecoregion <- crop(eco, extent(-82, -72, 28, 39))
ecoregion<-spTransform(ecoregion,CRS(proj4string(bathy)))
#plot(ecoregion)
#plot(scoters,add=TRUE)
sco2$eco=extract(ecoregion,sco2)


#Fine Scale Weather
library(gstat)

#subsetting data
sco<-as.data.frame(scoters)
coordinates(sco)<-c("longitude_dd","latitude_dd")
scofeb2.2009<-subset(sco,dateFlown==202.2009,select=SurveyId:NAO)
scofeb5.2009<-subset(sco,dateFlown==205.2009,select=SurveyId:NAO)
scofeb6.2009<-subset(sco,dateFlown==206.2009,select=SurveyId:NAO)
scofeb7.2009<-subset(sco,dateFlown==207.2009,select=SurveyId:NAO)
scofeb8.2009<-subset(sco,dateFlown==208.2009,select=SurveyId:NAO)
scofeb9.2009<-subset(sco,dateFlown==209.2009,select=SurveyId:NAO)
scofeb1.2010<-subset(sco,dateFlown==201.2010,select=SurveyId:NAO)
scofeb3.2010<-subset(sco,dateFlown==203.2010,select=SurveyId:NAO)
scofeb4.2010<-subset(sco,dateFlown==204.2010,select=SurveyId:NAO)
scofeb9.2010<-subset(sco,dateFlown==209.2010,select=SurveyId:NAO)
scofeb11.2010<-subset(sco,dateFlown==211.2010,select=SurveyId:NAO)
scofeb12.2010<-subset(sco,dateFlown==212.2010,select=SurveyId:NAO)
scofeb15.2010<-subset(sco,dateFlown==215.2010,select=SurveyId:NAO)
scofeb3.2011<-subset(sco,dateFlown==203.2011,select=SurveyId:NAO)
scofeb6.2011<-subset(sco,dateFlown==206.2011,select=SurveyId:NAO)
scofeb9.2011<-subset(sco,dateFlown==209.2011,select=SurveyId:NAO)
scofeb11.2011<-subset(sco,dateFlown==211.2011,select=SurveyId:NAO)
scofeb12.2011<-subset(sco,dateFlown==212.2011,select=SurveyId:NAO)
scofeb13.2011<-subset(sco,dateFlown==213.2011,select=SurveyId:NAO)
scofeb16.2011<-subset(sco,dateFlown==216.2011,select=SurveyId:NAO)
scofeb17.2011<-subset(sco,dateFlown==217.2011,select=SurveyId:NAO)
scofeb4.2012<-subset(sco,dateFlown==204.2012,select=SurveyId:NAO)
scofeb5.2012<-subset(sco,dateFlown==205.2012,select=SurveyId:NAO)
scofeb8.2012<-subset(sco,dateFlown==208.2012,select=SurveyId:NAO)
scofeb17.2012<-subset(sco,dateFlown==217.2012,select=SurveyId:NAO)
scofeb18.2012<-subset(sco,dateFlown==218.2012,select=SurveyId:NAO)
scofeb21.2012<-subset(sco,dateFlown==221.2012,select=SurveyId:NAO)


#WIND
#feb2.2009
windfeb2.2009=read.csv("Wind/feb2.2009.csv", header=TRUE)
windfeb2.2009 <-na.exclude(windfeb2.2009)
coordinates(windfeb2.2009)<-c("Long","Lat") 
proj4string(windfeb2.2009)<-CRS("+proj=longlat +datum=WGS84") 
windfeb2.2009<-spTransform(windfeb2.2009,CRS(proj4string(bathy)))

x.range<-as.numeric(c(-82,-72))
y.range<-as.numeric(c(28,39))
grd<-expand.grid(x=seq(from=x.range[1], to=x.range[2], by =0.1),
                           y=seq(from=y.range[1], to=y.range[2],by=0.1))

names(grd)<-c("x","y")
coordinates(grd)<-c("x","y")
gridded(grd)<-TRUE
fullgrid(grd)<-TRUE
proj4string(grd)<-proj4string(bathy)

p.idw<-gstat::idw(feb2.2009~1, windfeb2.2009, newdata=grd, idp=2.0)
p.idw<-raster(p.idw)
#plot(p.idw)
#image(p.idw)
#map("state", add=TRUE)
proj4string(scofeb2.2009)<-CRS("+proj=longlat +datum=WGS84")
scofeb2.2009=spTransform(scofeb2.2009,CRS(proj4string(bathy))) 
#plot(scofeb2.2009,add=TRUE)
scofeb2.2009$wind=extract(p.idw,scofeb2.2009)
scofeb2.2009$wind2=scale(scofeb2.2009$wind)

#feb5.2009
windfeb5.2009=read.csv("Wind/feb5.2009.csv", header=TRUE)
windfeb5.2009 <-na.exclude(windfeb5.2009)
coordinates(windfeb5.2009)<-c("Long","Lat") 
proj4string(windfeb5.2009)<-CRS("+proj=longlat +datum=WGS84") 
windfeb5.2009<-spTransform(windfeb5.2009,CRS(proj4string(bathy)))

p1.idw<-gstat::idw(feb5.2009~1, windfeb5.2009, newdata=grd, idp=2.0)
p1.idw<-raster(p1.idw)
#plot(p1.idw)
proj4string(scofeb5.2009)<-CRS("+proj=longlat +datum=WGS84")
scofeb5.2009=spTransform(scofeb5.2009,CRS(proj4string(bathy)))
#plot(scofeb5.2009,add=TRUE)
scofeb5.2009$wind=extract(p1.idw,scofeb5.2009)
scofeb5.2009$wind2=scale(scofeb5.2009$wind)

#feb6.2009
windfeb6.2009=read.csv("Wind/feb6.2009.csv", header=TRUE)
windfeb6.2009 <-na.exclude(windfeb6.2009)
coordinates(windfeb6.2009)<-c("Long","Lat") 
proj4string(windfeb6.2009)<-CRS("+proj=longlat +datum=WGS84") 
windfeb6.2009<-spTransform(windfeb6.2009,CRS(proj4string(bathy)))

p2.idw<-gstat::idw(feb6.2009~1, windfeb6.2009, newdata=grd, idp=2.0)
p2.idw<-raster(p2.idw)
#plot(p2.idw)
proj4string(scofeb6.2009)<-CRS("+proj=longlat +datum=WGS84")
scofeb6.2009=spTransform(scofeb6.2009,CRS(proj4string(bathy)))
#plot(scofeb6.2009,add=TRUE)
scofeb6.2009$wind=extract(p2.idw,scofeb6.2009)
scofeb6.2009$wind2=scale(scofeb6.2009$wind)

#feb7.2009
windfeb7.2009=read.csv("Wind/feb7.2009.csv", header=TRUE)
windfeb7.2009 <-na.exclude(windfeb7.2009)
coordinates(windfeb7.2009)<-c("Long","Lat") 
proj4string(windfeb7.2009)<-CRS("+proj=longlat +datum=WGS84") 
windfeb7.2009<-spTransform(windfeb7.2009,CRS(proj4string(bathy)))

p3.idw<-gstat::idw(feb7.2009~1, windfeb7.2009, newdata=grd, idp=2.0)
p3.idw<-raster(p3.idw)
#plot(p3.idw)
proj4string(scofeb7.2009)<-CRS("+proj=longlat +datum=WGS84")
scofeb7.2009=spTransform(scofeb7.2009,CRS(proj4string(bathy)))
#plot(scofeb7.2009,add=TRUE)
scofeb7.2009$wind=extract(p3.idw,scofeb7.2009)
scofeb7.2009$wind2=scale(scofeb7.2009$wind)

#feb8.2009
windfeb8.2009=read.csv("Wind/feb8.2009.csv", header=TRUE)
windfeb8.2009 <-na.exclude(windfeb8.2009)
coordinates(windfeb8.2009)<-c("Long","Lat") 
proj4string(windfeb8.2009)<-CRS("+proj=longlat +datum=WGS84") 
windfeb8.2009<-spTransform(windfeb8.2009,CRS(proj4string(bathy)))

p4.idw<-gstat::idw(feb8.2009~1, windfeb8.2009, newdata=grd, idp=2.0)
p4.idw<-raster(p4.idw)
#plot(p4.idw)
proj4string(scofeb8.2009)<-CRS("+proj=longlat +datum=WGS84")
scofeb8.2009=spTransform(scofeb8.2009,CRS(proj4string(bathy)))
#plot(scofeb8.2009,add=TRUE)
scofeb8.2009$wind=extract(p4.idw,scofeb8.2009)
scofeb8.2009$wind2=scale(scofeb8.2009$wind)

#feb9.2009
windfeb9.2009=read.csv("Wind/feb9.2009.csv", header=TRUE)
windfeb9.2009 <-na.exclude(windfeb9.2009)
coordinates(windfeb9.2009)<-c("Long","Lat") 
proj4string(windfeb9.2009)<-CRS("+proj=longlat +datum=WGS84") 
windfeb9.2009<-spTransform(windfeb9.2009,CRS(proj4string(bathy)))

p5.idw<-gstat::idw(feb9.2009~1, windfeb9.2009, newdata=grd, idp=2.0)
p5.idw<-raster(p5.idw)
#plot(p5.idw)
proj4string(scofeb9.2009)<-CRS("+proj=longlat +datum=WGS84")
scofeb9.2009=spTransform(scofeb9.2009,CRS(proj4string(bathy)))
#plot(scofeb9.2009,add=TRUE)
scofeb9.2009$wind=extract(p5.idw,scofeb9.2009)
scofeb9.2009$wind2=scale(scofeb9.2009$wind)

#feb1.2010
windfeb1.2010=read.csv("Wind/feb1.2010.csv", header=TRUE)
windfeb1.2010 <-na.exclude(windfeb1.2010)
coordinates(windfeb1.2010)<-c("Long","Lat") 
proj4string(windfeb1.2010)<-CRS("+proj=longlat +datum=WGS84") 
windfeb1.2010<-spTransform(windfeb1.2010,CRS(proj4string(bathy)))

p6.idw<-gstat::idw(feb1.2010~1, windfeb1.2010, newdata=grd, idp=2.0)
p6.idw<-raster(p6.idw)
#plot(p6.idw)
proj4string(scofeb1.2010)<-CRS("+proj=longlat +datum=WGS84")
scofeb1.2010=spTransform(scofeb1.2010,CRS(proj4string(bathy)))
#plot(scofeb1.2010,add=TRUE)
scofeb1.2010$wind=extract(p6.idw,scofeb1.2010)
scofeb1.2010$wind2=scale(scofeb1.2010$wind)

#feb3.2010
windfeb3.2010=read.csv("Wind/feb3.2010.csv", header=TRUE)
windfeb3.2010 <-na.exclude(windfeb3.2010)
coordinates(windfeb3.2010)<-c("Long","Lat") 
proj4string(windfeb3.2010)<-CRS("+proj=longlat +datum=WGS84") 
windfeb3.2010<-spTransform(windfeb3.2010,CRS(proj4string(bathy)))

p7.idw<-gstat::idw(feb3.2010~1, windfeb3.2010, newdata=grd, idp=2.0)
p7.idw<-raster(p7.idw)
#plot(p7.idw)
proj4string(scofeb3.2010)<-CRS("+proj=longlat +datum=WGS84")
scofeb3.2010=spTransform(scofeb3.2010,CRS(proj4string(bathy)))
#plot(scofeb3.2010,add=TRUE)
scofeb3.2010$wind=extract(p7.idw,scofeb3.2010)
scofeb3.2010$wind2=scale(scofeb3.2010$wind)

#feb4.2010
windfeb4.2010=read.csv("Wind/feb4.2010.csv", header=TRUE)
windfeb4.2010 <-na.exclude(windfeb4.2010)
coordinates(windfeb4.2010)<-c("Long","Lat") 
proj4string(windfeb4.2010)<-CRS("+proj=longlat +datum=WGS84") 
windfeb4.2010<-spTransform(windfeb4.2010,CRS(proj4string(bathy)))

p8.idw<-gstat::idw(feb4.2010~1, windfeb4.2010, newdata=grd, idp=2.0)
p8.idw<-raster(p8.idw)
#plot(p8.idw)
proj4string(scofeb4.2010)<-CRS("+proj=longlat +datum=WGS84")
scofeb4.2010=spTransform(scofeb4.2010,CRS(proj4string(bathy)))
#plot(scofeb4.2010,add=TRUE)
scofeb4.2010$wind=extract(p8.idw,scofeb4.2010)
scofeb4.2010$wind2=scale(scofeb4.2010$wind)

#feb9.2010
windfeb9.2010=read.csv("Wind/feb9.2010.csv", header=TRUE)
windfeb9.2010 <-na.exclude(windfeb9.2010)
coordinates(windfeb9.2010)<-c("Long","Lat") 
proj4string(windfeb9.2010)<-CRS("+proj=longlat +datum=WGS84") 
windfeb9.2010<-spTransform(windfeb9.2010,CRS(proj4string(bathy)))

p9.idw<-gstat::idw(feb9.2010~1, windfeb9.2010, newdata=grd, idp=2.0)
p9.idw<-raster(p9.idw)
#plot(p9.idw)
proj4string(scofeb9.2010)<-CRS("+proj=longlat +datum=WGS84")
scofeb9.2010=spTransform(scofeb9.2010,CRS(proj4string(bathy)))
#plot(scofeb9.2010,add=TRUE)
scofeb9.2010$wind=extract(p9.idw,scofeb9.2010)
scofeb9.2010$wind2=scale(scofeb9.2010$wind)

#feb11.2010
windfeb11.2010=read.csv("Wind/feb11.2010.csv", header=TRUE)
windfeb11.2010 <-na.exclude(windfeb11.2010)
coordinates(windfeb11.2010)<-c("Long","Lat") 
proj4string(windfeb11.2010)<-CRS("+proj=longlat +datum=WGS84") 
windfeb11.2010<-spTransform(windfeb11.2010,CRS(proj4string(bathy)))

p10.idw<-gstat::idw(feb11.2010~1, windfeb11.2010, newdata=grd, idp=2.0)
p10.idw<-raster(p10.idw)
#plot(p10.idw)
proj4string(scofeb11.2010)<-CRS("+proj=longlat +datum=WGS84")
scofeb11.2010=spTransform(scofeb11.2010,CRS(proj4string(bathy)))
#plot(scofeb11.2010,add=TRUE)
scofeb11.2010$wind=extract(p10.idw,scofeb11.2010)
scofeb11.2010$wind2=scale(scofeb11.2010$wind)

#feb12.2010
windfeb12.2010=read.csv("Wind/feb12.2010.csv", header=TRUE)
windfeb12.2010 <-na.exclude(windfeb12.2010)
coordinates(windfeb12.2010)<-c("Long","Lat") 
proj4string(windfeb12.2010)<-CRS("+proj=longlat +datum=WGS84") 
windfeb12.2010<-spTransform(windfeb12.2010,CRS(proj4string(bathy)))

p11.idw<-gstat::idw(feb12.2010~1, windfeb12.2010, newdata=grd, idp=2.0)
p11.idw<-raster(p11.idw)
#plot(p11.idw)
proj4string(scofeb12.2010)<-CRS("+proj=longlat +datum=WGS84")
scofeb12.2010=spTransform(scofeb12.2010,CRS(proj4string(bathy)))
#plot(scofeb12.2010,add=TRUE)
scofeb12.2010$wind=extract(p11.idw,scofeb12.2010)
scofeb12.2010$wind2=scale(scofeb12.2010$wind)

#feb15.2010
windfeb15.2010=read.csv("Wind/feb15.2010.csv", header=TRUE)
windfeb15.2010 <-na.exclude(windfeb15.2010)
coordinates(windfeb15.2010)<-c("Long","Lat") 
proj4string(windfeb15.2010)<-CRS("+proj=longlat +datum=WGS84") 
windfeb15.2010<-spTransform(windfeb15.2010,CRS(proj4string(bathy)))

p12.idw<-gstat::idw(feb15.2010~1, windfeb15.2010, newdata=grd, idp=2.0)
p12.idw<-raster(p12.idw)
#plot(p12.idw)
proj4string(scofeb15.2010)<-CRS("+proj=longlat +datum=WGS84")
scofeb15.2010=spTransform(scofeb15.2010,CRS(proj4string(bathy)))
#plot(scofeb15.2010,add=TRUE)
scofeb15.2010$wind=extract(p12.idw,scofeb15.2010)
scofeb15.2010$wind2=scale(scofeb15.2010$wind)

#feb3.2011
windfeb3.2011=read.csv("Wind/wind.csv",header=TRUE)
coordinates(windfeb3.2011)<-c("Long","Lat") 
proj4string(windfeb3.2011)<-CRS("+proj=longlat +datum=WGS84") 
windfeb3.2011<-spTransform(windfeb3.2011,CRS(proj4string(bathy)))

p12.idw<-gstat::idw(feb3.2011~1, windfeb3.2011, newdata=grd, idp=2.0)
p12.idw<-raster(p12.idw)
#plot(p12.idw)
proj4string(scofeb3.2011)<-CRS("+proj=longlat +datum=WGS84")
scofeb3.2011=spTransform(scofeb3.2011,CRS(proj4string(bathy)))
#plot(scofeb3.2011,add=TRUE)
scofeb3.2011$wind=extract(p12.idw,scofeb3.2011)
scofeb3.2011$wind2=scale(scofeb3.2011$wind)

#feb6.2011
windfeb6.2011=read.csv("Wind/feb6.2011.csv", header=TRUE)
windfeb6.2011 <-na.exclude(windfeb6.2011)
coordinates(windfeb6.2011)<-c("Long","Lat") 
proj4string(windfeb6.2011)<-CRS("+proj=longlat +datum=WGS84") 
windfeb6.2011<-spTransform(windfeb6.2011,CRS(proj4string(bathy)))

p13.idw<-gstat::idw(feb6.2011~1, windfeb6.2011, newdata=grd, idp=2.0)
p13.idw<-raster(p13.idw)
#plot(p13.idw)
proj4string(scofeb6.2011)<-CRS("+proj=longlat +datum=WGS84")
scofeb6.2011=spTransform(scofeb6.2011,CRS(proj4string(bathy)))
#plot(scofeb6.2011,add=TRUE)
scofeb6.2011$wind=extract(p13.idw,scofeb6.2011)
scofeb6.2011$wind2=scale(scofeb6.2011$wind)

#feb9.2011
windfeb9.2011=read.csv("Wind/feb9.2011.csv", header=TRUE)
windfeb9.2011 <-na.exclude(windfeb9.2011)
coordinates(windfeb9.2011)<-c("Long","Lat") 
proj4string(windfeb9.2011)<-CRS("+proj=longlat +datum=WGS84") 
windfeb9.2011<-spTransform(windfeb9.2011,CRS(proj4string(bathy)))

p14.idw<-gstat::idw(feb9.2011~1, windfeb9.2011, newdata=grd, idp=2.0)
p14.idw<-raster(p14.idw)
#plot(p14.idw)
proj4string(scofeb9.2011)<-CRS("+proj=longlat +datum=WGS84")
scofeb9.2011=spTransform(scofeb9.2011,CRS(proj4string(bathy)))
#plot(scofeb9.2011,add=TRUE)
scofeb9.2011$wind=extract(p14.idw,scofeb9.2011)
scofeb9.2011$wind2=scale(scofeb9.2011$wind)

#feb11.2011
windfeb11.2011=read.csv("Wind/feb11.2011.csv", header=TRUE)
windfeb11.2011 <-na.exclude(windfeb11.2011)
coordinates(windfeb11.2011)<-c("Long","Lat") 
proj4string(windfeb11.2011)<-CRS("+proj=longlat +datum=WGS84") 
windfeb11.2011<-spTransform(windfeb11.2011,CRS(proj4string(bathy)))

p15.idw<-gstat::idw(feb11.2011~1, windfeb11.2011, newdata=grd, idp=2.0)
p15.idw<-raster(p15.idw)
#plot(p15.idw)
proj4string(scofeb11.2011)<-CRS("+proj=longlat +datum=WGS84")
scofeb11.2011=spTransform(scofeb11.2011,CRS(proj4string(bathy)))
#plot(scofeb11.2011,add=TRUE)
scofeb11.2011$wind=extract(p15.idw,scofeb11.2011)
scofeb11.2011$wind2=scale(scofeb11.2011$wind)

#feb12.2011
windfeb12.2011=read.csv("Wind/feb12.2011.csv", header=TRUE)
windfeb12.2011 <-na.exclude(windfeb12.2011)
coordinates(windfeb12.2011)<-c("Long","Lat") 
proj4string(windfeb12.2011)<-CRS("+proj=longlat +datum=WGS84") 
windfeb12.2011<-spTransform(windfeb12.2011,CRS(proj4string(bathy)))

p16.idw<-gstat::idw(feb12.2011~1, windfeb12.2011, newdata=grd, idp=2.0)
p16.idw<-raster(p16.idw)
#plot(p16.idw)
proj4string(scofeb12.2011)<-CRS("+proj=longlat +datum=WGS84")
scofeb12.2011=spTransform(scofeb12.2011,CRS(proj4string(bathy)))
#plot(scofeb12.2011,add=TRUE)
scofeb12.2011$wind=extract(p16.idw,scofeb12.2011)
scofeb12.2011$wind2=scale(scofeb12.2011$wind)

#feb13.2011
windfeb13.2011=read.csv("Wind/feb13.2011.csv", header=TRUE)
windfeb13.2011 <-na.exclude(windfeb13.2011)
coordinates(windfeb13.2011)<-c("Long","Lat") 
proj4string(windfeb13.2011)<-CRS("+proj=longlat +datum=WGS84") 
windfeb13.2011<-spTransform(windfeb13.2011,CRS(proj4string(bathy)))

p17.idw<-gstat::idw(feb13.2011~1, windfeb13.2011, newdata=grd, idp=2.0)
p17.idw<-raster(p17.idw)
#plot(p17.idw)
proj4string(scofeb13.2011)<-CRS("+proj=longlat +datum=WGS84")
scofeb13.2011=spTransform(scofeb13.2011,CRS(proj4string(bathy)))
#plot(scofeb13.2011,add=TRUE)
scofeb13.2011$wind=extract(p17.idw,scofeb13.2011)
scofeb13.2011$wind2=scale(scofeb13.2011$wind)

#feb16.2011
windfeb16.2011=read.csv("Wind/feb16.2011.csv", header=TRUE)
windfeb16.2011 <-na.exclude(windfeb16.2011)
coordinates(windfeb16.2011)<-c("Long","Lat") 
proj4string(windfeb16.2011)<-CRS("+proj=longlat +datum=WGS84") 
windfeb16.2011<-spTransform(windfeb16.2011,CRS(proj4string(bathy)))

p18.idw<-gstat::idw(feb16.2011~1, windfeb16.2011, newdata=grd, idp=2.0)
p18.idw<-raster(p18.idw)
#plot(p18.idw)
proj4string(scofeb16.2011)<-CRS("+proj=longlat +datum=WGS84")
scofeb16.2011=spTransform(scofeb16.2011,CRS(proj4string(bathy)))
#plot(scofeb16.2011,add=TRUE)
scofeb16.2011$wind=extract(p18.idw,scofeb16.2011)
scofeb16.2011$wind2=scale(scofeb16.2011$wind)

#feb17.2011
windfeb17.2011=read.csv("Wind/feb17.2011.csv", header=TRUE)
windfeb17.2011 <-na.exclude(windfeb17.2011)
coordinates(windfeb17.2011)<-c("Long","Lat") 
proj4string(windfeb17.2011)<-CRS("+proj=longlat +datum=WGS84") 
windfeb17.2011<-spTransform(windfeb17.2011,CRS(proj4string(bathy)))

p19.idw<-gstat::idw(feb17.2011~1, windfeb17.2011, newdata=grd, idp=2.0)
p19.idw<-raster(p19.idw)
#plot(p19.idw)
proj4string(scofeb17.2011)<-CRS("+proj=longlat +datum=WGS84")
scofeb17.2011=spTransform(scofeb17.2011,CRS(proj4string(bathy)))
#plot(scofeb17.2011,add=TRUE)
scofeb17.2011$wind=extract(p19.idw,scofeb17.2011)
scofeb17.2011$wind2=scale(scofeb17.2011$wind)

#feb4.2012
windfeb4.2012=read.csv("Wind/feb4.2012.csv", header=TRUE)
windfeb4.2012 <-na.exclude(windfeb4.2012)
coordinates(windfeb4.2012)<-c("Long","Lat") 
proj4string(windfeb4.2012)<-CRS("+proj=longlat +datum=WGS84") 
windfeb4.2012<-spTransform(windfeb4.2012,CRS(proj4string(bathy)))

p20.idw<-gstat::idw(feb4.2012~1, windfeb4.2012, newdata=grd, idp=2.0)
p20.idw<-raster(p20.idw)
#plot(p20.idw)
proj4string(scofeb4.2012)<-CRS("+proj=longlat +datum=WGS84")
scofeb4.2012=spTransform(scofeb4.2012,CRS(proj4string(bathy)))
#plot(scofeb4.2012,add=TRUE)
scofeb4.2012$wind=extract(p20.idw,scofeb4.2012)
scofeb4.2012$wind2=scale(scofeb4.2012$wind)

#feb5.2012
windfeb5.2012=read.csv("Wind/feb5.2012.csv", header=TRUE)
windfeb5.2012 <-na.exclude(windfeb5.2012)
coordinates(windfeb5.2012)<-c("Long","Lat") 
proj4string(windfeb5.2012)<-CRS("+proj=longlat +datum=WGS84") 
windfeb5.2012<-spTransform(windfeb5.2012,CRS(proj4string(bathy)))

p21.idw<-gstat::idw(feb5.2012~1, windfeb5.2012, newdata=grd, idp=2.0)
p21.idw<-raster(p21.idw)
#plot(p21.idw)
proj4string(scofeb5.2012)<-CRS("+proj=longlat +datum=WGS84")
scofeb5.2012=spTransform(scofeb5.2012,CRS(proj4string(bathy)))
#plot(scofeb5.2012,add=TRUE)
scofeb5.2012$wind=extract(p21.idw,scofeb5.2012)
scofeb5.2012$wind2=scale(scofeb5.2012$wind)

#feb8.2012
windfeb8.2012=read.csv("Wind/feb8.2012.csv", header=TRUE)
windfeb8.2012 <-na.exclude(windfeb8.2012)
coordinates(windfeb8.2012)<-c("Long","Lat") 
proj4string(windfeb8.2012)<-CRS("+proj=longlat +datum=WGS84") 
windfeb8.2012<-spTransform(windfeb8.2012,CRS(proj4string(bathy)))

p22.idw<-gstat::idw(feb8.2012~1, windfeb8.2012, newdata=grd, idp=2.0)
p22.idw<-raster(p22.idw)
#plot(p22.idw)
proj4string(scofeb8.2012)<-CRS("+proj=longlat +datum=WGS84")
scofeb8.2012=spTransform(scofeb8.2012,CRS(proj4string(bathy)))
#plot(scofeb8.2012,add=TRUE)
scofeb8.2012$wind=extract(p22.idw,scofeb8.2012)
scofeb8.2012$wind2=scale(scofeb8.2012$wind)

#feb17.2012
windfeb17.2012=read.csv("Wind/feb17.2012.csv", header=TRUE)
windfeb17.2012 <-na.exclude(windfeb17.2012)
coordinates(windfeb17.2012)<-c("Long","Lat") 
proj4string(windfeb17.2012)<-CRS("+proj=longlat +datum=WGS84") 
windfeb17.2012<-spTransform(windfeb17.2012,CRS(proj4string(bathy)))

p23.idw<-gstat::idw(feb17.2012~1, windfeb17.2012, newdata=grd, idp=2.0)
p23.idw<-raster(p23.idw)
#plot(p23.idw)
proj4string(scofeb17.2012)<-CRS("+proj=longlat +datum=WGS84")
scofeb17.2012=spTransform(scofeb17.2012,CRS(proj4string(bathy)))
#plot(scofeb17.2012,add=TRUE)
scofeb17.2012$wind=extract(p23.idw,scofeb17.2012)
scofeb17.2012$wind2=scale(scofeb17.2012$wind)

#feb18.2012
windfeb18.2012=read.csv("Wind/feb18.2012.csv", header=TRUE)
windfeb18.2012 <-na.exclude(windfeb18.2012)
coordinates(windfeb18.2012)<-c("Long","Lat") 
proj4string(windfeb18.2012)<-CRS("+proj=longlat +datum=WGS84") 
windfeb18.2012<-spTransform(windfeb18.2012,CRS(proj4string(bathy)))

p24.idw<-gstat::idw(feb18.2012~1, windfeb18.2012, newdata=grd, idp=2.0)
p24.idw<-raster(p24.idw)
#plot(p24.idw)
proj4string(scofeb18.2012)<-CRS("+proj=longlat +datum=WGS84")
scofeb18.2012=spTransform(scofeb18.2012,CRS(proj4string(bathy)))
#plot(scofeb18.2012,add=TRUE)
scofeb18.2012$wind=extract(p24.idw,scofeb18.2012)
scofeb18.2012$wind2=scale(scofeb18.2012$wind)

#feb21.2012
windfeb21.2012=read.csv("Wind/feb21.2012.csv", header=TRUE)
windfeb21.2012 <-na.exclude(windfeb21.2012)
coordinates(windfeb21.2012)<-c("Long","Lat") 
proj4string(windfeb21.2012)<-CRS("+proj=longlat +datum=WGS84") 
windfeb21.2012<-spTransform(windfeb21.2012,CRS(proj4string(bathy)))

p25.idw<-gstat::idw(feb21.2012~1, windfeb21.2012, newdata=grd, idp=2.0)
p25.idw<-raster(p25.idw)
#plot(p25.idw)
proj4string(scofeb21.2012)<-CRS("+proj=longlat +datum=WGS84")
scofeb21.2012=spTransform(scofeb21.2012,CRS(proj4string(bathy)))
#plot(scofeb21.2012,add=TRUE)
scofeb21.2012$wind=extract(p25.idw,scofeb21.2012)
scofeb21.2012$wind2=scale(scofeb21.2012$wind)



#WAVE
#feb2.2009
wavefeb2.2009=read.csv("Wave/feb2.2009.csv", header=TRUE)
wavefeb2.2009 <-na.exclude(wavefeb2.2009)
coordinates(wavefeb2.2009)<-c("Long","Lat") 
proj4string(wavefeb2.2009)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb2.2009<-spTransform(wavefeb2.2009,CRS(proj4string(bathy)))

x.range<-as.numeric(c(-82,-72))
y.range<-as.numeric(c(28,39))
grd<-expand.grid(x=seq(from=x.range[1], to=x.range[2], by =0.1),
                 y=seq(from=y.range[1], to=y.range[2],by=0.1))

names(grd)<-c("x","y")
coordinates(grd)<-c("x","y")
gridded(grd)<-TRUE
fullgrid(grd)<-TRUE
proj4string(grd)<-proj4string(bathy)

pa.idw<-gstat::idw(feb2.2009~1, wavefeb2.2009, newdata=grd, idp=2.0)
pa.idw<-raster(pa.idw)
#plot(pa.idw)
#plot(scofeb2.2009,add=TRUE)
scofeb2.2009$wave=extract(pa.idw,scofeb2.2009)
scofeb2.2009$wave2=scale(scofeb2.2009$wave)

#feb5.2009
wavefeb5.2009=read.csv("Wave/feb5.2009.csv", header=TRUE)
wavefeb5.2009 <-na.exclude(wavefeb5.2009)
coordinates(wavefeb5.2009)<-c("Long","Lat") 
proj4string(wavefeb5.2009)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb5.2009<-spTransform(wavefeb5.2009,CRS(proj4string(bathy)))

pb.idw<-gstat::idw(feb5.2009~1, wavefeb5.2009, newdata=grd, idp=2.0)
pb.idw<-raster(pb.idw)
#plot(pb.idw)
#plot(scofeb5.2009,add=TRUE)
scofeb5.2009$wave=extract(pb.idw,scofeb5.2009)
scofeb5.2009$wave2=scale(scofeb5.2009$wave)

#feb6.2009
pc.idw<-gstat::idw(feb6.2009~1, wavefeb2.2009, newdata=grd, idp=2.0)
pc.idw<-raster(pc.idw)
#plot(pc.idw)
#plot(scofeb6.2009,add=TRUE)
scofeb6.2009$wave=extract(pc.idw,scofeb6.2009)
scofeb6.2009$wave2=scale(scofeb6.2009$wave)

#feb7.2009
pd.idw<-gstat::idw(feb7.2009~1, wavefeb2.2009, newdata=grd, idp=2.0)
pd.idw<-raster(pd.idw)
#plot(pd.idw)
#plot(scofeb7.2009,add=TRUE)
scofeb7.2009$wave=extract(pd.idw,scofeb7.2009)
scofeb7.2009$wave2=scale(scofeb7.2009$wave)

#feb8.2009
wavefeb8.2009=read.csv("Wave/feb8.2009.csv", header=TRUE)
wavefeb8.2009 <-na.exclude(wavefeb8.2009)
coordinates(wavefeb8.2009)<-c("Long","Lat") 
proj4string(wavefeb8.2009)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb8.2009<-spTransform(wavefeb8.2009,CRS(proj4string(bathy)))

pe.idw<-gstat::idw(feb8.2009~1, wavefeb8.2009, newdata=grd, idp=2.0)
pe.idw<-raster(pe.idw)
#plot(pe.idw)
#plot(scofeb8.2009,add=TRUE)
scofeb8.2009$wave=extract(pe.idw,scofeb8.2009)
scofeb8.2009$wave2=scale(scofeb8.2009$wave)

#feb9.2009
pf.idw<-gstat::idw(feb9.2009~1, wavefeb8.2009, newdata=grd, idp=2.0)
pf.idw<-raster(pf.idw)
#plot(pf.idw)
#plot(scofeb9.2009,add=TRUE)
scofeb9.2009$wave=extract(pf.idw,scofeb9.2009)
scofeb9.2009$wave2=scale(scofeb9.2009$wave)

#feb1.2010
wavefeb1.2010=read.csv("Wave/feb1.2010.csv", header=TRUE)
wavefeb1.2010 <-na.exclude(wavefeb1.2010)
coordinates(wavefeb1.2010)<-c("Long","Lat") 
proj4string(wavefeb1.2010)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb1.2010<-spTransform(wavefeb1.2010,CRS(proj4string(bathy)))

pg.idw<-gstat::idw(feb1.2010~1, wavefeb1.2010, newdata=grd, idp=2.0)
pg.idw<-raster(pg.idw)
#plot(pg.idw)
#plot(scofeb1.2010,add=TRUE)
scofeb1.2010$wave=extract(pg.idw,scofeb1.2010)
scofeb1.2010$wave2=scale(scofeb1.2010$wave)

#feb3.2010
ph.idw<-gstat::idw(feb3.2010~1, wavefeb1.2010, newdata=grd, idp=2.0)
ph.idw<-raster(ph.idw)
#plot(ph.idw)
#plot(scofeb3.2010,add=TRUE)
scofeb3.2010$wave=extract(ph.idw,scofeb3.2010)
scofeb3.2010$wave2=scale(scofeb3.2010$wave)

#feb4.2010
pi.idw<-gstat::idw(feb4.2010~1, wavefeb1.2010, newdata=grd, idp=2.0)
pi.idw<-raster(pi.idw)
#plot(pi.idw)
#plot(scofeb4.2010,add=TRUE)
scofeb4.2010$wave=extract(pi.idw,scofeb4.2010)
scofeb4.2010$wave2=scale(scofeb4.2010$wave)

#feb9.2010
wavefeb9.2010=read.csv("Wave/feb9.2010.csv", header=TRUE)
wavefeb9.2010 <-na.exclude(wavefeb9.2010)
coordinates(wavefeb9.2010)<-c("Long","Lat") 
proj4string(wavefeb9.2010)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb9.2010<-spTransform(wavefeb9.2010,CRS(proj4string(bathy)))

pj.idw<-gstat::idw(feb9.2010~1, wavefeb9.2010, newdata=grd, idp=2.0)
pj.idw<-raster(pj.idw)
#plot(pj.idw)
#plot(scofeb9.2010,add=TRUE)
scofeb9.2010$wave=extract(pj.idw,scofeb9.2010)
scofeb9.2010$wave2=scale(scofeb9.2010$wave)

#feb11.2010
pk.idw<-gstat::idw(feb11.2010~1, wavefeb9.2010, newdata=grd, idp=2.0)
pk.idw<-raster(pk.idw)
#plot(pk.idw)
#plot(scofeb11.2010,add=TRUE)
scofeb11.2010$wave=extract(pk.idw,scofeb11.2010)
scofeb11.2010$wave2=scale(scofeb11.2010$wave)

#feb12.2010
pl.idw<-gstat::idw(feb12.2010~1, wavefeb1.2010, newdata=grd, idp=2.0)
pl.idw<-raster(pl.idw)
#plot(pl.idw)
#plot(scofeb12.2010,add=TRUE)
scofeb12.2010$wave=extract(pl.idw,scofeb12.2010)
scofeb12.2010$wave2=scale(scofeb12.2010$wave)

#feb15.2010
pm.idw<-gstat::idw(feb15.2010~1, wavefeb9.2010, newdata=grd, idp=2.0)
pm.idw<-raster(pm.idw)
#plot(pm.idw)
#plot(scofeb15.2010,add=TRUE)
scofeb15.2010$wave=extract(pm.idw,scofeb15.2010)
scofeb15.2010$wave2=scale(scofeb15.2010$wave)

#feb3.2011
wavefeb3.2011=read.csv("Wave/feb3.2011.csv", header=TRUE)
wavefeb3.2011 <-na.exclude(wavefeb3.2011)
coordinates(wavefeb3.2011)<-c("Long","Lat") 
proj4string(wavefeb3.2011)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb3.2011<-spTransform(wavefeb3.2011,CRS(proj4string(bathy)))

pn.idw<-gstat::idw(feb3.2011~1, wavefeb3.2011, newdata=grd, idp=2.0)
pn.idw<-raster(pn.idw)
#plot(pn.idw)
#plot(scofeb3.2011,add=TRUE)
scofeb3.2011$wave=extract(pn.idw,scofeb3.2011)
scofeb3.2011$wave2=scale(scofeb3.2011$wave)

#feb6.2011
wavefeb6.2011=read.csv("Wave/feb6.2011.csv", header=TRUE)
wavefeb6.2011 <-na.exclude(wavefeb6.2011)
coordinates(wavefeb6.2011)<-c("Long","Lat") 
proj4string(wavefeb6.2011)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb6.2011<-spTransform(wavefeb6.2011,CRS(proj4string(bathy)))

po.idw<-gstat::idw(feb6.2011~1, wavefeb6.2011, newdata=grd, idp=2.0)
po.idw<-raster(po.idw)
#plot(po.idw)
#plot(scofeb6.2011,add=TRUE)
scofeb6.2011$wave=extract(po.idw,scofeb6.2011)
scofeb6.2011$wave2=scale(scofeb6.2011$wave)

#feb9.2011
wavefeb9.2011=read.csv("Wave/feb9.2011.csv", header=TRUE)
wavefeb9.2011 <-na.exclude(wavefeb9.2011)
coordinates(wavefeb9.2011)<-c("Long","Lat") 
proj4string(wavefeb9.2011)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb9.2011<-spTransform(wavefeb9.2011,CRS(proj4string(bathy)))

pp.idw<-gstat::idw(feb9.2011~1, wavefeb9.2011, newdata=grd, idp=2.0)
pp.idw<-raster(pp.idw)
#plot(pp.idw)
#plot(scofeb9.2011,add=TRUE)
scofeb9.2011$wave=extract(pp.idw,scofeb9.2011)
scofeb9.2011$wave2=scale(scofeb9.2011$wave)

#feb11.2011
wavefeb11.2011=read.csv("Wave/wave.csv", header=TRUE)
wavefeb11.2011 <-na.exclude(wavefeb11.2011)
coordinates(wavefeb11.2011)<-c("Long","Lat") 
proj4string(wavefeb11.2011)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb11.2011<-spTransform(wavefeb11.2011,CRS(proj4string(bathy)))

pq.idw<-gstat::idw(feb11.2011~1, wavefeb11.2011, newdata=grd, idp=2.0)
pq.idw<-raster(pq.idw)
#plot(pq.idw)
#plot(scofeb11.2011,add=TRUE)
scofeb11.2011$wave=extract(pq.idw,scofeb11.2011)
scofeb11.2011$wave2=scale(scofeb11.2011$wave)

#feb12.2011
wavefeb12.2011=read.csv("Wave/feb12.2011.csv", header=TRUE)
wavefeb12.2011 <-na.exclude(wavefeb12.2011)
coordinates(wavefeb12.2011)<-c("Long","Lat") 
proj4string(wavefeb12.2011)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb12.2011<-spTransform(wavefeb12.2011,CRS(proj4string(bathy)))

pr.idw<-gstat::idw(feb12.2011~1, wavefeb12.2011, newdata=grd, idp=2.0)
pr.idw<-raster(pr.idw)
#plot(pr.idw)
#plot(scofeb12.2011,add=TRUE)
scofeb12.2011$wave=extract(pr.idw,scofeb12.2011)
scofeb12.2011$wave2=scale(scofeb12.2011$wave)

#feb13.2011
ps.idw<-gstat::idw(feb13.2011~1, wavefeb6.2011, newdata=grd, idp=2.0)
ps.idw<-raster(ps.idw)
#plot(ps.idw)
#plot(scofeb13.2011,add=TRUE)
scofeb13.2011$wave=extract(ps.idw,scofeb13.2011)
scofeb13.2011$wave2=scale(scofeb13.2011$wave)

#feb16.2011
wavefeb16.2011=read.csv("Wave/feb16.2011.csv", header=TRUE)
wavefeb16.2011 <-na.exclude(wavefeb16.2011)
coordinates(wavefeb16.2011)<-c("Long","Lat") 
proj4string(wavefeb16.2011)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb16.2011<-spTransform(wavefeb16.2011,CRS(proj4string(bathy)))

pt.idw<-gstat::idw(feb16.2011~1, wavefeb16.2011, newdata=grd, idp=2.0)
pt.idw<-raster(pt.idw)
#plot(pt.idw)
#plot(scofeb16.2011,add=TRUE)
scofeb16.2011$wave=extract(pt.idw,scofeb16.2011)
scofeb16.2011$wave2=scale(scofeb16.2011$wave)

#feb17.2011
pu.idw<-gstat::idw(feb17.2011~1, wavefeb11.2011, newdata=grd, idp=2.0)
pu.idw<-raster(pu.idw)
#plot(pu.idw)
#plot(scofeb17.2011,add=TRUE)
scofeb17.2011$wave=extract(pu.idw,scofeb17.2011)
scofeb17.2011$wave2=scale(scofeb17.2011$wave)

#feb4.2012
wavefeb4.2012=read.csv("Wave/feb4.2012.csv", header=TRUE)
wavefeb4.2012 <-na.exclude(wavefeb4.2012)
coordinates(wavefeb4.2012)<-c("Long","Lat") 
proj4string(wavefeb4.2012)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb4.2012<-spTransform(wavefeb4.2012,CRS(proj4string(bathy)))

pv.idw<-gstat::idw(feb4.2012~1, wavefeb4.2012, newdata=grd, idp=2.0)
pv.idw<-raster(pv.idw)
#plot(pv.idw)
#plot(scofeb4.2012,add=TRUE)
scofeb4.2012$wave=extract(pv.idw,scofeb4.2012)
scofeb4.2012$wave2=scale(scofeb4.2012$wave)

#feb5.2012
pw.idw<-gstat::idw(feb5.2012~1, wavefeb4.2012, newdata=grd, idp=2.0)
pw.idw<-raster(pw.idw)
#plot(pw.idw)
#plot(scofeb5.2012,add=TRUE)
scofeb5.2012$wave=extract(pw.idw,scofeb5.2012)
scofeb5.2012$wave2=scale(scofeb5.2012$wave)

#feb8.2012
px.idw<-gstat::idw(feb8.2012~1, wavefeb4.2012, newdata=grd, idp=2.0)
px.idw<-raster(px.idw)
#plot(px.idw)
#plot(scofeb8.2012,add=TRUE)
scofeb8.2012$wave=extract(px.idw,scofeb8.2012)
scofeb8.2012$wave2=scale(scofeb8.2012$wave)

#feb17.2012
px.idw<-gstat::idw(feb17.2012~1, wavefeb4.2012, newdata=grd, idp=2.0)
px.idw<-raster(px.idw)
#plot(px.idw)
#plot(scofeb17.2012,add=TRUE)
scofeb17.2012$wave=extract(px.idw,scofeb17.2012)
scofeb17.2012$wave2=scale(scofeb17.2012$wave)

#feb18.2012
wavefeb18.2012=read.csv("Wave/feb18.2012.csv", header=TRUE)
wavefeb18.2012 <-na.exclude(wavefeb18.2012)
coordinates(wavefeb18.2012)<-c("Long","Lat") 
proj4string(wavefeb18.2012)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb18.2012<-spTransform(wavefeb18.2012,CRS(proj4string(bathy)))

py.idw<-gstat::idw(feb18.2012~1, wavefeb18.2012, newdata=grd, idp=2.0)
py.idw<-raster(py.idw)
#plot(py.idw)
#plot(scofeb18.2012,add=TRUE)
scofeb18.2012$wave=extract(py.idw,scofeb18.2012)
scofeb18.2012$wave2=scale(scofeb18.2012$wave)

#feb21.2012
pz.idw<-gstat::idw(feb21.2012~1, wavefeb18.2012, newdata=grd, idp=2.0)
pz.idw<-raster(pz.idw)
#plot(pz.idw)
#plot(scofeb21.2012,add=TRUE)
scofeb21.2012$wave=extract(pz.idw,scofeb21.2012)
scofeb21.2012$wave2=scale(scofeb21.2012$wave)


sco2009<-rbind(scofeb2.2009,scofeb5.2009,scofeb6.2009,scofeb7.2009,scofeb8.2009,scofeb9.2009)
sco2010<-rbind(scofeb1.2010,scofeb3.2010,scofeb4.2010,scofeb9.2010,scofeb11.2010,scofeb12.2010,scofeb15.2010)
sco2011<-rbind(scofeb3.2011,scofeb6.2011,scofeb9.2011,scofeb11.2011,scofeb12.2011,scofeb13.2011,scofeb16.2011,scofeb17.2011)
sco2012<-rbind(scofeb4.2012,scofeb5.2012,scofeb8.2012,scofeb17.2012,scofeb18.2012,scofeb21.2012)
scototal<-rbind(sco2009,sco2010,sco2011,sco2012)

sco2<-cbind(sco2,scototal[18:21])
sco2$sednum=as.factor(sco2@data$substrate$SEDNUM)
sco2$substrate<-NULL
sco2$eco=as.factor(sco2@data$eco$ECOREGION)
sco2$bival=as.factor(sco2$bival)
sco2$wind=as.numeric(sco2$wind)
sco2$wind2=as.numeric(sco2$wind2)
sco2$wave=as.numeric(sco2$wave)
sco2$wave2=as.numeric(sco2$wave2)



#transect data

transect=rgdal::readOGR("Layers/transects/transect.shp")
transect=spTransform(transect,CRS(proj4string(bathy)))
tran2009<-subset(transect, Year==2009, select=UID:Year)
tran2010<-subset(transect, Year==2010, select=UID:Year)
tran2011<-subset(transect, Year==2011, select=UID:Year)
tran2012<-subset(transect, Year==2012, select=UID:Year)
tran2009<-subset(tran2009, Seat=="LF", select=UID:Year)
tran2010<-subset(tran2010, Seat=="LF", select=UID:Year)
tran2011<-subset(tran2011, Seat=="LF", select=UID:Year)
tran2012<-subset(tran2012, Seat=="LF", select=UID:Year)

#segmenting transects

library(DSpat)
library(GISTools)
library(spatstat)
#2009
tran2009=spTransform(tran2009,
                  CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0"))
tran.sub=tran2009
#tran.sub=tran2009[!duplicated(tran2009$Transect),] #subset to remove duplicates
tran.sub$id=seq(1,157) #add a unique ID to each transect

#this just uses transect 1 as an example
x0=coordinates(tran.sub)[[1]][[1]][1,1]
x1=coordinates(tran.sub)[[1]][[1]][90,1]
y0=coordinates(tran.sub)[[1]][[1]][1,2]
y1=coordinates(tran.sub)[[1]][[1]][90,2]

lines=data.frame(label=1,x0=x0,x1=x1,y0=y0,y1=y1)

tmp=as.psp(tran.sub[tran.sub$id==1,])

strtransect<-lines_to_strips(lines,as.owin(tmp), width=550)
plot(tran.sub[tran.sub$id==1,])
points(strtransect$full.transects[[1]]) #not subdividing transects still

## functions below taken from http://rstudio-pubs-static.s3.amazonaws.com/10685_1f7266d60db7432486517a111c76ac8b.html

#First, basic segmentation
CreateSegment <- function(coords, from, to) {
  distance <- 0
  coordsOut <- c()
  biggerThanFrom <- F
  for (i in 1:(nrow(coords) - 1)) {
    d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 
                                                                             1, 2])^2)
    distance <- distance + d
    if (!biggerThanFrom && (distance > from)) {
      w <- 1 - (distance - from)/d
      x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
      y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
      coordsOut <- rbind(coordsOut, c(x, y))
      biggerThanFrom <- T
    }
    if (biggerThanFrom) {
      if (distance > to) {
        w <- 1 - (distance - to)/d
        x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
        y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
        coordsOut <- rbind(coordsOut, c(x, y))
        break
      }
      coordsOut <- rbind(coordsOut, c(coords[i + 1, 1], coords[i + 1, 
                                                               2]))
    }
  }
  return(coordsOut)
}

#now create multiple segments building on last function
CreateSegments <- function(coords, length = 0, n.parts = 0) {
  stopifnot((length > 0 || n.parts > 0))
  # calculate total length line
  total_length <- 0
  for (i in 1:(nrow(coords) - 1)) {
    d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 
                                                                             1, 2])^2)
    total_length <- total_length + d
  }
  
  # calculate stationing of segments
  if (length > 0) {
    stationing <- c(seq(from = 0, to = total_length, by = length), total_length)
  } else {
    stationing <- c(seq(from = 0, to = total_length, length.out = n.parts), 
                    total_length)
  }
  
  # calculate segments and store in list
  newlines <- list()
  for (i in 1:(length(stationing) - 1)) {
    newlines[[i]] <- CreateSegment(coords, stationing[i], stationing[i + 
                                                                       1])
  }
  return(newlines)
}

#extract x and y locations of segments for functions above. Example with transect 1
transect.locs=coordinates(tran.sub)[[1]][[1]]

#length in m -- update segment length to relevant length for analysis
segs = CreateSegments(transect.locs,length=1000)

plot(tran.sub[tran.sub$id==1,])
col = "red"
for (i in 1:length(segs)) {
  col <- ifelse(col == "red", "black", "red")
  lines(as.matrix(segs[[i]]), col = col, lwd = 2)
}

MergeLast <- function(lst) {
  l <- length(lst)
  lst[[l - 1]] <- rbind(lst[[l - 1]], lst[[l]])
  lst <- lst[1:(l - 1)]
  return(lst)
}

#translate above to SpatialLines structure
SegmentSpatialLines <- function(sl, length = 0, n.parts = 0, merge.last = FALSE) {
  stopifnot((length > 0 || n.parts > 0))
  id <- 0
  newlines <- list()
  sl <- as(sl, "SpatialLines")
  for (lines in sl@lines) {
    for (line in lines@Lines) {
      crds <- line@coords
      # create segments
      segments <- CreateSegments(coords = crds, length, n.parts)
      if (merge.last && length(segments) > 1) {
        # in case there is only one segment, merging would result into error
        segments <- MergeLast(segments)
      }
      # transform segments to lineslist for SpatialLines object
      for (segment in segments) {
        newlines <- c(newlines, Lines(list(Line(unlist(segment))), ID = as.character(id)))
        id <- id + 1
      }
    }
  }
  return(SpatialLines(newlines))
}

#example above with transect 2 added
tran2=coordinates(tran.sub)[[2]][[1]]

#transects 1 & 2 as spatial lines
sl <- SpatialLines(list(Lines(list(Line(coords = transect.locs)), 
                              ID = "1"), Lines(list(Line(coords = tran2)),ID = "2")))

#segmenting spatial lines objects
sl2 <- SegmentSpatialLines(sl, length = 1000, merge.last = TRUE)

# plot
plot(sl2, col = rep(c(1, 2), length.out = length(sl2)), axes = T)

#now create a for loop to convert all transects to Lines objects
ntransect=length(unique(tran.sub$id))
new.line=list(Line(coords=transect.locs))
new.lines=list(Lines(new.line,ID=1))
for(i in 2:ntransect){
  new.line=list(Line(coords=coordinates(tran.sub)[[i]][[1]]))
  new.lines[[i]]=Lines(new.line,ID=i)
}

sp.out=SpatialLines(new.lines,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0"))
#segment all transects
sp.seg <- SegmentSpatialLines(sp.out, length = 1000, merge.last = TRUE)
#plot(sp.seg, col = rep(c(1, 2), length.out = length(sp.seg)), axes = T)

#head(sp.seg)

#loop over 18453 segments. Needs to have x0 etc. indexed 
#so that each polygon corner (x0,x1,y0,y1) is saved as a unique point
x0=as.numeric()
x1=as.numeric()
y0=as.numeric()
y1=as.numeric()

for(i in 1:length(sp.seg)){
  x0[i]=coordinates(sp.seg)[[i]][[1]][1,1]
  x1[i]=coordinates(sp.seg)[[i]][[1]][dim(coordinates(sp.seg)[[i]][[1]])[1],1]
  y0[i]=coordinates(sp.seg)[[i]][[1]][1,2]
  y1[i]=coordinates(sp.seg)[[i]][[1]][dim(coordinates(sp.seg)[[i]][[1]])[1],2]
}

lines=data.frame(label=seq(1,length(sp.seg)),x0=x0,x1=x1,y0=y0,y1=y1)

#tmp=as.psp(tran.sub[tran.sub$id==1,])
tmp=as.psp(sp.seg)
#still need to figure out how to convert these files back to a spatialpolygon 
#to sample within each one. 
strtransect<-lines_to_strips(lines,as.owin(tmp), width=550)
#over 50 warnings they were all the same as below
#1: In `[<-`(`*tmp*`, i, value = gpc) :
#implicit list embedding of S4 objects is deprecated

#convert each row of lines to polygon
npoly=dim(lines)[1]
new.poly=list(Polygon(coords=coordinates(strtransect$full.transects[[1]])))
new.polys=list(Polygons(new.poly,ID=1))
for(i in 2:(dim(lines)[1])){
  new.poly=list(Polygon(coords=coordinates(strtransect$full.transects[[i]])))
  new.polys[[i]]=Polygons(new.poly,ID=i)
}

out2009=SpatialPolygons(new.polys)
proj4string(out2009)=CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0")
out2009=spTransform(out2009,CRS(proj4string(bathy)))
#out2009<-as(out2009, "SpatialPolygonsDataFrame")
#writeOGR(obj=out2009, dsn="tempdir", layer="transect2009", driver="ESRI Shapefile")





#2010
tran2010=spTransform(tran2010,
                     CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0"))
tran.sub=tran2010
#tran.sub=tran2010[!duplicated(tran2010$Transect),] #subset to remove duplicates
tran.sub$id=seq(1,152) #add a unique ID to each transect

#this just uses transect 1 as an example
x0=coordinates(tran.sub)[[1]][[1]][1,1]
x1=coordinates(tran.sub)[[1]][[1]][90,1]
y0=coordinates(tran.sub)[[1]][[1]][1,2]
y1=coordinates(tran.sub)[[1]][[1]][90,2]

lines=data.frame(label=1,x0=x0,x1=x1,y0=y0,y1=y1)

tmp=as.psp(tran.sub[tran.sub$id==1,])

strtransect<-lines_to_strips(lines,as.owin(tmp), width=550)
plot(tran.sub[tran.sub$id==1,])
points(strtransect$full.transects[[1]]) #not subdividing transects still

## functions below taken from http://rstudio-pubs-static.s3.amazonaws.com/10685_1f7266d60db7432486517a111c76ac8b.html

#First, basic segmentation
CreateSegment <- function(coords, from, to) {
  distance <- 0
  coordsOut <- c()
  biggerThanFrom <- F
  for (i in 1:(nrow(coords) - 1)) {
    d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 
                                                                             1, 2])^2)
    distance <- distance + d
    if (!biggerThanFrom && (distance > from)) {
      w <- 1 - (distance - from)/d
      x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
      y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
      coordsOut <- rbind(coordsOut, c(x, y))
      biggerThanFrom <- T
    }
    if (biggerThanFrom) {
      if (distance > to) {
        w <- 1 - (distance - to)/d
        x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
        y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
        coordsOut <- rbind(coordsOut, c(x, y))
        break
      }
      coordsOut <- rbind(coordsOut, c(coords[i + 1, 1], coords[i + 1, 
                                                               2]))
    }
  }
  return(coordsOut)
}

#now create multiple segments building on last function
CreateSegments <- function(coords, length = 0, n.parts = 0) {
  stopifnot((length > 0 || n.parts > 0))
  # calculate total length line
  total_length <- 0
  for (i in 1:(nrow(coords) - 1)) {
    d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 
                                                                             1, 2])^2)
    total_length <- total_length + d
  }
  
  # calculate stationing of segments
  if (length > 0) {
    stationing <- c(seq(from = 0, to = total_length, by = length), total_length)
  } else {
    stationing <- c(seq(from = 0, to = total_length, length.out = n.parts), 
                    total_length)
  }
  
  # calculate segments and store in list
  newlines <- list()
  for (i in 1:(length(stationing) - 1)) {
    newlines[[i]] <- CreateSegment(coords, stationing[i], stationing[i + 
                                                                       1])
  }
  return(newlines)
}

#extract x and y locations of segments for functions above. Example with transect 1
transect.locs=coordinates(tran.sub)[[1]][[1]]

#length in m -- update segment length to relevant length for analysis
segs = CreateSegments(transect.locs,length=1000)

plot(tran.sub[tran.sub$id==1,])
col = "red"
for (i in 1:length(segs)) {
  col <- ifelse(col == "red", "black", "red")
  lines(as.matrix(segs[[i]]), col = col, lwd = 2)
}

MergeLast <- function(lst) {
  l <- length(lst)
  lst[[l - 1]] <- rbind(lst[[l - 1]], lst[[l]])
  lst <- lst[1:(l - 1)]
  return(lst)
}

#translate above to SpatialLines structure
SegmentSpatialLines <- function(sl, length = 0, n.parts = 0, merge.last = FALSE) {
  stopifnot((length > 0 || n.parts > 0))
  id <- 0
  newlines <- list()
  sl <- as(sl, "SpatialLines")
  for (lines in sl@lines) {
    for (line in lines@Lines) {
      crds <- line@coords
      # create segments
      segments <- CreateSegments(coords = crds, length, n.parts)
      if (merge.last && length(segments) > 1) {
        # in case there is only one segment, merging would result into error
        segments <- MergeLast(segments)
      }
      # transform segments to lineslist for SpatialLines object
      for (segment in segments) {
        newlines <- c(newlines, Lines(list(Line(unlist(segment))), ID = as.character(id)))
        id <- id + 1
      }
    }
  }
  return(SpatialLines(newlines))
}

#example above with transect 2 added
tran2=coordinates(tran.sub)[[2]][[1]]

#transects 1 & 2 as spatial lines
sl <- SpatialLines(list(Lines(list(Line(coords = transect.locs)), 
                              ID = "1"), Lines(list(Line(coords = tran2)),ID = "2")))

#segmenting spatial lines objects
sl2 <- SegmentSpatialLines(sl, length = 1000, merge.last = TRUE)

# plot
plot(sl2, col = rep(c(1, 2), length.out = length(sl2)), axes = T)

#now create a for loop to convert all transects to Lines objects
ntransect=length(unique(tran.sub$id))
new.line=list(Line(coords=transect.locs))
new.lines=list(Lines(new.line,ID=1))
for(i in 2:ntransect){
  new.line=list(Line(coords=coordinates(tran.sub)[[i]][[1]]))
  new.lines[[i]]=Lines(new.line,ID=i)
}

sp.out=SpatialLines(new.lines,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0"))
#segment all transects
sp.seg <- SegmentSpatialLines(sp.out, length = 1000, merge.last = TRUE)
#plot(sp.seg, col = rep(c(1, 2), length.out = length(sp.seg)), axes = T)

#head(sp.seg)

#loop over 18453 segments. Needs to have x0 etc. indexed 
#so that each polygon corner (x0,x1,y0,y1) is saved as a unique point
x0=as.numeric()
x1=as.numeric()
y0=as.numeric()
y1=as.numeric()

for(i in 1:length(sp.seg)){
  x0[i]=coordinates(sp.seg)[[i]][[1]][1,1]
  x1[i]=coordinates(sp.seg)[[i]][[1]][dim(coordinates(sp.seg)[[i]][[1]])[1],1]
  y0[i]=coordinates(sp.seg)[[i]][[1]][1,2]
  y1[i]=coordinates(sp.seg)[[i]][[1]][dim(coordinates(sp.seg)[[i]][[1]])[1],2]
}

lines=data.frame(label=seq(1,length(sp.seg)),x0=x0,x1=x1,y0=y0,y1=y1)

#tmp=as.psp(tran.sub[tran.sub$id==1,])
tmp=as.psp(sp.seg)
#still need to figure out how to convert these files back to a spatialpolygon 
#to sample within each one. 
strtransect<-lines_to_strips(lines,as.owin(tmp), width=550)
#over 50 warnings they were all the same as below
#1: In `[<-`(`*tmp*`, i, value = gpc) :
#implicit list embedding of S4 objects is deprecated

#convert each row of lines to polygon
npoly=dim(lines)[1]
new.poly=list(Polygon(coords=coordinates(strtransect$full.transects[[1]])))
new.polys=list(Polygons(new.poly,ID=1))
for(i in 2:(dim(lines)[1])){
  new.poly=list(Polygon(coords=coordinates(strtransect$full.transects[[i]])))
  new.polys[[i]]=Polygons(new.poly,ID=i)
}

out2010=SpatialPolygons(new.polys)
proj4string(out2010)=CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0")
out2010=spTransform(out2010,CRS(proj4string(bathy)))
#out2010<-as(out2010, "SpatialPolygonsDataFrame")
#writeOGR(obj=out2010, dsn="tempdir", layer="transect2010", driver="ESRI Shapefile")





#2011
tran2011=spTransform(tran2011,
                     CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0"))
tran.sub=tran2011
#tran.sub=tran2011[!duplicated(tran2011$Transect),] #subset to remove duplicates
tran.sub$id=seq(1,162) #add a unique ID to each transect

#this just uses transect 1 as an example
x0=coordinates(tran.sub)[[1]][[1]][1,1]
x1=coordinates(tran.sub)[[1]][[1]][90,1]
y0=coordinates(tran.sub)[[1]][[1]][1,2]
y1=coordinates(tran.sub)[[1]][[1]][90,2]

lines=data.frame(label=1,x0=x0,x1=x1,y0=y0,y1=y1)

tmp=as.psp(tran.sub[tran.sub$id==1,])

strtransect<-lines_to_strips(lines,as.owin(tmp), width=550)
plot(tran.sub[tran.sub$id==1,])
points(strtransect$full.transects[[1]]) #not subdividing transects still

## functions below taken from http://rstudio-pubs-static.s3.amazonaws.com/10685_1f7266d60db7432486517a111c76ac8b.html

#First, basic segmentation
CreateSegment <- function(coords, from, to) {
  distance <- 0
  coordsOut <- c()
  biggerThanFrom <- F
  for (i in 1:(nrow(coords) - 1)) {
    d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 
                                                                             1, 2])^2)
    distance <- distance + d
    if (!biggerThanFrom && (distance > from)) {
      w <- 1 - (distance - from)/d
      x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
      y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
      coordsOut <- rbind(coordsOut, c(x, y))
      biggerThanFrom <- T
    }
    if (biggerThanFrom) {
      if (distance > to) {
        w <- 1 - (distance - to)/d
        x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
        y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
        coordsOut <- rbind(coordsOut, c(x, y))
        break
      }
      coordsOut <- rbind(coordsOut, c(coords[i + 1, 1], coords[i + 1, 
                                                               2]))
    }
  }
  return(coordsOut)
}

#now create multiple segments building on last function
CreateSegments <- function(coords, length = 0, n.parts = 0) {
  stopifnot((length > 0 || n.parts > 0))
  # calculate total length line
  total_length <- 0
  for (i in 1:(nrow(coords) - 1)) {
    d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 
                                                                             1, 2])^2)
    total_length <- total_length + d
  }
  
  # calculate stationing of segments
  if (length > 0) {
    stationing <- c(seq(from = 0, to = total_length, by = length), total_length)
  } else {
    stationing <- c(seq(from = 0, to = total_length, length.out = n.parts), 
                    total_length)
  }
  
  # calculate segments and store in list
  newlines <- list()
  for (i in 1:(length(stationing) - 1)) {
    newlines[[i]] <- CreateSegment(coords, stationing[i], stationing[i + 
                                                                       1])
  }
  return(newlines)
}

#extract x and y locations of segments for functions above. Example with transect 1
transect.locs=coordinates(tran.sub)[[1]][[1]]

#length in m -- update segment length to relevant length for analysis
segs = CreateSegments(transect.locs,length=1000)

plot(tran.sub[tran.sub$id==1,])
col = "red"
for (i in 1:length(segs)) {
  col <- ifelse(col == "red", "black", "red")
  lines(as.matrix(segs[[i]]), col = col, lwd = 2)
}

MergeLast <- function(lst) {
  l <- length(lst)
  lst[[l - 1]] <- rbind(lst[[l - 1]], lst[[l]])
  lst <- lst[1:(l - 1)]
  return(lst)
}

#translate above to SpatialLines structure
SegmentSpatialLines <- function(sl, length = 0, n.parts = 0, merge.last = FALSE) {
  stopifnot((length > 0 || n.parts > 0))
  id <- 0
  newlines <- list()
  sl <- as(sl, "SpatialLines")
  for (lines in sl@lines) {
    for (line in lines@Lines) {
      crds <- line@coords
      # create segments
      segments <- CreateSegments(coords = crds, length, n.parts)
      if (merge.last && length(segments) > 1) {
        # in case there is only one segment, merging would result into error
        segments <- MergeLast(segments)
      }
      # transform segments to lineslist for SpatialLines object
      for (segment in segments) {
        newlines <- c(newlines, Lines(list(Line(unlist(segment))), ID = as.character(id)))
        id <- id + 1
      }
    }
  }
  return(SpatialLines(newlines))
}

#example above with transect 2 added
tran2=coordinates(tran.sub)[[2]][[1]]

#transects 1 & 2 as spatial lines
sl <- SpatialLines(list(Lines(list(Line(coords = transect.locs)), 
                              ID = "1"), Lines(list(Line(coords = tran2)),ID = "2")))

#segmenting spatial lines objects
sl2 <- SegmentSpatialLines(sl, length = 1000, merge.last = TRUE)

# plot
plot(sl2, col = rep(c(1, 2), length.out = length(sl2)), axes = T)

#now create a for loop to convert all transects to Lines objects
ntransect=length(unique(tran.sub$id))
new.line=list(Line(coords=transect.locs))
new.lines=list(Lines(new.line,ID=1))
for(i in 2:ntransect){
  new.line=list(Line(coords=coordinates(tran.sub)[[i]][[1]]))
  new.lines[[i]]=Lines(new.line,ID=i)
}

sp.out=SpatialLines(new.lines,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0"))
#segment all transects
sp.seg <- SegmentSpatialLines(sp.out, length = 1000, merge.last = TRUE)
#plot(sp.seg, col = rep(c(1, 2), length.out = length(sp.seg)), axes = T)

#head(sp.seg)

#loop over 18453 segments. Needs to have x0 etc. indexed 
#so that each polygon corner (x0,x1,y0,y1) is saved as a unique point
x0=as.numeric()
x1=as.numeric()
y0=as.numeric()
y1=as.numeric()

for(i in 1:length(sp.seg)){
  x0[i]=coordinates(sp.seg)[[i]][[1]][1,1]
  x1[i]=coordinates(sp.seg)[[i]][[1]][dim(coordinates(sp.seg)[[i]][[1]])[1],1]
  y0[i]=coordinates(sp.seg)[[i]][[1]][1,2]
  y1[i]=coordinates(sp.seg)[[i]][[1]][dim(coordinates(sp.seg)[[i]][[1]])[1],2]
}

lines=data.frame(label=seq(1,length(sp.seg)),x0=x0,x1=x1,y0=y0,y1=y1)

#tmp=as.psp(tran.sub[tran.sub$id==1,])
tmp=as.psp(sp.seg)
#still need to figure out how to convert these files back to a spatialpolygon 
#to sample within each one. 
strtransect<-lines_to_strips(lines,as.owin(tmp), width=550)
#over 50 warnings they were all the same as below
#1: In `[<-`(`*tmp*`, i, value = gpc) :
#implicit list embedding of S4 objects is deprecated

#convert each row of lines to polygon
npoly=dim(lines)[1]
new.poly=list(Polygon(coords=coordinates(strtransect$full.transects[[1]])))
new.polys=list(Polygons(new.poly,ID=1))
for(i in 2:(dim(lines)[1])){
  new.poly=list(Polygon(coords=coordinates(strtransect$full.transects[[i]])))
  new.polys[[i]]=Polygons(new.poly,ID=i)
}

out2011=SpatialPolygons(new.polys)
proj4string(out2011)=CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0")
out2011=spTransform(out2011,CRS(proj4string(bathy)))
#out2011<-as(out2011, "SpatialPolygonsDataFrame")
#writeOGR(obj=out2011, dsn="tempdir", layer="transect2011", driver="ESRI Shapefile")





#2012
tran2012=spTransform(tran2012,
                     CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0"))
tran.sub=tran2012
#tran.sub=tran2012[!duplicated(tran2012$Transect),] #subset to remove duplicates
tran.sub$id=seq(1,131) #add a unique ID to each transect

#this just uses transect 1 as an example
x0=coordinates(tran.sub)[[1]][[1]][1,1]
x1=coordinates(tran.sub)[[1]][[1]][90,1]
y0=coordinates(tran.sub)[[1]][[1]][1,2]
y1=coordinates(tran.sub)[[1]][[1]][90,2]

lines=data.frame(label=1,x0=x0,x1=x1,y0=y0,y1=y1)

tmp=as.psp(tran.sub[tran.sub$id==1,])

strtransect<-lines_to_strips(lines,as.owin(tmp), width=550)
plot(tran.sub[tran.sub$id==1,])
points(strtransect$full.transects[[1]]) #not subdividing transects still

## functions below taken from http://rstudio-pubs-static.s3.amazonaws.com/10685_1f7266d60db7432486517a111c76ac8b.html

#First, basic segmentation
CreateSegment <- function(coords, from, to) {
  distance <- 0
  coordsOut <- c()
  biggerThanFrom <- F
  for (i in 1:(nrow(coords) - 1)) {
    d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 
                                                                             1, 2])^2)
    distance <- distance + d
    if (!biggerThanFrom && (distance > from)) {
      w <- 1 - (distance - from)/d
      x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
      y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
      coordsOut <- rbind(coordsOut, c(x, y))
      biggerThanFrom <- T
    }
    if (biggerThanFrom) {
      if (distance > to) {
        w <- 1 - (distance - to)/d
        x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
        y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
        coordsOut <- rbind(coordsOut, c(x, y))
        break
      }
      coordsOut <- rbind(coordsOut, c(coords[i + 1, 1], coords[i + 1, 
                                                               2]))
    }
  }
  return(coordsOut)
}

#now create multiple segments building on last function
CreateSegments <- function(coords, length = 0, n.parts = 0) {
  stopifnot((length > 0 || n.parts > 0))
  # calculate total length line
  total_length <- 0
  for (i in 1:(nrow(coords) - 1)) {
    d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 
                                                                             1, 2])^2)
    total_length <- total_length + d
  }
  
  # calculate stationing of segments
  if (length > 0) {
    stationing <- c(seq(from = 0, to = total_length, by = length), total_length)
  } else {
    stationing <- c(seq(from = 0, to = total_length, length.out = n.parts), 
                    total_length)
  }
  
  # calculate segments and store in list
  newlines <- list()
  for (i in 1:(length(stationing) - 1)) {
    newlines[[i]] <- CreateSegment(coords, stationing[i], stationing[i + 
                                                                       1])
  }
  return(newlines)
}

#extract x and y locations of segments for functions above. Example with transect 1
transect.locs=coordinates(tran.sub)[[1]][[1]]

#length in m -- update segment length to relevant length for analysis
segs = CreateSegments(transect.locs,length=1000)

plot(tran.sub[tran.sub$id==1,])
col = "red"
for (i in 1:length(segs)) {
  col <- ifelse(col == "red", "black", "red")
  lines(as.matrix(segs[[i]]), col = col, lwd = 2)
}

MergeLast <- function(lst) {
  l <- length(lst)
  lst[[l - 1]] <- rbind(lst[[l - 1]], lst[[l]])
  lst <- lst[1:(l - 1)]
  return(lst)
}

#translate above to SpatialLines structure
SegmentSpatialLines <- function(sl, length = 0, n.parts = 0, merge.last = FALSE) {
  stopifnot((length > 0 || n.parts > 0))
  id <- 0
  newlines <- list()
  sl <- as(sl, "SpatialLines")
  for (lines in sl@lines) {
    for (line in lines@Lines) {
      crds <- line@coords
      # create segments
      segments <- CreateSegments(coords = crds, length, n.parts)
      if (merge.last && length(segments) > 1) {
        # in case there is only one segment, merging would result into error
        segments <- MergeLast(segments)
      }
      # transform segments to lineslist for SpatialLines object
      for (segment in segments) {
        newlines <- c(newlines, Lines(list(Line(unlist(segment))), ID = as.character(id)))
        id <- id + 1
      }
    }
  }
  return(SpatialLines(newlines))
}

#example above with transect 2 added
tran2=coordinates(tran.sub)[[2]][[1]]

#transects 1 & 2 as spatial lines
sl <- SpatialLines(list(Lines(list(Line(coords = transect.locs)), 
                              ID = "1"), Lines(list(Line(coords = tran2)),ID = "2")))

#segmenting spatial lines objects
sl2 <- SegmentSpatialLines(sl, length = 1000, merge.last = TRUE)

# plot
plot(sl2, col = rep(c(1, 2), length.out = length(sl2)), axes = T)

#now create a for loop to convert all transects to Lines objects
ntransect=length(unique(tran.sub$id))
new.line=list(Line(coords=transect.locs))
new.lines=list(Lines(new.line,ID=1))
for(i in 2:ntransect){
  new.line=list(Line(coords=coordinates(tran.sub)[[i]][[1]]))
  new.lines[[i]]=Lines(new.line,ID=i)
}

sp.out=SpatialLines(new.lines,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0"))
#segment all transects
sp.seg <- SegmentSpatialLines(sp.out, length = 1000, merge.last = TRUE)
#plot(sp.seg, col = rep(c(1, 2), length.out = length(sp.seg)), axes = T)

#head(sp.seg)

#loop over 18453 segments. Needs to have x0 etc. indexed 
#so that each polygon corner (x0,x1,y0,y1) is saved as a unique point
x0=as.numeric()
x1=as.numeric()
y0=as.numeric()
y1=as.numeric()

for(i in 1:length(sp.seg)){
  x0[i]=coordinates(sp.seg)[[i]][[1]][1,1]
  x1[i]=coordinates(sp.seg)[[i]][[1]][dim(coordinates(sp.seg)[[i]][[1]])[1],1]
  y0[i]=coordinates(sp.seg)[[i]][[1]][1,2]
  y1[i]=coordinates(sp.seg)[[i]][[1]][dim(coordinates(sp.seg)[[i]][[1]])[1],2]
}

lines=data.frame(label=seq(1,length(sp.seg)),x0=x0,x1=x1,y0=y0,y1=y1)

#tmp=as.psp(tran.sub[tran.sub$id==1,])
tmp=as.psp(sp.seg)
#still need to figure out how to convert these files back to a spatialpolygon 
#to sample within each one. 
strtransect<-lines_to_strips(lines,as.owin(tmp), width=550)
#over 50 warnings they were all the same as below
#1: In `[<-`(`*tmp*`, i, value = gpc) :
#implicit list embedding of S4 objects is deprecated

#convert each row of lines to polygon
npoly=dim(lines)[1]
new.poly=list(Polygon(coords=coordinates(strtransect$full.transects[[1]])))
new.polys=list(Polygons(new.poly,ID=1))
for(i in 2:(dim(lines)[1])){
  new.poly=list(Polygon(coords=coordinates(strtransect$full.transects[[i]])))
  new.polys[[i]]=Polygons(new.poly,ID=i)
}

out2012=SpatialPolygons(new.polys)
proj4string(out2012)=CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0")
out2012=spTransform(out2012,CRS(proj4string(bathy)))
#out2012<-as(out2012, "SpatialPolygonsDataFrame")
#writeOGR(obj=out2012, dsn="tempdir", layer="transect2012", driver="ESRI Shapefile")



#finding coordinates for grid cells

out2009=spTransform(out2009,CRS(proj4string(bathy)))
out2009$id=seq(1,4381)

c1 = gCentroid(out2009,byid=TRUE)
c1$id=seq(1,4381)
c1=spTransform(c1,CRS(proj4string(bathy)))

join2009=merge(out2009,c1,by="id")
join2009<-as(join2009, "SpatialPolygonsDataFrame")
writeOGR(obj=join2009, dsn="tempdir", layer="join2009", driver="ESRI Shapefile")

proj4string(sco2)=CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0")
sco2=spTransform(sco2,CRS(proj4string(bathy)))
sco2$wave2=as.numeric(sco2$wave2)
sco2$wind2=as.numeric(sco2$wind2)
writeOGR(obj=sco2, dsn="tempdir", layer="sco2", driver="ESRI Shapefile")

grid2009=rgdal::readOGR("tempdir/gri2009.shp")
grid2009=spTransform(grid2009,CRS(proj4string(bathy)))

grid.2009<-data.frame(grid2009)
write.table(grid.2009, "test.txt", sep="\t")


#2010

out2010=spTransform(out2010,CRS(proj4string(bathy)))
out2010$id=seq(1,4235)

c2 = gCentroid(out2010,byid=TRUE)
c2$id=seq(1,4235)
c2=spTransform(c2,CRS(proj4string(bathy)))

join2010=merge(out2010,c2,by="id")
join2010<-as(join2010, "SpatialPolygonsDataFrame")
writeOGR(obj=join2010, dsn="tempdir", layer="join2010", driver="ESRI Shapefile")

grid2010=rgdal::readOGR("tempdir/grid2010.shp")
grid2010=spTransform(grid2010,CRS(proj4string(bathy)))

grid.2010<-data.frame(grid2010)
write.table(grid.2010, "test2.txt", sep="\t")

#2011

out2011=spTransform(out2011,CRS(proj4string(bathy)))
out2011$id=seq(1,4469)

c3 = gCentroid(out2011,byid=TRUE)
c3$id=seq(1,4469)
c3=spTransform(c3,CRS(proj4string(bathy)))

join2011=merge(out2011,c3,by="id")
join2011<-as(join2011, "SpatialPolygonsDataFrame")
writeOGR(obj=join2011, dsn="tempdir", layer="join2011", driver="ESRI Shapefile")

grid2011=rgdal::readOGR("tempdir/grid2011.shp")
grid2011=spTransform(grid2011,CRS(proj4string(bathy)))

grid.2011<-data.frame(grid2011)
write.table(grid.2011, "test3.txt", sep="\t")

#2012

out2012=spTransform(out2012,CRS(proj4string(bathy)))
out2012$id=seq(1,3824)

c4 = gCentroid(out2012,byid=TRUE)
c4$id=seq(1,3824)
c4=spTransform(c4,CRS(proj4string(bathy)))

join2012=merge(out2012,c4,by="id")
join2012<-as(join2012, "SpatialPolygonsDataFrame")
writeOGR(obj=join2012, dsn="tempdir", layer="join2012", driver="ESRI Shapefile")

grid2012=rgdal::readOGR("tempdir/grid2012.shp")
grid2012=spTransform(grid2012,CRS(proj4string(bathy)))

grid.2012<-data.frame(grid2012)
write.table(grid.2012, "test4.txt", sep="\t")

#subseting scoter data by year

scoters2009<-subset(sco2, SurveyBeginYear==2009, select=SurveyId:sednum)
scoters2010<-subset(sco2, SurveyBeginYear==2010, select=SurveyId:sednum)
scoters2011<-subset(sco2, SurveyBeginYear==2011, select=SurveyId:sednum)
scoters2012<-subset(sco2, SurveyBeginYear==2012, select=SurveyId:sednum)

summary(scoters2012) #277 (grid 241)

#MISSING POINTS NOT SURE WHY OR HOW



#getting varible values for grid cells

#2009
grid2009$Count<-as.numeric(grid2009$Count)
count.grid=aggregate(grid2009["Count"], join2009,sum)
join2009$Count=as.numeric(count.grid$Count)
join2009$Count[is.na(join2009$Count)]=0
data2009<-merge(join2009,grid2009,by="id")
data2009$Count[is.na(data2009$Count)]=0

#names(data2009)
#coordinates(data2009)<-c("longitude_dd","latitude_dd") 
#proj4string(data2009)<-CRS("+proj=longlat +datum=WGS84")
#data2009=spTransform(data2009,CRS(proj4string(bathy)))

data2009$NAO=-1.428

data2009$bathy=extract(bathy,data2009,fun=mean)
data2009$bathy=as.numeric(data2009$bathy)
data2009$bathy2=scale(data2009$bathy)
data2009$bathy2=as.numeric(data2009$bathy2)

#data2009$substrate=extract(substrate,data2009, fun=mean)
data2009$substrate=over(data2009,substrate)

data2009$slope=extract(slope,data2009, fun=mean)
data2009$slope=as.numeric(data2009$slope)
data2009$slope2=scale(data2009$slope)
data2009$slope2=as.numeric(data2009$slope2)

data2009$NAO2=scale(data2009$NAO)
data2009$NAO2=-0.3057961
data2009$NAO2=as.numeric(data2009$NAO2)
#will not scale need to look into

names(scot)

scot<-as.data.frame(data2009,header=TRUE)
scosc<-subset(scot,y>=32.0 & y<33.75,select=id:substrate)
coordinates(scosc)<-c("x","y")
sconc<-subset(scot,y>=33.75,select=id:substrate)
coordinates(sconc)<-c("x","y")
scoga<-subset(scot,y>=30.7 & y<32.0,select=id:substrate)
coordinates(scoga)<-c("x","y")
scofl<-subset(scot,y<30.7, select=id:substrate)
coordinates(scofl)<-c("x","y")

#south carolina
proj4string(scosc)<-CRS("+proj=longlat +datum=WGS84")
scosc=spTransform(scosc,CRS(proj4string(bathy)))
scosc$bival=extract(bival1,scosc)

#north carolina
proj4string(sconc)<-CRS("+proj=longlat +datum=WGS84")
sconc=spTransform(sconc,CRS(proj4string(bathy)))
sconc$bival=extract(bival2,sconc)

#georgia
proj4string(scoga)<-CRS("+proj=longlat +datum=WGS84")
scoga=spTransform(scoga,CRS(proj4string(bathy)))
scoga$bival=extract(bival4,scoga)

#florida
proj4string(scofl)<-CRS("+proj=longlat +datum=WGS84")
scofl=spTransform(scofl,CRS(proj4string(bathy)))
#scofl$bival=extract(bival3,scofl)
scofl$bival=over(scofl,bival3)

scosc$bival=as.factor(scosc@data$bival$RARNUM)
sconc$bival=as.factor(sconc@data$bival$RARNUM)
scoga$bival=as.factor(scoga@data$bival$RARNUM)
scofl$bival=as.factor(scofl@data$bival$RARNUM)
scobival<-rbind(scosc,sconc,scoga,scofl)
names(scobival)
data2009$bival<-scobival$bival
summary(data2009$bival)

data2009$eco=over(data2009, ecoregion)

sco<-as.data.frame(join2009)
coordinates(sco)<-c("x","y")

p.idw<-gstat::idw(feb2.2009~1, windfeb2.2009, newdata=grd, idp=2.0)
p.idw<-raster(p.idw)
proj4string(sco)<-CRS("+proj=longlat +datum=WGS84")
sco=spTransform(sco,CRS(proj4string(bathy)))
sco$w1=extract(p.idw,sco,fun=mean)
sco$w1s=scale(sco$w1)

p1.idw<-gstat::idw(feb5.2009~1, windfeb5.2009, newdata=grd, idp=2.0)
p1.idw<-raster(p1.idw)
sco$w6=extract(p1.idw,sco,fun=mean)
sco$w6s=scale(sco$w6)

p2.idw<-gstat::idw(feb6.2009~1, windfeb6.2009, newdata=grd, idp=2.0)
p2.idw<-raster(p2.idw)
sco$w2=extract(p2.idw,sco,fun=mean)
sco$w2s=scale(sco$w2)

p3.idw<-gstat::idw(feb7.2009~1, windfeb7.2009, newdata=grd, idp=2.0)
p3.idw<-raster(p3.idw)
sco$w3=extract(p3.idw,sco,fun=mean)
sco$w3s=scale(sco$w3)

p4.idw<-gstat::idw(feb8.2009~1, windfeb8.2009, newdata=grd, idp=2.0)
p4.idw<-raster(p4.idw)
sco$w4=extract(p4.idw,sco,fun=mean)
sco$w4s=scale(sco$w4)

p5.idw<-gstat::idw(feb9.2009~1, windfeb9.2009, newdata=grd, idp=2.0)
p5.idw<-raster(p5.idw)
sco$w5=extract(p5.idw,sco,fun=mean)
sco$w5s=scale(sco$w5)

pa.idw<-gstat::idw(feb2.2009~1, wavefeb2.2009, newdata=grd, idp=2.0)
pa.idw<-raster(pa.idw)
sco$wa1=extract(pa.idw,sco,fun=mean)
sco$wa1s=scale(sco$wa1)

pb.idw<-gstat::idw(feb5.2009~1, wavefeb5.2009, newdata=grd, idp=2.0)
pb.idw<-raster(pb.idw)
sco$wa2=extract(pb.idw,sco,fun=mean)
sco$wa2s=scale(sco$wa2)

pc.idw<-gstat::idw(feb6.2009~1, wavefeb2.2009, newdata=grd, idp=2.0)
pc.idw<-raster(pc.idw)
sco$wa3=extract(pc.idw,sco,fun=mean)
sco$wa3s=scale(sco$wa3)

pd.idw<-gstat::idw(feb7.2009~1, wavefeb2.2009, newdata=grd, idp=2.0)
pd.idw<-raster(pd.idw)
sco$wa4=extract(pd.idw,sco,fun=mean)
sco$wa4s=scale(sco$wa4)

pe.idw<-gstat::idw(feb8.2009~1, wavefeb8.2009, newdata=grd, idp=2.0)
pe.idw<-raster(pe.idw)
sco$wa5=extract(pe.idw,sco,fun=mean)
sco$wa5s=scale(sco$wa5)

pf.idw<-gstat::idw(feb9.2009~1, wavefeb8.2009, newdata=grd, idp=2.0)
pf.idw<-raster(pf.idw)
sco$wa6=extract(pf.idw,sco,fun=mean)
sco$wa6s=scale(sco$wa6)

sco<-as.data.frame(sco)
sco$wind<-rowMeans(subset(sco, select = c(w1,w2,w3,w4,w5,w6)), na.rm = TRUE)
sco$wind2<-rowMeans(subset(sco, select = c(w1s,w2s,w3s,w4s,w5s,w6s)), na.rm = TRUE)
sco$wave<-rowMeans(subset(sco, select = c(wa1,wa2,wa3,wa4,wa5,wa6)), na.rm = TRUE)
sco$wave2<-rowMeans(subset(sco, select = c(wa1s,wa2s,wa3s,wa4s,wa5s,wa6s)), na.rm = TRUE)
coordinates(sco)<-c("x","y")
proj4string(sco)<-CRS("+proj=longlat +datum=WGS84")
sco=spTransform(sco,CRS(proj4string(bathy)))

sco <- sco[c(-3:-26)]
names(sco)
data2009<-cbind(data2009,sco[3:6])


data2009<-as.data.frame(data2009)
coordinates(data2009)<-c("x","y")
proj4string(data2009)<-CRS("+proj=longlat +datum=WGS84")
data2009=spTransform(data2009,CRS(proj4string(bathy)))
dist<-distanceFromPoints(seshoreline,data2009)
proj4string(dist)<-CRS("+proj=longlat +datum=WGS84")
data2009$dist=extract(dist,data2009)
data2009$dist2=scale(data2009$dist)
data2009$dist2=as.numeric(data2009$dist2)

data2009$sednum=as.factor(data2009@data$substrate$SEDNUM)
data2009$substrate<-NULL
data2009$eco=as.factor(data2009@data$eco$ECOREGION)
data2009$bival=as.factor(data2009$bival)

summary(data2009)

data.2009<-data.frame(data2009)
write.table(data.2009, "data2009.txt", sep="\t")

data2009=read.csv("Grid/data2009.csv",header=TRUE)
coordinates(data2009)<-c("x","y") 
proj4string(data2009)<-CRS("+proj=longlat +datum=WGS84")
data2009=spTransform(data2009,CRS(proj4string(bathy)))

grid2009=read.csv("Grid/g2009.csv",header=TRUE)
coordinates(grid2009)<-c("x","y")
proj4string(grid2009)<-CRS("+proj=longlat +datum=WGS84")
grid2009=spTransform(grid2009,CRS(proj4string(bathy)))

year2009<-rbind(data2009,grid2009)


#2010
grid2010$Count<-as.numeric(grid2010$Count)
grid2010$Count=as.numeric(grid2010$Count)
count.grid=aggregate(grid2010["Count"], join2010,sum)
join2010$Count=as.numeric(count.grid$Count)
join2010$Count[is.na(join2010$Count)]=0
data2010<-merge(join2010,grid2010,by="id")
data2010$Count[is.na(data2010$Count)]=0

data2010$NAO=-3.924

data2010$bathy=extract(bathy,data2010,fun=mean)
data2010$bathy=as.numeric(data2010$bathy)
data2010$bathy2=scale(data2010$bathy)
data2010$bathy2=as.numeric(data2010$bathy2)

#data2010$substrate=extract(substrate,data2010, fun=mean)
data2010$substrate=over(data2010,substrate)

data2010$slope=extract(slope,data2010, fun=mean)
data2010$slope=as.numeric(data2010$slope)
data2010$slope2=scale(data2010$slope)
data2010$slope2=as.numeric(data2010$slope2)

data2010$NAO2=scale(data2010$NAO)
data2010$NAO2=-1.2221421
data2010$NAO2=as.numeric(data2010$NAO2)
#will not scale need to look into

names(scot)

scot<-as.data.frame(data2010,header=TRUE)
scosc<-subset(scot,y>=32.0 & y<33.75,select=id:substrate)
coordinates(scosc)<-c("x","y")
sconc<-subset(scot,y>=33.75,select=id:substrate)
coordinates(sconc)<-c("x","y")
scoga<-subset(scot,y>=30.7 & y<32.0,select=id:substrate)
coordinates(scoga)<-c("x","y")
scofl<-subset(scot,y<30.7, select=id:substrate)
coordinates(scofl)<-c("x","y")

#south carolina
proj4string(scosc)<-CRS("+proj=longlat +datum=WGS84")
scosc=spTransform(scosc,CRS(proj4string(bathy)))
scosc$bival=extract(bival1,scosc)

#north carolina
proj4string(sconc)<-CRS("+proj=longlat +datum=WGS84")
sconc=spTransform(sconc,CRS(proj4string(bathy)))
sconc$bival=extract(bival2,sconc)

#georgia
proj4string(scoga)<-CRS("+proj=longlat +datum=WGS84")
scoga=spTransform(scoga,CRS(proj4string(bathy)))
scoga$bival=extract(bival4,scoga)

#florida
proj4string(scofl)<-CRS("+proj=longlat +datum=WGS84")
scofl=spTransform(scofl,CRS(proj4string(bathy)))
#scofl$bival=extract(bival3,scofl)
scofl$bival=over(scofl,bival3)

scosc$bival=as.factor(scosc@data$bival$RARNUM)
sconc$bival=as.factor(sconc@data$bival$RARNUM)
scoga$bival=as.factor(scoga@data$bival$RARNUM)
scofl$bival=as.factor(scofl@data$bival$RARNUM)
scobival<-rbind(scosc,sconc,scoga,scofl)
names(scobival)
data2010$bival<-scobival$bival
summary(data2010$bival)

data2010$eco=over(data2010, ecoregion)

sco<-as.data.frame(join2010)
coordinates(sco)<-c("x","y")

p6.idw<-gstat::idw(feb1.2010~1, windfeb1.2010, newdata=grd, idp=2.0)
p6.idw<-raster(p6.idw)
proj4string(sco)<-CRS("+proj=longlat +datum=WGS84")
sco=spTransform(sco,CRS(proj4string(bathy)))
sco$w7=extract(p6.idw,sco, fun=mean)
sco$w7s=scale(sco$w7)

p7.idw<-gstat::idw(feb3.2010~1, windfeb3.2010, newdata=grd, idp=2.0)
p7.idw<-raster(p7.idw)
sco$w8=extract(p7.idw,sco, fun=mean)
sco$w8s=scale(sco$w8)

p8.idw<-gstat::idw(feb4.2010~1, windfeb4.2010, newdata=grd, idp=2.0)
p8.idw<-raster(p8.idw)
sco$w9=extract(p8.idw,sco,fun=mean)
sco$w9s=scale(sco$w9)

p9.idw<-gstat::idw(feb9.2010~1, windfeb9.2010, newdata=grd, idp=2.0)
p9.idw<-raster(p9.idw)
sco$w10=extract(p9.idw,sco,fun=mean)
sco$w10s=scale(sco$w10)

p10.idw<-gstat::idw(feb11.2010~1, windfeb11.2010, newdata=grd, idp=2.0)
p10.idw<-raster(p10.idw)
sco$w11=extract(p10.idw,sco,fun=mean)
sco$w11s=scale(sco$w11)

p11.idw<-gstat::idw(feb12.2010~1, windfeb12.2010, newdata=grd, idp=2.0)
p11.idw<-raster(p11.idw)
sco$w12=extract(p11.idw,sco,fun=mean)
sco$w12s=scale(sco$w12)

p12.idw<-gstat::idw(feb15.2010~1, windfeb15.2010, newdata=grd, idp=2.0)
p12.idw<-raster(p12.idw)
sco$w13=extract(p12.idw,sco, fun=mean)
sco$w13s=scale(sco$w13)

pg.idw<-gstat::idw(feb1.2010~1, wavefeb1.2010, newdata=grd, idp=2.0)
pg.idw<-raster(pg.idw)
sco$wa7=extract(pg.idw,sco, fun=mean)
sco$wa7s=scale(sco$wa7)

ph.idw<-gstat::idw(feb3.2010~1, wavefeb1.2010, newdata=grd, idp=2.0)
ph.idw<-raster(ph.idw)
sco$wa8=extract(ph.idw,sco, fun=mean)
sco$wa8s=scale(sco$wa8)

pi.idw<-gstat::idw(feb4.2010~1, wavefeb1.2010, newdata=grd, idp=2.0)
pi.idw<-raster(pi.idw)
sco$wa9=extract(pi.idw,sco,fun=mean)
sco$wa9s=scale(sco$wa9)

pj.idw<-gstat::idw(feb9.2010~1, wavefeb9.2010, newdata=grd, idp=2.0)
pj.idw<-raster(pj.idw)
sco$wa10=extract(pj.idw,sco,fun=mean)
sco$wa10s=scale(sco$wa10)

pk.idw<-gstat::idw(feb11.2010~1, wavefeb9.2010, newdata=grd, idp=2.0)
pk.idw<-raster(pk.idw)
sco$wa11=extract(pk.idw,sco,fun=mean)
sco$wa11s=scale(sco$wa11)

pl.idw<-gstat::idw(feb12.2010~1, wavefeb1.2010, newdata=grd, idp=2.0)
pl.idw<-raster(pl.idw)
sco$wa12=extract(pl.idw,sco,fun=mean)
sco$wa12s=scale(sco$wa12)

pm.idw<-gstat::idw(feb15.2010~1, wavefeb9.2010, newdata=grd, idp=2.0)
pm.idw<-raster(pm.idw)
sco$wa13=extract(pm.idw,sco,fun=mean)
sco$wa13s=scale(sco$wa13)

sco<-as.data.frame(sco)
sco$wind<-rowMeans(subset(sco, select = c(w7,w8,w9,w10,w11,w12,w13)), na.rm = TRUE)
sco$wind2<-rowMeans(subset(sco, select = c(w7s,w8s,w9s,w10s,w11s,w12s,w13s)), na.rm = TRUE)
sco$wave<-rowMeans(subset(sco, select = c(wa7,wa8,wa9,wa10,wa11,wa12,w13)), na.rm = TRUE)
sco$wave2<-rowMeans(subset(sco, select = c(wa7s,wa8s,wa9s,wa10s,wa11s,wa12s,wa13s)), na.rm = TRUE)
coordinates(sco)<-c("x","y")
proj4string(sco)<-CRS("+proj=longlat +datum=WGS84")
sco=spTransform(sco,CRS(proj4string(bathy)))

sco <- sco[c(-3:-29)]
names(sco)
data2010<-cbind(data2010,sco[4:7])


data2010<-as.data.frame(data2010)
coordinates(data2010)<-c("x","y")
proj4string(data2010)<-CRS("+proj=longlat +datum=WGS84")
data2010=spTransform(data2010,CRS(proj4string(bathy)))
dist<-distanceFromPoints(seshoreline,data2010)
proj4string(dist)<-CRS("+proj=longlat +datum=WGS84")
data2010$dist=extract(dist,data2010)
data2010$dist2=scale(data2010$dist)
data2010$dist2=as.numeric(data2010$dist2)

data2010$sednum=as.factor(data2010@data$substrate$SEDNUM)
data2010$substrate<-NULL
data2010$eco=as.factor(data2010@data$eco$ECOREGION)
data2010$bival=as.factor(data2010$bival)

summary(data2010)

data.2010<-data.frame(data2010)
write.table(data.2010, "data2010.txt", sep="\t")

data2010=read.csv("Grid/data2010.csv",header=TRUE)
coordinates(data2010)<-c("x","y") 
proj4string(data2010)<-CRS("+proj=longlat +datum=WGS84")
data2010=spTransform(data2010,CRS(proj4string(bathy)))

grid2010=read.csv("Grid/g2010.csv",header=TRUE)
coordinates(grid2010)<-c("x","y")
proj4string(grid2010)<-CRS("+proj=longlat +datum=WGS84")
grid2010=spTransform(grid2010,CRS(proj4string(bathy)))

year2010<-rbind(data2010,grid2010)

#2011
grid2011$Count<-as.numeric(grid2011$Count)
grid2011$Count=as.numeric(grid2011$Count)
count.grid=aggregate(grid2011["Count"], join2011,sum)
join2011$Count=as.numeric(count.grid$Count)
join2011$Count[is.na(join2011$Count)]=0
data2011<-merge(join2011,grid2011,by="id")
data2011$Count[is.na(data2011$Count)]=0

data2011$NAO=2.791

data2011$bathy=extract(bathy,data2011,fun=mean)
data2011$bathy=as.numeric(data2011$bathy)
data2011$bathy2=scale(data2011$bathy)
data2011$bathy2=as.numeric(data2011$bathy2)

#data2011$substrate=extract(substrate,data2011, fun=mean)
data2011$substrate=over(data2011,substrate)

data2011$slope=extract(slope,data2011, fun=mean)
data2011$slope=as.numeric(data2011$slope)
data2011$slope2=scale(data2011$slope)
data2011$slope2=as.numeric(data2011$slope2)

data2011$NAO2=scale(data2011$NAO)
data2011$NAO2=1.2431077
data2011$NAO2=as.numeric(data2011$NAO2)
#will not scale need to look into

names(scot)

scot<-as.data.frame(data2011,header=TRUE)
scosc<-subset(scot,y>=32.0 & y<33.75,select=id:substrate)
coordinates(scosc)<-c("x","y")
sconc<-subset(scot,y>=33.75,select=id:substrate)
coordinates(sconc)<-c("x","y")
scoga<-subset(scot,y>=30.7 & y<32.0,select=id:substrate)
coordinates(scoga)<-c("x","y")
scofl<-subset(scot,y<30.7, select=id:substrate)
coordinates(scofl)<-c("x","y")

#south carolina
proj4string(scosc)<-CRS("+proj=longlat +datum=WGS84")
scosc=spTransform(scosc,CRS(proj4string(bathy)))
scosc$bival=extract(bival1,scosc)

#north carolina
proj4string(sconc)<-CRS("+proj=longlat +datum=WGS84")
sconc=spTransform(sconc,CRS(proj4string(bathy)))
sconc$bival=extract(bival2,sconc)

#georgia
proj4string(scoga)<-CRS("+proj=longlat +datum=WGS84")
scoga=spTransform(scoga,CRS(proj4string(bathy)))
scoga$bival=extract(bival4,scoga)

#florida
proj4string(scofl)<-CRS("+proj=longlat +datum=WGS84")
scofl=spTransform(scofl,CRS(proj4string(bathy)))
#scofl$bival=extract(bival3,scofl)
scofl$bival=over(scofl,bival3)

scosc$bival=as.factor(scosc@data$bival$RARNUM)
sconc$bival=as.factor(sconc@data$bival$RARNUM)
scoga$bival=as.factor(scoga@data$bival$RARNUM)
scofl$bival=as.factor(scofl@data$bival$RARNUM)
scobival<-rbind(scosc,sconc,scoga,scofl)
names(scobival)
data2011$bival<-scobival$bival
summary(data2011$bival)

data2011$eco=over(data2011, ecoregion)

sco<-as.data.frame(join2011)
coordinates(sco)<-c("x","y")

p12.idw<-gstat::idw(feb3.2011~1, windfeb3.2011, newdata=grd, idp=2.0)
p12.idw<-raster(p12.idw)
proj4string(sco)<-CRS("+proj=longlat +datum=WGS84")
sco=spTransform(sco,CRS(proj4string(bathy)))
sco$w14=extract(p12.idw,sco, fun=mean)
sco$w14s=scale(sco$w14)

p13.idw<-gstat::idw(feb6.2011~1, windfeb6.2011, newdata=grd, idp=2.0)
p13.idw<-raster(p13.idw)
sco$w15=extract(p13.idw,sco, fun=mean)
sco$w15s=scale(sco$w15)

p14.idw<-gstat::idw(feb9.2011~1, windfeb9.2011, newdata=grd, idp=2.0)
p14.idw<-raster(p14.idw)
sco$w16=extract(p14.idw,sco, fun=mean)
sco$w16s=scale(sco$w16)

p15.idw<-gstat::idw(feb11.2011~1, windfeb11.2011, newdata=grd, idp=2.0)
p15.idw<-raster(p15.idw)
sco$w17=extract(p15.idw,sco, fun=mean)
sco$w17s=scale(sco$w17)

p16.idw<-gstat::idw(feb12.2011~1, windfeb12.2011, newdata=grd, idp=2.0)
p16.idw<-raster(p16.idw)
sco$w18=extract(p16.idw,sco, fun=mean)
sco$w18s=scale(sco$w18)

p17.idw<-gstat::idw(feb13.2011~1, windfeb13.2011, newdata=grd, idp=2.0)
p17.idw<-raster(p17.idw)
sco$w19=extract(p17.idw,sco, fun=mean)
sco$w19s=scale(sco$w19)

p18.idw<-gstat::idw(feb16.2011~1, windfeb16.2011, newdata=grd, idp=2.0)
p18.idw<-raster(p18.idw)
sco$w20=extract(p18.idw,sco, fun=mean)
sco$w20s=scale(sco$w20)

p19.idw<-gstat::idw(feb17.2011~1, windfeb17.2011, newdata=grd, idp=2.0)
p19.idw<-raster(p19.idw)
sco$w21=extract(p19.idw,sco, fun=mean)
sco$w21s=scale(sco$w21)

pn.idw<-gstat::idw(feb3.2011~1, wavefeb3.2011, newdata=grd, idp=2.0)
pn.idw<-raster(pn.idw)
sco$wa14=extract(pn.idw,sco, fun=mean)
sco$wa14s=scale(sco$wa14)

po.idw<-gstat::idw(feb6.2011~1, wavefeb6.2011, newdata=grd, idp=2.0)
po.idw<-raster(po.idw)
sco$wa15=extract(po.idw,sco, fun=mean)
sco$wa15s=scale(sco$wa15)

pp.idw<-gstat::idw(feb9.2011~1, wavefeb9.2011, newdata=grd, idp=2.0)
pp.idw<-raster(pp.idw)
sco$wa16=extract(pp.idw,sco, fun=mean)
sco$wa16s=scale(sco$wa16)

pq.idw<-gstat::idw(feb11.2011~1, wavefeb11.2011, newdata=grd, idp=2.0)
pq.idw<-raster(pq.idw)
sco$wa17=extract(pq.idw,sco, fun=mean)
sco$wa17s=scale(sco$wa17)

pr.idw<-gstat::idw(feb12.2011~1, wavefeb12.2011, newdata=grd, idp=2.0)
pr.idw<-raster(pr.idw)
sco$wa18=extract(pr.idw,sco, fun=mean)
sco$wa18s=scale(sco$wa18)

ps.idw<-gstat::idw(feb13.2011~1, wavefeb6.2011, newdata=grd, idp=2.0)
ps.idw<-raster(ps.idw)
sco$wa19=extract(ps.idw,sco, fun=mean)
sco$wa19s=scale(sco$wa19)

pt.idw<-gstat::idw(feb16.2011~1, wavefeb16.2011, newdata=grd, idp=2.0)
pt.idw<-raster(pt.idw)
sco$wa20=extract(pt.idw,sco, fun=mean)
sco$wa20s=scale(sco$wa20)

pu.idw<-gstat::idw(feb17.2011~1, wavefeb11.2011, newdata=grd, idp=2.0)
pu.idw<-raster(pu.idw)
sco$wa21=extract(pu.idw,sco, fun=mean)
sco$wa21s=scale(sco$wa21)


sco<-as.data.frame(sco)
sco$wind<-rowMeans(subset(sco, select = c(w14,w15,w16,w17,w18,w19,w20,w21)), na.rm = TRUE)
sco$wind2<-rowMeans(subset(sco, select = c(w14s,w15s,w16s,w17s,w18s,w19s,w20s,w21s)), na.rm = TRUE)
sco$wave<-rowMeans(subset(sco, select = c(wa14,wa15,wa16,wa17,wa18,wa19,wa20,wa21)), na.rm = TRUE)
sco$wave2<-rowMeans(subset(sco, select = c(wa14s,wa15s,wa16s,wa17s,wa18s,wa19s,wa20s,wa21s)), na.rm = TRUE)
coordinates(sco)<-c("x","y")
proj4string(sco)<-CRS("+proj=longlat +datum=WGS84")
sco=spTransform(sco,CRS(proj4string(bathy)))

sco <- sco[c(-3:-34)]
names(sco)
data2011<-cbind(data2011,sco[3:6])


data2011<-as.data.frame(data2011)
coordinates(data2011)<-c("x","y")
proj4string(data2011)<-CRS("+proj=longlat +datum=WGS84")
data2011=spTransform(data2011,CRS(proj4string(bathy)))
dist<-distanceFromPoints(seshoreline,data2011)
proj4string(dist)<-CRS("+proj=longlat +datum=WGS84")
data2011$dist=extract(dist,data2011)
data2011$dist2=scale(data2011$dist)
data2011$dist2=as.numeric(data2011$dist2)

data2011$sednum=as.factor(data2011@data$substrate$SEDNUM)
data2011$substrate<-NULL
data2011$eco=as.factor(data2011@data$eco$ECOREGION)
data2011$bival=as.factor(data2011$bival)

summary(data2011)

data.2011<-data.frame(data2011)
write.table(data.2011, "data2011.txt", sep="\t")

data2011=read.csv("Grid/data2011.csv",header=TRUE)
coordinates(data2011)<-c("x","y") 
proj4string(data2011)<-CRS("+proj=longlat +datum=WGS84")
data2011=spTransform(data2011,CRS(proj4string(bathy)))

grid2011=read.csv("Grid/g2011.csv",header=TRUE)
coordinates(grid2011)<-c("x","y")
proj4string(grid2011)<-CRS("+proj=longlat +datum=WGS84")
grid2011=spTransform(grid2011,CRS(proj4string(bathy)))

year2011<-rbind(data2011,grid2011)

#2012
grid2012$Count<-as.numeric(grid2012$Count)
grid2012$Count=as.numeric(grid2012$Count)
count.grid=aggregate(grid2012["Count"], join2012,sum)
join2012$Count=as.numeric(count.grid$Count)
join2012$Count[is.na(join2012$Count)]=0
data2012<-merge(join2012,grid2012,by="id")
data2012$Count[is.na(data2012$Count)]=0

data2012$NAO=1.279

data2012$bathy=extract(bathy,data2012,fun=mean)
data2012$bathy=as.numeric(data2012$bathy)
data2012$bathy2=scale(data2012$bathy)
data2012$bathy2=as.numeric(data2012$bathy2)

#data2012$substrate=extract(substrate,data2012, fun=mean)
data2012$substrate=over(data2012,substrate)

data2012$slope=extract(slope,data2012, fun=mean)
data2012$slope=as.numeric(data2012$slope)
data2012$slope2=scale(data2012$slope)
data2012$slope2=as.numeric(data2012$slope2)

data2012$NAO2=scale(data2012$NAO)
data2012$NAO2=0.6880134
data2012$NAO2=as.numeric(data2012$NAO2)
#will not scale need to look into

names(scot)

scot<-as.data.frame(data2012,header=TRUE)

scosc<-subset(scot,y>=32.0 & y<33.75,select=id:substrate)
coordinates(scosc)<-c("x","y")
scoga<-subset(scot,y>=30.7 & y<32.0,select=id:substrate)
coordinates(scoga)<-c("x","y")
scofl<-subset(scot,y<30.7, select=id:substrate)
coordinates(scofl)<-c("x","y")

#south carolina
proj4string(scosc)<-CRS("+proj=longlat +datum=WGS84")
scosc=spTransform(scosc,CRS(proj4string(bathy)))
scosc$bival=extract(bival1,scosc)

#georgia
proj4string(scoga)<-CRS("+proj=longlat +datum=WGS84")
scoga=spTransform(scoga,CRS(proj4string(bathy)))
scoga$bival=extract(bival4,scoga)

#florida
proj4string(scofl)<-CRS("+proj=longlat +datum=WGS84")
scofl=spTransform(scofl,CRS(proj4string(bathy)))
#scofl$bival=extract(bival3,scofl)
scofl$bival=over(scofl,bival3)

scosc$bival=as.factor(scosc@data$bival$RARNUM)
scoga$bival=as.factor(scoga@data$bival$RARNUM)
scofl$bival=as.factor(scofl@data$bival$RARNUM)
scobival<-rbind(scosc,scoga,scofl)
names(scobival)
data2012$bival<-scobival$bival
summary(data2012$bival)

data2012$eco=over(data2012, ecoregion)

sco<-as.data.frame(join2012)
coordinates(sco)<-c("x","y")

p12.idw<-gstat::idw(feb3.2011~1, windfeb3.2011, newdata=grd, idp=2.0)
p12.idw<-raster(p12.idw)
proj4string(sco)<-CRS("+proj=longlat +datum=WGS84")
sco=spTransform(sco,CRS(proj4string(bathy)))
sco$w14=extract(p12.idw,sco, fun=mean)
sco$w14s=scale(sco$w14)

p19.idw<-gstat::idw(feb17.2011~1, windfeb17.2011, newdata=grd, idp=2.0)
p19.idw<-raster(p19.idw)
sco$w21=extract(p19.idw,sco, fun=mean)
sco$w21s=scale(sco$w21)


p20.idw<-gstat::idw(feb4.2012~1, windfeb4.2012, newdata=grd, idp=2.0)
p20.idw<-raster(p20.idw)
proj4string(scofeb4.2012)<-CRS("+proj=longlat +datum=WGS84")
sco=spTransform(sco,CRS(proj4string(bathy)))
sco$w22=extract(p20.idw,sco, fun=mean)
sco$w22s=scale(sco$w22)

p21.idw<-gstat::idw(feb5.2012~1, windfeb5.2012, newdata=grd, idp=2.0)
p21.idw<-raster(p21.idw)
sco$w23=extract(p21.idw,sco, fun=mean)
sco$w23s=scale(sco$w23)

p22.idw<-gstat::idw(feb8.2012~1, windfeb8.2012, newdata=grd, idp=2.0)
p22.idw<-raster(p22.idw)
sco$w24=extract(p22.idw,sco, fun=mean)
sco$w24s=scale(sco$w24)

p23.idw<-gstat::idw(feb17.2012~1, windfeb17.2012, newdata=grd, idp=2.0)
p23.idw<-raster(p23.idw)
sco$w25=extract(p23.idw,sco, fun=mean)
sco$w25s=scale(sco$w25)

p24.idw<-gstat::idw(feb18.2012~1, windfeb18.2012, newdata=grd, idp=2.0)
p24.idw<-raster(p24.idw)
sco$w26=extract(p24.idw,sco, fun=mean)
sco$w26s=scale(sco$w26)

p25.idw<-gstat::idw(feb21.2012~1, windfeb21.2012, newdata=grd, idp=2.0)
p25.idw<-raster(p25.idw)
sco$w27=extract(p25.idw,sco, fun=mean)
sco$w27s=scale(sco$w27)


pv.idw<-gstat::idw(feb4.2012~1, wavefeb4.2012, newdata=grd, idp=2.0)
pv.idw<-raster(pv.idw)
sco$wa22=extract(pv.idw,sco, fun=mean)
sco$wa22s=scale(sco$wa22)

pw.idw<-gstat::idw(feb5.2012~1, wavefeb4.2012, newdata=grd, idp=2.0)
pw.idw<-raster(pw.idw)
sco$wa23=extract(pw.idw,sco, fun=mean)
sco$wa23s=scale(sco$wa23)

px.idw<-gstat::idw(feb8.2012~1, wavefeb4.2012, newdata=grd, idp=2.0)
px.idw<-raster(px.idw)
sco$wa24=extract(px.idw,sco, fun=mean)
sco$wa24s=scale(sco$wa24)

py.idw<-gstat::idw(feb17.2012~1, wavefeb4.2012, newdata=grd, idp=2.0)
py.idw<-raster(py.idw)
sco$wa25=extract(py.idw,sco, fun=mean)
sco$wa25s=scale(sco$wa25)

pz.idw<-gstat::idw(feb18.2012~1, wavefeb18.2012, newdata=grd, idp=2.0)
pz.idw<-raster(pz.idw)
sco$wa26=extract(pz.idw,sco, fun=mean)
sco$wa26s=scale(sco$wa26)

pza.idw<-gstat::idw(feb21.2012~1, wavefeb18.2012, newdata=grd, idp=2.0)
pza.idw<-raster(pza.idw)
sco$wa27=extract(pza.idw,sco, fun=mean)
sco$wa27s=scale(sco$wa27)




sco<-as.data.frame(sco)
sco$wind<-rowMeans(subset(sco, select = c(w22,w23,w24,w25,w26,w27)), na.rm = TRUE)
sco$wind2<-rowMeans(subset(sco, select = c(w22s,w23s,w24s,w25s,w26s,w27s)), na.rm = TRUE)
sco$wave<-rowMeans(subset(sco, select = c(wa22,wa23,wa24,wa25,wa26,wa27)), na.rm = TRUE)
sco$wave2<-rowMeans(subset(sco, select = c(wa22s,wa23s,wa24s,wa25s,wa26s,wa27s)), na.rm = TRUE)
coordinates(sco)<-c("x","y")
proj4string(sco)<-CRS("+proj=longlat +datum=WGS84")
sco=spTransform(sco,CRS(proj4string(bathy)))

sco <- sco[c(-3:-30)]
names(sco)
data2012<-cbind(data2012,sco[3:6])


data2012<-as.data.frame(data2012)
coordinates(data2012)<-c("x","y")
proj4string(data2012)<-CRS("+proj=longlat +datum=WGS84")
data2012=spTransform(data2012,CRS(proj4string(bathy)))
dist<-distanceFromPoints(seshoreline,data2012)
proj4string(dist)<-CRS("+proj=longlat +datum=WGS84")
data2012$dist=extract(dist,data2012)
data2012$dist2=scale(data2012$dist)
data2012$dist2=as.numeric(data2012$dist2)

data2012$sednum=as.factor(data2012@data$substrate$SEDNUM)
data2012$substrate<-NULL
data2012$eco=as.factor(data2012@data$eco$ECOREGION)
data2012$bival=as.factor(data2012$bival)

summary(data2012)

data.2012<-data.frame(data2012)
write.table(data.2012, "data2012.txt", sep="\t")

data2012=read.csv("Grid/data2012.csv",header=TRUE)
coordinates(data2012)<-c("x","y") 
proj4string(data2012)<-CRS("+proj=longlat +datum=WGS84")
data2012=spTransform(data2012,CRS(proj4string(bathy)))

grid2012=read.csv("Grid/g2012.csv",header=TRUE)
coordinates(grid2012)<-c("x","y")
proj4string(grid2012)<-CRS("+proj=longlat +datum=WGS84")
grid2012=spTransform(grid2012,CRS(proj4string(bathy)))

year2012<-rbind(data2012,grid2012)




#Adding the fall(Sept-Nov) and summer(Jun-Aug) NAO values
year<-rbind(year2009,year2010,year2011,year2012)
#year<-data.frame(year)
#write.table(year, "year.txt", sep="\t")
#year=read.csv("Grid/year.csv", header=TRUE)
#coordinates(year)<-c("x","y")
#proj4string(year)<-CRS("+proj=longlat +datum=WGS84")
#year=spTransform(year,CRS(proj4string(bathy)))
#year$S.NAO2=scale(year$S.NAO)
#year$S.NAO2=as.numeric(year$S.NAO2)
#year$F.NAO2=scale(year$F.NAO)
#year$F.NAO2=as.numeric(year$F.NAO2)
#year$eco=as.factor(year$eco)

#pca= nao, wind, wave
year<-na.omit(year)
a<-princomp(year[c(3,13,15)])
year$air<-a$scores[,1]

#testing LASSO
library(glmnet)
year<-rbind(year2009,year2010,year2011,year2012)
year<-data.frame(year)
year$SrvyBgY=as.factor(year$SrvyBgY)
year$eco=as.factor(year$eco)
year$sednum=as.factor(year$sednum)
year$bival=as.factor(year$bival)
year<-na.omit(year)

x=model.matrix(Count~NAO2+eco+poly(bathy2,2)+poly(wind2,2)+bival+poly(dist2,2)+slope2+sednum+wave2,data=year)
y=year$Count

set.seed(489)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

lasso<-glmnet(x,year$Count, family = "poisson", alpha=1)
cv.lasso=cv.glmnet(x,year$Count,family="poisson",alpha=1)
coef(cv.lasso,s="lambda.1se")
#removes eco, wind2 and wave2
#keeps NAO2, bathy2, dist2, slope2, and sednum (6,9)

cv.lasso$cvm
# cv MSE is 99
bestlam <- cv.lasso$lambda.1se
#1se=2.224351

pfit <- predict(lasso, newx=cbind(x[1:16233,94],matrix(0,16233,dim(x)[2]-1)),s=bestlam,type="response")

lasso.pred <- predict(lasso, newx=x[1:16233,],s=bestlam,family="poisson",type="response")
mean((lasso.pred-ytest)^2)
# MSE=20211.72
# RMSE=142.244

lasso.coef  <- predict(lasso, type = 'coefficients', s = bestlam)[1:96,]
lasso.coef
#intercept=2.58339973, NAO=0.11447979, eco=0.0, bathy2= -0.09149139
#wind2=0.06115596, bival=0.0, dist2=0.31419008, slope2=0.08479828
#sednum=0.20577241, and wave2=0.0

#Note with year as a variable there was no difference


dat=data.frame(x=x[1:16233,], X1=lasso.pred)
ggplot(data=dat,aes(x=x,y=X1)) + geom_line()


#Negative binomial glm testing


year$sednum=as.numeric(year$sednum)
year$bival=as.numeric(year$bival)

m1<-glm(Count~poly(bathy2,2)+poly(dist2,2)+slope2+sednum+poly(wind2,2), family='poisson', data=year)
summary(m1)
m2<-glm(Count~bathy2+dist2+slope2+sednum+wind2+wave2+NAO2+eco+bival,family='poisson', data=year)
summary(m2)

library(lme4)
m3<-glmer.nb(Count~bathy2+dist2+slope2+sednum+wind2+(1|SrvyBgY),data=year,control=glmerControl(optimizer="bobyqa"))
summary(m3)

m4<-glmer.nb(Count~bathy2+dist2+slope2+sednum+(1|SrvyBgY),data=year,control=glmerControl(optimizer="bobyqa"))
summary(m4)

m5<-glmer.nb(Count~bathy2+dist2+slope2+sednum+wind2+wave2+NAO2+eco+bival+(1|SrvyBgY),data=year,control=glmerControl(optimizer="bobyqa"))
summary(m5)

library(MuMIn)

out.put<-model.sel(m1,m2,m3,m4,m5)
out.put



#multicollinearity

year$sednum=as.numeric(year$sednum)
year$eco=as.numeric(year$eco)
year$bival=as.numeric(year$bival)

cor.test(year$bathy,year$dist) #-0.075
cor.test(year$bathy,year$slope) #0.233
cor.test(year$bathy,year$sednum) #0.126
cor.test(year$dist,year$slope) #0.118
cor.test(year$dist,year$sednum) #0.036
cor.test(year$slope,year$sednum) #-0.076
cor.test(year$bathy,year$NAO) #0.099
cor.test(year$dist,year$NAO) #-0.067
cor.test(year$slope,year$NAO) #-0.007
cor.test(year$sednum,year$NAO) #-0.161
cor.test(year$bathy,year$eco) #0.216
cor.test(year$dist,year$eco) #-0.008
cor.test(year$slope,year$eco) #0.098
cor.test(year$NAO,year$eco) #0.096
cor.test(year$sednum,year$eco) #0.449
cor.test(year$bathy,year$wind) #-0.040
cor.test(year$bathy,year$wave) #-0.033
cor.test(year$dist,year$wind) #-0.042
cor.test(year$dist,year$wave) #0.007
cor.test(year$slope,year$wind) #0.054
cor.test(year$slope,year$wave) #0.054
cor.test(year$sednum,year$wind) #0.085
cor.test(year$sednum,year$wave) #0.273
cor.test(year$NAO,year$wind) #-0.398
cor.test(year$NAO,year$wave) #-0.301
cor.test(year$eco,year$wind) #0.302
cor.test(year$eco,year$wave) #0.218
cor.test(year$wind,year$wave) #0.044
cor.test(year$bathy,year$bival) #0.045
cor.test(year$dist,year$bival) #0.125
cor.test(year$slope,year$bival) #-0.0614
cor.test(year$sednum,year$bival) #-0.150
cor.test(year$NAO,year$bival) #-0140
cor.test(year$eco,year$bival) #-0.257
cor.test(year$wind,year$bival) #0.213
cor.test(year$wave,year$bival) #-0.156
#cor.test(year$bathy,year$S.NAO) #-0.090
#cor.test(year$dist,year$S.NAO) #0.050
#cor.test(year$slope,year$S.NAO) #0.005
#cor.test(year$sednum,year$S.NAO) #0.093
#cor.test(year$NAO,year$S.NAO) #-0.943
#cor.test(year$eco,year$S.NAO) #-0.027
#cor.test(year$wind,year$S.NAO) #0.087
#cor.test(year$wave,year$S.NAO) #0.299
#cor.test(year$bival,year$S.NAO) #0.027
#cor.test(year$bathy,year$F.NAO) #-0.062
#cor.test(year$dist,year$F.NAO) #0.060
#cor.test(year$slope,year$F.NAO) #0.0006
#cor.test(year$sednum,year$F.NAO) #0.132
#cor.test(year$NAO,year$F.NAO) #-0.999
#cor.test(year$eco,year$F.NAO) #0.025
#cor.test(year$wind,year$F.NAO) #0.314
#cor.test(year$wave,year$F.NAO) #0.328
#cor.test(year$bival,year$F.NAO) #0.030
#cor.test(year$S.NAO,year$F.NAO) #0.942
cor.test(year$bathy,year$air) #0.099
cor.test(year$dist,year$air) # -0.062
cor.test(year$slope,year$air) #-0.011
cor.test(year$sednum,year$air) #-0.163
cor.test(year$NAO,year$air) #0.997
cor.test(year$eco,year$air) #-0.116
cor.test(year$wind,year$air) #-0.457
cor.test(year$wave,year$air) #-0.300
cor.test(year$bival,year$air) #-0.150
#cor.test(year$S.NAO,year$air) #-0.934
#cor.test(year$F.NAO,year$air) #-0.999







#negative binomial
library(MASS)
#sco2$slopesq=sco2$slope^2
#sco2$distsq=sco2$dist^2#random effects (1|Transect) and (1|SurveyBeginYear)
#sco2=data.frame(sco2)
#sco2$Transect=as.factor(sco2$Transect)
#sco2$SurveyBeginYear=as.factor(sco2$SurveyBeginYear)

library(lme4)
#m0<-glm.nb(Count~1,data=sco2,na.action='na.omit')
#m0a<-glmer.nb(Count~1+(1|Transect)+(1|SurveyBeginYear),data=sco2,na.action='na.omit')
#m0b<-glmer.nb(Count~1+(1|Transect),data=sco2, na.action='na.omit')
#m0c<-glmer.nb(Count~1+(1|SurveyBeginYear),data=sco2,na.action='na.omit')
#m1<-glmer.nb(Count~bathy2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m1a<-glm.nb(Count~bathy2, data=sco2)
#summary(m1)
#m2<-glmer.nb(Count~bathy2 + dist2+(1|Transect)+(1|SurveyBeginYear),data=sco2)
#m2a<-glm.nb(Count~bathy2 + dist2,data=sco2)
#summary(m2)
#m3<-glmer.nb(Count~bathy2 + slope2+(1|Transect)+(1|SurveyBeginYear),data=sco2)
#summary(m3)
#m4<-glmer.nb(Count~bathy2 + sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data = sco2,na.action='na.omit')
#summary(m4)
#m5<-glmer.nb(Count~bathy2 + sco2$sedmob2, data=sco2)
#m6<-glmer.nb(Count~bathy2 + dist2 + slope2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m7<-glmer.nb(Count~bathy2 + dist2 + sco2$sedmob2, data=sco2)
#m8<-glmer.nb(Count~bathy2 + dist2 + sco2$sednum +(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m9<-glmer.nb(Count~bathy2 + dist2 + slope2 + sco2$sedmob2, data=sco2)
#m10<-glmer.nb(Count~dist2 + slope2 + sco2$sednum+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m10a<-glmer.nb(Count~poly(dist2,2)+slope2+sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
#m10b<-glmer.nb(Count~dist2+poly(slope2,2)+sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
#m10c<-glmer.nb(Count~poly(bathy2,2)+dist2+slope2+sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
#m10d<-glmer.nb(Count~poly(dist2,2)+ poly(slope2,2)+sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
#m10e<-glmer.nb(Count~poly(bathy2,2)+poly(dist2,2)+slope2+
#                 sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
#m10f<-glmer.nb(Count~poly(bathy2,2)+dist2+poly(slope2,2)+
#                 sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
#m10g<-glmer.nb(Count~poly(bathy2,2)+poly(dist2,2)+poly(slope2,2)+
#                 sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
#m11<-glmer.nb(Count~bathy2 + dist2 + slope2 + sco2$sednum +
#           sco2$sedmob2, data=sco2)
#m12<-glmer.nb(Count~bathy2 + slope2 + sco2$sedmob2, data=sco2)
#m13<-glmer.nb(Count~bathy2 + slope2 + sco2$sednum+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m14<-glmer.nb(Count~bathy2 + slope2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m15<-glmer.nb(Count~bathy2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m16<-glmer.nb(Count~dist2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#summary(m16)
#m17<-glmer.nb(Count~dist2 + slope2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m18<-glmer.nb(Count~dist2 + sco2$sedmob2, data=sco2)
#m19<-glmer.nb(Count~dist2 + sco2$sednum+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m20<-glmer.nb(Count~dist2 + slope2 + sco2$sedmob2, data=sco2)
#m21<-glmer.nb(Count~dist2 + slope2 + sco2$sednum+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m22<-glmer.nb(Count~dist2 + slope2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m23<-glmer.nb(Count~dist2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m24<-glmer.nb(Count~slope2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m25<-glmer.nb(Count~slope2 + sco2$sedmob2, data=sco2)
#m26<-glmer.nb(Count~slope2 + sco2$sednum+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m27<-glmer.nb(Count~slope2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m28<-glmer.nb(Count~sco2$sedmob2, data=sco2)
#m29<-glmer.nb(Count~sco2$sedmob2 + sco2$sednum, data=sco2)
#m30<-glmer.nb(Count~sco2$sednum+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m31<-glmer.nb(Count~NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m32<-glmer.nb(Count~dist2+NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m33<-glmer.nb(Count~slope2+NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
 
#m72a<-glmer.nb(Count~sednum+poly(NAO2,2)+eco+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m73<-glmer.nb(Count~NAO2+eco+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m74<-glmer.nb(Count~eco+(1|Transect)+(1|SurveyBeginYear), data=sco2)

#Delta AIC

#Table = AIC(m10, m10a, m10b, m10d)
#n = dim(sco2)[1]  #sample size
# Table$df yields K, the number of parameters estimated in each model
# Table$AIC yields the AIC value for each model
#AICc = Table$AIC + (2*Table$df*(Table$df+1))/(n-Table$df-1) #Calculate AICc from AIC, n, and K
#Table = cbind(Table,AICc)
#deltaAICc = Table$AICc - min(Table$AICc) #Calculates the delta AICc values
#Table = cbind(Table,deltaAICc)
#Table = Table[order(Table$AICc), ]
#Table

#Weighted AIC
library(MuMIn)

out.put<-model.sel(m41b,m36c,m41a,m64c,m64a,m34,m36,m36a,m38,m39,m41,m45,m64,m62,m72)
out.put
#top model m36c(wt=0.293),m41b(delta=1.16,wt=0.164),m64c(delta=1.24,wt=0.158)

summary(m36c)
#(dist)2 and (NAO)2 were the significant


#prediction of top model


slope2=seq(min(year$slope2), max(year$slope2),length=16233)
bathy2=seq(min(year$slope2), max(year$slope2),length=16233)

bathmetry=predict(lasso,
                  data.frame(bathy2=seq(min(year$bathy2), 
                                       max(year$bathy2),length=16233),
                             dist2=as.numeric(rep(0,16233)),
                             slope2=as.numeric(rep(0,16233)),
                             wind2=as.numeric(rep(0,16233)),
                             sednum=as.factor(rep(4,16233))),
                  type="response")

plot(y=bathmetry,x=seq(min(year$bathy),max(year$bathy),length=16233),
     type="l", lwd=1, xlab = "Bathymetry (meters)",
     ylab = "Expected Count", cex.lab=1.3)

year$dist2=as.numeric(year$dist2)
dist2=seq(min(year$dist2), max(year$dist2), length=16233)

dist2shore=predict(m1,
                   data.frame(dist2=seq(min(year$dist2), 
                                         max(year$dist2),length=16233),
                              bathy2=as.numeric(rep(0,16233)),
                              slope2=as.numeric(rep(0,16233)),
                              wind2=as.numeric(rep(0,16233)),
                              sednum=as.factor(rep(4,16233))),
                   type="response")

plot(y=dist2shore,x=seq(min(year$dist),max(year$dist),length=16233),
     type="l", lwd=1, xlab = "Distance to Shore (meters)",
     ylab = "Expected Count", cex.lab=1.3)


#transect data

library(GISTools)
library(RgoogleMaps)
library(ggmap)
library(ggplot2)

transect=readShapeLines("Layers/transects/WinterSurvey_TrackLines_sCoast.shp")

proj4string(transect)<-CRS("+proj=longlat +datum=WGS84")

scoters=read.csv("ObsData2.csv",header=TRUE)
scoters <-na.omit(scoters)
scoters<-data.frame(scoters)
scoters$SurveyBeginYear<-as.factor(scoters$SurveyBeginYear)
tranbox<-make_bbox(lon=scoters$longitude_dd, lat = scoters$latitude_dd,
                   f=.01)

map<-get_map(location=tranbox, maptype = "terrain", source = 'google',
             color='color')

p<-ggmap(map)
p+geom_point(data = scoters, aes(scoters$longitude_dd, 
                                 scoters$latitude_dd,
                                col=scoters$SurveyBeginYear,size=Count)) +
  scale_color_brewer(palette = "Set1", name="Year")+
  scale_size_continuous(breaks = c(1, 1000, 2000, 3000, 4000, 5000))+
  theme(legend.position = c(.8,.22), legend.box = "horizontal")+
  xlab("Longitude") +
  ylab("Latitude")


year<-data.frame(year)
year<-na.omit(year)

#stat_smooth(method="lm", se=TRUE)

year$fit<-fitted(lasso)
distance<-ggplot(year, aes(x=dist, y=pred))
distance+geom_point()+ #x-axis is in meters
  stat_smooth(method="lm", formula=y~poly(x,2),se=TRUE)+
  theme(panel.background = element_rect(colour = 'black', fill='white'))+
  theme(axis.title.x=element_text(size=15, color = "black"))+
  theme(axis.title.y=element_text(size=15, color = "black"))+
  xlab("Distance from Shore (meters)")+
  ylab("Expected Count")


bath<-ggplot(year, aes(x=bathy, y=pred))
bath+geom_point()+ #x-axis is NAO values
  stat_smooth(method="lm",formula=y~poly(x,2), se=TRUE)+
  theme(panel.background = element_rect(colour = 'black', fill='white'))+
  theme(axis.title.x=element_text(size=15, color = "black"))+
  theme(axis.title.y=element_text(size=15, color = "black"))+
  xlab("Bathymetry (meters)")+
  ylab("Expected Count")

sub<-ggplot(year, aes(x=sednum))
sub+geom_bar()+
  theme(panel.background = element_rect(colour = 'black', fill='white'))+
  theme(axis.title.x=element_text(size=15, color = "black"))+
  theme(axis.title.y=element_text(size=15, color = "black"))+
  xlab("Substrate Type")+
  ylab("Expected Count")

sco2$fit<-fitted(m41a)
bath<-ggplot(sco2, aes(x=bathy, y=fit))
bath+geom_point()+
  stat_smooth(method="lm",formula=y~poly(x,2), se=TRUE)+
  theme(panel.background = element_rect(colour = 'black', fill='white'))+
  theme(axis.title.x=element_text(size=15, color = "black"))+
  theme(axis.title.y=element_text(size=15, color = "black"))+
  xlab("Water Depth (meters)")+
  ylab("Estimated number of Black Scoters")


summary(sco2$bathy)
summary(sco2$dist)
summary(sco2$slope)
summary(sco2$NAO)
summary(sco2$sednum)
#3=gravel-sand, 4=sand, 5=clay-silt/sand, 6=sand-clay/silt, 
#9=sand/silt/clay
summary(sco2$eco)
#20041=Virginian=400, 25042=Carolinian=518


#Home range: kernel density (adehabitatHR) function getvolumeUD, h=LSCV
#

