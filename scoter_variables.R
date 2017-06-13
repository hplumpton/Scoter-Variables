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
seshoreline <- crop(shoreline, extent(-82, -72, 30, 39))
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
scosc<-subset(scot,latitude_dd>=32.0 & latitude_dd<33.77,select=SurveyId:NAO)
coordinates(scosc)<-c("longitude_dd","latitude_dd")
sconc<-subset(scot,latitude_dd>=33.77,select=SurveyId:NAO)
coordinates(sconc)<-c("longitude_dd","latitude_dd")
scoga<-subset(scot,latitude_dd>=30.8 & latitude_dd<32.0,select=SurveyId:NAO)
coordinates(scoga)<-c("longitude_dd","latitude_dd")
scofl<-subset(scot,latitude_dd<30.8,select=SurveyId:NAO)
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
bival3=rgdal::readOGR("Layers/Florida/Florida/layer/invert.shp")
bival3<-spTransform(bival4,CRS(proj4string(bathy)))
#plot(bival3)
proj4string(scofl)<-CRS("+proj=longlat +datum=WGS84")
scofl=spTransform(scofl,CRS(proj4string(bathy)))
#plot(scofl,add=TRUE)
scofl$bival=extract(bival3,scofl)

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
summary(sco2$bival)

#Marine Ecoregions
eco=rgdal::readOGR("Layers/MEOW/meow_ecos.shp")
ecoregion <- crop(eco, extent(-82, -72, 30, 39))
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
scomar20.2012<-subset(sco,dateFlown==320.2012,select=SurveyId:NAO)
scomar21.2012<-subset(sco,dateFlown==321.2012,select=SurveyId:NAO)
scomar24.2012<-subset(sco,dateFlown==324.2012,select=SurveyId:NAO)
scomar28.2012<-subset(sco,dateFlown==328.2012,select=SurveyId:NAO)
scomar29.2012<-subset(sco,dateFlown==329.2012,select=SurveyId:NAO)

#WIND
#feb2.2009
windfeb2.2009=read.csv("Wind/feb2.2009.csv", header=TRUE)
windfeb2.2009 <-na.exclude(windfeb2.2009)
coordinates(windfeb2.2009)<-c("Long","Lat") 
proj4string(windfeb2.2009)<-CRS("+proj=longlat +datum=WGS84") 
windfeb2.2009<-spTransform(windfeb2.2009,CRS(proj4string(bathy)))

x.range<-as.numeric(c(-82,-72))
y.range<-as.numeric(c(30,39))
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

#mar20.2012
windmar20.2012=read.csv("Wind/mar20.2012.csv", header=TRUE)
windmar20.2012 <-na.exclude(windmar20.2012)
coordinates(windmar20.2012)<-c("Long","Lat") 
proj4string(windmar20.2012)<-CRS("+proj=longlat +datum=WGS84") 
windmar20.2012<-spTransform(windmar20.2012,CRS(proj4string(bathy)))

p26.idw<-gstat::idw(mar20.2012~1, windmar20.2012, newdata=grd, idp=2.0)
p26.idw<-raster(p26.idw)
#plot(p26.idw)
proj4string(scomar20.2012)<-CRS("+proj=longlat +datum=WGS84")
scomar20.2012=spTransform(scomar20.2012,CRS(proj4string(bathy)))
#plot(scomar20.2012,add=TRUE)
scomar20.2012$wind=extract(p26.idw,scomar20.2012)
scomar20.2012$wind2=scale(scomar20.2012$wind)

#mar21.2012
windmar21.2012=read.csv("Wind/mar21.2012.csv", header=TRUE)
windmar21.2012 <-na.exclude(windmar21.2012)
coordinates(windmar21.2012)<-c("Long","Lat") 
proj4string(windmar21.2012)<-CRS("+proj=longlat +datum=WGS84") 
windmar21.2012<-spTransform(windmar21.2012,CRS(proj4string(bathy)))

p27.idw<-gstat::idw(mar21.2012~1, windmar21.2012, newdata=grd, idp=2.0)
p27.idw<-raster(p27.idw)
#plot(p27.idw)
proj4string(scomar21.2012)<-CRS("+proj=longlat +datum=WGS84")
scomar21.2012=spTransform(scomar21.2012,CRS(proj4string(bathy)))
#plot(scomar21.2012,add=TRUE)
scomar21.2012$wind=extract(p27.idw,scomar21.2012)
scomar21.2012$wind2=scale(scomar21.2012$wind)

#mar24.2012
windmar24.2012=read.csv("Wind/mar24.2012.csv", header=TRUE)
windmar24.2012 <-na.exclude(windmar24.2012)
coordinates(windmar24.2012)<-c("Long","Lat") 
proj4string(windmar24.2012)<-CRS("+proj=longlat +datum=WGS84")
windmar24.2012<-spTransform(windmar24.2012,CRS(proj4string(bathy)))

p28.idw<-gstat::idw(mar24.2012~1, windmar24.2012, newdata=grd, idp=2.0)
p28.idw<-raster(p28.idw)
#plot(p28.idw)
proj4string(scomar24.2012)<-CRS("+proj=longlat +datum=WGS84")
scomar24.2012=spTransform(scomar24.2012,CRS(proj4string(bathy)))
#plot(scomar24.2012,add=TRUE)
scomar24.2012$wind=extract(p28.idw,scomar24.2012)
scomar24.2012$wind2=scale(scomar24.2012$wind)

#mar28.2012
windmar28.2012=read.csv("Wind/mar28.2012.csv", header=TRUE)
windmar28.2012 <-na.exclude(windmar28.2012)
coordinates(windmar28.2012)<-c("Long","Lat") 
proj4string(windmar28.2012)<-CRS("+proj=longlat +datum=WGS84") 
windmar28.2012<-spTransform(windmar28.2012,CRS(proj4string(bathy)))

p29.idw<-gstat::idw(mar28.2012~1, windmar28.2012, newdata=grd, idp=2.0)
p29.idw<-raster(p29.idw)
#plot(p29.idw)
proj4string(scomar28.2012)<-CRS("+proj=longlat +datum=WGS84")
scomar28.2012=spTransform(scomar28.2012,CRS(proj4string(bathy)))
#plot(scomar28.2012,add=TRUE)
scomar28.2012$wind=extract(p29.idw,scomar28.2012)
scomar28.2012$wind2=scale(scomar28.2012$wind)

#mar29.2012
windmar29.2012=read.csv("Wind/mar29.2012.csv", header=TRUE)
windmar29.2012 <-na.exclude(windmar29.2012)
coordinates(windmar29.2012)<-c("Long","Lat") 
proj4string(windmar29.2012)<-CRS("+proj=longlat +datum=WGS84") 
windmar29.2012<-spTransform(windmar29.2012,CRS(proj4string(bathy)))

p30.idw<-gstat::idw(mar29.2012~1, windmar29.2012, newdata=grd, idp=2.0)
p30.idw<-raster(p30.idw)
#plot(p30.idw)
proj4string(scomar29.2012)<-CRS("+proj=longlat +datum=WGS84")
scomar29.2012=spTransform(scomar29.2012,CRS(proj4string(bathy)))
#plot(scomar29.2012,add=TRUE)
scomar29.2012$wind=extract(p30.idw,scomar29.2012)
scomar29.2012$wind2=scale(scomar29.2012$wind)

#WAVE
#feb2.2009
wavefeb2.2009=read.csv("Wave/feb2.2009.csv", header=TRUE)
wavefeb2.2009 <-na.exclude(wavefeb2.2009)
coordinates(wavefeb2.2009)<-c("Long","Lat") 
proj4string(wavefeb2.2009)<-CRS("+proj=longlat +datum=WGS84") 
wavefeb2.2009<-spTransform(wavefeb2.2009,CRS(proj4string(bathy)))

x.range<-as.numeric(c(-82,-72))
y.range<-as.numeric(c(30,39))
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

#mar20.2012
paa.idw<-gstat::idw(mar20.2012~1, wavefeb18.2012, newdata=grd, idp=2.0)
paa.idw<-raster(paa.idw)
#plot(paa.idw)
#plot(scomar20.2012,add=TRUE)
scomar20.2012$wave=extract(paa.idw,scomar20.2012)
scomar20.2012$wave2=scale(scomar20.2012$wave)

#mar21.2012
wavemar21.2012=read.csv("Wave/mar21.2012.csv", header=TRUE)
wavemar21.2012 <-na.exclude(wavemar21.2012)
coordinates(wavemar21.2012)<-c("Long","Lat") 
proj4string(wavemar21.2012)<-CRS("+proj=longlat +datum=WGS84") 
wavemar21.2012<-spTransform(wavemar21.2012,CRS(proj4string(bathy)))

pbb.idw<-gstat::idw(mar21.2012~1, wavemar21.2012, newdata=grd, idp=2.0)
pbb.idw<-raster(pbb.idw)
#plot(pbb.idw)
#plot(scomar21.2012,add=TRUE)
scomar21.2012$wave=extract(pbb.idw,scomar21.2012)
scomar21.2012$wave2=scale(scomar21.2012$wave)

#mar24.2012
wavemar24.2012=read.csv("Wave/mar24.2012.csv", header=TRUE)
wavemar24.2012 <-na.exclude(wavemar24.2012)
coordinates(wavemar24.2012)<-c("Long","Lat") 
proj4string(wavemar24.2012)<-CRS("+proj=longlat +datum=WGS84") 
wavemar24.2012<-spTransform(wavemar24.2012,CRS(proj4string(bathy)))

pcc.idw<-gstat::idw(mar24.2012~1, wavemar24.2012, newdata=grd, idp=2.0)
pcc.idw<-raster(pcc.idw)
#plot(pcc.idw)
#plot(scomar24.2012,add=TRUE)
scomar24.2012$wave=extract(pcc.idw,scomar24.2012)
scomar24.2012$wave2=scale(scomar24.2012$wave)

#mar28.2012
pdd.idw<-gstat::idw(mar28.2012~1, wavefeb4.2012, newdata=grd, idp=2.0)
pdd.idw<-raster(pdd.idw)
#plot(pdd.idw)
#plot(scomar28.2012,add=TRUE)
scomar28.2012$wave=extract(pdd.idw,scomar28.2012)
scomar28.2012$wave2=scale(scomar28.2012$wave)

#mar29
pee.idw<-gstat::idw(mar29.2012~1, wavemar24.2012, newdata=grd, idp=2.0)
pee.idw<-raster(pee.idw)
#plot(pee.idw)
#plot(scomar29.2012,add=TRUE)
scomar29.2012$wave=extract(pee.idw,scomar29.2012)
scomar29.2012$wave2=scale(scomar29.2012$wave)


sco2009<-rbind(scofeb2.2009,scofeb5.2009,scofeb6.2009,scofeb7.2009,scofeb8.2009,scofeb9.2009)
sco2010<-rbind(scofeb1.2010,scofeb3.2010,scofeb4.2010,scofeb9.2010,scofeb11.2010,scofeb12.2010,scofeb15.2010)
sco2011<-rbind(scofeb3.2011,scofeb6.2011,scofeb9.2011,scofeb11.2011,scofeb12.2011,scofeb13.2011,scofeb16.2011,scofeb17.2011)
sco2012<-rbind(scofeb4.2012,scofeb5.2012,scofeb8.2012,scofeb17.2012,scofeb18.2012,scofeb21.2012,scomar20.2012,scomar21.2012,scomar24.2012,scomar28.2012,scomar29.2012)
scototal<-rbind(sco2009,sco2010,sco2011,sco2012)

sco2<-cbind(sco2,scototal[18:21])
sco2$sednum=as.factor(sco2@data$substrate$SEDNUM)
sco2$substrate<-NULL
sco2$eco=as.factor(sco2@data$eco$ECOREGION)
sco2$bival=as.factor(sco2$bival)



#transect data

transect=rgdal::readOGR("Layers/transects/transect.shp")
transect=spTransform(transect,CRS(proj4string(bathy)))
tran2009<-subset(transect, Year==2009, select=UID:Year)
tran2010<-subset(transect, Year==2010, select=UID:Year)
tran2011<-subset(transect, Year==2011, select=UID:Year)
tran2012<-subset(transect, Year==2012, select=UID:Year)

#segmenting transects

library(DSpat)
library(GISTools)
library(spatstat)
#2009
tran2009=spTransform(tran2009,
                  CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0"))
tran.sub=tran2009[!duplicated(tran2009$Transect),] #subset to remove duplicates
tran.sub$id=seq(1,114) #add a unique ID to each transect

#this just uses transect 1 as an example
x0=coordinates(tran.sub)[[1]][[1]][1,1]
x1=coordinates(tran.sub)[[1]][[1]][90,1]
y0=coordinates(tran.sub)[[1]][[1]][1,2]
y1=coordinates(tran.sub)[[1]][[1]][90,2]

lines=data.frame(label=1,x0=x0,x1=x1,y0=y0,y1=y1)

tmp=as.psp(tran.sub[tran.sub$id==1,])

strtransect<-lines_to_strips(lines,as.owin(tmp), width=250)
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
tran2009=coordinates(tran.sub)[[2]][[1]]

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
strtransect<-lines_to_strips(lines,as.owin(tmp), width=250)
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
tran.sub=tran2010[!duplicated(tran2010$Transect),] #subset to remove duplicates
tran.sub$id=seq(1,114) #add a unique ID to each transect

#this just uses transect 1 as an example
x0=coordinates(tran.sub)[[1]][[1]][1,1]
x1=coordinates(tran.sub)[[1]][[1]][90,1]
y0=coordinates(tran.sub)[[1]][[1]][1,2]
y1=coordinates(tran.sub)[[1]][[1]][90,2]

lines=data.frame(label=1,x0=x0,x1=x1,y0=y0,y1=y1)

tmp=as.psp(tran.sub[tran.sub$id==1,])

strtransect<-lines_to_strips(lines,as.owin(tmp), width=250)
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
tran2010=coordinates(tran.sub)[[2]][[1]]

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
strtransect<-lines_to_strips(lines,as.owin(tmp), width=250)
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
tran.sub=tran2011[!duplicated(tran2011$Transect),] #subset to remove duplicates
tran.sub$id=seq(1,101) #add a unique ID to each transect

#this just uses transect 1 as an example
x0=coordinates(tran.sub)[[1]][[1]][1,1]
x1=coordinates(tran.sub)[[1]][[1]][90,1]
y0=coordinates(tran.sub)[[1]][[1]][1,2]
y1=coordinates(tran.sub)[[1]][[1]][90,2]

lines=data.frame(label=1,x0=x0,x1=x1,y0=y0,y1=y1)

tmp=as.psp(tran.sub[tran.sub$id==1,])

strtransect<-lines_to_strips(lines,as.owin(tmp), width=250)
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
tran2011=coordinates(tran.sub)[[2]][[1]]

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
strtransect<-lines_to_strips(lines,as.owin(tmp), width=250)
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
tran.sub=tran2012[!duplicated(tran2012$Transect),] #subset to remove duplicates
tran.sub$id=seq(1,86) #add a unique ID to each transect

#this just uses transect 1 as an example
x0=coordinates(tran.sub)[[1]][[1]][1,1]
x1=coordinates(tran.sub)[[1]][[1]][90,1]
y0=coordinates(tran.sub)[[1]][[1]][1,2]
y1=coordinates(tran.sub)[[1]][[1]][90,2]

lines=data.frame(label=1,x0=x0,x1=x1,y0=y0,y1=y1)

tmp=as.psp(tran.sub[tran.sub$id==1,])

strtransect<-lines_to_strips(lines,as.owin(tmp), width=250)
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
tran2012=coordinates(tran.sub)[[2]][[1]]

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
strtransect<-lines_to_strips(lines,as.owin(tmp), width=250)
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

proj4string(out)=CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0")
out2=spTransform(out,CRS(proj4string(bathy)))


#intersecting grid and point data

scoters2009<-subset(sco2, SurveyBeginYear==2009, select=SurveyId:sednum)
scoters2010<-subset(sco2, SurveyBeginYear==2010, select=SurveyId:sednum)
scoters2011<-subset(sco2, SurveyBeginYear==2011, select=SurveyId:sednum)
scoters2012<-subset(sco2, SurveyBeginYear==2012, select=SurveyId:sednum)


test=gIntersection(out2,sco2,byid=TRUE)
ids<-rownames(data.frame(test))
ids<-strsplit(ids, " ")
index<-as.numeric(sapply(ids,"[[",2))
df.sco<-data.frame(sco2[index,])
#write.table(df.sco, "grid.txt", sep="\t")

#Original worked fine

test1=gIntersection(out2009,scoters2009,byid=TRUE)
ids<-rownames(data.frame(test1))
ids<-strsplit(ids, " ")
index1<-as.numeric(sapply(ids,"[[",2))
df.sco2009<-data.frame(scoters2009[index1,])
#write.table(df.sco2009, "grid2009.txt", sep="\t")

#line 1911 gives subscript out of bounds error

test.a=gIntersection(out2009,sco2,byid=TRUE)
ids.a<-rownames(data.frame(test.a))
ids.a<-strsplit(ids.a, " ")
index1.a<-as.numeric(sapply(ids.a,"[[",2))
df.sco2009a<-data.frame(sco2[index1.a,])
#write.table(df.sco2009a, "grid2009.txt", sep="\t")

#line 1921 runs with no error
#the only difference (that I can tell) between sco2 and scoters2009 is that
#scoters2009 is a subset of sco2 and has less points

test2=gIntersection(out2010,scoters2010,byid=TRUE)
ids<-rownames(data.frame(test2))
ids<-strsplit(ids, " ")
index2<-as.numeric(sapply(ids,"[[",2))
df.sco2010<-data.frame(scoters2010[index2,])
write.table(df.sco2010, "grid2010.txt", sep="\t")

test3=gIntersection(out2011,scoters2011,byid=TRUE)
ids<-rownames(data.frame(test3))
ids<-strsplit(ids, " ")
index3<-as.numeric(sapply(ids,"[[",2))
df.sco2011<-data.frame(scoters2011[index3,])
write.table(df.sco2011, "grid2011.txt", sep="\t")

test4=gIntersection(out2012,scoters2012,byid=TRUE)
ids<-rownames(data.frame(test4))
ids<-strsplit(ids, " ")
index4<-as.numeric(sapply(ids,"[[",2))
df.sco2012<-data.frame(scoters2012[index4,])
write.table(df.sco2012, "grid2012.txt", sep="\t")



#multicollinearity

sco2@data$substrate$SEDNUM=as.numeric(sco2@data$substrate$SEDNUM)
sco2@data$eco$ECOREGION=as.numeric(sco2@data$eco$ECOREGION)
sco2$bival=as.numeric(sco2$bival)

cor.test(sco2$bathy,sco2$dist)
cor.test(sco2$bathy,sco2$slope)
cor.test(sco2$bathy,sco2@data$substrate$SEDNUM)
cor.test(sco2$dist,sco2$slope)
cor.test(sco2$dist,sco2@data$substrate$SEDNUM)
cor.test(sco2$slope,sco2@data$substrate$SEDNUM)
cor.test(sco2$bathy,sco2$NAO)
cor.test(sco2$dist,sco2$NAO)
cor.test(sco2$slope,sco2$NAO)
cor.test(sco2@data$substrate$SEDNUM,sco2$NAO)
cor.test(sco2$bathy,sco2@data$eco$ECOREGION)
cor.test(sco2$dist,sco2@data$eco$ECOREGION)
cor.test(sco2$slope,sco2@data$eco$ECOREGION)
cor.test(sco2$NAO,sco2@data$eco$ECOREGION)
cor.test(sco2@data$substrate$SEDNUM,sco2@data$eco$ECOREGION)
cor.test(sco2$bathy,sco2$wind)
cor.test(sco2$bathy,sco2$wave)
cor.test(sco2$dist,sco2$wind)
cor.test(sco2$dist,sco2$wave)
cor.test(sco2$slope,sco2$wind)
cor.test(sco2$slope,sco2$wave)
cor.test(sco2@data$substrate$SEDNUM,sco2$wind)
cor.test(sco2@data$substrate$SEDNUM,sco2$wave)
cor.test(sco2$NAO,sco2$wind)
cor.test(sco2$NAO,sco2$wave)
cor.test(sco2@data$eco$ECOREGION,sco2$wind)
cor.test(sco2@data$eco$ECOREGION,sco2$wave)
cor.test(sco2$wind,sco2$wave)
cor.test(sco2$bathy,sco2$bival)
cor.test(sco2$dist,sco2$bival)
cor.test(sco2$slope,sco2$bival)
cor.test(sco2@data$substrate$SEDNUM,sco2$bival)
cor.test(sco2$NAO,sco2$bival)
cor.test(sco2@data$eco$ECOREGION,sco2$bival)
cor.test(sco2$wind,sco2$bival)
cor.test(sco2$wave,sco2$bival)

#LASSO
library(glmnet)#lasso package

sco2$sednum=as.factor(sco2@data$substrate$SEDNUM)
sco2$substrate<-NULL
sco2$eco=as.factor(sco2@data$eco$ECOREGION)
sco2$bival=as.factor(sco2$bival)


sco2<-data.frame(sco2)
sco2 <-na.omit(sco2)
x=model.matrix(Count~bathy2+dist2+slope2+sednum+NAO2+wind+wave-1,data=sco2)
lasso<-glmnet(x,sco2$Count, family = "gaussian", alpha=1)
plot(lasso,xvar="lambda",label=TRUE)

cv.lasso=cv.glmnet(x,sco2$Count)
plot(cv.lasso)
coef(cv.lasso)





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


#slope2=seq(min(sco2$slope2), max(sco2$slope2),length=1116)

#oceanslope=predict(m10d,
#                  data.frame(slope2=seq(min(sco2$slope2), 
#                                       max(sco2$slope2),length=1116),
#                             dist2=as.numeric(rep(0,1116)),
#                             sednum=as.factor(rep(4,1116))),
#                  type="response")

#plot(y=oceanslope,x=seq(min(sco2$slope2),max(sco2$slope2),length=1116),
#     type="l", lwd=1, xlab = "Ocean Floor Slope",
#     ylab = "Expected Count", cex.lab=1.3)

#dist2=seq(min(sco2$dist2), max(sco2$dist2), length=1116)

#dist2shore=predict(m10d,
#                   data.frame(dist2=seq(min(sco2$dist2), 
#                                         max(sco2$dist2),length=1116),
#                              slope2=as.numeric(rep(0,1116)),
#                              sednum=as.factor(rep(4,1116))),
#                   type="response")

#plot(y=dist2shore,x=seq(min(sco2$dist2),max(sco2$dist2),length=1116),
#     type="l", lwd=1, xlab = "Distance to Shore",
#     ylab = "Expected Count", cex.lab=1.3)


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

#Plotting top model variables (fitted values)
m41a<-na.omit(m41a)
m36a<-na.omit(m36a)
sco2<-data.frame(sco2)
sco2<-na.omit(sco2)

#stat_smooth(method="lm", se=TRUE)

sco2$fit<-fitted(m36a)
distance<-ggplot(sco2, aes(x=dist, y=fit))
distance+geom_point()+ #x-axis is in meters
  stat_smooth(method="lm", formula=y~poly(x,2),se=TRUE)+
  theme(panel.background = element_rect(colour = 'black', fill='white'))+
  theme(axis.title.x=element_text(size=15, color = "black"))+
  theme(axis.title.y=element_text(size=15, color = "black"))+
  xlab("Distance from Shore (meters)")+
  ylab("Number of Black Scoters")


nao<-ggplot(sco2, aes(x=NAO, y=fit))
nao+geom_point()+ #x-axis is NAO values
  stat_smooth(method="lm",formula=y~poly(x,2), se=TRUE)+
  theme(panel.background = element_rect(colour = 'black', fill='white'))+
  theme(axis.title.x=element_text(size=15, color = "black"))+
  theme(axis.title.y=element_text(size=15, color = "black"))+
  xlab("North Atlantic Oscillation")+
  ylab("Number of Black Scoters")

sub<-ggplot(sco2, aes(x=sednum))
sub+geom_bar()+
  theme(panel.background = element_rect(colour = 'black', fill='white'))+
  theme(axis.title.x=element_text(size=15, color = "black"))+
  theme(axis.title.y=element_text(size=15, color = "black"))+
  xlab("Substrate Type")+
  ylab("Number of Black Scoters")

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

