library(rgdal)
library(maps)
library(raster)
library(rgeos)
library(maptools)
library(ggplot2)

#bathymetry

bathy=raster("Layers/etopo1 bathymetry.tif")
image(bathy)
summary(bathy)

scoters=read.csv("ObsData2.csv",header=TRUE)
scoters <-na.omit(scoters)
summary(scoters)

coordinates(scoters)<-c("longitude_dd","latitude_dd") 
#define x&y coordinates
proj4string(scoters)<-CRS("+proj=longlat +datum=WGS84") 
#assigning a projection
plot(scoters,add=TRUE)

sco2=spTransform(scoters,CRS(proj4string(bathy))) 
#assign same projection as bathy
map("state", add=TRUE)

sco3=SpatialPoints(sco2) 
#extract function didn't like SpatialPointsDataFrame
sco2$bathy=extract(bathy,sco3) 
#extract bathymetry measure at each spatial location 
#combined with scoters data
sco2$bathy2=scale(sco2$bathy) 
#standardize covariates for comparison of beta estimates later on

#substrate

substrate=readShapePoly("Layers/substrate/conmapsg.shp")
proj4string(substrate)<-CRS("+proj=longlat +datum=WGS84")
image(substrate)
summary(substrate)

scoters=read.csv("ObsData2.csv",header=TRUE)
scoters <-na.omit(scoters)
summary(scoters)

coordinates(scoters)<-c("longitude_dd","latitude_dd") 
#define x&y coordinates
proj4string(scoters)<-CRS("+proj=longlat +datum=WGS84") 
#assigning a projection
plot(scoters,add=TRUE)

sub<- spTransform(substrate, CRS("+proj=longlat +datum=WGS84"))
#assigned same projection as bathy
sco2=spTransform(scoters,CRS(proj4string(bathy))) 
#assign same projection as bathy

scotsubtr=SpatialPoints(sco2)
scotsubtr<- spTransform(substrate, CRS("+proj=longlat +datum=WGS84"))
proj4string(scotsubtr)<-CRS("+proj=longlat +datum=WGS84")

sco2$substrate=extract(sub,scotsubtr)
#extract substrate measure at each spatial laoction 
#combined with scoters data
sco2$sub2=scale(sco2$substrate)
#standardize covariates for comparison of beta estimates later on

#sediment mobility

sedmobility=readShapePoly("Layers/sediment mobility/SAB_median/SAB_median.shp")
proj4string(sedmobility)<-CRS("+proj=longlat +datum=WGS84")
image(sedmobility)
summary(sedmobility)

scoters=read.csv("ObsData2.csv",header=TRUE)
scoters <-na.omit(scoters)
summary(scoters)

coordinates(scoters)<-c("longitude_dd","latitude_dd") 
#define x&y coordinates
proj4string(scoters)<-CRS("+proj=longlat +datum=WGS84") 
#assigning a projection
plot(scoters,add=TRUE)

sedmob<-spTransform(sedmobility, CRS("+proj=longlat +datum=WGS84"))
#assigned same projection as bathy
sco2=spTransform(scoters,CRS(proj4string(bathy))) 
#assign same projection as bathy
head(sedmobility)

scotsedmob=SpatialPoints(sco2)
proj4string(scotsedmob)<-CRS("+proj=longlat +datum=WGS84")
sco2$sedmobility=extract(sedmob,scotsedmob)
#extract sediment mobility measure at each spatial location 
#combined with scoters data
sco2$sedmobility2=scale(sco2$sedmobility)
#standardize covariates for comparison of beta estimates later on

#ocean floor slope

bathy=raster("Layers/etopo1 bathymetry.tif")
image(bathy)
summary(bathy)

slope<-terrain(bathy, opt=c('slope'), unit='degrees')
summary(slope)
head(slope)

scoters=read.csv("ObsData2.csv",header=TRUE)
scoters <-na.omit(scoters)
summary(scoters)

coordinates(scoters)<-c("longitude_dd","latitude_dd") 
#define x&y coordinates
proj4string(scoters)<-CRS("+proj=longlat +datum=WGS84") 
#assigning a projection
plot(scoters,add=TRUE)

sco2=spTransform(scoters,CRS(proj4string(bathy))) 
#assign same projection as bathy

scotslope=SpatialPoints(sco2)
proj4string(scotslope)<-CRS("+proj=longlat +datum=WGS84")
sco2$slope=extract(slope,scotslope)
#extract sediment mobility measure at each spatial location 
#combined with scoters data
sco2$slope2=scale(sco2$slope)
#standardize covariates for comparison of beta estimates later on

#distance to shore

shoreline=readShapePoly("Layers/shoreline/GSHHS_shp/i/GSHHS_i_L1.shp")
proj4string(shoreline)<-CRS("+proj=longlat +datum=WGS84")
plot(shoreline)



#transect data

transect=readShapeLines("Layers/transects/WinterSurvey_TrackLines_sCoast.shp")
proj4string(transect)<-CRS("+proj=longlat +datum=WGS84")
plot(transect)
map("state", add=TRUE)
head(transect)

#dividing transects into grids

library(DSpat)
strtransect<-lines_to_strips(transects, study.area ='transect', width=250)



#North Atlantic Oscillation
#Fine Scale Weather
#Bivalve Distribution
