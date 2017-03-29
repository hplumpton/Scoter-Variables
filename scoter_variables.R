library(rgdal)
library(maps)
library(raster)
library(rgeos)
library(maptools)


#bathymetry

bathy=raster("Layers/etopo1 bathymetry.tif")

scoters=read.csv("ObsData2.csv",header=TRUE)
scoters <-na.omit(scoters)

coordinates(scoters)<-c("longitude_dd","latitude_dd") 
#define x&y coordinates
proj4string(scoters)<-CRS("+proj=longlat +datum=WGS84") 
#assigning a projection

sco2=spTransform(scoters,CRS(proj4string(bathy))) 
#assign same projection as bathy

#sco3=SpatialPoints(sco2) 
#extract function didn't like SpatialPointsDataFrame



#substrate

substrate=readShapePoly("Layers/substrate/conmapsg.shp")
proj4string(substrate)<-CRS("+proj=longlat +datum=WGS84")

plot(substrate)

plot(scoters,add=TRUE)

substrate<-spTransform(substrate,CRS(proj4string(bathy)))

sco2$substrate=over(sco2,substrate)


#ocean floor slope

slope<-terrain(bathy, opt=c('slope'), unit='degrees')

plot(scoters,add=TRUE)

scotslope=SpatialPoints(sco2)
proj4string(scotslope)<-CRS("+proj=longlat +datum=WGS84")

sco2$slope=extract(slope,scotslope)

sco2$slope2=scale(sco2$slope)
#standardize covariates for comparison of beta estimates later on
sco2$slope2=as.numeric(scale(sco2$slope))


#distance to shore

shoreline=readShapePoly("Layers/shoreline/GSHHS_shp/i/GSHHS_i_L1.shp")
proj4string(shoreline)<-CRS("+proj=longlat +datum=WGS84")

seshoreline <- crop(shoreline, extent(-82, -72, 30, 39))
proj4string(seshoreline)<-CRS("+proj=longlat +datum=WGS84")
plot(seshoreline)

plot(scoters,add=TRUE)

dist<-distanceFromPoints(seshoreline,scoters)
proj4string(dist)<-CRS("+proj=longlat +datum=WGS84")

sco2$dist=extract(dist,sco2)

sco2$dist2=scale(sco2$dist)

sco2$dist2=as.numeric(scale(sco2$dist)) 
#You'll need to add as.numeric here too


#sediment mobility
#large amount of NA's probably due to the layers do not go
#close enough to shore for a lot of the data points
#sedmobility1=readShapePoly("Layers/sediment mobility/SAB_median/SAB_median.shp")
#sedmobility2=readShapePoly("Layers/sediment mobility/MAB_median/MAB_median.shp")
#sedmobility<-union(sedmobility1,sedmobility2)
#proj4string(sedmobility)<-CRS("+proj=longlat +datum=WGS84")
#plot(sedmobility)
#plot(scoters,add=TRUE)
#sedmob<-spTransform(sedmobility, CRS(proj4string(bathy)))
#co2$sedmob=extract(sedmob,sco2)
#summary(sco2)



#multicollinearity

#cor.test(sco2$bathy2,sco2$dist2)
#cor.test(sco2$bathy2,sco2$slope2)
#cor.test(sco2$bathy2,sco2$sednum)
cor.test(sco2$dist2,sco2$slope2)
cor.test(sco2$dist2,sco2$sednum)
cor.test(sco2$slope2,sco2$sednum)

#negative binomial
library(MASS)

sco2$sednum=as.factor(sco2@data$substrate$SEDNUM)
sco2$slopesq=sco2$slope^2
sco2$distsq=sco2$dist^2

#m1<-glm.nb(Count~bathy2, data=sco2)
#m1$aic
#m2<-glm.nb(Count~bathy2 + dist2, data=sco2)
#m2$aic
#m3<-glm.nb(Count~bathy2 + slope2, data=sco2)
#m3$aic
#m4<-glm.nb(Count~bathy2 + sco2$sednum, data = sco2)
#m4$aic
#m5<-glm.nb(Count~bathy2 + sco2$sedmob2, data=sco2)
#m5$aic
#m6<-glm.nb(Count~bathy2 + dist2 + slope2, data=sco2)
#m6$aic
#m7<-glm.nb(Count~bathy2 + dist2 + sco2$sedmob2, data=sco2)
#m7$aic
#m8<-glm.nb(Count~bathy2 + dist2 + sco2$sednum, data=sco2)
#m8$aic
#m9<-glm.nb(Count~bathy2 + dist2 + slope2 + sco2$sedmob2, data=sco2)
#m9$aic
m10<-glm.nb(Count~dist2 + slope2 + sco2$sednum, data=sco2)
summary(m10)
m10$aic

m10a<-glm.nb(Count~poly(dist2,2)+slope2+sco2$sednum,data=sco2)
m10a$aic

m10b<-glm.nb(Count~dist2+poly(slope2,2)+sco2$sednum,data=sco2)
m10b$aic

#m10c<-glm.nb(Count~(bathy2+I(bathy2^2))+dist2+slope2+sco2$sednum,data=sco2)
#m10c$aic

m10d<-glm.nb(Count~poly(dist2,2)+ poly(slope2,2)+sednum,data=sco2)
m10d$aic

#m10e<-glm.nb(Count~(bathy2+I(bathy2^2))+(dist2+I(dist2^2))+slope2+
#               sco2$sednum,data=sco2)
#m10e$aic
#m10f<-glm.nb(Count~(bathy2+I(bathy2^2))+dist2+(slope2+I(slope2^2))+
#               sco2$sednum,data=sco2)
#m10f$aic
#m10g<-glm.nb(Count~(bathy2+I(bathy2^2))+(dist2+I(dist2^2))+(slope2+I(slope2^2))+
#               sco2$sednum,data=sco2)
#m10g$aic
#m11<-glm.nb(Count~bathy2 + dist2 + slope2 + sco2$sednum +
#            sco2$sedmob2, data=sco2)
#m11$aic
#m12<-glm.nb(Count~bathy2 + slope2 + sco2$sedmob2, data=sco2)
#m12$aic
#m13<-glm.nb(Count~bathy2 + slope2 + sco2$sednum, data=sco2)
#m13$aic
#m14<-glm.nb(Count~bathy2 + slope2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m14$aic
#m15<-glm.nb(Count~bathy2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m15$aic
m16<-glm.nb(Count~dist2, data=sco2)
m16$aic
m17<-glm.nb(Count~dist2 + slope2, data=sco2)
m17$aic
#m18<-glm.nb(Count~dist2 + sco2$sedmob2, data=sco2)
#m18$aic
m19<-glm.nb(Count~dist2 + sco2$sednum, data=sco2)
m19$aic
#m20<-glm.nb(Count~dist2 + slope2 + sco2$sedmob2, data=sco2)
#m20$aic
m21<-glm.nb(Count~dist2 + slope2 + sco2$sednum, data=sco2)
m21$aic
#m22<-glm.nb(Count~dist2 + slope2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m22$aic
#m23<-glm.nb(Count~dist2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m23$aic
m24<-glm.nb(Count~slope2, data=sco2)
m24$aic
#m25<-glm.nb(Count~slope2 + sco2$sedmob2, data=sco2)
#m25$aic
m26<-glm.nb(Count~slope2 + sco2$sednum, data=sco2)
m26$aic
#m27<-glm.nb(Count~slope2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m27$aic
#m28<-glm.nb(Count~sco2$sedmob2, data=sco2)
#m28$aic
#m29<-glm.nb(Count~sco2$sedmob2 + sco2$sednum, data=sco2)
#m29$aic
m30<-glm.nb(Count~sco2$sednum, data=sco2)
m30$aic

#Delta AIC

Table = AIC(m10, m10a, m10b, m10d)
n = dim(sco2)[1]  #sample size
# Table$df yields K, the number of parameters estimated in each model
# Table$AIC yields the AIC value for each model
AICc = Table$AIC + (2*Table$df*(Table$df+1))/(n-Table$df-1) #Calculate AICc from AIC, n, and K
Table = cbind(Table,AICc)
deltaAICc = Table$AICc - min(Table$AICc) #Calculates the delta AICc values
Table = cbind(Table,deltaAICc)
Table = Table[order(Table$AICc), ]
Table

#Weighted AIC
library(MuMIn)
out.put<-model.sel(m10,m10a,m10b,m10d)
out.put
summary(m10d)

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
m10d<-na.omit(m10d)
sco2<-data.frame(sco2)
sco2<-na.omit(sco2)


sco2$fit<-fitted(m10d)
distance<-ggplot(sco2, aes(x=dist, y=fit))
distance+geom_point() #x-axis is in meters

floorslope<-ggplot(sco2, aes(x=slope, y=fit))
floorslope+geom_point() #x-axis is in degrees

#Bivalve Distribution
bival1=substrate=readShapePoly("Layers/NCarolina_2016_GDB/LAYER FILES/INVERTEBRATE POLYS.lyr")


#North Atlantic Oscillation
#Fine Scale Weather


#Home range: kernel density (adehabitatHR) function getvolumeUD, h=LSCV
#
