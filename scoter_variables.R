library(rgdal)
library(maps)
library(raster)
library(rgeos)
library(maptools)


#bathymetry

bathy=raster("Layers/etopo1 bathymetry.tif")

scoters=read.csv("ObsData2.csv",header=TRUE)
scoters <-na.omit(scoters)
image(bathy)

coordinates(scoters)<-c("longitude_dd","latitude_dd") 
#define x&y coordinates
proj4string(scoters)<-CRS("+proj=longlat +datum=WGS84") 
#assigning a projection
plot(scoters, add=TRUE)
sco2=spTransform(scoters,CRS(proj4string(bathy))) 
#assign same projection as bathy

sco3=SpatialPoints(sco2) 
#extract function didn't like SpatialPointsDataFrame
sco2$bathy=extract(bathy,sco3)
sco2$bathy2=scale(sco2$bathy)
sco2$bathy2=as.numeric(sco2$bathy)


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
#sco2$sedmob=extract(sedmob,sco2)
#summary(sco2)
#624 NA's probably because they were closer to the shoreline than layer was

#North Atlantic Oscillation
sco2$NAO2=scale(sco2$NAO)


#Bivalve Distribution
#bival1=readShapePoly("Layers/SCarolina/Layers/invertebrates.shp")
#proj4string(bival1)<-CRS("+proj=longlat +datum=WGS84")
#bival2=readShapePoly("Layers/NCarolina/LAYER FILES/invert.shp")
#proj4string(bival2)<-CRS("+proj=longlat +datum=WGS84")
#bival3=readShapePoly("Layers/ChesapeakeBay/LAYER FILES/invert.shp")
#proj4string(bival3)<-CRS("+proj=longlat +datum=WGS84")
#bival4=readShapePoly("Layers/Georgia/LAYER_FILES/invert.shp")
#proj4string(bival4)<-CRS("+proj=longlat +datum=WGS84")

#bival=union(bival1,bival2)
#proj4string(bival)<-CRS("+proj=longlat +datum=WGS84")
#bivalv=union(bival3,bival4)
#proj4string(bivalv)<-CRS("+proj=longlat +datum=WGS84")
#bival=union(bival,bivalv)

#proj4string(bival)<-CRS("+proj=longlat +datum=WGS84")
#bival<-spTransform(bival,CRS(proj4string(bathy)))
#summary(bival)
#plot(bival)
#plot(scoters,add=TRUE)
  
#sco2$bival=over(bival,sco2)
#sco2$bival2=scale(sco2$bival)
#summary(sco2$bival)


#Fine Scale Weather


#multicollinearity

cor.test(sco2$bathy2,sco2$dist2)
cor.test(sco2$bathy2,sco2$slope2)
cor.test(sco2$bathy2,sco2$sednum)
cor.test(sco2$dist2,sco2$slope2)
cor.test(sco2$dist2,sco2$sednum)
cor.test(sco2$slope2,sco2$sednum)
cor.test(sco2$bathy2,sco2$NAO2)
cor.test(sco2$dist2,sco2$NAO2)
cor.test(sco2$slope2,sco2$NAO2)

#negative binomial
library(MASS)

sco2$sednum=as.factor(sco2@data$substrate$SEDNUM)
sco2$NAO2=as.factor(sco2$NAO)
sco2$slopesq=sco2$slope^2
sco2$distsq=sco2$dist^2

#random effects (1|Transect) and (1|SurveyBeginYear)
sco2=data.frame(sco2)
sco2$Transect=as.factor(sco2$Transect)
sco2$SurveyBeginYear=as.factor(sco2$SurveyBeginYear)

library(lme4)
#m0<-glmer.nb(Count~1+(1|Transect)+(1|SurveyBeginYear),data=sco2, 
#             na.action='na.omit')
#m1<-glmer.nb(Count~bathy2+(1|Transect)+(1|SurveyBeginYear), data=sco2)

m2<-glmer.nb(Count~bathy2 + dist2+(1|Transect)+(1|SurveyBeginYear),
             data=sco2)
#m3<-glmer.nb(Count~bathy2 + slope2+(1|Transect)+(1|SurveyBeginYear),
#             data=sco2)
#m4<-glmer.nb(Count~bathy2 + sco2$sednum+(1|Transect)+(1|SurveyBeginYear),
#             data = sco2,na.action='na.omit')
#m5<-glmer.nb(Count~bathy2 + sco2$sedmob2, data=sco2)
#m6<-glmer.nb(Count~bathy2 + dist2 + slope2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m7<-glmer.nb(Count~bathy2 + dist2 + sco2$sedmob2, data=sco2)
m8<-glmer.nb(Count~bathy2 + dist2 + sco2$sednum +(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m9<-glmer.nb(Count~bathy2 + dist2 + slope2 + sco2$sedmob2, data=sco2)
m10<-glmer.nb(Count~dist2 + slope2 + sco2$sednum+(1|Transect)+(1|SurveyBeginYear), data=sco2)
m10a<-glmer.nb(Count~poly(dist2,2)+slope2+sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
m10b<-glmer.nb(Count~dist2+poly(slope2,2)+sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
m10c<-glmer.nb(Count~poly(bathy2,2)+dist2+slope2+sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
m10d<-glmer.nb(Count~poly(dist2,2)+ poly(slope2,2)+sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
m10e<-glmer.nb(Count~poly(bathy2,2)+poly(dist2,2)+slope2+
                 sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
m10f<-glmer.nb(Count~poly(bathy2,2)+dist2+poly(slope2,2)+
                 sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
m10g<-glmer.nb(Count~poly(bathy2,2)+poly(dist2,2)+poly(slope2,2)+
                 sco2$sednum+(1|Transect)+(1|SurveyBeginYear),data=sco2)
#m11<-glmer.nb(Count~bathy2 + dist2 + slope2 + sco2$sednum +
#           sco2$sedmob2, data=sco2)
#m12<-glmer.nb(Count~bathy2 + slope2 + sco2$sedmob2, data=sco2)
m13<-glmer.nb(Count~bathy2 + slope2 + sco2$sednum+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m14<-glmer.nb(Count~bathy2 + slope2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m15<-glmer.nb(Count~bathy2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m16<-glmer.nb(Count~dist2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m17<-glmer.nb(Count~dist2 + slope2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m18<-glmer.nb(Count~dist2 + sco2$sedmob2, data=sco2)
m19<-glmer.nb(Count~dist2 + sco2$sednum+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m20<-glmer.nb(Count~dist2 + slope2 + sco2$sedmob2, data=sco2)
m21<-glmer.nb(Count~dist2 + slope2 + sco2$sednum+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m22<-glmer.nb(Count~dist2 + slope2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m23<-glmer.nb(Count~dist2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m24<-glmer.nb(Count~slope2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m25<-glmer.nb(Count~slope2 + sco2$sedmob2, data=sco2)
m26<-glmer.nb(Count~slope2 + sco2$sednum+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m27<-glmer.nb(Count~slope2 + sco2$sedmob2 + sco2$sednum, data=sco2)
#m28<-glmer.nb(Count~sco2$sedmob2, data=sco2)
#m29<-glmer.nb(Count~sco2$sedmob2 + sco2$sednum, data=sco2)
m30<-glmer.nb(Count~sco2$sednum+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m31<-glmer.nb(Count~NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m32<-glmer.nb(Count~dist2+NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m33<-glmer.nb(Count~slope2+NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
m34<-glmer.nb(Count~sco2$sednum+NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m35<-glmer.nb(Count~dist2+slope2+NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
m36<-glmer.nb(Count~dist2+sco2$sednum+NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
m36a<-glmer.nb(Count~poly(dist2,2)+sco2$sednum+NAO2+(1|Transect)+(1|SurveyBeginYear),
               data=sco2)
#m36b<-glmer.nb(Count~poly(dist2,2)+sco2$sednum+NAO2+(1|SurveyBeginYear),data=sco2)
m37<-glmer.nb(Count~slope2+sco2$sednum+NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
m38<-glmer.nb(Count~dist2+slope2+sco2$sednum+NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
m39<-glmer.nb(Count~bathy2 + dist2 + slope2 + sco2$sednum + NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
m40<-glmer.nb(Count~bathy2 + slope2 + sco2$sednum + NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
m41<-glmer.nb(Count~bathy2 + dist2 + sco2$sednum + NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m42<-glmer.nb(Count~bathy2 + dist2 + slope2 + NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m43<-glmer.nb(Count~bathy2 + dist2 + NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m44<-glmer.nb(Count~bathy2 + slope2 + NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
m45<-glmer.nb(Count~bathy2 + sco2$sednum + NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)
#m46<-glmer.nb(Count~bathy2 + NAO2+(1|Transect)+(1|SurveyBeginYear), data=sco2)



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

out.put<-model.sel(m10a,m10d,m19,m34,m36,m36a,m38,m37,m39,m40,m41,m45)
out.put
#top model m36a(wt=0.518), m36(delta=2.75, wt=0.131), m34(delta=3.44 wt=0.093)
summary(m36a)


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
tranbox<-make_bbox(lon=scoters$longitude_dd, lat = scoters$latitude_dd+1,
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
m10g<-na.omit(m10g)
m36a<-na.omit(m36a)
sco2<-data.frame(sco2)
sco2<-na.omit(sco2)

#stat_smooth(method="lm", se=TRUE)

sco2$fit<-fitted(m36a)
distance<-ggplot(sco2, aes(x=dist, y=fit))
distance+geom_point()+ #x-axis is in meters
  stat_smooth(method="lm", formula=y~poly(x,2),se=FALSE)+
  theme(panel.background = element_rect(colour = 'black', fill='white'))+
  theme(axis.title.x=element_text(size=15, color = "black"))+
  theme(axis.title.y=element_text(size=15, color = "black"))+
  xlab("Distance from Shore (meters)")+
  ylab("Estimated number of Black Scoters")


nao<-ggplot(sco2, aes(x=NAO, y=fit))
nao+geom_point()+ #x-axis is NAO values
  stat_smooth(method="lm", se=FALSE)+
  theme(panel.background = element_rect(colour = 'black', fill='white'))+
  theme(axis.title.x=element_text(size=15, color = "black"))+
  theme(axis.title.y=element_text(size=15, color = "black"))+
  xlab("North Atlantic Oscillation Values")+
  ylab("Estimated number of Black Scoters")

sco2$fit<-fitted(m10g)
bath<-ggplot(sco2, aes(x=bathy, y=fit))
bath+geom_point()+
  stat_smooth(method="lm",formula=y~poly(x,2), se=FALSE)+
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

#Home range: kernel density (adehabitatHR) function getvolumeUD, h=LSCV
#
