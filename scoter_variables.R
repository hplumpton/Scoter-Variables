library(rgdal)
library(maps)
library(raster)
library(rgeos)
library(maptools)

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
plot(substrate)
summary(substrate)

scoters=data.frame(read.csv("ObsData2.csv",header=TRUE))
scoters <-na.omit(scoters)
summary(scoters)

coordinates(scoters)<-c("longitude_dd","latitude_dd") 
#define x&y coordinates
proj4string(scoters)<-CRS("+proj=longlat +datum=WGS84") 
#assigning a projection
plot(scoters,add=TRUE)

sco2=spTransform(scoters,CRS(proj4string(substrate))) 
#assign same projection as substrate

sco2$substrate=over(sco2,substrate)
#extract substrate measure at each spatial location 
#combined with scoters data

#gives error in colMeans(x, na.rm=TRUE) : 'x' must be numeric
sco2$substrate2=scale(sco2$substrate)
#standardize covariates for comparison of beta estimates later on

head(sco2)




#sediment mobility

sedmobility=readShapePoly("Layers/sediment mobility/SAB_median/SAB_median.shp")
proj4string(sedmobility)<-CRS("+proj=longlat +datum=WGS84")
plot(sedmobility)
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
#assigned same projection as sedmobility
sco2=spTransform(scoters,CRS(proj4string(sedmobility))) 
#assign same projection as sedmobility
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

seshoreline <- crop(shoreline, extent(-82, -72, 30, 39))
proj4string(seshoreline)<-CRS("+proj=longlat +datum=WGS84")
summary(seshoreline)
plot(seshoreline)

scoters=read.csv("ObsData2.csv",header=TRUE)
scoters <-na.omit(scoters)
summary(scoters)

coordinates(scoters)<-c("longitude_dd","latitude_dd") 
#define x&y coordinates
proj4string(scoters)<-CRS("+proj=longlat +datum=WGS84") 
#assigning a projection
plot(scoters,add=TRUE)

dist<-distanceFromPoints(seshoreline,scoters)
proj4string(dist)<-CRS("+proj=longlat +datum=WGS84")
head(dist)

sco2=spTransform(scoters,CRS(proj4string(dist))) 
#assign same projection as dist

sco2$dist=extract(dist,sco2)
#extract sediment mobility measure at each spatial location 
#combined with scoters data
sco2$dist2=scale(sco2$dist)
#standardize covariates for comparison of beta estimates later on





#transect data

transect=readShapeLines("Layers/transects/WinterSurvey_TrackLines_sCoast.shp")
proj4string(transect)<-CRS("+proj=longlat +datum=WGS84")
plot(transect)
map("state", add=TRUE)

#dividing transects into grids
#segmenting transects

library(DSpat)
library(GISTools)
library(spatstat)

tran2=spTransform(transect,
                  CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0"))
tran.sub=tran2[!duplicated(tran2$Transect),] #subset to remove duplicates
tran.sub$id=seq(1,544) #add a unique ID to each transect

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
plot(sp.seg, col = rep(c(1, 2), length.out = length(sp.seg)), axes = T)





#gridding transects

x0=coordinates(sp.seg)[[1]][[1]][1,1]
x1=coordinates(sp.seg)[[1]][[1]][1,1]
y0=coordinates(sp.seg)[[1]][[1]][1,2]
y1=coordinates(sp.seg)[[1]][[1]][1,2]

lines=data.frame(label=1,x0=x0,x1=x1,y0=y0,y1=y1)


tran.locs=coordinates(sp.seg)[[1]][[1]]
tran2=coordinates(sp.seg)[[2]][[1]]
sp.seg$id=seq(1,18453) #add a unique ID to each transect

sl <- SpatialLines(list(Lines(list(Line(coords = tran.locs)), 
                              ID = "1"), Lines(list(Line(coords = tran2)),ID = "2")))


sp.seg=SpatialLines(new.lines,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0"))

names(sp.seg)
summary(sp.seg)

temp=as.psp(sp.seg[sp.seg$id==1,])

grid<-lines_to_strips(lines,as.owin(temp), width=250)
plot(grid, col = rep(c(1, 2), length.out = length(sp.seg)), axes = T)



#North Atlantic Oscillation
#Fine Scale Weather
#Bivalve Distribution

