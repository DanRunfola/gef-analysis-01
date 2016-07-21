if(createCases==TRUE)
{
  
library(sp)
library(spatstat)
library(maptools)
library(rgeos)
library(rgdal)

max.buffer <- 500000
buffer.hole <- 50000
control.buffer <- 25000
set.seed(424)

GEF <- read.table("/home/aiddata/Desktop/Github/GEF/Data/GlobalEnvironmentFacility_GeocodedResearchRelease_Level1_v1.0/data/locations.csv", 
                  sep=",", header=TRUE)

#Create a quick/dirty dataframe to store statistics in to print out details later.
#To any students reading this code - never build dataframes like this.  It's wrong.
results.save <- GEF[1:2]
results.save <- results.save[1,]

names(results.save)[1] <- "InitialTotalProj"
results.save$InitialTotalProj <- length(GEF[[1]])

lonlat <- GEF[,c("longitude", "latitude")]
GEF.full.spdf <- SpatialPointsDataFrame(coords = lonlat, data = GEF,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

#Drop out country and coarse level ADM data
GEF <- GEF[GEF$location_type_code != "ADM1",]
GEF <- GEF[GEF$location_type_code != "ADM1H",]
GEF <- GEF[GEF$location_type_code != "PCLI",]
GEF <- GEF[GEF$location_type_code != "ADM2",]
GEF <- GEF[GEF$location_type_code != "ADM2H",]

names(results.save)[2] <- "HighSpatialPrecisionProj"
results.save$HighSpatialPrecisionProj <- length(GEF[[1]])

lonlat <- GEF[,c("longitude", "latitude")]

GEF.spdf <- SpatialPointsDataFrame(coords = lonlat, data = GEF,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))




#Create a buffered polygon from each GEF area extending to
#the outer buffer (max)
#Then exclude all areas that fall within 20km of a GEF project
#Lambert Cylindrical Equal Area Transform
GEF.spdf.trans <- spTransform(GEF.spdf, CRS("+proj=cea"))

#Apply outer buffer
buffers<-list()
for(i in 1:nrow(GEF.spdf.trans)) {
  GEF.buffer <-disc(radius=max.buffer, centre=c(GEF.spdf.trans@coords[,1][i],GEF.spdf.trans@coords[,2][i]))
  discpoly<-Polygon(rbind(cbind(GEF.buffer$bdry[[1]]$x,
                                y=GEF.buffer$bdry[[1]]$y), c(GEF.buffer$bdry[[1]]$x[1],
                                                           y=GEF.buffer$bdry[[1]]$y[1])))
  buffers<-c(buffers, discpoly)
}

spolys<-list()
for(i in 1:length(buffers)) {
  spolybuff<-Polygons(list(buffers[[i]]), ID=row.names(GEF.spdf.trans)[i])
  spolys<-c(spolys, spolybuff)
}

GEF.buffers.spdf <-SpatialPolygons(spolys)

#Create the treated unit polygons
buffers<-list()
for(i in 1:nrow(GEF.spdf.trans)) {
  GEF.buffer <-disc(radius=control.buffer, centre=c(GEF.spdf.trans@coords[,1][i],GEF.spdf.trans@coords[,2][i]))
  discpoly<-Polygon(rbind(cbind(GEF.buffer$bdry[[1]]$x,
                                y=GEF.buffer$bdry[[1]]$y), c(GEF.buffer$bdry[[1]]$x[1],
                                                             y=GEF.buffer$bdry[[1]]$y[1])))
  buffers<-c(buffers, discpoly)
}

spolys<-list()
for(i in 1:length(buffers)) {
  spolybuff<-Polygons(list(buffers[[i]]), ID=row.names(GEF.spdf.trans)[i])
  spolys<-c(spolys, spolybuff)
}

TREAT.buffers.spdf <-SpatialPolygons(spolys)



#Apply buffer hole
buffers<-list()
for(i in 1:nrow(GEF.spdf.trans)) {
  GEF.buffer <-disc(radius=buffer.hole, centre=c(GEF.spdf.trans@coords[,1][i],GEF.spdf.trans@coords[,2][i]))
  discpoly<-Polygon(rbind(cbind(GEF.buffer$bdry[[1]]$x,
                                y=GEF.buffer$bdry[[1]]$y), c(GEF.buffer$bdry[[1]]$x[1],
                                                             y=GEF.buffer$bdry[[1]]$y[1])))
  buffers<-c(buffers, discpoly)
}

spolys<-list()
for(i in 1:length(buffers)) {
  spolybuff<-Polygons(list(buffers[[i]]), ID=row.names(GEF.spdf.trans)[i])
  spolys<-c(spolys, spolybuff)
}

GEF.buffers.exclusion.spdf <-SpatialPolygons(spolys)

#Remove the exclusion zones from the outer buffers
GEF.SampleZone.A = gDifference(GEF.buffers.spdf, GEF.buffers.exclusion.spdf)
proj4string(GEF.SampleZone.A) <- CRS("+proj=cea")

#Remove areas which do not fall within a continent (water)
land.mask <- readOGR("Data/countries.geojson", "OGRGeoJSON")

land.mask.proj <- spTransform(land.mask, CRS("+proj=cea"))

GEF.SampleZone = gIntersection(GEF.SampleZone.A, land.mask.proj)




#Subsample within new buffers
GEF.controls <- spsample(GEF.SampleZone, n = 3000, "stratified")
proj4string(GEF.controls) <- CRS("+proj=cea")

#Buffer Control Cases
buffers<-list()
for(i in 1:length(GEF.controls)) {
  GEF.buffer <-disc(radius=control.buffer, centre=c(GEF.controls@coords[,1][i],GEF.controls@coords[,2][i]))
  discpoly<-Polygon(rbind(cbind(GEF.buffer$bdry[[1]]$x,
                                y=GEF.buffer$bdry[[1]]$y), c(GEF.buffer$bdry[[1]]$x[1],
                                                             y=GEF.buffer$bdry[[1]]$y[1])))
  buffers<-c(buffers, discpoly)
}

spolys<-list()
for(i in 1:length(buffers)) {
  spolybuff<-Polygons(list(buffers[[i]]), ID=row.names(GEF.controls)[i])
  spolys<-c(spolys, spolybuff)
}

GEF.controls <-SpatialPolygons(spolys)
proj4string(GEF.controls) <- CRS("+proj=cea")

GEF.controls.proj <- spTransform(GEF.controls, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

proj4string(TREAT.buffers.spdf) <- CRS("+proj=cea")
TREAT.buffers.proj <- spTransform(TREAT.buffers.spdf, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

# Make spatial polygon data frame for export
df<- data.frame(id = getSpPPolygonsIDSlots(GEF.controls.proj))
row.names(df) <- getSpPPolygonsIDSlots(GEF.controls.proj)
GEF.out <- SpatialPolygonsDataFrame(GEF.controls.proj, data =df)

proj4string(GEF.out) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")

df2<- data.frame(id = getSpPPolygonsIDSlots(TREAT.buffers.proj))
row.names(df2) <- getSpPPolygonsIDSlots(TREAT.buffers.proj)
TREAT.out <- SpatialPolygonsDataFrame(TREAT.buffers.proj, data =df2)

proj4string(TREAT.out) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")

#Save outputs
writePolyShape(GEF.out, "~/Desktop/Github/GEF/TreatControl/GEF_controls.shp")

writePolyShape(TREAT.out, "~/Desktop/Github/GEF/TreatControl/GEF_treatments.shp")

write.csv(coordinates(GEF.out), "~/Desktop/Github/GEF/TreatControl/GEF_control_centroids.csv")

write.csv(coordinates(TREAT.out), "~/Desktop/Github/GEF/TreatControl/GEF_treatment_centroids.csv")

write.csv(results.save,"~/Desktop/Github/GEF/Summary/control_production_summary.csv" )
#Create map of projects
#All: GEF.full.spdf
png("~/Desktop/Github/GEF/Summary/all_projects.png",
    width = 4.5, 
    height = 2, 
    units = 'in', 
    res = 300)
spplot(GEF.full.spdf, zcol="precision_code", colorkey=FALSE, cex=0.25,
       xlim=c(-180,180),
       ylim=c(-60,90),
       legendEntries = c("1-2", "3", "4-5", "6", "7-8"),
       key.space="right",
       main="Spatial Precision of Geocoded GEF Projects",
       sp.layout = list(list(land.mask, fill="grey", first=TRUE)))
dev.off()

#Analysis: GEF.spdf
png("~/Desktop/Github/GEF/Summary/analysis_projects.png",
    width = 4.5, 
    height = 2, 
    units = 'in', 
    res = 300)
spplot(GEF.spdf, zcol="precision_code", colorkey=FALSE, cex=0.25,
       col.regions=c("red"),
       xlim=c(-180,180),
       ylim=c(-60,90),
       key=list(lines=TRUE, col="transparent"),
       main="Location of Analyzed Projects",
       sp.layout = list(list(land.mask, fill="grey", first=TRUE)))
dev.off()

#Treat buffers: TREAT.out
png("~/Desktop/Github/GEF/Summary/treat_buffers.png",
    width = 4.5, 
    height = 2, 
    units = 'in', 
    res = 300)
spplot(TREAT.out, colorkey=FALSE, 
       col.regions=c("red"),
       xlim=c(-180,180),
       ylim=c(-60,90),
       key=list(lines=TRUE, col="transparent"),
       main="Buffers for Analyzed Projects",
       sp.layout = list(list(land.mask, fill="grey", first=TRUE)))
dev.off()

#Sample zone: GEF.SampleZone
df2<- data.frame(id = getSpPPolygonsIDSlots(GEF.SampleZone))
row.names(df2) <- getSpPPolygonsIDSlots(GEF.SampleZone)
GEF.SampleZone.B <- SpatialPolygonsDataFrame(GEF.SampleZone, data =df2)
proj4string(GEF.SampleZone.B) <- CRS("+proj=cea")
GEF.SampleZone.B <- spTransform(GEF.SampleZone.B, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

png("~/Desktop/Github/GEF/Summary/control_sample_zone.png",
    width = 4.5, 
    height = 2, 
    units = 'in', 
    res = 300)
spplot(GEF.SampleZone.B, colorkey=FALSE, 
       col.regions=c("red"),
       xlim=c(-180,180),
       ylim=c(-60,90),
       key=list(lines=TRUE, col="transparent"),
       main="Control Sampling Zone",
       sp.layout = list(list(land.mask, fill="grey", first=TRUE)))
dev.off()

#Control locations: GEF.controls
png("~/Desktop/Github/GEF/Summary/control_locations.png",
    width = 4.5, 
    height = 2, 
    units = 'in', 
    res = 300)
spplot(GEF.out, zcol="id", colorkey=FALSE, cex=0.25,
       col.regions=c("red"),
       xlim=c(-180,180),
       ylim=c(-60,90),
       key=list(lines=TRUE, col="transparent"),
       main="Location of Eligible Controls",
       sp.layout = list(list(land.mask, fill="grey", first=TRUE)))
dev.off()

}
