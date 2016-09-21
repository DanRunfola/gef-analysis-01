library(sp)
library(spatstat)
library(maptools)
library(rgeos)
library(rgdal)

max.buffer <- 500000
buffer.hole <- 50000
control.buffer <- 25000

GEF <- read.table("/home/aiddata/Desktop/Github/GEF/Data/GlobalEnvironmentFacility_GeocodedResearchRelease_Level1_v1.0/data/locations.csv", 
                  sep=",", header=TRUE)

#Remove DC and Europe locations
#European Locations - in lat/long cases, it has been confirmed they're the only instance.
GEF <- GEF[!GEF$longitude == 9.14062,]
GEF <- GEF[!GEF$gazetteer_adm_name == "Earth|Europe|Ireland",]
GEF <- GEF[!GEF$gazetteer_adm_name == "Earth|Europe|Iceland",]
GEF <- GEF[!GEF$gazetteer_adm_name == "Earth|Europe|Andorra",]

#DC 
GEF <- GEF[!GEF$gazetteer_adm_name == "Earth|North America|United States|Washington, D.C.|Washington",]

#Remove low precision cases
GEF <- GEF[GEF$location_type_code != "ADM1",]
GEF <- GEF[GEF$location_type_code != "ADM1H",]
GEF <- GEF[GEF$location_type_code != "PCLI",]
GEF <- GEF[GEF$location_type_code != "ADM2",]
GEF <- GEF[GEF$location_type_code != "ADM2H",]


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

df2<- data.frame(id = getSpPPolygonsIDSlots(GEF.SampleZone))
row.names(df2) <- getSpPPolygonsIDSlots(GEF.SampleZone)
GEF.SampleZone.B <- SpatialPolygonsDataFrame(GEF.SampleZone, data =df2)
proj4string(GEF.SampleZone.B) <- CRS("+proj=cea")
GEF.SampleZone.B <- spTransform(GEF.SampleZone.B, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))





land.mask <- readOGR("Data/countries.geojson", "OGRGeoJSON")

png("~/Desktop/Github/GEF/Report_Visualizations/gefControlLocations.png",
    width = 6, 
    height = 4, 
    units = 'in', 
    res = 300)
spplot(GEF.SampleZone.B, colorkey=FALSE, cex=0.25,
       col.regions=c("red"),
       xlim=c(-180,180),
       ylim=c(-60,90),
       key=list(lines=TRUE, col="transparent"),
       main=list(label="Locations Assessed for Control Comparison Cases",cex=0.5),
       sp.layout = list(list(land.mask, fill="grey", first=TRUE)))
dev.off()