library(sp)

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


lonlat <- GEF[,c("longitude", "latitude")]
GEF.full.spdf <- SpatialPointsDataFrame(coords = lonlat, data = GEF,
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
land.mask <- readOGR("Data/countries.geojson", "OGRGeoJSON")




png("~/Desktop/Github/GEF/Report_Visualizations/allGEFProjectLocations.png",
    width = 6, 
    height = 4, 
    units = 'in', 
    res = 300)
spplot(GEF.full.spdf, zcol="precision_code", colorkey=FALSE, cex=0.25,
       col.regions=c(colors()[c(81)]),
       xlim=c(-180,180),
       ylim=c(-60,90),
       key=list(lines=TRUE, col="transparent"),
       pch=5,
       main=list(label="Location of GEF Land Degradation Projects",cex=0.5),
       sp.layout = list(list(land.mask, fill="grey", first=TRUE)))
dev.off()