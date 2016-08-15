library(rpart)
library(sp)
library(maptools)
library(rgdal)

#Load Data
GEF.control.dta <- read.table("/home/aiddata/Desktop/Github/GEF/Data/Extractions/merge_gef_control.csv", 
                  sep=",", header=TRUE)
GEF.treat.dta <- read.table("/home/aiddata/Desktop/Github/GEF/Data/Extractions/merge_gef_treatment.csv", 
                            sep=",", header=TRUE)


#Load in information on Fragmentation
frag.stats.csv <-  read.table("/home/aiddata/Desktop/Github/GEF/Data/Fragmentation/Hansen_mean_patch_size.csv", 
                              sep=",", header=TRUE)

#Rename fragstat headers
names(frag.stats.csv)[5:19] <- sub("X", "mean_patch_size", names(frag.stats.csv[5:19]))

#Split frag stats by T and C
frag.stats.T <- frag.stats.csv[grepl("T", frag.stats.csv[,1]),]
frag.stats.C <- frag.stats.csv[grepl("C", frag.stats.csv[,1]),]

#Merge the fragmentation data into the relevant CSVs
GEF.control.dta <- merge(GEF.control.dta, frag.stats.C, by.x="id", by.y="Id")

GEF.treat.dta <- merge(GEF.treat.dta, frag.stats.T, by.x="id", by.y="Id")

GEF.control.shp <- readShapePoly("/home/aiddata/Desktop/Github/GEF/TreatControl/GEF_controls.shp")
proj4string(GEF.control.shp) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
GEF.treat.shp <- readShapePoly("/home/aiddata/Desktop/Github/GEF/TreatControl/GEF_treatments.shp")
proj4string(GEF.treat.shp) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")

land.mask <- readOGR("Data/countries.geojson", "OGRGeoJSON")
land.mask <- spTransform(land.mask, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))


GEF.control <- merge(GEF.control.shp, GEF.control.dta, by="id")
GEF.treat <- merge(GEF.treat.shp, GEF.treat.dta, by="id")

#Prepare treatment and control cases
GEF.control@data$treatment <- 0
GEF.treat@data$treatment <- 1





#---------------------------------------------------
#---------------------------------------------------
#Associate treated locations with relevant metadata
treat.meta.loc <- read.table("/home/aiddata/Desktop/Github/GEF/Data/GlobalEnvironmentFacility_GeocodedResearchRelease_Level1_v1.0/data/locations.csv", 
                  sep=",", header=TRUE)
#Drop out country and coarse level ADM data
treat.meta.loc <- treat.meta.loc[treat.meta.loc$location_type_code != "ADM1",]
treat.meta.loc <- treat.meta.loc[treat.meta.loc$location_type_code != "ADM1H",]
treat.meta.loc <- treat.meta.loc[treat.meta.loc$location_type_code != "PCLI",]
treat.meta.loc <- treat.meta.loc[treat.meta.loc$location_type_code != "ADM2",]
treat.meta.loc <- treat.meta.loc[treat.meta.loc$location_type_code != "ADM2H",]

lonlat <- treat.meta.loc[,c("longitude", "latitude")]

treat.meta.loc.spdf <- SpatialPointsDataFrame(coords = lonlat, data = treat.meta.loc,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))


treat.meta.loc.spdf$joincodes <- NA
GEF.treat$joincodes <- NA
for(i in 1:length(GEF.treat))
  {
  GEF.treat@data$joincodes[[i]] <- i
  it_dists <- spDistsN1(treat.meta.loc.spdf[is.na(treat.meta.loc.spdf$joincodes),], coordinates(GEF.treat)[i,])
  matchID <- which(min(it_dists) == it_dists)[1]
  treat.meta.loc.spdf@data[is.na(treat.meta.loc.spdf$joincodes),][matchID,]["joincodes"] <- i
  }


#Merge in the data with the new joincodes.
GEF.treat <- merge(GEF.treat, treat.meta.loc.spdf@data, by="joincodes")



#Add empty columns to the GEF control set (i.e., for monetary)
for(j in 1:length(names(GEF.treat)))
{
if(!(names(GEF.treat)[j] %in% names(GEF.control)))
{
GEF.control@data[names(GEF.treat)[j]] <- NA 
}
}


new_treat_IDs <- as.numeric(row.names(GEF.treat)) + (1+max(as.numeric(row.names(GEF.control))))
GEF.treat.ids <- spChFIDs(GEF.treat, as.character(new_treat_IDs))

GEF.control.ids <- spChFIDs(GEF.control, as.character(row.names(GEF.control)))




#Create single spatial dataframe with both treatment and control
GEF <- spRbind(GEF.control.ids, GEF.treat.ids)
proj4string(GEF) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
#Drop out project locations due to the DC geocoded location
USA.mask <- land.mask[land.mask$sovereignt == "United States of America",]

GEF.noUS <- GEF[(is.na(over(GEF, USA.mask))[,1]),]

png("~/Desktop/Github/GEF/Summary/treatment_control_comparison.png",
    width = 4.5, 
    height = 2, 
    units = 'in', 
    res = 300)
spplot(GEF.noUS, colorkey=FALSE, 
       z="treatment",
       xlim=c(-180,180),
       ylim=c(-60,90),
       col="transparent",
       col.regions=c("blue", "red"),
       key=list(lines=TRUE, col="transparent"),
       main="GEF Projects (Red)\nand Control Candidates (Blue)",
       sp.layout = list(list(land.mask, fill="grey", first=TRUE)))
dev.off()