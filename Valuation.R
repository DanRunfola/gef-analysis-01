#valuation
source("CarbonSequestration.R")
GEF.LandCover.Result <- read.table("/home/aiddata/Desktop/Github/GEF/Summary/GEF_LandCover.csv", 
                            sep=",", header=TRUE)

GEF.Frag.Result <- read.table("/home/aiddata/Desktop/Github/GEF/Summary/GEF_Frag.csv", 
                                   sep=",", header=TRUE)

GEF.NDVI.Result <- read.table("/home/aiddata/Desktop/Github/GEF/Summary/GEF_NDVI.csv", 
                                   sep=",", header=TRUE)

#Calculate summary stats for each set of impacts

summary(GEF.LandCover.Result$cover_outcome, omit.na=TRUE)

summary(GEF.NDVI.Result$LTDR_outcome_max/10000)


summary(GEF.Frag.Result$mean_patch_size2014)



#Join in project info for dates
GEF.project.dta <- read.table("/home/aiddata/Desktop/Github/GEF/Data/GlobalEnvironmentFacility_GeocodedResearchRelease_Level1_v1.0/data/projects.csv", 
                              sep=",", header=TRUE)
GEF.spdf.prjA <- merge(GEF.noUS.seq, GEF.project.dta, by="project_id")

GEF.noUS.seqA <- GEF.spdf.prjA[GEF.spdf.prjA$start_actual_isodate != "2014-01-07",]
GEF.noUS.seq <- GEF.noUS.seqA[GEF.noUS.seqA$start_actual_isodate != "2014-01-01",]

GEF.noUS.seq$LandCover_Impact <- GEF.LandCover.Result["pred_trt"]
GEF.noUS.seq$Frag_Impact <- GEF.Frag.Result["pred_trt"]
GEF.noUS.seq$NDVI_Impact <- GEF.NDVI.Result["pred_trt"]


predDF <- GEF.noUS.seq
 
predDF$lossyr25.na.categorical_2010 <- predDF$LandCover_Impact[[1]]
predDF$mean_patch_size2010 <- predDF$Frag_Impact[[1]]
predDF$ltdr_yearly_ndvi_max.2010.mean  <- predDF$NDVI_Impact[[1]]

predDF$LandCover_Impact <- predDF$LandCover_Impact[[1]]
predDF$Frag_Impact <- predDF$Frag_Impact[[1]]
predDF$NDVI_Impact <- predDF$NDVI_Impact[[1]]


#Set GEZ terms to nearest analogues for those lacking - temporary
levels(predDF$GEZ_TERM)[levels(predDF$GEZ_TERM)=="Subtropical dry forest"] <- "Tropical dry forest"
levels(predDF$GEZ_TERM)[levels(predDF$GEZ_TERM)=="Temperate continental forest"] <- "Tropical dry forest"
levels(predDF$GEZ_TERM)[levels(predDF$GEZ_TERM)=="Temperate oceanic forest"] <- "Tropical dry forest"

predDF$carbonTonnes <- predict(CarbonModel, newdata=predDF)

#Multiply by 2500 (Saachi is tonnes / hectacre; we have 25km areas)
predDF$carbonTonnes <- predDF$carbonTonnes * 2500

#Estimate tonnes
summary(predDF$carbonTonnes)

#Estimate Value
summary(predDF$carbonTonnes) * 12.9

library(doBy)

summary(summaryBy(carbonTonnes ~ project_id , data = predDF, FUN=sum, na.rm=TRUE)["carbonTonnes.sum"])

summary(summaryBy(carbonTonnes ~ project_id , data = predDF, FUN=sum, na.rm=TRUE)["carbonTonnes.sum"]*12.9)

write.csv(predDF, "/home/aiddata/Desktop/Github/GEF/Summary/GEF_Valuations_Prediction.csv")

#Valuation SPDF
lonlat <- predDF[,c("LON", "LAT")]
Val.spdf <- SpatialPointsDataFrame(coords = lonlat, data = predDF,
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))


png("~/Desktop/Github/GEF/Summary/Valuations.png",
    width = 4.5, 
    height = 2, 
    units = 'in', 
    res = 300)
spplot(Val.spdf, colorkey=TRUE, 
       z="carbonTonnes",
       xlim=c(-180,180),
       ylim=c(-60,90),
       col="transparent",
       cex=0.25,
       #cuts=c(-10000000000000000,0,1000000000000000000),
       #col.regions=c("blue", "red", "green"),
       #key=list(lines=TRUE, col="transparent"),
       main="GEF Project Location Valuations",
       sp.layout = list(list(land.mask, fill="grey", first=TRUE)))
dev.off()
 