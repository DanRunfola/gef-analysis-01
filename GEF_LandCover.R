library(rpart)
library(doBy)
library(Rcpp)
library(MatchIt)
library(rpart.plot)

source("causal_tree_functions.R")

#Causal Tree
source("data_prep_and_joins.R")
sourceCpp("split.cpp")
#From data load script: GEF.noUS
GEF.spdf <- GEF.noUS

#Join in project info for dates
GEF.project.dta <- read.table("/home/aiddata/Desktop/Github/GEF/Data/GlobalEnvironmentFacility_GeocodedResearchRelease_Level1_v1.0/data/projects.csv", 
                              sep=",", header=TRUE)
GEF.spdf.prjA <- merge(GEF.spdf, GEF.project.dta, by="project_id")

#Join in ancillary data for multi vs. single sector
GEF.ancil.dta <- read.table("/home/aiddata/Desktop/Github/GEF/Data/GlobalEnvironmentFacility_GeocodedResearchRelease_Level1_v1.0/data/projects_ancillary.csv", 
                            sep=",",header=TRUE)
GEF.spdf.prj <- merge(GEF.spdf.prjA, GEF.ancil.dta, by.x="project_id", by.y="AidData.Project.ID")

#Add random start to control cases
set.seed(424)
earliest = 2002
max_add <- 10
GEF.spdf.prj$year <- NA

GEF.spdf.prj@data$start_actual_isodate <- as.character(GEF.spdf.prj@data$start_actual_isodate)

GEF.spdf.prj@data[GEF.spdf.prj$treatment == 0,][,"start_actual_isodate"] <- 
  as.character(sample(earliest:(earliest+max_add),
                      length(GEF.spdf.prj[GEF.spdf.prj$treatment == 0,]),
                      replace=TRUE))

#Drop treatments that started after 2012 so a suitable post-trend can be built
GEF.spdf.prj <- GEF.spdf.prj[GEF.spdf.prj$start_actual_isodate != "2014-01-07",]
GEF.spdf.prj <- GEF.spdf.prj[GEF.spdf.prj$start_actual_isodate != "2014-01-01",]

#Create a year column to construct pre and post intervention data
GEF.spdf.prj$year <- substr(GEF.spdf.prj$start_actual_isodate,1,4)

#Setting unknown project dates to 2002, as a conservative option.
#Can remove this at the cost of a lower N.
GEF.spdf.prj@data[GEF.spdf.prj$year == "",]["year"] <- 2002

GEF.spdf.prj <- GEF.spdf.prj[!is.na(GEF.spdf.prj@data$year),]

#Calculate post-implementation-years
GEF.spdf.prj@data$post_implementation_time <- (2014-as.numeric(GEF.spdf.prj@data$year))


#Calculate pre- and post-average values for NDVI
timeRangeAvg <- function(dta,prefix,affix,startyr,endyr)
{

  searchS = paste("^",prefix,startyr,affix,sep="")
  searchE = paste("^",prefix,endyr,affix,sep="")
  
  
  strt_id <- grep(searchS,colnames(dta@data))
  end_id <- grep(searchE,colnames(dta@data))
  
  rmean <- rowMeans(dta@data[strt_id[[1]]:end_id[[length(end_id)]]], 
                    na.rm=TRUE)
  return(rmean)
}

timeRangeSum <- function(dta,prefix,affix,startyr,endyr)
{
  
  searchS = paste("^",prefix,startyr,affix,sep="")
  searchE = paste("^",prefix,endyr,affix,sep="")
  
  
  strt_id <- grep(searchS,colnames(dta@data))
  end_id <- grep(searchE,colnames(dta@data))
  
  rsum <- rowSums(dta@data[strt_id[[1]]:end_id[[length(end_id)]]], 
                    na.rm=TRUE)
  return(rsum)
}


GEF.spdf.prj$LTDR_outcome_mean <- NA
GEF.spdf.prj$LTDR_outcome_max <- NA
GEF.spdf.prj$pre_average_precip <- NA
GEF.spdf.prj$pre_max_precip <- NA
GEF.spdf.prj$pre_min_precip <- NA
GEF.spdf.prj$pre_average_temp <- NA
GEF.spdf.prj$pre_max_temp <- NA
GEF.spdf.prj$pre_min_temp <- NA

#Other pre-averages
GEF.spdf.prj$pre_average_NTL <- NA
GEF.spdf.prj$pre_average_LTDR <- NA
GEF.spdf.prj$pre_max_LTDR <- NA
GEF.spdf.prj$Hansen_loss <- NA

for(i in 1:length(GEF.spdf.prj))
{
  loc_year <- as.numeric(GEF.spdf.prj$year[[i]])
  GEF.spdf.prj$pre_average_precip[[i]] <- timeRangeAvg(GEF.spdf.prj[i,], "udel_precip_v4_01_yearly_mean.", ".mean", 1982, loc_year)
  GEF.spdf.prj$pre_max_precip[[i]] <- timeRangeAvg(GEF.spdf.prj[i,], "udel_precip_v4_01_yearly_max.", ".mean", 1982, loc_year)
  GEF.spdf.prj$pre_min_precip[[i]] <- timeRangeAvg(GEF.spdf.prj[i,], "udel_precip_v4_01_yearly_min.", ".mean", 1982, loc_year)
  GEF.spdf.prj$pre_average_temp[[i]] <- timeRangeAvg(GEF.spdf.prj[i,], "udel_air_temp_v4_01_yearly_mean.", ".mean", 1982, loc_year)
  GEF.spdf.prj$pre_max_temp[[i]] <- timeRangeAvg(GEF.spdf.prj[i,], "udel_air_temp_v4_01_yearly_max.", ".mean", 1982, loc_year)
  GEF.spdf.prj$pre_min_temp[[i]] <- timeRangeAvg(GEF.spdf.prj[i,], "udel_air_temp_v4_01_yearly_min.", ".mean", 1982, loc_year)
  
  #Other pre-averages
  GEF.spdf.prj$pre_average_NTL[[i]] <- timeRangeAvg(GEF.spdf.prj[i,], "v4composites_calibrated.", ".mean", 1992, loc_year)
  GEF.spdf.prj$pre_average_LTDR[[i]] <- timeRangeAvg(GEF.spdf.prj[i,], "ltdr_yearly_ndvi_mean.", ".mean", 1982, loc_year)
  GEF.spdf.prj$pre_max_LTDR[[i]] <- timeRangeAvg(GEF.spdf.prj[i,], "ltdr_yearly_ndvi_max.", ".mean", 1982, loc_year)
  
  GEF.spdf.prj$Hansen_loss[[i]] <- timeRangeSum(GEF.spdf.prj[i,], "lossyr25.na.categorical_", "", loc_year, 2014)
  
}

GEF.spdf.prj@data$MultiFocal <- NA
GEF.spdf.prj@data$MultiFocal[GEF.spdf.prj$Focal.Area.single.letter.code == "M"] <- 1
GEF.spdf.prj@data$MultiFocal[GEF.spdf.prj$Focal.Area.single.letter.code == "L"] <- 0

GEF.pred <- GEF.spdf.prj
GEF.pred$longitude <- as.vector(coordinates(GEF.pred)[,1])
GEF.pred$latitude <- as.vector(coordinates(GEF.pred)[,2])



#Calculate hansen loss outcome
GEF.spdf.prj$cover_outcome <- GEF.spdf.prj$Hansen_loss / GEF.spdf.prj$X00forest25.na.sum

#Drop cases for which no hansen data existed
GEF.spdf.prj <- GEF.spdf.prj[!is.na(GEF.spdf.prj$cover_outcome),]

GEF.spdf.prj@data$MultiFocal <- NA
GEF.spdf.prj@data$MultiFocal[GEF.spdf.prj$Focal.Area.single.letter.code == "M"] <- 1
GEF.spdf.prj@data$MultiFocal[GEF.spdf.prj$Focal.Area.single.letter.code == "L"] <- 0

#Choose pscore variables
pVars <- c("pre_average_NTL", 
           "pre_average_LTDR", "pre_max_LTDR", "pre_min_temp", "pre_max_temp",
           "pre_average_temp", "pre_max_precip", "pre_min_precip", 
           "pre_average_precip", "post_implementation_time", "year",
           "dist_to_all_rivers.na.mean", "dist_to_roads.na.mean",
           "srtm_elevation_500m.na.mean", "srtm_slope_500m.na.mean",
           "accessibility_map.na.mean", "gpw_v3_density.2000.mean",
           "wdpa_5km.na.sum", "treecover2000.na.mean", "treatment", "latitude",
           "longitude", "udel_precip_v4_01_yearly_max.2002.mean", 
           "udel_precip_v4_01_yearly_min.2002.mean", 
           "udel_precip_v4_01_yearly_mean.2002.mean",
           "udel_air_temp_v4_01_yearly_max.2002.mean",   
           "udel_air_temp_v4_01_yearly_min.2002.mean",   
           "udel_air_temp_v4_01_yearly_mean.2002.mean",
           "v4composites_calibrated.2002.mean",
           "ltdr_yearly_ndvi_mean.2002.mean")

#Choose analysis variables
aVars <- c("pre_average_NTL", 
           "pre_average_LTDR", "pre_max_LTDR", "pre_min_temp", "pre_max_temp",
           "pre_average_temp", "pre_max_precip", "pre_min_precip", 
           "pre_average_precip", "post_implementation_time", "year",
           "dist_to_all_rivers.na.mean", "dist_to_roads.na.mean",
           "srtm_elevation_500m.na.mean", "srtm_slope_500m.na.mean",
           "accessibility_map.na.mean", "gpw_v3_density.2000.mean",
           "wdpa_5km.na.sum", "treecover2000.na.mean", "treatment", "latitude",
           "longitude", "total_disbursements", "MultiFocal", "cover_outcome",
           "udel_precip_v4_01_yearly_max.2002.mean", 
           "udel_precip_v4_01_yearly_min.2002.mean", 
           "udel_precip_v4_01_yearly_mean.2002.mean",
           "udel_air_temp_v4_01_yearly_max.2002.mean",   
           "udel_air_temp_v4_01_yearly_min.2002.mean",   
           "udel_air_temp_v4_01_yearly_mean.2002.mean",
           "v4composites_calibrated.2002.mean",
           "ltdr_yearly_ndvi_mean.2002.mean")

analysis.dtaA <- GEF.spdf.prj[!is.na(GEF.spdf.prj@data$pre_max_precip),]
analysis.dtaB <- analysis.dtaA[!is.na(analysis.dtaA@data$gpw_v3_density.2000.mean),]

#Drop areas for which no weather station ever existed
analysis.dta <- analysis.dtaB[,aVars]

#Add latitude and longitude columns to control cases
analysis.dta$longitude <- as.vector(coordinates(analysis.dta)[,1])
analysis.dta$latitude <- as.vector(coordinates(analysis.dta)[,2])

#Randomize commitment dollar levels for control cases
#(Note: this is not used in the propensity score calculation)
analysis.dta@data[is.na(analysis.dta$total_disbursements),]["total_disbursements"] <- 
  sample(min(analysis.dta$total_disbursements, na.rm=TRUE):max(analysis.dta$total_disbursements, na.rm=TRUE),
         length(analysis.dta@data[is.na(analysis.dta$total_disbursements),][[1]]))

#Randomize focal area for control cases
#(Note: this is not used in the propenstiy score calculation)
analysis.dta@data$MultiFocal <- NA
analysis.dta@data$MultiFocal[analysis.dta$Focal.Area.single.letter.code == "M"] <- 1
analysis.dta@data$MultiFocal[analysis.dta$Focal.Area.single.letter.code == "L"] <- 0
analysis.dta@data[is.na(analysis.dta$MultiFocal),]["MultiFocal"] <- 
  sample(0:1,length(analysis.dta@data[is.na(analysis.dta$MultiFocal),][[1]]), replace=TRUE)


#Calculate Propensity Score
pscore.Calc <- matchit(treatment ~ pre_average_NTL + pre_max_LTDR +
                         pre_min_temp + pre_max_temp + pre_average_temp + pre_max_precip +
                         pre_min_precip + pre_average_precip +
                         year + dist_to_all_rivers.na.mean + dist_to_roads.na.mean +
                         srtm_elevation_500m.na.mean + srtm_slope_500m.na.mean +
                         accessibility_map.na.mean + gpw_v3_density.2000.mean +
                         wdpa_5km.na.sum + treecover2000.na.mean + latitude +
                         longitude,
                         data= analysis.dta[pVars]@data,
                       method="nearest", distance="logit")

matched.dta <- match.data(pscore.Calc)

matched.dta <- merge(matched.dta, analysis.dta[c("cover_outcome")], by="row.names")

#Remove 0 and 1 cases, if any
matched.dta <- matched.dta[as.numeric(matched.dta$distance) < 0.99,]
matched.dta <- matched.dta[as.numeric(matched.dta$distance) > 0.01,]

analysis.dtaC <- analysis.dta@data
analysis.dtaC$distance <- predict(pscore.Calc$model, analysis.dtaC, type="response")
analysis.dtaC <- analysis.dtaC[as.numeric(analysis.dtaC$distance) < 0.99,]
analysis.dtaC <- analysis.dtaC[as.numeric(analysis.dtaC$distance) > 0.01,]

transOutcome <- list(rep(0,nrow(matched.dta)))

for(i in 1:nrow(matched.dta))
{
  if(matched.dta$treatment[i] == 1)
  {
    #Treated
    transOutcome[i] = matched.dta$cover_outcome[i] * 
      (1 / matched.dta$distance[i])
  }
  else
  {
    #Untreated
    transOutcome[i] = -1 * (matched.dta$cover_outcome[i] * 
                              ((1-0) / (1 - matched.dta$distance[i])))
  }
}
matched.dta$transOutcome <- unlist(transOutcome)

transOutcome <- list(rep(0,nrow(analysis.dtaC)))

for(i in 1:nrow(analysis.dtaC))
{
  if(analysis.dtaC$treatment[i] == 1)
  {
    #Treated
    transOutcome[i] = analysis.dtaC$cover_outcome[i] * 
      (1 / analysis.dtaC$distance[i])
  }
  else
  {
    #Untreated
    transOutcome[i] = -1 * (analysis.dtaC$cover_outcome[i] * 
                              ((1-0) / (1 - analysis.dtaC$distance[i])))
  }
}
analysis.dtaC$transOutcome <- unlist(transOutcome)

#Add variables back in that did not have measurements
matched.dta <- merge(matched.dta, analysis.dta@data[c("total_disbursements")], by="row.names")
matched.dta <- merge(matched.dta, analysis.dta@data[c("MultiFocal")], by="row.names")



#------------------
#------------------
#CT
#------------------
#------------------



alist <- list(eval=ctev, split=ctsplit, init=ctinit)
dbb = analysis.dtaC
#dbb = matched.dta
k = 10
n = dim(dbb)[1]
crxvdata = dbb
crxvdata$id <- sample(1:k, nrow(crxvdata), replace = TRUE)
list = 1:k
m.split = round(length(dbb[[1]]) / 10,0)

errset = list()

  for (i in 1:k){
    errset[[i]] = list()
    trainingset <- subset(crxvdata, id %in% list[-i])
    sub.fit = rpart(cbind(cover_outcome,treatment,distance,transOutcome) ~
                      pre_max_LTDR + pre_min_temp + pre_max_temp + 
                      pre_average_temp + pre_max_precip +
                      pre_min_precip + pre_average_precip + post_implementation_time +
                      dist_to_all_rivers.na.mean + dist_to_roads.na.mean +
                      srtm_elevation_500m.na.mean + srtm_slope_500m.na.mean +
                      accessibility_map.na.mean + gpw_v3_density.2000.mean +
                      wdpa_5km.na.sum + treecover2000.na.mean + latitude +
                      longitude +MultiFocal,
                    trainingset,
                    control = rpart.control(cp = 0,minsplit = m.split),
                    method=alist)
    sub.fit.dm = data.matrix(sub.fit$frame)
    index = as.numeric(rownames(sub.fit$frame))
    removed_nodes = 0
    #fit1$frame$var = as.numeric(fit1$frame$var)
    removed_nodes = cross_validate(sub.fit.dm, index,removed_nodes)
    removed_nodes = removed_nodes[-1]
    #errset[i] = rep(0,length(removed_nodes))
    for(l in 1:length(removed_nodes)){
      error = 0
      sub.fit.pred = snip.rpart(sub.fit, removed_nodes[1:l])
      
      #Subset Fit
      testset <- subset(crxvdata, id %in% c(i))
      pt = predict(sub.fit.pred,testset,type = "matrix")
      y = data.frame(pt)
      val = data.matrix(y)
      idx = as.numeric(rownames(testset))
      dbidx = as.numeric(rownames(dbb))
      
      for(pid in 1:(dim(y)[1])){
         id = match(idx[pid],dbidx)
         error = error + (dbb$transOutcome[id] - val[pid])^2
      }
      
      if(error == 0){
        errset[[i]][l] = 1000000
      }
      else{
        errset[[i]][l] = error/k
      }
  }
  }

#Identify the average error to depth ratio across all cross-validations
avg.index <- vector()
for(e in 1:length(errset))
{
 avg.index[e] <- which.min(errset[[e]])
}

#---------------
#Build Final Tree
#---------------
fit1 = rpart(cbind(cover_outcome,treatment,distance,transOutcome) ~ 
               pre_max_LTDR + pre_min_temp + pre_max_temp +
               pre_average_temp + pre_max_precip +
               pre_min_precip + pre_average_precip + post_implementation_time +
               dist_to_all_rivers.na.mean + dist_to_roads.na.mean +
               srtm_elevation_500m.na.mean + srtm_slope_500m.na.mean +
               accessibility_map.na.mean + gpw_v3_density.2000.mean +
               wdpa_5km.na.sum + treecover2000.na.mean + latitude +
               longitude + MultiFocal,
             crxvdata,
             control = rpart.control(cp = 0,minsplit = m.split),
             method=alist)
fit = data.matrix(fit1$frame)
index = as.numeric(rownames(fit1$frame))


removed_nodes = 0
removed_nodes = cross_validate(fit, index,removed_nodes)
removed_nodes = removed_nodes[-1]
pruned_nodes = removed_nodes[1:round(mean(avg.index))]
final.tree <- snip.rpart(fit1, pruned_nodes)

#Prep for output
print.tree <- final.tree
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="accessibility_map.na.mean"] <- "Urb Dist"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="total_disbursements"] <- "Disbursements"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="pre_max_precip"] <- "Max Precip"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="pre_min_precip"] <- "Min Precip"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="treecover2000.na.mean"] <- "Treecover"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="srtm_elevation_500m.na.mean"] <- "Elevation"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="dist_to_roads.na.mean"] <- "Road Dist"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="wdpa_5km.na.sum"] <- "Protected"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="dist_to_all_rivers.na.mean"] <- "River Dist"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="pre_average_NTL"] <- "Nighttime Lights"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="pre_average_precip"] <- "Avg Precip"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="pre_max_LTDR"] <- "Max NDVI"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="pre_max_temp"] <- "Max Temp"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="pre_average_temp"] <- "Avg Temp"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="gpw_v3_density.2000.mean"] <- "Pop Density"
levels(print.tree $frame$var)[levels(print.tree $frame$var)=="post_implementation_time"] <- "Years Since Proj. Imp."

png("~/Desktop/Github/GEF/Summary/GEF_LandCover.png")
rpart.plot(print.tree , cex=0.3, extra=1, branch=1, type=4, tweak=1.4, clip.right.labs=FALSE,
           box.col=c("palegreen3", "pink")[findInterval(print.tree $frame$yval, v = c(-1,0))],
           faclen=0,
           varlen=0
           )
dev.off()


GEF.pred <- GEF.pred[aVars]@data
GEF.pred <- GEF.pred[GEF.pred$treatment==1,]

GEF.pred$pred_trt <- predict(final.tree, GEF.pred)

lonlat <- GEF.pred[,c("longitude", "latitude")]

trt.dta.out <- SpatialPointsDataFrame(coords = lonlat, data = GEF.pred,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

writePointsShape(trt.dta.out, "/home/aiddata/Desktop/Github/GEF/Summary/GEF_Cover.shp")
write.csv(GEF.pred, "/home/aiddata/Desktop/Github/GEF/Summary/GEF_Cover.csv")