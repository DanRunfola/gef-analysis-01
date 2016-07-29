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
  
  GEF.spdf.prj$LTDR_outcome_mean[[i]] <- timeRangeAvg(GEF.spdf.prj[i,], "ltdr_yearly_ndvi_mean.", ".mean", loc_year, 2014)
  GEF.spdf.prj$LTDR_outcome_max[[i]] <- timeRangeAvg(GEF.spdf.prj[i,], "ltdr_yearly_ndvi_max.", ".mean", loc_year, 2014)
  
}

#Choose pscore variables
pVars <- c("LTDR_outcome_mean", "LTDR_outcome_max", "pre_average_NTL", 
           "pre_average_LTDR", "pre_max_LTDR", "pre_min_temp", "pre_max_temp",
           "pre_average_temp", "pre_max_precip", "pre_min_precip", 
           "pre_average_precip", "post_implementation_time", "year",
           "dist_to_all_rivers.na.mean", "dist_to_roads.na.mean",
           "srtm_elevation_500m.na.mean", "srtm_slope_500m.na.mean",
           "accessibility_map.na.mean", "gpw_v3_density.2000.mean",
           "wdpa_5km.na.sum", "treecover2000.na.mean", "treatment", "latitude",
           "longitude")

#Choose analysis variables
aVars <- c("LTDR_outcome_mean", "LTDR_outcome_max", "pre_average_NTL", 
           "pre_average_LTDR", "pre_max_LTDR", "pre_min_temp", "pre_max_temp",
           "pre_average_temp", "pre_max_precip", "pre_min_precip", 
           "pre_average_precip", "post_implementation_time", "year",
           "dist_to_all_rivers.na.mean", "dist_to_roads.na.mean",
           "srtm_elevation_500m.na.mean", "srtm_slope_500m.na.mean",
           "accessibility_map.na.mean", "gpw_v3_density.2000.mean",
           "wdpa_5km.na.sum", "treecover2000.na.mean", "treatment", "latitude",
           "longitude", "total_disbursements", "Focal.Area.single.letter.code")

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
pscore.Calc <- matchit(treatment ~ pre_average_NTL + pre_average_LTDR + pre_max_LTDR +
                         pre_min_temp + pre_max_temp + pre_average_temp + pre_max_precip +
                         pre_min_precip + pre_average_precip + post_implementation_time +
                         year + dist_to_all_rivers.na.mean + dist_to_roads.na.mean +
                         srtm_elevation_500m.na.mean + srtm_slope_500m.na.mean +
                         accessibility_map.na.mean + gpw_v3_density.2000.mean +
                         wdpa_5km.na.sum + treecover2000.na.mean + latitude +
                         longitude,
                         data= analysis.dta[pVars]@data,
                       method="nearest", distance="logit")

matched.dta <- match.data(pscore.Calc)

#Remove 0 and 1 cases, if any
matched.dta <- matched.dta[(matched.dta@data$m1.pscore != 0 &
                              matched.dta@data$m1.pscore != 1),]

transOutcome <- list(rep(0,nrow(matched.dta)))

for(i in 1:nrow(matched.dta))
{
  if(matched.dta$treatment[i] == 1)
  {
    #Treated
    transOutcome[i] = matched.dta$LTDR_outcome_mean[i] * 
      (1 / matched.dta$distance[i])
  }
  else
  {
    #Untreated
    transOutcome[i] = -1 * (matched.dta$LTDR_outcome_mean[i] * 
                              ((1-0) / (1 - matched.dta$distance[i])))
  }
}
matched.dta$transOutcome <- unlist(transOutcome)

#Add variables back in that did not have measurements
matched.dta <- merge(matched.dta, analysis.dta@data[c("total_disbursements")], by="row.names")
matched.dta <- merge(matched.dta, analysis.dta@data[c("Focal.Area.single.letter.code")], by="row.names")

#------------------
#------------------
#CT
#------------------
#------------------

alist <- list(eval=ctev, split=ctsplit, init=ctinit)
dbb = matched.dta
k = 10
n = dim(dbb)[1]
crxvdata = dbb
crxvdata$id <- sample(1:k, nrow(crxvdata), replace = TRUE)
list = 1:k
fit1 = rpart(cbind(LTDR_outcome_mean,treatment,distance,transOutcome) ~ pre_average_NTL + 
               pre_average_LTDR + pre_max_LTDR + pre_min_temp + pre_max_temp + 
               pre_average_temp + pre_max_precip +
               pre_min_precip + pre_average_precip + post_implementation_time +
               dist_to_all_rivers.na.mean + dist_to_roads.na.mean +
               srtm_elevation_500m.na.mean + srtm_slope_500m.na.mean +
               accessibility_map.na.mean + gpw_v3_density.2000.mean +
               wdpa_5km.na.sum + treecover2000.na.mean + latitude +
               longitude +total_disbursements +Focal.Area.single.letter.code,
             crxvdata,
             control = rpart.control(cp = 0,minsplit = 10),
             method=alist)
fit = data.matrix(fit1$frame)
index = as.numeric(rownames(fit1$frame))
tsize = dim(fit1$frame[which(fit1$frame$var=="<leaf>"),])[1]

alpha = 0
alphalist = 0
alphalist = cross_validate(fit, index,alphalist)
res = rep(0,length(alphalist)-1)
if(length(alphalist) <= 2){
  res = alphalist
}else{
  for(j in 2:(length(alphalist)-1)){
    res[j] = sqrt(alphalist[j]*alphalist[j+1])
  }
}

alphacandidate = res
alphaset = rep(0,length(alphacandidate))
errset = rep(0,length(alphacandidate))
tsize = 0

for(l in 1:length(alphacandidate)){
  alpha = alphacandidate[l]
  error = 0
  treesize = 0
  for (i in 1:k){
    trainingset <- subset(crxvdata, id %in% list[-i])
    testset <- subset(crxvdata, id %in% c(i))
    fit1 = rpart (cbind(LTDR_outcome_mean,treatment,distance,transOutcome)  ~ pre_average_NTL + 
                    pre_average_LTDR + pre_max_LTDR + pre_min_temp + pre_max_temp + 
                    pre_average_temp + pre_max_precip +
                    pre_min_precip + pre_average_precip + post_implementation_time +
                    dist_to_all_rivers.na.mean + dist_to_roads.na.mean +
                    srtm_elevation_500m.na.mean + srtm_slope_500m.na.mean +
                    accessibility_map.na.mean + gpw_v3_density.2000.mean +
                    wdpa_5km.na.sum + treecover2000.na.mean + latitude +
                    longitude +total_disbursements +Focal.Area.single.letter.code,
                  trainingset,
                  control = rpart.control(cp = alpha,minsplit = 10),
                  method=alist)
    
    
    treesize = treesize + dim(fit1$frame[which(fit1$frame$var=="<leaf>"),])[1]
    pt = predict(fit1,testset,type = "matrix")
    y = data.frame(pt)
    val = data.matrix(y)
    idx = as.numeric(rownames(testset))
    dbidx = as.numeric(rownames(crxvdata))
    for(pid in 1:(dim(testset)[1])){
      id = match(idx[pid],dbidx)
      error = error + (crxvdata$transOutcome[id] - val[pid])^2
    }
  }
  
  tsize = c(tsize,treesize/k)
  if(error == 0){
    errset[l] = 1000000
  }
  else{
    errset[l] = error/k
  }
  msg = paste(l,": ",errset[l]*k,sep="")
}

tsize = tsize[-1]
alpha_res = alphacandidate[which.min(errset)]

fit_ctpred = rpart(cbind(LTDR_outcome_mean,treatment,distance,transOutcome) ~ pre_average_NTL + 
               pre_average_LTDR + pre_max_LTDR + pre_min_temp + pre_max_temp + 
               pre_average_temp + pre_max_precip +
               pre_min_precip + pre_average_precip + post_implementation_time +
               dist_to_all_rivers.na.mean + dist_to_roads.na.mean +
               srtm_elevation_500m.na.mean + srtm_slope_500m.na.mean +
               accessibility_map.na.mean + gpw_v3_density.2000.mean +
               wdpa_5km.na.sum + treecover2000.na.mean + latitude +
               longitude +total_disbursements +Focal.Area.single.letter.code,
             crxvdata,
             control=rpart.control(minsplit=10,cp=alpha_res),
             method=alist)



