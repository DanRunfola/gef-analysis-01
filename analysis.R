library(rpart)
library(doBy)

#Causal Tree
source("data_prep_and_joins.R")
#From data load script: GEF.noUS
GEF.spdf <- GEF.noUS

#Join in project info for dates
GEF.project.dta <- read.table("/home/aiddata/Desktop/Github/GEF/Data/GlobalEnvironmentFacility_GeocodedResearchRelease_Level1_v1.0/data/projects.csv", 
                              sep=",", header=TRUE)
GEF.spdf.prj <- merge(GEF.spdf, GEF.project.dta, by="project_id")

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
GEF.spdf.prj@data <- GEF.spdf.prj@data[GEF.spdf.prj$start_actual_isodate != "2014-01-07",]
GEF.spdf.prj@data <- GEF.spdf.prj@data[GEF.spdf.prj$start_actual_isodate != "2014-01-01",]

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

for(i in 1:length(GEF.spdf.prj$year))
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

#Choose analysis variables
aVars <- c("LTDR_outcome_mean", "LTDR_outcome_max", "pre_average_NTL", 
           "pre_average_LTDR", "pre_max_LTDR", "pre_min_temp", "pre_max_temp",
           "pre_average_temp", "pre_max_precip", "pre_min_precip", 
           "pre_average_precip", "post_implementation_time", "year",
           "dist_to_all_rivers.na.mean", "dist_to_roads.na.mean",
           "srtm_elevation_500m.na.mean", "srtm_slope_500m.na.mean",
           "accessibility_map.na.mean", "gpw_v3_density.2000.mean",
           "wdpa_5km.na.sum", "treecover2000.na.mean", "treatment", "latitude",
           "longitude", "total_disbursements")


