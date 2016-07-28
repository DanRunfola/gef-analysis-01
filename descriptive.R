library(sp)
library(stargazer)

#From data load script: GEF.noUS
GEF.spdf <- GEF.noUS

#Stargazer Summary of GEF Projects for Description
GEF.projects <- GEF.spdf[GEF.spdf$treatment == 1,]
GEF.stargaze <- GEF.projects@data[c("dist_to_all_rivers.na.mean",
                                    "treatment",
                                    "dist_to_roads.na.mean",
                                    "srtm_elevation_500m.na.mean",
                                    "srtm_slope_500m.na.mean",
                                    "accessibility_map.na.mean",
                                    "gpw_v4_density.2005.mean",
                                    "ltdr_yearly_ndvi_mean.1982.mean",
                                    "ltdr_yearly_ndvi_mean.2014.mean",
                                    "v4composites_calibrated.2013.mean",
                                    "udel_air_temp_v4_01_yearly_min.2014.mean",
                                    "udel_air_temp_v4_01_yearly_max.2014.mean",
                                    "udel_air_temp_v4_01_yearly_mean.2014.mean",
                                    "udel_precip_v4_01_yearly_max.2014.mean",
                                    "udel_precip_v4_01_yearly_min.2014.mean",
                                    "udel_precip_v4_01_yearly_mean.2014.mean",
                                    "wdpa_5km.na.count",
                                    "treecover2000.na.mean",
                                    "lossyear.na.categorical_count",
                                    "X00forest25.na.sum"
                                    )]
stargazer(GEF.stargaze, type="text", mean.sd=FALSE, median=TRUE)


GEF.project.dta <- read.table("/home/aiddata/Desktop/Github/GEF/Data/GlobalEnvironmentFacility_GeocodedResearchRelease_Level1_v1.0/data/projects.csv", 
                            sep=",", header=TRUE)


GEF.project.dta.dates <- GEF.project.dta[GEF.project.dta$start_actual_isodate != "",]
min(as.Date(GEF.project.dta.dates$start_actual_isodate))
max(as.Date(GEF.project.dta.dates$start_actual_isodate))

GEF.locations.dta <- read.table("/home/aiddata/Desktop/Github/GEF/Data/GlobalEnvironmentFacility_GeocodedResearchRelease_Level1_v1.0/data/locations.csv", 
                              sep=",", header=TRUE)
