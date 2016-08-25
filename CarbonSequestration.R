#Valuation Analysis
library(rgdal)

source("data_prep_and_joins.R")

GEF.seq.dta = readOGR("/home/aiddata/Desktop/Github/GEF/Data/GEF_treatment_centroids_carbon_extract.geojson", "OGRGeoJSON")

GEF.noUS.trt <- GEF.noUS[GEF.noUS$treatment == 1,]
GEF.noUS.seq <- merge(GEF.noUS.trt@data, GEF.seq.dta, by="id")


#Carbon sequestration model
if(names(GEF.noUS.seq)[391]  == "mean")
{
  names(GEF.noUS.seq)[391] <- "CarbonBiomass"
} else {
  print("Error!!!!!!!-----------------")
}





#Fit the carbon model, which provides the relationship between M T / HA and our
#Treatment terms (+ controls).
CarbonModel <- lm(CarbonBiomass ~ factor(GEZ_TERM) + latitude.x + longitude.x + 
     mean_patch_size2010 + lossyr25.na.categorical_2010  +
     ltdr_yearly_ndvi_max.2010.mean, data=GEF.noUS.seq)


