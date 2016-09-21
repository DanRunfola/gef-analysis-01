library(plyr)
library(ggplot2)
library(doBy)

#Read in Landcover Project Information
GEF.lc.impacts <- read.table("/home/aiddata/Desktop/Github/GEF/Summary/GEF_LandCover.csv", 
                               sep=",", header=TRUE)

#Calculate the average per-location dollar value for later collapsing.
proj_count <- table(GEF.lc.impacts$project_id)

for (i in 1:length(GEF.lc.impacts[[1]]))
{
  GEF.lc.impacts$lc_dollar[[i]] <- 
    GEF.lc.impacts$pred_trt[[i]] / (GEF.lc.impacts$total_disbursements[[i]]/
                                        proj_count[as.character(GEF.lc.impacts["project_id"][i,])][[1]])

  GEF.lc.impacts$continent[[i]] <- strsplit(as.character(GEF.lc.impacts["gazetteer_adm_name"][i,]), "\\|")[[1]][2]
  }



summaryBy(pred_trt ~ continent, data=GEF.lc.impacts)

# (1) AFR – Africa;
#(2) Asia; (3) CEX – Global – more than one country and
#one region; (4) ECA – Europe and Central Asia; (5) LAC
#- Latin America; and (6) REG  Regional, meaning more
#than one country in a region. 



#Read in NDVI
GEF.ndvi.impacts <- read.table("/home/aiddata/Desktop/Github/GEF/Summary/GEF_NDVI.csv", 
                             sep=",", header=TRUE)

#Calculate the average per-location dollar value for later collapsing.
proj_count <- table(GEF.ndvi.impacts$project_id)



for (i in 1:length(GEF.ndvi.impacts[[1]]))
{
  
  GEF.ndvi.impacts$continent[[i]] <- strsplit(as.character(GEF.ndvi.impacts["gazetteer_adm_name"][i,]), "\\|")[[1]][2]
}

GEF.ndvi.impacts$continent[GEF.ndvi.impacts$continent == "Southern Africa"] <- "Africa"
GEF.ndvi.impacts$continent[GEF.ndvi.impacts$continent == "Western Africa"] <- "Africa"

summaryBy(pred_trt ~ continent, data=GEF.ndvi.impacts)


#Read in Frag
GEF.frag.impacts <- read.table("/home/aiddata/Desktop/Github/GEF/Summary/GEF_Frag.csv", 
                               sep=",", header=TRUE)

#Calculate the average per-location dollar value for later collapsing.
proj_count <- table(GEF.frag.impacts$project_id)



for (i in 1:length(GEF.frag.impacts[[1]]))
{
  
  GEF.frag.impacts$continent[[i]] <- strsplit(as.character(GEF.frag.impacts["gazetteer_adm_name"][i,]), "\\|")[[1]][2]
}

#GEF.ndvi.impacts$continent[GEF.ndvi.impacts$continent == "Southern Africa"] <- "Africa"
#GEF.ndvi.impacts$continent[GEF.ndvi.impacts$continent == "Western Africa"] <- "Africa"

summaryBy(pred_trt ~ continent, data=GEF.frag.impacts)
