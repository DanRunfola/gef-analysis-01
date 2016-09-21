library(plyr)
library(ggplot2)


#Read in NDVI project information
GEF.NDVI.impacts <- read.table("/home/aiddata/Desktop/Github/GEF/Summary/GEF_NDVI.csv", 
                              sep=",", header=TRUE)

#Calculate the average per-location dollar value for later collapsing.
proj_count <- table(GEF.NDVI.impacts$project_id)

for (i in 1:length(GEF.NDVI.impacts[[1]]))
{
GEF.NDVI.impacts$NDVI_dollar[[i]] <- 
  GEF.NDVI.impacts$pred_trt[[i]] / (GEF.NDVI.impacts$total_disbursements[[i]]/
                                 proj_count[as.character(GEF.NDVI.impacts["project_id"][i,])][[1]])
}


png("~/Desktop/Github/GEF/Report_Visualizations/GEF_impact_over_time_NDVI.png",
    width = 6, 
    height = 4, 
    units = 'in', 
    res = 300)
ggplot(GEF.NDVI.impacts,aes(x=GEF.NDVI.impacts$year,
                                 y=NDVI_dollar,colour="Average Project Disbursement",
                                 group=1, size=2)) +
  stat_summary(fun.y ="mean", geom = "smooth", colour="darkgreen") +
  theme(legend.position="none")+
  ylab("Avg. NDVI Shift Attributable to\nUSD $1 of GEF LD investment")+
  xlab("Year")+
  #scale_y_continuous(labels = scales::dollar)+
  scale_x_continuous(breaks=2002:2012, labels=2002:2012)+
  theme(text = element_text(size=15))

dev.off()




#Read in Fragmentation project information
GEF.frag.impacts <- read.table("/home/aiddata/Desktop/Github/GEF/Summary/GEF_Frag.csv", 
                               sep=",", header=TRUE)

#Calculate the average per-location dollar value for later collapsing.
proj_count <- table(GEF.frag.impacts$project_id)

for (i in 1:length(GEF.frag.impacts[[1]]))
{
  GEF.frag.impacts$frag_dollar[[i]] <- 
    GEF.frag.impacts$pred_trt[[i]] / (GEF.frag.impacts$total_disbursements[[i]]/
                                        proj_count[as.character(GEF.frag.impacts["project_id"][i,])][[1]])
}


png("~/Desktop/Github/GEF/Report_Visualizations/GEF_impact_over_time_frag.png",
    width = 6.25, 
    height = 4, 
    units = 'in', 
    res = 300)
ggplot(GEF.frag.impacts,aes(x=GEF.frag.impacts$year,
                            y=frag_dollar,colour="Average Project Disbursement",
                            group=1, size=2)) +
  stat_summary(fun.y ="mean", geom = "smooth", colour="darkgreen") +
  theme(legend.position="none")+
  ylab("Mean Forest Cover Patch Size\nShift Attributable to\nUSD $1 of GEF LD investment")+
  xlab("Year")+
  #scale_y_continuous(labels = scales::dollar)+
  scale_x_continuous(breaks=2002:2012, labels=2002:2012)+
  theme(text = element_text(size=15))

dev.off()



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
}


png("~/Desktop/Github/GEF/Report_Visualizations/GEF_impact_over_time_LC.png",
    width = 6.0, 
    height = 4, 
    units = 'in', 
    res = 300)
ggplot(GEF.lc.impacts,aes(x=GEF.lc.impacts$year,
                            y=lc_dollar,colour="Average Project Disbursement",
                            group=1, size=2)) +
  stat_summary(fun.y ="mean", geom = "smooth", colour="darkgreen") +
  theme(legend.position="none")+
  ylab("Change in Forest Loss Rate Attributable\nto USD $1 of GEF LD investment")+
  xlab("Year")+
  #scale_y_continuous(labels = scales::dollar)+
  scale_x_continuous(breaks=2002:2012, labels=2002:2012)+
  theme(text = element_text(size=14))

dev.off()