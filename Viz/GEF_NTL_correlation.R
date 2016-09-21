library(plyr)
library(ggplot2)


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

GEF.lc.impacts$NTL_change <- GEF.lc.impacts$v4composites_calibrated.2013.mean - GEF.lc.impacts$v4composites_calibrated.2002.mean 
plot(GEF.lc.impacts$pred_trt, GEF.lc.impacts$NTL_change,
     xlab="Change in Land Cover Rate of Loss\nAttributable to GEF Projects",
     ylab="Change in Nighttime Lights")

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