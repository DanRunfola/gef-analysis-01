library(plyr)
library(ggplot2)
GEF.project.dta <- read.table("/home/aiddata/Desktop/Github/GEF/Data/GlobalEnvironmentFacility_GeocodedResearchRelease_Level1_v1.0/data/projects.csv", 
                              sep=",", header=TRUE)

GEF.project.dta.dates <- GEF.project.dta[GEF.project.dta$transactions_start_year != "",]
GEF.project.dta.dates <- GEF.project.dta.dates[GEF.project.dta.dates$total_disbursements != "",]
#Average Disbursement


#png("~/Desktop/Github/GEF/Report_Visualizations/GEF_funding_over_time.png",
#    width = 6, 
#    height = 4, 
#    units = 'in', 
#    res = 300)
dollars <- ggplot(GEF.project.dta.dates,aes(x=GEF.project.dta.dates$transactions_start_year,
                                 y=total_disbursements,colour="Average Project Disbursement",
                                 group=1, size=2)) +
  stat_summary(fun.y ="mean", geom = "smooth", colour="darkgreen") +
  theme(legend.position="none")+
  ylab("Average Project Disbursement")+
  xlab("Year")+
  scale_y_continuous(labels = scales::dollar)+
  theme(text = element_text(size=15))

GEF.project.dta.dates$counts <- 1

counts <- ggplot(GEF.project.dta.dates,aes(x=GEF.project.dta.dates$transactions_start_year,
                                            y=counts,colour="Average Project Disbursement",
                                            group=1, size=2)) +
  stat_summary(fun.y ="sum", geom = "smooth", colour="grey") +
  theme(legend.position="none")+
  ylab("Total Projects")+
  xlab("Year")+
  #scale_y_continuous(labels = scales::dollar)+
  theme(text = element_text(size=15))

multiplot(dollars, counts)
dev.off()