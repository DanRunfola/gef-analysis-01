GEF.LandCover.Result <- read.table("/home/aiddata/Desktop/Github/GEF/Summary/GEF_LandCover.csv", 
                                   sep=",", header=TRUE)

GEF.Frag.Result <- read.table("/home/aiddata/Desktop/Github/GEF/Summary/GEF_Frag.csv", 
                              sep=",", header=TRUE)

GEF.NDVI.Result <- read.table("/home/aiddata/Desktop/Github/GEF/Summary/GEF_NDVI.csv", 
                              sep=",", header=TRUE)

GEF.Val.Result <- read.table("/home/aiddata/Desktop/Github/GEF/Summary/GEF_Valuations_Prediction.csv", 
                             sep=",", header=TRUE)


GEF.Val.Result_out <- GEF.Val.Result[c("carbonTonnes", "LAT", "LON")]

write.csv(GEF.Val.Result_out, 
          "/home/aiddata/Desktop/Github/GEF/Viz/GEF_Valuation.csv")

write.csv(GEF.NDVI.Result, 
          "/home/aiddata/Desktop/Github/GEF/Viz/GEF_NDVI.csv")

write.csv(GEF.Frag.Result, 
          "/home/aiddata/Desktop/Github/GEF/Viz/GEF_Fragmentation.csv")

write.csv(GEF.LandCover.Result, 
          "/home/aiddata/Desktop/Github/GEF/Viz/GEF_LandCover.csv")


