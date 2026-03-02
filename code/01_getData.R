# download NEON water isotope data
options(stringsAsFactors=F)
library(neonUtilities)

wd <- getwd() #waterVaporIso project file
savepath <- file.path("..") #local file path for zipsByProduct savepath


#choose site
site <- "WREF"

#entire data file for a site:
zipsByProduct(dpID="DP4.00200.001", 
              package="basic", #expanded gets qm data (basic just has qf final)
              site= site, #choose site, could choose multiple (but slow download)
              startdate= NA, #NA = all data 
              enddate= NA, 
              savepath= savepath, 
              check.size= F, 
              include.provisional= F) #default doesn't include provisional


#just isotopes and water vapor:
iso <- stackEddy(filepath= "../filesToStack00200",
                 level="dp01",
                 avg = 30, 
                 var = c("dlta18OH2o", "dlta2HH2o", "rtioMoleWetH2o"))


#isotopes plus other data:
iso <- stackEddy(filepath="../filesToStack00200",
                 level="dp01",
                 avg = 30, 
                 var = c("dlta18OH2o", "dlta2HH2o", "rtioMoleWetH2o", 
                         "presAtm", "presCor", "temp", 
                         "tempAir", "tempSoni"))

#pull out Science Review flags
sciRevw <- iso[["scienceReviewFlags"]]
write.csv(sciRevw, paste0("../sciRevw_", site, ".csv"))

#issue log
#issLog <- iso[["issueLog"]]

#pull out isotope data
df <- iso[[site]] #convert to df

#remove validation rows
unique(df$verticalPosition)
df <- subset(df, !verticalPosition %in% c("co2Arch", "co2High", 
                                              "co2Low", "co2Med", 
                                              "co2Zero", "h2oHigh", 
                                              "h2oLow", "h2oMed"))

#remove unwanted columns for a smaller file to save
df <- df[,c("timeBgn", "timeEnd", "verticalPosition", "data.isoH2o.dlta18OH2o.mean", 
             "data.isoH2o.dlta2HH2o.mean", "data.isoH2o.rtioMoleWetH2o.mean",
             "qfqm.isoH2o.dlta18OH2o.qfFinl", "qfqm.isoH2o.dlta2HH2o.qfFinl")]

#save only the bottom and top measurement levels
df <- subset(df, verticalPosition %in% c("010", max(df$verticalPosition)))

#create .csv
write.csv(df, paste0(wd, "/data/iso_", site, "_release2026.csv"), row.names = F)


