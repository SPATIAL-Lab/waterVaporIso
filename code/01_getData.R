# download NEON water isotope and met data for a site 

library(neonUtilities)


#choose site
site <- "SCBI"

#entire data file for a site:
zipsByProduct(dpID = "DP4.00200.001", 
              package = "expanded", #expanded gets qm data (basic just has qf final)
              site = site, #choose site, could choose multiple (but slow download)
              startdate = "2021-01", 
              enddate = NA, 
              savepath = "data/", 
              check.size = T, 
              include.provisional = F) #default doesn't include provisional


#just isotopes and water vapor:
#data <- stackEddy(filepath= "data/filesToStack00200",
#                 level="dp01",
#                 avg = 30, 
#                 var = c("dlta18OH2o", "dlta2HH2o", "rtioMoleWetH2o"))


#isotopes plus met data:
data <- stackEddy(filepath = "data/filesToStack00200",
                 level = "dp01",
                 avg = 30, 
                 var = c("dlta18OH2o", 
                         "dlta2HH2o", 
                         "rtioMoleWetH2o", 
                         "presAtm", 
                         "presCor", 
                         "temp", 
                         "tempAir", 
                         "tempSoni", 
                         "radiLwOut", 
                         "radiSwIn",
                         "veloXaxsYaxsErth"
                         ))


#pull out Science Review flags
sciRevw <- data[["scienceReviewFlags"]]
write.csv(sciRevw, paste0("data/sciRevw_", site, ".csv"))

#issue log
issLog <- data[["issueLog"]]
write.csv(issLog, paste0("data/supplementals/sciRevw_", site, ".csv"))

#pull out data
df <- data[[site]]

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

#rename columns
colnames(df) <- c("timeBgn", "timeEnd", "verticalPosition", "dlta18OH2o", 
                  "dlta2HH2o", "rtioMoleWetH2o",
                  "dlta18OH2o.qfFinl", "dlta2HH2o.qfFinl")

#use only 2021 and later data (For 2026 release, end dates should all be June 2025)
#df <- subset(df, timeBgn >= as.POSIXct("2021-01-01 00:00", tz="GMT"))

#create .csv
write.csv(df, paste0(wd, "/data/iso_", site, "_release2026.csv"), row.names = F)


