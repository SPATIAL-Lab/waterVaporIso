# download NEON water isotope and met data for a NEON site 

library(neonUtilities)


#choose site
site <- "CPER"

# !!!!!!!!!!
#before downloading another site, check current filesToStack folder is empty !!!


#entire data file for a site:
zipsByProduct(dpID = "DP4.00200.001", 
              package = "expanded", #expanded gets qm data (basic just has qf final)
              release = "RELEASE-2026",
              site = site, #choose site, could choose multiple (but slow download)
              startdate = "2021-01", 
              enddate = "2025-06", 
              savepath = "data/", 
              check.size = F, 
              include.provisional = F) #default doesn't include provisional


#isotopes and water vapor data:
iso <- stackEddy(filepath= "data/filesToStack00200",
                 level="dp01",
                 avg = 30, 
                 var = c("dlta18OH2o", "dlta2HH2o", "rtioMoleWetH2o"))

#met data:
met <- stackEddy(filepath = "data/filesToStack00200",
                 level = "dp01",
                 avg = 30, 
                 var = c("presAtm", 
                         "presCor", 
                         "temp", #may not need this if I have tempAir?
                         "tempAir", 
                         #"tempSoni", 
                         "radiLwOut", 
                         "radiSwIn",
                         "veloXaxsYaxsErth"))

## iso
#pull out Science Review flags
#sciRevw <- met[["scienceReviewFlags"]]

#if(!is.null(sciRevw)){
#  write.csv(sciRevw, paste0("data/supplementals/", site, "_sciRevw_iso.csv"))
#} else{
#  print("No science review flags. No csv created.")
#}

#issLog <- met[["issueLog"]] 
#issue log available at https://data.neonscience.org/data-products/DP4.00200.001#issueLog

#pull out data
df <- iso[[site]]

#remove validation rows
unique(df$verticalPosition)
df <- subset(df, !verticalPosition %in% c("co2Arch", "co2High", 
                                              "co2Low", "co2Med", 
                                              "co2Zero", "h2oHigh", 
                                              "h2oLow", "h2oMed"))

#remove unwanted columns for a smaller file to save
colnames(df)

#### iso data ####
#save only the bottom and top measurement levels
df <- subset(df, verticalPosition %in% c("010", max(df$verticalPosition)))

df <- df[,c("timeBgn", "timeEnd", 
            "verticalPosition", 
            "data.isoH2o.dlta18OH2o.mean", 
            "data.isoH2o.dlta2HH2o.mean", 
            "data.isoH2o.rtioMoleWetH2o.mean",
            "qfqm.isoH2o.dlta18OH2o.qmRngPresCrdH2oFail",
            "qfqm.isoH2o.dlta2HH2o.qmRngPresCrdH2oFail",
            "qfqm.isoH2o.dlta18OH2o.qmFrt00MfmFail",
            "qfqm.isoH2o.dlta2HH2o.qmFrt00MfmFail",
            "qfqm.isoH2o.dlta18OH2o.qfFinl", 
            "qfqm.isoH2o.dlta2HH2o.qfFinl")]

#rename columns
colnames(df) <- c("timeBgn", "timeEnd", 
                  "verticalPosition", 
                  "dlta18OH2o", 
                  "dlta2HH2o", 
                  "rtioMoleWetH2o",
                  "qmRngPresCrdH2oFail.dlta18OH2o",
                  "qmRngPresCrdH2oFail.dlta2HH2o",
                  "qmFrt00MfmFail.dlta18OH2o",
                  "qmFrt00MfmFail.dlta2HH2o",
                  "qfFinl.dlta18OH2o", 
                  "qfFinl.dlta2HH2o")

#use only 2021 and later data (For 2026 release, end dates should all be June 2025)
#df <- subset(df, timeBgn >= as.POSIXct("2021-01-01 00:00", tz="GMT"))

#create .csv
write.csv(df, paste0("data/iso/iso_", site, "_release2026.csv"), row.names = F)


#### met data ####
#pull out data
df <- met[[site]]

#remove validation rows
unique(df$verticalPosition)
df <- subset(df, !verticalPosition %in% c("co2Arch", "co2High", 
                                          "co2Low", "co2Med", 
                                          "co2Zero", "h2oHigh", 
                                          "h2oLow", "h2oMed"))


colnames(df)

df <- df[,c("timeBgn", "timeEnd", 
            "verticalPosition",
            "data.presBaro.presAtm.mean", #atm pressure from the barometer in between ML1 and ML2
            "data.tempAirLvl.temp.mean", #air temp on the ML
            "data.tempAirTop.temp.mean", #air temp on the top (triple aspirated)
            "data.radiNet.radiSwIn.mean", #incoming shortwave radiation
            "data.radiNet.radiLwOut.mean", #outgoing longwave radiation
            "data.soni.veloXaxsYaxsErth.mean", #wind speed from CSAT3D
            "qfqm.presBaro.presAtm.qfFinl", 
            "qfqm.tempAirLvl.temp.qfFinl", 
            "qfqm.tempAirTop.temp.qfFinl", 
            "qfqm.radiNet.radiSwIn.qfFinl",
            "qfqm.radiNet.radiLwOut.qfFinl",
            "qfqm.soni.veloXaxsYaxsErth.qfFinl", #qfs more important for the outliers here
            "qfqm.soni.veloXaxsYaxsErth.qmSoniSgnlPoorFail"
            )]

colnames(df) <- c("timeBgn", "timeEnd", 
                  "verticalPosition",
                  "presAtm", 
                  "tempAirLvl", 
                  "tempAirTop", 
                  "radiSwIn", 
                  "radiLwOut", 
                  "veloXaxsYaxsErth", 
                  "presAtm.qfFinl", 
                  "tempAirLvl.qfFinl", 
                  "tempAirTop.qfFinl", 
                  "radiSwIn.qfFinl",
                  "radiLwOut.qfFinl",
                  "veloXaxsYaxsErth.qfFinl", 
                  "veloXaxsYaxsErth.qmSoniSgnlPoorFail")



##save only the bottom, top, and CHECK WITCH LEVEL THE BARO IS ON (sometimes 1.5, sometime 3.5)
unique(df$verticalPosition)
df <- subset(df, verticalPosition %in% c("010", "035", max(df$verticalPosition)))

#create .csv
write.csv(df, paste0("data/met/met_", site, "_release2026.csv"), row.names = F)


#### met data - relative humidity ####
rh <- loadByProduct(dpID = "DP1.00098.001", 
              package = "expanded", #expanded gets qm data (basic just has qf final)
              release = "RELEASE-2026",
              timeIndex = 30,
              site = site, #choose site, could choose multiple (but slow download)
              startdate = "2021-01", 
              enddate = "2025-06",
              check.size = F, 
              include.provisional = F) #default doesn't include provisional

names(rh)

#pull out Science Review flags
#sciRevw <- rh[["science_review_flags_00098"]]

#get data
rh_dat <- rh[["RH_30min"]]

colnames(rh_dat)

rh_dat <- rh_dat[,c("startDateTime",
                    "horizontalPosition",
                    "verticalPosition",
                    "RHMean",
                    "RHRangeFailQM",
                    "RHPersistenceFailQM",
                    "RHNullFailQM",
                    "RHGapFailQM",
                    "RHSpikeFailQM",
                    "RHValidCalFailQM",
                    "RHFinalQF",
                    "RHFinalQFSciRvw",
                    "tempRHMean",
                    "tempRHRangeFailQM",
                    "tempRHPersistenceFailQM",
                    "tempRHNullFailQM",
                    "tempRHGapFailQM",
                    "tempRHSpikeFailQM",
                    "tempRHValidCalFailQM",
                    "tempRHFinalQF",
                    "tempRHFinalQFSciRvw",
                    "dewTempMean",
                    "dewTempRangeFailQM",
                    "dewTempPersistenceFailQM",
                    "dewTempNullFailQM",
                    "dewTempGapFailQM",
                    "dewTempSpikeFailQM",
                    "dewTempValidCalFailQM",
                    "dewTempFinalQF",
                    "dewTempFinalQFSciRvw",
                    "RHSensorErrorFailQM" 
                    )]   

write.csv(rh_dat, paste0("data/met/met_", site, "_release2026_RH.csv"), row.names = F)
