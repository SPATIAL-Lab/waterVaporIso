# download NEON water isotope data
options(stringsAsFactors=F)
library(neonUtilities)

wd <- getwd()
savepath <- #BEFORE COMMITTING, CREATE SAVEPATH SHORTCUT

#choose site
site <- "ONAQ"

#entire data file for a site:
zipsByProduct(dpID="DP4.00200.001", 
              package="basic", #expanded gets qm data (basic just has qf final)
              site= site, #choose site, could choose multiple (but slow download)
              startdate= NA, #NA = all data 
              enddate= NA, 
              savepath= wd, 
              check.size= F, 
              include.provisional= T) #default doesn't include provisional


#just isotopes and water vapor:
iso <- stackEddy(filepath="~/GEO_Thesis/filesToStack00200",
                 level="dp01",
                 avg = 30, 
                 var = c("dlta18OH2o", "dlta2HH2o", "rtioMoleWetH2o"))


#isotopes plus other data:
iso <- stackEddy(filepath="~/GEO_Thesis/filesToStack00200",
                 level="dp01",
                 avg = 30, 
                 var = c("dlta18OH2o", "dlta2HH2o", "rtioMoleWetH2o", 
                         "presAtm", "presCor", "temp", 
                         "tempAir", "tempSoni"))

#pull out Science Review flags
sciRevw <- iso[["scienceReviewFlags"]]
write.csv(sciRevw, paste0("~/GEO_Thesis/sciRevw_", site, ".csv"))

#pull out isotope data
df <- iso[[site]] #convert to df

#remove validation rows
unique(df$verticalPosition)
df <- subset(df, !verticalPosition %in% c("co2Arch", "co2High", 
                                              "co2Low", "co2Med", 
                                              "co2Zero", "h2oHigh", 
                                              "h2oLow", "h2oMed"))

#create .csv
write.csv(df, paste0(wd, "iso_", site, "_release2026.csv"), row.names = F)



