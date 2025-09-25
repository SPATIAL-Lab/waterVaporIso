# download NEON water isotope data
options(stringsAsFactors=F)
library(neonUtilities)

#choose site
site <- "YELL"

#entire data file for a site:
zipsByProduct(dpID="DP4.00200.001", 
              package="expanded", #expanded gets qm data (basic just has qf final)
              site= site, #choose site, could choose multiple (slow download)
              startdate="2017-01", enddate="2024-10", #default doesn't include provisional
              savepath="~/GEO_Thesis", 
              check.size=F)

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

#pull out isotope data
df <- iso[[site]] #convert to df, DOUBLE CHECK THIS WORKS
write.csv(df, paste0("~/GEO_Thesis/iso_", site, ".csv")) #create .csv

#pull out Science Review flags for review
sciRevw <- iso[["scienceReviewFlags"]]
write.csv(sciRevw, paste0("~/GEO_Thesis/sciRevw_", site, ".csv"))

