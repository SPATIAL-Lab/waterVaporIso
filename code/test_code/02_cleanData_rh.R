#
# clean RH data and convert to specific humidity
#
#

library(ggplot2)


site <- "CLBJ"

dfrh <- read.csv(paste0("data/met/met_", site, "_release2026_RH.csv")) 

#ml <- "top"  #other option is soil plot 3 (horizontal position = 3)

dfrh <- subset(dfrh, verticalPosition %in% max(dfrh$verticalPosition))

dfrh$startDateTime <- ifelse(nchar(dfrh$startDateTime) == 10,       # length of "YYYY-MM-DD"
                             paste0(dfrh$startDateTime, " 00:00:00"), # append midnight
                             dfrh$startDateTime)
dfrh$startDateTime <- as.POSIXct(dfrh$startDateTime, format="%Y-%m-%d %H:%M:%S", tz="GMT")


# plot with qf
ggplot(dfrh, aes(x = startDateTime, y = RHMean)) + 
  geom_point(size = 0.8, aes(color = RHFinalQF)) + 
  labs(x = "date", 
       title = paste(site, "_RHFinalQF")) +
  scale_color_gradient(name = "QF", low="darkgray", high="red") +
  theme_minimal()

# compare temp RH to met temp
ggplot(dfm, aes(x = timeBgn, y = tempAirTop)) + 
  geom_point(size = 0.4, color = "red") + 
  theme_minimal() +
  geom_point(dfrh, mapping = aes(x = startDateTime, y = tempRHMean), size = 0.4, color = "lightblue")



# to get q: 
# Use the formula: q = (r * Mw) / (r * Mw + (1 - r) * Md)
# where r = mole fraction of water vapor (rtioMoleWetH2o mol/mol (NEON is mmol/mol)), Mw = molar mass of water (18.015 g/mol), Md = molar mass of dry air (28.97 g/mol)

df$q <- (df$rtioMoleWetH2o * 0.001 * 18.015) /
  ((df$rtioMoleWetH2o * 0.001 * 18.015) + ((1 - (df$rtioMoleWetH2o * 0.001)) * 28.97))


