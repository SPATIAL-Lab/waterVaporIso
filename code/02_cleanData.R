## 
# CLEAN ISOTOPE DATA
#
# Clean 18O, 2H, and Wet mole H2O
# Review data for quality flags, outliers, and low humidity (<5000 ppm)
# ONLY INCLUDES REMOVING FLOW RATE FLAGS and CLEANING OUTLIERS (by removing data >3 sd from a rolling 6-week mean)
#     ---> Eventually add back in LH?
# Also calculates specific humidity from mole fraction of water vapor (rtioMoleWetH2o) and adds it to the cleaned csv
#
# output = iso/clean directory, csv with outlier rows removed (7 columns in final csv)
# 
#
#


library(zoo)
library(ggplot2)




site <- "ORNL"

df <- read.csv(paste0("data/iso/iso_", site, "_release2026.csv"))

unique(df$verticalPosition)
ml <- "top" #choose measurement level: 10 or "top"

#reduce to chosen measurement level
if (ml == 10) {
  df <- subset(df, verticalPosition %in% 10)
} else if (ml == "top") {
  df <- subset(df, verticalPosition %in% max(df$verticalPosition))
} else {
  print("no ml")
}


df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")

i_rows <- nrow(df)

#
#
#
#### dlta18OH2o ####


# basic plot
ggplot(df, aes(x = timeBgn, y = dlta18OH2o)) +
  geom_point(size = 0.8) +
  labs(x = "date", y = expression(paste(delta^{18}, "O permil")), 
       title = paste(site, "Water Vapor Isotopes")) +
  theme_minimal()

##### test qfs and outlier removal

## QF plots

# qfFinl
ggplot(df, aes(x = timeBgn, y = dlta18OH2o)) +
  geom_point(size = 0.8, aes(color = qfFinl.dlta18OH2o)) +
  labs(x = "date", y = expression(paste(delta^{18}, "O permil")),
       title = paste0(site, " with qfFinl, ml = ", ml,
                      "\nqf > 0 = ", sum(df$qfFinl.dlta18OH2o > 0 & !is.na(df$dlta18OH2o)),
                      " (", round(sum(df$qfFinl.dlta18OH2o > 0 & !is.na(df$dlta18OH2o)) / sum(!is.na(df$dlta18OH2o)), digits = 4)*100, "%)")) +
  scale_color_gradient(name = "QF", low="darkgray", high="red") +
  theme_minimal()

# save plot
ggsave(paste0("media/qfs/", site, "_qfFinl.png"), width = 2140, height = 1360, units = "px", dpi = 240)

# flow rate
ggplot(df, aes(x = timeBgn, y = dlta18OH2o)) +
  geom_point(size = 0.8, aes(color = qmFrt00MfmFail.dlta18OH2o)) +
  labs(x = "date", y = expression(paste(delta^{18}, "O permil")),
       title = paste0(site, " with qmFrt00Mfm, ml = ", ml,
                     "\nqf > 0 = ", sum(df$qmFrt00MfmFail.dlta18OH2o > 0 & !is.na(df$dlta18OH2o)),
                     " (", round(sum(df$qmFrt00MfmFail.dlta18OH2o > 0 & !is.na(df$dlta18OH2o)) / sum(!is.na(df$dlta18OH2o)), digits = 4)*100, "%)")) +
  scale_color_gradient(name = "QF", low="darkgray", high="red") +
  theme_minimal()

# save plot
ggsave(paste0("media/qfs/", site, "_qmFrt00Mfm.png"), width = 2140, height = 1360, units = "px", dpi = 240)

# range pressure
ggplot(df, aes(x = timeBgn, y = dlta18OH2o)) +
  geom_point(size = 0.8, aes(color = qmRngPresCrdH2oFail.dlta18OH2o)) +
  labs(x = "date", y = expression(paste(delta^{18}, "O permil")),
       title = paste0(site, " with qmRngPresCrdH2o, ml = ", ml,
                      "\nqf > 0 = ", sum(df$qmRngPresCrdH2oFail.dlta18OH2o > 0 & !is.na(df$dlta18OH2o)),
                      " (", round(sum(df$qmRngPresCrdH2oFail.dlta18OH2o > 0 & !is.na(df$dlta18OH2o)) / sum(!is.na(df$dlta18OH2o)), digits = 4)*100, "%)")) +
  scale_color_gradient(name = "QF", low="darkgray", high="red") +
  theme_minimal()

# save plot
ggsave(paste0("media/qfs/", site, "_qmRngPresCrdH2o.png"), width = 2140, height = 1360, units = "px", dpi = 240)


# Clean data by eliminating SDs from the mean
roll_mean <- rollapply(df$dlta18OH2o,
                       2016,  #2016 = average 6 weeks of data points, on the ends avg just available weeks
                       mean,
                       na.rm = T,
                       fill = NA,
                       partial = T)
#sum(is.na(roll_mean))

roll_sd <- rollapply(df$dlta18OH2o, 2016, FUN = function(z) sd(z, na.rm = T),
                     fill = NA, partial = T)

upper_bound <- roll_mean + roll_sd*3
lower_bound <- roll_mean - roll_sd*3

ggplot(df, aes(x = timeBgn, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "lightblue") +
  labs(x = "date", y = expression(paste(delta^{18}, "O permil")),
       title = paste0(site, ", ml = ", ml,
                     "\nOut of bounds = ", sum(df$dlta18OH2o > upper_bound | df$dlta18OH2o < lower_bound, na.rm = T), " points")) +
  theme_minimal() +
  geom_line(aes(y = upper_bound)) +
  geom_line(aes(y = roll_mean), color = "red") +
  geom_line(aes(y = lower_bound))

# save plot
ggsave(paste0("media/qfs/", site, "_3stdDeviations.png"), width = 2140, height = 1360, units = "px", dpi = 240)


#### double check flow rate qfs


ggplot(df, aes(x = timeBgn, y = dlta18OH2o)) +
  geom_point(size = 0.8, aes(color = qmFrt00MfmFail.dlta18OH2o)) +
  labs(x = "date", y = expression(paste(delta^{18}, "O permil")),
       title = paste0(site, " with qmFrt00Mfm, ml = ", ml,
                      "\nqf > 0 = ", sum(df$qmFrt00MfmFail.dlta18OH2o > 0 & !is.na(df$dlta18OH2o)),
                      " (", round(sum(df$qmFrt00MfmFail.dlta18OH2o > 0 & !is.na(df$dlta18OH2o)) / sum(!is.na(df$dlta18OH2o)), digits = 4)*100, "%)")) +
  scale_color_gradient(name = "QF", low="darkgray", high="red") +
  theme_minimal()

sub <- subset(df, timeBgn >= as.POSIXct("2021-01-01 00:00", tz="GMT") &
                  timeBgn <= as.POSIXct("2021-06-01 00:00", tz="GMT"))

ggplot(sub, aes(x = timeBgn, y = dlta18OH2o)) +
  geom_point(size = 0.8, aes(color = qmFrt00MfmFail.dlta18OH2o)) +
  labs(x = "date", y = expression(paste(delta^{18}, "O permil")),
       title = paste0(site, ", qmFrt00MfmFail")) +
                       scale_color_gradient(name = "QF", low="darkgray", high="red") +
  theme_minimal()

ggsave(paste0("media/qfs/", site, "_zoomedInFrt00_2.png"), width = 2140, height = 1360, units = "px", dpi = 240)


##### clean data (flow rate and outliers)

# remove all data with flow rate flags
sum(df$qmFrt00MfmFail.dlta18OH2o > 0) 

cleaned <- subset(df, qmFrt00MfmFail.dlta18OH2o == 0) 


# remove outliers 3 sd from the rolling mean AFTER flow rate flags are removed
roll_mean <- rollapply(cleaned$dlta18OH2o, 
                       2016,  #2016 = average 6 weeks of data points, on the ends avg just available weeks
                       mean, 
                       na.rm = T, 
                       fill = NA, 
                       partial = T)

roll_sd <- rollapply(cleaned$dlta18OH2o, 2016, FUN = function(z) sd(z, na.rm = T),
                     fill = NA, partial = T)

upper_bound <- roll_mean + roll_sd*3
lower_bound <- roll_mean - roll_sd*3

ggplot(cleaned, aes(x = timeBgn, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "lightblue") +
  labs(x = "date", y = expression(paste(delta^{18}, "O permil"))) +
  theme_minimal() +
  geom_line(aes(y = upper_bound)) +
  geom_line(aes(y = roll_mean), color = "red") +
  geom_line(aes(y = lower_bound))


cleaned$outlier <- cleaned$dlta18OH2o > upper_bound | cleaned$dlta18OH2o < lower_bound

sum(cleaned$outlier == TRUE, na.rm = T) # data points that are outliers

cleaned <- subset(cleaned, outlier == FALSE | is.na(outlier))
cleaned <- cleaned[,1:12] # remove outlier column

# save cleaned data
write.csv(cleaned, paste0("data/iso/clean/iso_", site,"_", ml, "_clean.csv"), row.names = F)

df <- cleaned

#
#
#
#### dlta2HH2o ####


# basic plot
ggplot(df, aes(x = timeBgn, y = dlta2HH2o)) +
  geom_point(size = 0.8) +
  labs(x = "date", y = "2HH2o permil", 
       title = paste(site, "Water Vapor Isotopes")) +
  theme_minimal()

##### outlier removal

# Clean data by eliminating SDs from the mean
roll_mean <- rollapply(df$dlta2HH2o, 
                       2016,  #2016 = average 6 weeks of data points, on the ends avg just available weeks
                       mean, 
                       na.rm = T, 
                       fill = NA, 
                       partial = T)
#sum(is.na(roll_mean))

roll_sd <- rollapply(df$dlta2HH2o, 2016, FUN = function(z) sd(z, na.rm = T),
                     fill = NA, partial = T)

upper_bound <- roll_mean + roll_sd*3
lower_bound <- roll_mean - roll_sd*3

ggplot(df, aes(x = timeBgn, y = dlta2HH2o)) +
  geom_point(size = 0.8, color = "lightblue") +
  labs(x = "date", y = "dlta2HH2o", 
       title = paste0(site, ", ml = ", ml,
                      "\nOut of bounds = ", sum(df$dlta2HH2o > upper_bound | df$dlta2HH2o < lower_bound, na.rm = T), " points")) +
  theme_minimal() +
  geom_line(aes(y = upper_bound)) +
  geom_line(aes(y = roll_mean), color = "red") +
  geom_line(aes(y = lower_bound))


cleaned <- df

cleaned$outlier <- cleaned$dlta2HH2o > upper_bound | cleaned$dlta2HH2o < lower_bound

sum(cleaned$outlier == TRUE, na.rm = T) # data points that are outliers

cleaned <- subset(cleaned, outlier == FALSE | is.na(outlier))
cleaned <- cleaned[,1:12] # remove outlier column

# save cleaned data
write.csv(cleaned, paste0("data/iso/clean/iso_", site,"_", ml, "_clean.csv"), row.names = F)

df <- cleaned


#
#
#
#### rtioMolWetH2o ####

# build off of cleaned data from above
# df <- read.csv(paste0("data/iso/clean/iso_", site,"_", ml, "_clean.csv"))
# 
# df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
#                       paste0(df$timeBgn, " 00:00:00"), # append midnight
#                       df$timeBgn)
# df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")


##### outlier check and cleaning

## QF plots

# qfFinl
ggplot(df, aes(x = timeBgn, y = rtioMoleWetH2o)) + 
  geom_point(size = 0.8, aes(color = qfFinl.dlta18OH2o)) + 
  labs(x = "date", y = "mmol/mol",
       title = paste0(site, " with qfFinl, ml = ", ml, 
                      "\nqf > 0 = ", sum(df$qfFinl.dlta18OH2o > 0 & !is.na(df$dlta18OH2o)),
                      " (", round(sum(df$qfFinl.dlta18OH2o > 0 & !is.na(df$dlta18OH2o)) / sum(!is.na(df$dlta18OH2o)), digits = 4)*100, "%)")) +
  scale_color_gradient(name = "QF", low="darkgray", high="red") +
  theme_minimal()


# Clean data by eliminating SDs from the mean
roll_mean <- rollapply(df$rtioMoleWetH2o, 
                       2016,  #2016 = average 6 weeks of data points, on the ends avg just available weeks
                       mean, 
                       na.rm = T, 
                       fill = NA, 
                       partial = T)
#sum(is.na(roll_mean))

roll_sd <- rollapply(df$rtioMoleWetH2o, 2016, FUN = function(z) sd(z, na.rm = T),
                     fill = NA, partial = T)

upper_bound <- roll_mean + roll_sd*3
lower_bound <- roll_mean - roll_sd*3

ggplot(df, aes(x = timeBgn, y = rtioMoleWetH2o)) +
  geom_point(size = 0.8, color = "lightblue") +
  labs(x = "date", y = "mmol/mol", 
       title = paste0(site, ", ml = ", ml,
                      "\nOut of bounds = ", sum(df$rtioMoleWetH2o > upper_bound | df$rtioMoleWetH2o < lower_bound, na.rm = T), " points")) +
  theme_minimal() +
  geom_line(aes(y = upper_bound)) +
  geom_line(aes(y = roll_mean), color = "red") +
  geom_line(aes(y = lower_bound))

# save plot
ggsave(paste0("media/qfs/", site, "_3stdDeviations_rtiomolH2o.png"), width = 2140, height = 1360, units = "px", dpi = 240)


#### remove outliers
cleaned <- df
cleaned$outlier <- cleaned$rtioMoleWetH2o > upper_bound | cleaned$rtioMoleWetH2o < lower_bound

sum(cleaned$outlier == TRUE, na.rm = T) # data points that are outliers

cleaned <- subset(cleaned, outlier == FALSE | is.na(outlier))
cleaned <- cleaned[,1:12] # remove outlier column


# save cleaned data by overwriting previously saved csv
#write.csv(cleaned, paste0("data/iso/clean/iso_", site,"_", ml, "_clean.csv"), row.names = F)


## calculate specific humidity from mole fraction of water vapor (rtioMoleWetH2o)

# to get q: 
# Use the formula: q = (r * Mw) / (r * Mw + (1 - r) * Md)
# where r = mole fraction of water vapor (rtioMoleWetH2o mol/mol (NEON is mmol/mol)), Mw = molar mass of water (18.015 g/mol), Md = molar mass of dry air (28.97 g/mol)

df <- cleaned

df$q <- (df$rtioMoleWetH2o * 0.001 * 18.015) /
  ((df$rtioMoleWetH2o * 0.001 * 18.015) + ((1 - (df$rtioMoleWetH2o * 0.001)) * 28.97))

ggplot(df, aes(x = timeBgn, y = q)) +
  geom_point(size = 0.8) +
  labs(x = "date", y = "specific humidity", 
       title = paste(site, "specific humidity")) +
  theme_minimal()




#
#
#
#### FINAL SAVE: remove qm and qf columns from df ####
df <- df[, !grepl("qm|qf", names(df))]



write.csv(df, paste0("data/iso/clean/iso_", site,"_", ml, "_clean.csv"), row.names = F)

f_rows <- nrow(df)
print(paste0("removed ", i_rows - f_rows, " rows"))

#
#
#
#### low humidity correction??




