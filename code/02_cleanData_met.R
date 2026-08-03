## 
# CLEAN MET DATA
#
# Temp and pres
# Review data for quality flags and outliers 
# Only clean data if needed (if outliers are visually present or if quality flags look suspect)
# the majority of met data isn't cleaned
# any cleaned data will have a second csv with "notcleaned" in the file name
#
#


library(ggplot2)
library(zoo)

site <- "CLBJ"

dfm <- read.csv(paste0("data/met/met_", site, "_release2026.csv")) 

ml <- "top"  #choose measurement level: 10 or "top"

#reduce to chosen measurement level
if (ml == 10) {
  dfm <- subset(dfm, verticalPosition %in% c(10, median(dfm$verticalPosition)))
} else if (ml == "top") {
  dfm <- subset(dfm, verticalPosition %in% c(max(dfm$verticalPosition), median(dfm$verticalPosition)))
} else {
  print("no ml")
}


dfm$timeBgn <- ifelse(nchar(dfm$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(dfm$timeBgn, " 00:00:00"), # append midnight
                     dfm$timeBgn)
dfm$timeBgn <- as.POSIXct(dfm$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")



# plot qfFnl for presAtm and tempAirTop
for (i in c(4,6)) {
  meas_data <- dfm[,i]
  qf_flag <- dfm[,i+6]
  x <- dfm$timeBgn
  p <- ggplot(mapping = aes(x = x, y = meas_data)) + 
      geom_point(size = 1, aes(color = qf_flag)) + 
      labs(x = "date", 
          title = paste(site, colnames(dfm[i]))) +
      scale_color_gradient(name = "QF", low="lightgray", high="red") +
      theme_minimal()
  print(p)
}

# check out subsets to determine cleaning
sub <- subset(dfm, timeBgn >= as.POSIXct("2022-01-01 00:00", tz="GMT") & 
                timeBgn <= as.POSIXct("2022-02-01 00:00", tz="GMT"))

ggplot(sub, aes(x = timeBgn, y = tempAirTop)) + 
  geom_point(size = 0.8, aes(color = tempAirTop.qfFinl)) + 
  labs(x = "date", 
       title = paste(site, "tempAirTop")) +
  scale_color_gradient(name = "QF", low="darkgray", high="red") +
  theme_minimal()


#### clean data if needed
# save new file with same file name
# save original file with "_uncleaned"


#choose which column to clean
ch <- "tempAirTop"

cleaned <- subset(dfm, verticalPosition %in% max(dfm$verticalPosition))

col <- cleaned[, ch]

roll_mean <- rollapply(col, 
                       2016,  #2016 = average 6 weeks of data points, on the ends avg just available weeks
                       mean, 
                       na.rm = T, 
                       fill = NA, 
                       partial = T)

roll_sd <- rollapply(col, 2016, FUN = function(z) sd(z, na.rm = T),
                     fill = NA, partial = T)

upper_bound <- roll_mean + roll_sd*3
lower_bound <- roll_mean - roll_sd*3

ggplot(cleaned, aes(x = timeBgn, y = col)) +
  geom_point(size = 0.8, color = "lightblue") +
  labs(x = "date", y = ch) +
  theme_minimal() +
  geom_line(aes(y = upper_bound)) +
  geom_line(aes(y = roll_mean), color = "red") +
  geom_line(aes(y = lower_bound))


# create new column with clean data
cleaned$outlier <- cleaned$tempAirTop > upper_bound | cleaned$tempAirTop < lower_bound

sum(cleaned$outlier == TRUE, na.rm = T) # data points that are outliers

dfm[rownames(cleaned), "outlier"] <- cleaned$outlier #add outlier column back to dfm by index

dfm <- subset(dfm, outlier == FALSE | is.na(outlier))
dfm <- dfm[,1:16] # remove outlier column

# make sure it worked
ggplot(dfm, aes(x = timeBgn, y = tempAirTop)) + 
  geom_point(size = 0.8, aes(color = tempAirTop.qfFinl)) + 
  labs(x = "date", 
       title = paste(site, "tempAirTop")) +
  scale_color_gradient(name = "QF", low="darkgray", high="red") +
  theme_minimal()

file.rename(paste0("data/met/met_", site, "_release2026.csv"), 
            paste0("data/met/met_", site, "_release2026_notcleaned.csv")) #rename uncleaned file



write.csv(dfm, paste0("data/met/met_", site,"_release2026.csv"), row.names = F)
