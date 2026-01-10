# Review data for quality flags, low humidity (<5000 ppm), and outliers 

#install.packages("zoo")
library(zoo)
library(ggplot2)

wd <- getwd()

sitech <- "YELL" #choose site

df <- read.csv(paste0(wd, "/data/iso_", sitech, "_release2024.csv"))

unique(df$verticalPosition)
ml <- 10 #choose measurement level
df <- subset(df, verticalPosition %in% ml)

df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")

ggplot(df, aes(x = timeBgn, y = data.isoH2o.dlta18OH2o.mean)) +
  geom_point(size = 0.8) +
  labs(x = "date", y = expression(paste(delta^{18}, "O permil")), 
       title = paste(sitech, "Water Vapor Isotopes")) +
  theme_minimal()


# Clean data by eliminating SDs from the mean
roll_mean <- rollapply(df$data.isoH2o.dlta18OH2o.mean, 
                       2016,  #2016 = average 6 weeks of data points, on the ends avg just available weeks
                       mean, 
                       na.rm = T, 
                       fill = NA, 
                       partial = T)
sum(is.na(roll_mean))

ggplot(df, aes(x = timeBgn, y = data.isoH2o.dlta18OH2o.mean)) +
  geom_point(size = 0.8, color = "lightblue") +
  labs(x = "date", y = expression(paste(delta^{18}, "O permil")), 
       title = paste(sitech, "Water Vapor Isotopes")) +
  guides(color = guide_legend(override.aes = list(size = 8))) +
  theme_minimal() +
  geom_line(aes(y = roll_mean))

roll_sd <- rollapply(df$data.isoH2o.dlta18OH2o.mean, 2016, FUN = function(z) sd(z, na.rm = T),
                     fill = NA, partial = T)

upper_bound <- roll_mean + roll_sd*3
lower_bound <- roll_mean - roll_sd*3

ggplot(df, aes(x = timeBgn, y = data.isoH2o.dlta18OH2o.mean)) +
  geom_point(size = 0.8, color = "lightblue") +
  labs(x = "date", y = expression(paste(delta^{18}, "O permil")), 
       title = paste(sitech, "Water Vapor Isotopes")) +
  theme_minimal() +
  geom_line(aes(y = upper_bound)) +
  geom_line(aes(y = roll_mean), color = "red") +
  geom_line(aes(y = lower_bound))


# create new df with clean data
cleaned <- df[,c("timeBgn", "data.isoH2o.dlta18OH2o.mean")]

cleaned$outlier <- df$data.isoH2o.dlta18OH2o.mean > upper_bound |
  df$data.isoH2o.dlta18OH2o.mean < lower_bound

for (i in 1:nrow(cleaned)) {
  if (isFALSE(cleaned$outlier[i])) {
    cleaned$iso[i] <- cleaned$data.isoH2o.dlta18OH2o.mean[i]
  } else {
    cleaned$iso[i] <- NA
  }
}

clean_data <- subset(cleaned, !is.na(iso))
clean_data <- clean_data[,c("timeBgn", "iso")]

write.csv(clean_data, paste0(wd, "/data/iso_", sitech, "_clean.csv"), row.names = F)



#ggplot(clean_data, aes(x = timeBgn, y = iso)) +
#  geom_point(size = 0.8) +
#  labs(x = "date", y = expression(paste(delta^{18}, "O permil")), 
#       title = paste(sitech, "Water Vapor Isotopes")) +
#  theme_minimal()

