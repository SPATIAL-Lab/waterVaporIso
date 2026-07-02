# clean met data 
# until I build this out, it'll just be looking at the data for visual outliers...

site <- "CPER"

dfm <- read.csv(paste0("data/met/met_", site, "_release2026.csv")) 

ml <- "top"  #choose measurement level: 10 or "top"

#reduce to chosen measurement level
if (ml == 10) {
  dfm <- subset(dfm, verticalPosition %in% 10)
} else if (ml == "top") {
  dfm <- subset(dfm, verticalPosition %in% max(dfm$verticalPosition))
} else {
  print("no ml")
}


dfm$timeBgn <- ifelse(nchar(dfm$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(dfm$timeBgn, " 00:00:00"), # append midnight
                     dfm$timeBgn)
dfm$timeBgn <- as.POSIXct(dfm$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")



#for each column of met data (4-9), plot the data with the quality flag as color using a for loop
for (i in 4:9) {
  meas_data <- dfm[,i]
  qf_flag <- dfm[,i+6]
  x <- dfm$timeBgn
  p <- ggplot(mapping = aes(x = x, y = meas_data)) + 
      geom_point(size = 1, aes(color = qf_flag)) + 
      labs(x = "date", 
          title = colnames(dfm[i])) +
      scale_color_gradient(name = "QF", low="lightgray", high="red") +
      theme_minimal()
  print(p)
}

#choose which column to clean
ch <- "veloXaxsYaxsErth"
col <- dfm[, ch]

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

ggplot(dfm, aes(x = timeBgn, y = col)) +
  geom_point(size = 0.8, color = "lightblue") +
  labs(x = "date", y = "velo") +
  theme_minimal() +
  geom_line(aes(y = upper_bound)) +
  geom_line(aes(y = roll_mean), color = "red") +
  geom_line(aes(y = lower_bound))


# create new df with clean data
cleaned <- dfm[,c("timeBgn", ch)]

cleaned$outlier <- col > upper_bound |
  col < lower_bound

for (i in 1:nrow(cleaned)) {
  if (isFALSE(cleaned$outlier[i])) {
    cleaned$new[i] <- cleaned[,ch][i]
  } else {
    cleaned$new[i] <- NA
  }
}

clean_data <- subset(cleaned, !is.na(new))
clean_data <- clean_data[,c("timeBgn", "new")]

colnames(clean_data) <- c("timeBgn", ch)

write.csv(clean_data, paste0("data/met/met_", site,"_", ml, "_", ch, "_clean.csv"), row.names = F)


## RH


