# compare isotope data with met data
#
# met data includes:
#   - atmospheric pressure (presAtm), only on ML 1.5 or 3.5
#   - air temperature (tempAirLvl, tempAirTop)
#   - incoming shortwave radiation (radiSwIn)
#   - outgoing longwave radiation (radiLwOut)
#   - horizontal wind speed (veloXaxsYaxsErth)
#   - relative humidity (rh, downloaded separately)
#
# some met data was "cleaned". Some left as is because it looks fine for now.
#

library(ggplot2)
library(dplyr)


site <- "CPER"

met <- read.csv(paste0("data/met/met_", site, "_release2026.csv"))

# create numeric dates
met$timeBgn <- ifelse(nchar(met$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(met$timeBgn, " 00:00:00"), # append midnight
                     met$timeBgn)
met$timeBgn <- as.POSIXct(met$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")

unique(met$verticalPosition)

# plot met data
colnames(met)

ggplot(mapping = aes(x = met$timeBgn, y = met$presAtm)) + 
  geom_point(size = 0.8, color = "darkblue") + 
  labs(x = "date", 
       y = "presAtm",
       title = site) +
  theme_minimal()

#ggplot(mapping = aes(x = x, y = met$tempAirLvl)) + 
#  geom_point(size = 0.8, color = "darkblue") + 
#  labs(x = "date", 
#       y = "tempAirLvl (ML1)",
#       title = site) +
#  theme_minimal()

ggplot(mapping = aes(x = met$timeBgn, y = met$tempAirTop)) + 
  geom_point(size = 0.8, color = "darkblue") + 
  labs(x = "date", 
       y = "tempAirTop",
       title = site) +
  theme_minimal()

ggplot(mapping = aes(x = met$timeBgn, y = met$radiSwIn)) + #goes to 0 every night, just get noon data?
  geom_point(size = 0.8, color = "darkblue") + 
  labs(x = "date", 
       y = "radiSwIn",
       title = site) +
  theme_minimal()

ggplot(mapping = aes(x = met$timeBgn, y = met$radiLwOut)) + 
  geom_point(size = 0.8, color = "darkblue") + 
  labs(x = "date", 
       y = "radiLwOut",
       title = site) +
  theme_minimal()
 
ggplot(mapping = aes(x = met$timeBgn, y = met$veloXaxsYaxsErth)) + 
  geom_point(size = 0.8, color = "darkblue") + 
  labs(x = "date", 
       y = "veloXaxsYaxsErth",
       title = site) +
  theme_minimal()



# relative humidity

rh <- read.csv(paste0("data/met/met_", site, "_release2026_RH.csv"))

unique(rh$verticalPosition)
rh <- subset(rh, verticalPosition %in% max(rh$verticalPosition))

#remove all values above 100
rh$RHMean[rh$RHMean > 100] <- NA

# create numeric dates
rh$startDateTime <- ifelse(nchar(rh$startDateTime) == 10,       # length of "YYYY-MM-DD"
                      paste0(rh$startDateTime, " 00:00:00"), # append midnight
                      rh$startDateTime)
rh$startDateTime <- as.POSIXct(rh$startDateTime, format="%Y-%m-%d %H:%M:%S", tz="GMT")

# plot
ggplot(mapping = aes(x = rh$startDateTime, y = rh$RHMean)) + 
  geom_point(size = 0.8, color = "darkblue") + 
  labs(x = "date", 
       y = "relative humidity",
       title = site) +
  theme_minimal()

# prep rh for use below
rh <- rh[,c("startDateTime", "RHMean")]
names(rh) <- c("timeBgn", "RH")
dat <- "RH"
met <- rh



################
#Covariance comparisons

# don't remove annual cycle first
met <- read.csv(paste0("data/met/met_", site, "_release2026.csv"))

# subset just top level
met <- subset(met, verticalPosition %in% max(met$verticalPosition)) 

# subset just midlevel for presAtm
#met <- subset(met, verticalPosition %in% 35) 

# create df with just timeBgn and one met parameter
dat <- "presAtm"

met <- met[,c("timeBgn", dat)]

# if using RH, start here:

iso <- read.csv(paste0("data/iso/iso_", site, "_release2026.csv"))
iso <- subset(iso, verticalPosition %in% max(iso$verticalPosition))
iso <- iso[,c("timeBgn", "dlta18OH2o")]

df_comb <- inner_join(iso, met, by = "timeBgn")

df_comb$timeBgn <- ifelse(nchar(df_comb$timeBgn) == 10,       # length of "YYYY-MM-DD"
                           paste0(df_comb$timeBgn, " 00:00:00"), # append midnight
                           df_comb$timeBgn)
df_comb$timeBgn <- as.POSIXct(df_comb$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")


#only want complete cases
df_comb <- df_comb[complete.cases(df_comb), ]

z_iso <- (df_comb$dlta18OH2o - mean(df_comb$dlta18OH2o)) / sd(df_comb$dlta18OH2o) 
z_met <- (df_comb[,3] - mean(df_comb[,3])) / sd(df_comb[,3])

{plot(df_comb$timeBgn, z_iso, cex = 0.3, pch = 19,
     main = paste0(site, ", annual included, ", dat), 
     xlab = "Date",
     ylab = "z-scored values",
     col = "blue")
points(df_comb$timeBgn, z_met, cex = 0.3, pch = 19, col = "pink")
annotation <- paste0("cov = ", round(cov(z_iso, z_met), 3))
legend("topright", legend = c("z_iso", "z_met", annotation), col = c("blue", "red", "black"), pch = 19)
}


# remove annual cycle for comparison

x <- as.numeric(df_comb$timeBgn) /60/60/24 #seconds to days
x <- x - x[1] #elapsed days

y <- z_met

# starting values for models
period <- 360 #period in days - an estimate based on what we know (i.e. annual cycle)
phi <- 0 #phase shift, 0 is fine to start
A <- (max(y)-min(y))/2 #amplitude
d <- mean(y) #vertical offset
fr <- 1/period

## USE NLS() WITH phi value
model <- y ~ A * sin(2 * pi * fr * x + phi) + d

fit <- nls(model, start = list(A = A, fr = fr, phi = phi, d = d))

pred_values <- predict(fit) # fitted values
resid <- y - pred_values # subtract that out

# visual check that it worked
plot(x, y, cex = 0.3, pch = 19, 
     main = "Model fits (red) \nand after residuals are subtracted (blue)", 
     col = "gray", 
     xlab = "elapsed days",
     ylab = paste("z-scored met: ", dat))
lines(x, pred_values, col = "red", lwd = 3)
lines(x, resid, col = "lightblue", lwd = 1)

df_comb$resmet <- resid


# even though I've already done this for the isotope data, do it again
y <- z_iso

# starting values for models
A <- (max(y)-min(y))/2 #amplitude
d <- mean(y) #vertical offset

## USE NLS() WITH phi value
model <- y ~ A * sin(2 * pi * fr * x + phi) + d

fit <- nls(model, start = list(A = A, fr = fr, phi = phi, d = d))

pred_values <- predict(fit) # fitted values
resid <- y - pred_values # subtract that out

# visual check that it worked
plot(x, y, cex = 0.3, pch = 19, 
     main = "Model fits (red) \nand after residuals are subtracted (blue)", 
     col = "gray", 
     xlab = "elapsed days",
     ylab = paste("z-scored iso"))
lines(x, pred_values, col = "red", lwd = 3)
lines(x, resid, col = "lightblue", lwd = 1)

df_comb$resiso <- resid

# covariance with annual removed
# don't need to z-score again because already z-scored from the model fit above?

{plot(df_comb$timeBgn, df_comb$resiso, cex = 0.3, pch = 19,
     main = paste0(site, ", annual subtracted, ", dat), 
     xlab = "Date",
     ylab = "z-scored values",
     col = "blue")
points(df_comb$timeBgn, df_comb$resmet, cex = 0.3, pch = 19, col = "red")
annotation <- paste0("cov = ", round(cov(df_comb$resiso, df_comb$resmet), 3))
legend("bottomright", legend = c("iso", "met", annotation), col = c("blue", "red", "black"), pch = 19)
}


#clear env






