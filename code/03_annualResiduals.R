##
# ANNUAL CYCLE FITTING AND REMOVAL
#
# create clean data from 02_cleanData first
# fit a sine wave to the longest frequency component (annual) using nonlinear least squares, subtract that out
# Output = data/residuals directory, csv with columns for timeBgn, original data, elapsed_days, and residuals
#
#


# choose site and level
site <- "ORNL"
ml <- "top"        # 10 or "top"


# define model starting values
period <- 365 #period in days - an estimate based on what we know (i.e. annual cycle)
phi <- 0 #phase shift, 0 is fine to start


##
#### ISO csv (18O, 2H, wet mole H2O, and q)
{
# load iso data
df <- read.csv(paste0("data/iso/clean/iso_", site, "_", ml, "_clean.csv"))

# remove lingering NAs in any column
df <- df[complete.cases(df), ]

# create numeric dates
df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")


# 
x <- as.numeric(df$timeBgn) /60/60/24 #seconds to days
x <- x - x[1] #elapsed days
df$elapsed_days <- x


col_names <- c("dlta18OH2o", "dlta2HH2o", "rtioMoleWetH2o", "q")
res_names <- c("dlta18O_residual", "dlta2H_residual", "rtioMoleWet_residual", "q_residual")

phi <- 0.01 # if needed, if there's a "singular gradient" error

for (i in seq_along(col_names)) {
  
  y        <- df[[col_names[i]]]
  res_name <- res_names[i]
  
  cat("\n── Fitting:", col_names[i], "──\n")
  
  # Starting values
  A  <- (max(y) - min(y)) / 2
  d  <- mean(y)
  fr <- 1 / period
  
  # # Plot raw data
  # plot(x, y, cex = 0.3, pch = 19,
  #      main = paste("Raw data:", col_names[i]),
  #      xlab = "elapsed days",
  #      ylab = col_names[i])
  
  # Fit model
  model <- y ~ A * sin(2 * pi * fr * x + phi) + d
  fit   <- nls(model, start = list(A = A, fr = fr, phi = phi, d = d))
  
  print(coef(fit))
  cat("Period:", 1 / coef(fit)["fr"], "\n")
  
  # Residuals
  pred_values <- predict(fit)
  resid       <- y - pred_values
  
  # Plot fits and residuals
  plot(x, y, cex = 0.3, pch = 19,
       main  = paste("Model fits (red) & residuals (blue):", col_names[i]),
       col   = "gray",
       xlab  = "elapsed days",
       ylab  = paste("residual", col_names[i]),
       ylim  = c(min(y, resid + d), max(y, resid + d)))
  lines(x, pred_values, col = "red",  lwd = 3)
  lines(x, resid + d,   col = "blue", lwd = 1)
  
  # Store residuals back in df
  df[[res_name]] <- resid
}


#if results folder doesn't exist, create one
if(!dir.exists("data/residuals")){
  dir.create("data/residuals")
}

write.csv(df, paste0("data/residuals/iso_", site, "_", ml, "_residuals.csv"), row.names = F)

}

#### MET -  temp
{  
# load met - temp data
df <- read.csv(paste0("data/met/met_", site, "_release2026.csv"))

df <- subset(df, verticalPosition %in% max(df$verticalPosition))

# remove NAs in temp column
df <- df[!is.na(df$tempAirTop), ]

# create numeric dates
df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")


# 
x <- as.numeric(df$timeBgn) /60/60/24 #seconds to days
x <- x - x[1] #elapsed days
y <- df$tempAirTop

#y <- (y - mean(y)) / sd(y) #z-score - take this out to keep the original values for fitting

plot(x, y, cex = 0.3, pch = 19, 
     main = "Raw data", 
     xlab = "elapsed days", 
     ylab = "temp")

# starting values for models
A <- (max(y)-min(y))/2 #amplitude
d <- mean(y) #vertical offset

fr <- 1/period
  
  
# create model using nls with phi
model <- y ~ A * sin(2 * pi * fr * x + phi) + d
  
fit <- nls(model, start = list(A = A, fr = fr, phi = phi, d = d))

#summary(fit)
coef(fit)
1/coef(fit)["fr"]


# fitted values
pred_values <- predict(fit)

# subtract that out
resid <- y - pred_values

# visual check that it worked
plot(x, y, cex = 0.3, pch = 19, 
     main = "Model fits (red) \nand after residuals are subtracted (blue)", 
     col = "gray", 
     xlab = "elapsed days",
     ylab = "temp", 
     ylim = c(min(y, resid), max(y, resid)))
lines(x, pred_values, col = "red", lwd = 3)
lines(x, resid, col = "lightblue", lwd = 1)


df_new <- df[,c("timeBgn", "tempAirTop")]
df_new$elapsed_days <- x
df_new$residuals <- resid

#if results folder doesn't exist, create one
if(!dir.exists("data/residuals")){
  dir.create("data/residuals")
}

write.csv(df_new, paste0("data/residuals/temp_", site, "_", ml, "_residuals.csv"), row.names = F)

}

#### MET -  pressure
{
# load met - pres data
df <- read.csv(paste0("data/met/met_", site, "_release2026.csv"))

df <- subset(df, verticalPosition %in% median(df$verticalPosition))

# remove NAs in pres column
df <- df[!is.na(df$presAtm), ]

# create numeric dates
df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")


# 
x <- as.numeric(df$timeBgn) /60/60/24 #seconds to days
x <- x - x[1] #elapsed days
y <- df$presAtm

#y <- (y - mean(y)) / sd(y) #z-score - take this out to keep the original values for fitting

plot(x, y, cex = 0.3, pch = 19, 
     main = "Raw data", 
     xlab = "elapsed days", 
     ylab = "pres")

# starting values for models
A <- (max(y)-min(y))/2 #amplitude
d <- mean(y) #vertical offset

fr <- 1/period


# create model using nls with phi
model <- y ~ A * sin(2 * pi * fr * x + phi) + d

fit <- nls(model, start = list(A = A, fr = fr, phi = phi, d = d))

#summary(fit)
coef(fit)
1/coef(fit)["fr"]


# fitted values
pred_values <- predict(fit)

# subtract that out
resid <- y - pred_values

# visual check that it worked
plot(x, y, cex = 0.3, pch = 19, 
     main = "Model fits (red) \nand after residuals are subtracted (blue)", 
     col = "gray", 
     xlab = "elapsed days",
     ylab = "pres", 
     ylim = c(min(y, resid+d), max(y, resid+d)))
lines(x, pred_values, col = "red", lwd = 3)
lines(x, resid+d, col = "lightblue", lwd = 1)


df_new <- df[,c("timeBgn", "presAtm")]
df_new$elapsed_days <- x
df_new$residuals <- resid

#if results folder doesn't exist, create one
if(!dir.exists("data/residuals")){
  dir.create("data/residuals")
}

write.csv(df_new, paste0("data/residuals/pres_", site, "_", ml, "_residuals.csv"), row.names = F)

}

