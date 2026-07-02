##
#
# ANNUAL CYCLE FITTING AND REMOVAL
#
# create clean data from 02_cleanData first
# fit a sine wave to the longest frequency component, subtract that out
# from there, use L-S or another method to detect smaller frequencies (not in this script)
# Output = data/output directory. .csv with the name "fitting_NEONdata_results_[site]_[ml].csv" with columns for timeBgn, elapsed_days, iso, and residuals
#
#

### INPUTS ##################################################



# choose site and level
site <- "WREF"
ml <- "top"        # 10 or "top"

# define model starting values (getting these close is very important)
period <- 340 #period in days - an estimate based on what we know (i.e. annual cycle)
phi <- 0 #phase shift, 0 is fine to start






### end inputs ##############################################


#load data
df <- read.csv(paste0("data/iso_", site, "_", ml, "_clean.csv"))

#remove lingering NAs in any column
df <- df[complete.cases(df), ]

#create numeric dates
df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")
df$time <- as.numeric(df$timeBgn) /60/60/24 #seconds to days
#create elapsed column
df$elapsed_days <- df$time - df$time[1]

x <- df$elapsed_days
y <- df$iso

y <- (y - mean(y)) / sd(y) #z-scored

plot(x, y, cex = 0.3, pch = 19, 
     main = "Raw data", 
     xlab = "elapsed days", 
     ylab = "z-scored")

# starting values for models
A <- (max(y)-min(y))/2 #amplitude
d <- mean(y) #vertical offset

fr <- 1/period

results <- data.frame(
  amplitude = A, 
  period = 1/fr, 
  phase = phi, 
  vert_offset = d, 
  row.names = "starting values"
)

## USE NLS() WITH PHI
model <- y ~ A * sin(2 * pi * fr * x + phi) + d

fit <- nls(model, 
           start = list(
             A = A, 
             fr = fr, 
             phi = phi, 
             d = d
           ))
#summary(fit)
#coef(fit)
#1/coef(fit)["fr"]
results <- rbind(results, model_with_phi = c(coef(fit)["A"], 
                                             1/coef(fit)["fr"], 
                                             coef(fit)["phi"],
                                             coef(fit)["d"]))

# fitted values
pred_values <- predict(fit)

# subtract that out
resid <- y - pred_values

# plot
png(paste0("media/fitting_01_", site, "_plot.png"), width = 800, height = 600)
plot(x, y, cex = 0.3, pch = 19, 
     main = "Model fits (red) \nand after residuals are subtracted (blue)", 
     col = "gray", 
     xlab = "elapsed days",
     ylab = "z-scored iso")
lines(x, pred_values, col = "red", lwd = 3)
lines(x, resid, col = "lightblue", lwd = 1)
dev.off() #use with png() to close writing the plot


df_new <- df[,c("timeBgn", "elapsed_days", "iso")]
df_new$residuals <- resid

#if results folder doesn't exist, create one
if(!dir.exists(paste0(wd, "/data/output"))){
  dir.create(paste0(wd, "/data/output"))
}
write.csv(df_new, paste0("data/output/fitting_NEONdata_results_", site, "_", ml, ".csv"), row.names = F)

print(c("MODELS USED:", model))
print(results)



#
## USE NLS() WITHOUT PHI
## not used, estimate with phi is pretty much the exact same
#
#model2 <- y ~ b * sin(2*pi*fr*x) + c * cos(2*pi*fr*x) + d
#fit2 <- nls(model2, 
#            start = list(
#              b = A,
#              c = 0, 
#              fr = fr, 
#              d = d))
#
#coef(fit2)
#b <- coef(fit2)["b"]
#c <- coef(fit2)["c"]
#A2 <- sqrt(b^2 + c^2)
#phi2 <- atan2(c, b)
#results <- rbind(results, model_without_phi = c(A2, 
#                                                1/coef(fit2)["fr"], 
#                                                phi2,
#                                                coef(fit2)["d"]))
#
# fitted values
#pred_values2 <- predict(fit2)
#
# subtract that out
#resid2 <- y - pred_values2
#
# plot
#lines(x, pred_values2, col = "orange", lwd = 1)
#lines(x, resid2, col = "darkgreen", lwd = 0.2)



