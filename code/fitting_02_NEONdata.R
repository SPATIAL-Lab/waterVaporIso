# Run fitting_01_NEONdata.R first to get data with annual taken out
# This script is to find/fit the meso and diurnal frequencies

site <- "CPER"
ml <- 10         #10 or "top"


# load data from fitting_01_NEONdata.R
wd <- getwd()

df_res<- read.csv(paste0(wd, "/data/output/fitting_NEONdata_results_", site, "_", ml, ".csv"))

df_res$timeBgn <- ifelse(nchar(df_res$timeBgn) == 10,       # length of "YYYY-MM-DD"
                         paste0(df_res$timeBgn, " 00:00:00"), # append midnight
                         df_res$timeBgn)
df_res$timeBgn <- as.POSIXct(df_res$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")

# plot data
plot(df_res$timeBgn, df_res$rediduals_no_phi, col = "darkblue", lwd = 0.2, type = "l", 
     xlab = "date",
     ylab = "residuals after z-scored annual model fit", 
     main = c(site, ml))


#### fit daily and weekly frequencies directly using nls() ####

# daily
period_day <- 1 #days

x <- df_res$elapsed_days
y <- df_res$residuals_phi
 
a <- (max(y)-min(y))/2 #amplitude
#d <- mean(y) #vertical offset
f <- 1/period_day #convert period to frequency
phi <- pi #phase offset

#model3 <- y ~ b * sin(2*pi*fr*x) + c * cos(2*pi*fr*x) + d
model3 <- y ~ a * sin(2*pi*f*x + phi)

fit3 <- nls(model3, 
            start = list(
              a = a,
              f = f,
              phi = phi
              ))

summary(fit3)
pred_values3 <- predict(fit3)

# plot
lines(df_res$timeBgn, pred_values3, col = "orange", lwd = 1)


# zoom in
sub1 <- df_res[17701:18989,]
sub1$pred3 <- pred_values3[17701:18989] 

plot(sub1$timeBgn, sub1$rediduals_no_phi, col = "darkblue", lwd = 0.2, type = "l", 
     xlab = "date",
     ylab = "z-scored annual model residuals", 
     main = "Daily cycle fit")
lines(sub1$timeBgn, sub1$pred3, col = "orange")





#### lomb periodogram on residuals ####

#load library
library(lomb)

lsp_resid <- lsp(df_res[,c("elapsed_days", "rediduals_no_phi")], 
                 type = "period",
                 normalize = "press",
                 to = 300,
                 ofac = 1)
getpeaks(lsp_resid)
summary(lsp_resid)
#doesn't do a great job...



#### play with "to" and "from" values to hone in on weekly/daily in lsp() ####

lsp_resid <- lsp(df_res[,c("elapsed_days", "rediduals_no_phi")], 
                 type = "period",
                 normalize = "press",
                 to = 20, #only analyze up to 20 day periods
                 ofac = 1)
getpeaks(lsp_resid)
summary(lsp_resid)

lsp_resid <- lsp(df_res[,c("elapsed_days", "rediduals_no_phi")], 
                 type = "period",
                 normalize = "press",
                 to = 14, #only analyze up to 14 day periods
                 ofac = 1)
getpeaks(lsp_resid)
summary(lsp_resid)
pershow(lsp_resid)

#pull out the power for a "daily" signal
day_pw <- max(lsp_resid$power[lsp_resid$scanned > 0.98 & lsp_resid$scanned < 1.02])
day_pw


#### window first, then L-S, average powers across segments ####

df_sub <- subset(df_res, timeBgn >= as.POSIXct("2021-01-01 00:00", tz="GMT") & 
                   timeBgn <= as.POSIXct("2021-02-01 00:00", tz="GMT"))
plot(x = df_sub$timeBgn, y = df_sub$rediduals_no_phi, pch = 19, cex = 0.3)
lsp_sub <- lsp(df_sub[,c("elapsed_days", "rediduals_no_phi")], 
               type = "period",
               normalize = "press",
               to = 20,
               ofac = 4)
getpeaks(lsp_sub)
summary(lsp_sub)



#### try randlsp ####
df_sub_rand <- subset(df_res, timeBgn >= as.POSIXct("2021-01-01 00:00", tz="GMT") & 
                   timeBgn <= as.POSIXct("2021-03-01 00:00", tz="GMT"))
lsp_sub <- lsp(df_sub_rand[,c("elapsed_days", "rediduals_no_phi")], 
               type = "period",
               normalize = "press",
               to = 20,
               ofac = 2)
getpeaks(lsp_sub)
lsp_sub_rand <- randlsp(repeats = 500, df_sub_rand[,c("elapsed_days", "rediduals_no_phi")], 
                    type = "period",
                    normalize = "press",
                    to = 20,
                    ofac = 2)
getpeaks(lsp_rand)




