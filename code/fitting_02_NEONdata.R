# Run fitting_01_NEONdata.R first to get data

#load libraries
library(lomb)


# load data from fiiting_01_NEONdata.R
wd <- getwd()

df_res<- read.csv(paste0(wd, "/data/output/fitting_NEONdata_results.csv"))

df_res$timeBgn <- ifelse(nchar(df_res$timeBgn) == 10,       # length of "YYYY-MM-DD"
                         paste0(df_res$timeBgn, " 00:00:00"), # append midnight
                         df_res$timeBgn)
df_res$timeBgn <- as.POSIXct(df_res$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")

# plot data
plot(df_res$timeBgn, df_res$rediduals_no_phi, col = "darkblue", lwd = 0.2, type = "l", 
     xlab = "elapsed days",
     ylab = "residuals after z-scored annual model fit")


#### lomb periodogram on residuals ####

lsp_resid <- lsp(df_res[,c("elapsed_days", "rediduals_no_phi")], 
                 type = "period",
                 normalize = "press",
                 to = 300,
                 ofac = 1)
getpeaks(lsp_resid)
summary(lsp_resid)
#doesn't do a great job...



#### play with "to" and "from" values to hone in on weekly/daily ####

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




