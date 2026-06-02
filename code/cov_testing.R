# Simple covariance testing

library(dplyr)


site <- "HARV"
      

# TOP - load data from fitting_01_NEONdata
df <- read.csv(paste0("data/output/fitting_NEONdata_results_", site, "_top.csv"))
df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")


# ML1 - load data from fitting_01_NEONdata
df2 <- read.csv(paste0("data/output/fitting_NEONdata_results_", site, "_10.csv"))
df2$timeBgn <- ifelse(nchar(df2$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df2$timeBgn, " 00:00:00"), # append midnight
                     df2$timeBgn)
df2$timeBgn <- as.POSIXct(df2$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")


df_combined <- inner_join(df, df2, by = "timeBgn")


# z-score to compare
z_top <- (df_combined$residuals_phi.x - mean(df_combined$residuals_phi.x)) / sd(df_combined$residuals_phi.x) #x = df
z_ml1 <- (df_combined$residuals_phi.y - mean(df_combined$residuals_phi.y)) / sd(df_combined$residuals_phi.y) #y = df2 

time <- df_combined$timeBgn

# plot
plot(time, z_top, cex = 0.3, pch = 19,
     main = site, 
     xlab = "Date", 
     ylab = "iso", 
     col = "blue")
points(time, z_ml1, cex = 0.3, pch = 19, col = "red")
legend("topright", legend = c("top", "ml1"), col = c("blue", "red"), pch = 19)


cov(z_top, z_ml1)


# HARV top vs bottom cov (residuals) = 0.9524292
# CPER top vs bottom cov = 0.9955617
# OSBS top vs bottom cov = 0.9615676