# Simple covariance testing

library(dplyr)


site <- "HARV"
      
#################
# ML1 and TOP ISOTOPE COMPARISONS

# TOP - load data from fitting_01_NEONdata
df <- read.csv(paste0("data/output/fitting_NEONdata_results_", site, "_top.csv"))
df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")

plot(df$timeBgn, df$residuals_phi, cex = 0.3, pch = 19, col = "blue")

# ML1 - load data from fitting_01_NEONdata
df2 <- read.csv(paste0("data/output/fitting_NEONdata_results_", site, "_10.csv"))
df2$timeBgn <- ifelse(nchar(df2$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df2$timeBgn, " 00:00:00"), # append midnight
                     df2$timeBgn)
df2$timeBgn <- as.POSIXct(df2$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")


# combine dfs
# compare ML1 and ML top isotope data
df_combined <- inner_join(df, df2, by = "timeBgn") #inner_join to keep only rows with matching timeBgn in both dataframes


# z-score to compare (shouldn't need this is z-scored already through removing annual cycle)
#z_top <- (df_combined$residuals_phi.x - mean(df_combined$residuals_phi.x)) / sd(df_combined$residuals_phi.x) #x = df
#z_ml1 <- (df_combined$residuals_phi.y - mean(df_combined$residuals_phi.y)) / sd(df_combined$residuals_phi.y) #y = df2 

# df (top)  = .x
# df2 (ML1) = .y

time <- df_combined$timeBgn
x_top <- df_combined$residuals_phi.x
x_bot <- df_combined$residuals_phi.y

# plot
plot(time, x_top, cex = 0.3, pch = 19,
     main = site, 
     xlab = "Date", 
     col = "blue")
points(time, x_bot, cex = 0.3, pch = 19, col = "red")
legend("topright", legend = c("top", "ML1"), col = c("blue", "red"), pch = 19)


cov(x_top, x_bot)

# HARV isotope top vs bottom cov (residuals) = 0.9524292
# CPER isotope top vs bottom cov = 0.9955617
# OSBS isotope top vs bottom cov = 0.9615676


# comparisons with met data in met_comparisons script
