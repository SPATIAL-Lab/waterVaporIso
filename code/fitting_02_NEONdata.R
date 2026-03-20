# Run fitting_01_NEONdata.R first to get data with annual taken out
# This script is to find/fit the meso and diurnal frequencies

library(ggplot2)


site <- "WREF"
ml <- 10        #10 or "top"

# load data from fitting_01_NEONdata
df_res <- read.csv(paste0("data/output/fitting_NEONdata_results_", site, "_", ml, ".csv"))

df_res$timeBgn <- ifelse(nchar(df_res$timeBgn) == 10,       # length of "YYYY-MM-DD"
                         paste0(df_res$timeBgn, " 00:00:00"), # append midnight
                         df_res$timeBgn)
df_res$timeBgn <- as.POSIXct(df_res$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")

# plot data in ggplot
plot <- ggplot(df_res, aes(x = timeBgn, y = rediduals_no_phi)) +
  geom_point(color = "darkblue", size = 0.2) +
  labs(x = "date", 
       y = "residuals after z-scored annual model fit", 
       title = paste(site, ml)) +
  theme_minimal()

#### fit daily and weekly frequencies directly using nls() ####

# daily
period_day <- 1 #days

x <- df_res$elapsed_days
y <- df_res$residuals_phi
 
a <- (max(y)-min(y))/2 #amplitude
#d <- mean(y) #vertical offset
f <- 1/period_day #convert period to frequency
phi <- 0 #phase offset

#model <- y ~ b * sin(2*pi*fr*x) + c * cos(2*pi*fr*x) + d
model <- y ~ a * sin(2*pi*f*x + phi)

fit <- nls(model, 
            start = list(
              a = a,
              f = f,
              phi = phi
              ))
summary(fit)

# plot
predicted <- predict(fit)

plot + 
  geom_line(aes(y = predicted), color = "orange", size = 1)



#plot just simple curves to compare phases
#x2 <- 1:100
#y2 <- coef(fit3)["a"] * sin(2*pi*coef(fit3)["f"]*x2 + coef(fit3)["phi"])
#plot(x2, y2, col = "purple", lwd = 1, type = "l")
#lines(x2, y2, col = "orange", lwd = 1 )



# zoom in to where there are minimal data gaps
sub <- subset(df_res, timeBgn >= as.POSIXct("2020-10-15 00:00", tz="GMT") & 
                      timeBgn <= as.POSIXct("2020-11-15 00:00", tz="GMT"))
sub$pred <- predicted[ #same index as sub
  which(sub[1, "timeBgn"] == df_res$timeBgn): 
  which(sub[nrow(sub), "timeBgn"] == df_res$timeBgn) 
]

plot <- ggplot(sub, aes(x = timeBgn, y = rediduals_no_phi)) +
  geom_line(color = "darkblue", size = 0.2) +
  labs(x = "date", 
       y = "residuals after z-scored annual model fit", 
       title = paste(site, ml, "- zoomed in daily fit")) +
  theme_minimal()
 

#display summary(fit) on plot: a, f, and phi estimates and pr for each
plot + 
  geom_line(aes(y = pred), color = "orange", size = 1) +
  labs(caption = paste0(
          "a = ", round(coef(fit)["a"], 3), ", p-value = ", signif(summary(fit)$coefficients["a", "Pr(>|t|)"], 3), "\n",
          "f = ", round(coef(fit)["f"], 3), ", p-value = ", signif(summary(fit)$coefficients["f", "Pr(>|t|)"], 3), "\n",
          "phi = ", round(coef(fit)["phi"], 3), ", p-value = ", signif(summary(fit)$coefficients["phi", "Pr(>|t|)"], 3), "\n")) +
  theme(plot.caption = element_text(hjust = 0, vjust = 1))
  
  
#save ggplot
ggsave(paste0(wd, "/media/fitting_02/", site, "_", ml, "_daily_fit_zoomed.jpeg"), width = 8, height = 6)


# weekly/meso
#same thing as daily but cycle through 5-14 day periods

site <- "HARV"
ml <- "top"        #10 or "top"

df_res<- read.csv(paste0(wd, "/data/output/fitting_NEONdata_results_", site, "_", ml, ".csv"))
df_res$timeBgn <- ifelse(nchar(df_res$timeBgn) == 10,       # length of "YYYY-MM-DD"
                         paste0(df_res$timeBgn, " 00:00:00"), # append midnight
                         df_res$timeBgn)
df_res$timeBgn <- as.POSIXct(df_res$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")

 
meso <- c(5:14)
meso_fits <- data.frame(
  period = meso,
  amp = NA,
  per = NA,
  phi = NA,
  amp_p_value = NA,
  per_p_value = NA,
  phi_p_value = NA
)

for (i in 1:length(meso)) {
  period <- meso[i] #days
  x <- df_res$elapsed_days
  y <- df_res$residuals_phi
  a <- (max(y)-min(y))/2 #amplitude
  f <- 1/period #convert period to frequency
  phi <- 0 #phase offset
  model <- y ~ a * sin(2*pi*f*x + phi)
  fit <- nls(model, 
             start = list(
             a = a,
             f = f,
             phi = phi
           ), 
           control = nls.control(maxiter = 5000, warnOnly = TRUE))
  meso_fits[i, "amp"] <- coef(fit)["a"]
  meso_fits[i, "per"] <- 1/coef(fit)["f"]
  meso_fits[i, "phi"] <- coef(fit)["phi"]
  meso_fits[i, "amp_p_value"] <- signif(summary(fit)$coefficients["a", "Pr(>|t|)"], 3)
  meso_fits[i, "per_p_value"] <- signif(summary(fit)$coefficients["f", "Pr(>|t|)"], 3)
  meso_fits[i, "phi_p_value"] <- signif(summary(fit)$coefficients["phi", "Pr(>|t|)"], 3)
}

# save meso fits as a csv
write.csv(meso_fits, paste0(wd, "/data/output/meso_fits_", site, "_", ml, ".csv"), row.names = FALSE)

mf_all <- read.csv(paste0(wd, "/data/output/meso_fits_all.csv"))

# plot
predicted <- predict(fit)

sub <- subset(df_res, timeBgn >= as.POSIXct("2023-04-01 00:00", tz="GMT") & 
                timeBgn <= as.POSIXct("2023-09-01 00:00", tz="GMT"))
sub$pred <- predicted[ #same index as sub
  which(sub[1, "timeBgn"] == df_res$timeBgn): 
    which(sub[nrow(sub), "timeBgn"] == df_res$timeBgn) 
]

plot <- ggplot(sub, aes(x = timeBgn, y = rediduals_no_phi)) +
  geom_line(color = "darkblue", size = 0.2) +
  labs(x = "date", 
       y = "residuals after z-scored annual model fit", 
       title = paste0(site, "_", ml, " - summer months meso fit (", period, " day period)")) +
  theme_minimal()



plot + 
  geom_line(aes(y = pred), color = "orange", size = 1)

ggsave(paste0(wd, "/media/fitting_02/", site, "_", ml, "_", period, "day_fit.jpeg"), width = 8, height = 6)


summary(fit)

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




