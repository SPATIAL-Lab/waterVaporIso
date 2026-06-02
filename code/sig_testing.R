# Significance testing using dplR package
#
# The redfit function in dplR is a part of Schulz’s REDFIT (version 3.8e) program 
# and estimates the red-noise spectrum of a time series (Schulz & Mudelsee, 2002) 
# with optional testing of that spectrum against a red-noise background using 
# Monte Carlo simulations.

#install.packages("dplR")
library(dplR)

site <- "HARV"
ml <- "top"        #10 or "top"

# load data from fitting_01_NEONdata
df <- read.csv(paste0("data/output/fitting_NEONdata_results_", site, "_", ml, ".csv"))

df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")

# get times and values for function
times <- df$elapsed_days 
values <- df$residuals_phi

#segment data
window <- 1200 #days

df$elapsed_days[which(df$timeBgn == "2020-01-01 00:00:00")] #to get idx of a specific date
idx <-  1037  #window start elapsed day

segment_times <- times[which(times == idx):which(times == (idx + window))] #get times and values for that segment
segment_values <- values[which(times ==idx):which(times == (idx + window))] #get times and values for that segment

plot(segment_times, segment_values, 
     #type = "l", 
     cex = 0.8, pch = 19, 
     main = paste("Segment starting", df$timeBgn[which(times == idx)]), 
     xlab = "Elapsed Days", ylab = "iso_pre-zeroed")




# 
redf.dat <- redfit(x = segment_values, t = segment_times, 
                   n50 = floor((max(segment_times)-min(segment_times))/20), #this will be close to 20-day windows, but with 50% overlap...
                   nsim = 100)

par(tcl = 0.5, mar = rep(2.2, 4), mgp = c(1.1, 0.1, 0),xaxs="i")


plot(1/redf.dat[["freq"]][7:200], redf.dat[["gxxc"]][7:200], #7:100 ensures only periods between ~20 and ~1 are included
     ylim = range(redf.dat[["ci99"]]/2, redf.dat[["gxxc"]]),
     type = "n", ylab = "Spectrum", xlab = "Period (days)",
     main = paste0(window, "-day segment starting ", df$timeBgn[which(times == idx)]),
     axes = FALSE)
grid()
lines(1/redf.dat[["freq"]][7:200], redf.dat[["gxxc"]][7:200], col = "black",lwd=1.5)
lines(1/redf.dat[["freq"]][7:200], smooth.spline(redf.dat[["ci99"]][7:200],spar = 0.8)$y, col = "#D95F02")
lines(1/redf.dat[["freq"]][7:200], smooth.spline(redf.dat[["ci95"]][7:200],spar = 0.8)$y, col = "#7570B3")
lines(1/redf.dat[["freq"]][7:200], smooth.spline(redf.dat[["ci90"]][7:200],spar = 0.8)$y, col = "#E7298A")
freqs <- pretty(1/redf.dat[["freq"]][7:200])
pers <- round(1 / freqs, 2)
axis(1, at = freqs, labels = TRUE)
axis(2)
legend("topright", c("dat", "CI99", "CI95", "CI90"), lwd = 2,
       col = c("black", "#D95F02", "#7570B3", "#E7298A"),
       bg = "white")
box()


# test out wavelet
out.wave <- morlet(y1 = segment_values, x1 = segment_times,
                   siglvl = 0.75)
wavelet.plot(out.wave, useRaster=NA, reverse.y = TRUE)
