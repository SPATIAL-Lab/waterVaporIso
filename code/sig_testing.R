#significance testing using dplR package
#The redfit function in dplR is a port of Schulz’s REDFIT (version 3.8e) program 
#and estimates the red-noise spectrum of a time series (Schulz & Mudelsee, 2002) 
#with optional testing of that spectrum against a red-noise background using 
#Monte Carlo simulations.

install.packages("dplR")
library(dplR)

redf.dat <- redfit(x = segment_values, t = segment_times, nsim = 100)

par(tcl = 0.5, mar = rep(2.2, 4), mgp = c(1.1, 0.1, 0),xaxs="i")

plot(1/redf.dat[["freq"]][7:100], redf.dat[["gxxc"]][7:100],
     ylim = range(redf.dat[["ci99"]], redf.dat[["gxxc"]]),
     type = "n", ylab = "Spectrum", xlab = "Period (days)",
     main = "testing redfit() on a 60-day segment of data",
     axes = FALSE)
grid()
lines(1/redf.dat[["freq"]][7:100], redf.dat[["gxxc"]][7:100], col = "black",lwd=1.5)
lines(1/redf.dat[["freq"]][7:100], smooth.spline(redf.dat[["ci99"]][7:100],spar = 0.8)$y, col = "#D95F02")
lines(1/redf.dat[["freq"]][7:100], smooth.spline(redf.dat[["ci95"]][7:100],spar = 0.8)$y, col = "#7570B3")
lines(1/redf.dat[["freq"]][7:100], smooth.spline(redf.dat[["ci90"]][7:100],spar = 0.8)$y, col = "#E7298A")
freqs <- pretty(1/redf.dat[["freq"]][7:100])
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
