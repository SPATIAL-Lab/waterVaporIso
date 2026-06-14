# Significance testing using dplR package
#
# The redfit function in dplR is a part of Schulz’s REDFIT (version 3.8e) program 
# and estimates the red-noise spectrum of a time series (Schulz & Mudelsee, 2002) 
# with optional testing of that spectrum against a red-noise background using 
# Monte Carlo simulations.

#install.packages("dplR")
library(dplR)

site <- "CPER"
ml <- "top"        #10 or "top"

# load data from fitting_01_NEONdata
df <- read.csv(paste0("data/output/fitting_NEONdata_results_", site, "_", ml, ".csv"))

df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")

#use only 2021 and later data
#eventually delete this (will be used in 01_getData)
df <- subset(df, timeBgn >= as.POSIXct("2021-01-01 00:00", tz="GMT"))


# get times and values for function
times = df$elapsed_days - df$elapsed_days[1] #in case elapsed days doesn't start at 0
values <- df$residuals

## optional
#segment data
window <- 600 #days

df$elapsed_days[which(df$timeBgn == "2020-01-01 00:00:00")] #to get idx of a specific date
idx <-  1050  #window start elapsed day

segment_times <- times[which(times == idx):which(times == (idx + window))] #get times and values for that segment
segment_values <- values[which(times ==idx):which(times == (idx + window))] #get times and values for that segment

plot(times, values, 
     #type = "l", 
     cex = 0.3, pch = 19, 
     main = paste("Segment starting", df$timeBgn[which(times == idx)]), 
     xlab = "Elapsed Days", ylab = "iso_pre-zeroed")
##





## 
redf.dat <- redfit(x = values, t = times, 
                   n50 = floor((max(times)-min(times))/30), # number is approx how many days per segment, 50% overlap
                   nsim = 100, 
                   rhopre = 0.98, #starting estimate for tau calculation. Can get more precise estimate using lines 288 to 297 in redfit breakdown script (copied below)
                   iwin = "hanning", #this is the default
                   verbose = TRUE)


par(tcl = 0.5, mar = rep(2.2, 4), mgp = c(1.1, 0.1, 0),xaxs="i") #graphical params: smaller margins, ticks closer to axis, and x-axis intervals are "pretty"

1/redf.dat[["freq"]][1:30] #look at largest periods
freq_plot <- 10:1000 #exclude periods above ~20 days

{plot(1/redf.dat[["freq"]][freq_plot], redf.dat[["gxxc"]][freq_plot], #7:100 ensures only periods between ~20 and ~1 are included
     ylim = range(redf.dat[["ci99"]]/2, redf.dat[["gxxc"]][freq_plot]),
     type = "n", ylab = "Spectrum", xlab = "Period (days)",
     #main = paste0(window, "-day segment starting ", df$timeBgn[which(times == idx)]),
     main = paste0(site," ", ml, ", ~", 
                  floor((max(times)-min(times))/redf.dat[["params"]][["n50"]]),
                  "-day windows, nsim =", redf.dat[["params"]][["nsim"]],
                  ", rhopre =", redf.dat[["params"]][["rhopre"]]
                  ),
     axes = FALSE)
grid()
lines(1/redf.dat[["freq"]][freq_plot], redf.dat[["gxxc"]][freq_plot], col = "black",lwd=1.5)
lines(1/redf.dat[["freq"]][freq_plot], smooth.spline(redf.dat[["ci99"]][freq_plot],spar = 0.8)$y, col = "#D95F02")
lines(1/redf.dat[["freq"]][freq_plot], smooth.spline(redf.dat[["ci95"]][freq_plot],spar = 0.8)$y, col = "#7570B3")
lines(1/redf.dat[["freq"]][freq_plot], smooth.spline(redf.dat[["ci90"]][freq_plot],spar = 0.8)$y, col = "#E7298A")
freqs <- pretty(1/redf.dat[["freq"]][freq_plot])
pers <- round(1 / freqs, 2)
axis(1, at = freqs, labels = TRUE)
axis(2)
legend("topright", c("dat", "CI99", "CI95", "CI90"), lwd = 2,
       col = c("black", "#D95F02", "#7570B3", "#E7298A"),
       bg = "white")
box()
}

# test out wavelet
out.wave <- morlet(y1 = segment_values, x1 = segment_times,
                   siglvl = 0.75)
wavelet.plot(out.wave, useRaster=NA, reverse.y = TRUE)



###
## get a good pre-estimate of rho via functions from redfit (run above function first to get the parameters)

{
np <- redf.dat[["params"]][["np"]]
nseg2 <- redf.dat[["params"]][["nseg"]]
segskip2 <- redf.dat[["params"]][["segskip"]]
n50 <- redf.dat[["params"]][["n50"]]
rhovec <- numeric(n50)

segfirst <- function(k, segskip, np, nseg) {
  pmax(0, pmin(np - nseg, round(k * segskip)))
}

seg50 <- function(k, nseg, segskip, np) {
  segloc <- 1 + segfirst(k - 1, segskip, np, nseg)
  seq(segloc, segloc + nseg - 1)
}

redfitTauest <- function(t, x) {
  np <- length(t)
  ## Scaling of x
  xscal <- x / sd(x)
  ## Scaling of t (=> start value of a = 1/e)
  dt <- (t[np] - t[1]) / (np - 1)
  ## dplR: rhoest() of REDFIT is now an "inline function" of two
  ## lines + comment line:
  ## Autocorrelation coefficient estimation (equidistant data)
  xscalMNP <- xscal[-np]
  rho <- sum(xscalMNP * xscal[-1]) / sum(xscalMNP * xscalMNP)
  #if (rho <= 0) {
  #  rho <- 0.05
  #  warning("rho estimation: <= 0")
  #} else if (rho > 1) {
  #  rho <- 0.95
  #  warning("rho estimation: > 1")
  #}
  scalt <- -log(rho) / dt
  tscal <- t * scalt
  ## Estimation
  minRes <- redfitMinls(tscal, xscal)
  amin <- minRes[["amin"]]
  mult <- minRes[["nmu"]]
  warnings <- FALSE
  #if (mult) {
  #  warning("estimation problem: LS function has > 1 minima")
  #  warnings <- TRUE
  #}
  #if (amin <= 0) {
  #  warning("estimation problem: a_min =< 0")
  #  warnings <- TRUE
  #} else if (amin >= 1) {
  #  warning("estimation problem: a_min >= 1")
  #  warnings <- TRUE
  #}
  if (!warnings) {
    ## determine tau
    tau <- -1 / (scalt * log(amin))
    ## determine rho, corresponding to tau
    exp(-dt / tau)
  } else {
    ## dplR: fail early
    ##stop("error in tau estimation")
    # or what if we just assume tau = 0.9 estimation?
    tau <- 0.9
    exp(-dt / tau)
  }
}

redfitMinls <- function(t, x) { #x and t must be the scaled vectors
  ## Least-squares function
  lsfun <- function(a, difft, xM1, xMNP) {
    if (a > 0) {
      tmp <- xMNP - xM1 * a^difft
    } else if (a < 0) {
      tmp <- xMNP + xM1 * (-a)^difft
    } else {
      tmp <- xMNP
    }
    sum(tmp * tmp)
  }
  a_ar1 <- exp(-1) # 1 / e
  tol   <- 3e-8    # Brent's search, precision
  tol2  <- 1e-6    # multiple solutions, precision
  #ADDED IN TO USE SCALED X AND T:
  #t <- tscal
  #x <- xscal
  #
  difft <- diff(t)
  np <- length(x)
  xM1 <- x[-1] #remove 1st element
  xMNP <- x[-np] #remove last element
  opt1 <- optimize(lsfun, c(-2, 2),     tol = tol, difft = difft,
                   xM1 = xM1, xMNP = xMNP)
  opt2 <- optimize(lsfun, c(a_ar1, 2),  tol = tol, difft = difft,
                   xM1 = xM1, xMNP = xMNP)
  opt3 <- optimize(lsfun, c(-2, a_ar1), tol = tol, difft = difft,
                   xM1 = xM1, xMNP = xMNP)
  a_ar11 <- opt1[["minimum"]]
  a_ar12 <- opt2[["minimum"]]
  a_ar13 <- opt3[["minimum"]]
  dum1 <- opt1[["objective"]]
  dum2 <- opt2[["objective"]]
  dum3 <- opt3[["objective"]]
  list(amin = c(a_ar11, a_ar12, a_ar13)[which.min(c(dum1, dum2, dum3))],
       nmu = ((abs(a_ar12 - a_ar11) > tol2 && abs(a_ar12 - a_ar1) > tol2) || # && = both of these must be true
                (abs(a_ar13 - a_ar11) > tol2 && abs(a_ar13 - a_ar1) > tol2))) # || = either of these must be true
}


for (i in as.numeric(seq_len(n50))) {
  ## copy data of (i+1)'th segment into workspace
  iseg <- seg50(i, nseg2, segskip2, np)
  # iseg is the indices of the segment
  # IMPORTANT: segments are in the index space, whereas tau/rho are in time space. So the correct number of points will be present per data segment, but maybe not the correct about of time.
  twk <-times[iseg]
  xwk <-values[iseg]     #x should be the scaled one...
  ## detrend data
  #xwk <- do.call(lmfitfun, list(twkM, xwk))[["residuals"]] #not doing this; data is already detrended enough I think
  ## estimate and sum rho for each segment
  rho <- redfitTauest(twk, xwk) ### THIS IS EXACTLY WHERE THE FUNCTION FAILS
  ## bias correction for rho (Kendall & Stuart, 1967; Vol. 3))
  rhovec[i] <- (rho * (nseg2 - 1) + 1) / (nseg2 - 4)
}
## average rho
mean(rhovec)
}



#################################

# refit 2022 by season
#    djf and mam work. jja breaks with tau at any n50 value - not enough data in these 3-month spans
djf <- subset(df, format(timeBgn, "%m") %in% c("12", "01", "02") & format(timeBgn, "%Y") == "2022")
mam <- subset(df, format(timeBgn, "%m") %in% c("03", "04", "05") & format(timeBgn, "%Y") == "2022")
jja <- subset(df, format(timeBgn, "%m") %in% c("06", "07", "08") & format(timeBgn, "%Y") == "2022")
son <- subset(df, format(timeBgn, "%m") %in% c("09", "10", "11") & format(timeBgn, "%Y") == "2022")

redf.djf <- redfit(x = jja$residuals_phi, t = (jja$elapsed_days - min(jja$elapsed_days)), 
                   n50 = 12,
                   nsim = 1000)
par(tcl = 0.5, mar = rep(2.2, 4), mgp = c(1.1, 0.1, 0),xaxs="i")
{plot(1/redf.djf[["freq"]][3:200], redf.djf[["gxxc"]][3:200], #7:100 ensures only periods between ~20 and ~1 are included
      ylim = range(redf.djf[["ci99"]]/2, redf.djf[["gxxc"]]),
      type = "n", ylab = "Spectrum", xlab = "Period (days)",
      main = "JJA 2022",
      axes = FALSE)
  grid()
  lines(1/redf.djf[["freq"]][3:200], redf.djf[["gxxc"]][3:200], col = "black",lwd=1.5)
  lines(1/redf.djf[["freq"]][3:200], smooth.spline(redf.djf[["ci99"]][3:200],spar = 0.8)$y, col = "#D95F02")
  lines(1/redf.djf[["freq"]][3:200], smooth.spline(redf.djf[["ci95"]][3:200],spar = 0.8)$y, col = "#7570B3")
  lines(1/redf.djf[["freq"]][3:200], smooth.spline(redf.djf[["ci90"]][3:200],spar = 0.8)$y, col = "#E7298A")
  freqs <- pretty(1/redf.djf[["freq"]][3:200])
  pers <- round(1 / freqs, 2)
  axis(1, at = freqs, labels = TRUE)
  axis(2)
  legend("topright", c("dat", "CI99", "CI95", "CI90"), lwd = 2,
         col = c("black", "#D95F02", "#7570B3", "#E7298A"),
         bg = "white")
  box()
}


