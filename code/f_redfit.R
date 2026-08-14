# redfit functions

######## rho calculation

get_rho <- function (times, values, winlength) {
  
  np       <- length(values)
  n50      <- floor((max(times) - min(times)) / winlength)
  nseg2    <- as.numeric(round(np / (n50 + 1) * 2))
  segskip2 <- (np - nseg2) / (n50 - 1)
  rhovec   <- numeric(n50)

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
    xscalMNP <- xscal[-np]
    rho <- sum(xscalMNP * xscal[-1]) / sum(xscalMNP * xscalMNP)
    scalt <- -log(rho) / dt
    tscal <- t * scalt
    ## Estimation
    minRes <- redfitMinls(tscal, xscal)
    amin <- minRes[["amin"]]
    mult <- minRes[["nmu"]]
    warnings <- FALSE
    if (!warnings) {
      ## determine tau
      tau <- -1 / (scalt * log(amin))
      ## determine rho, corresponding to tau
      exp(-dt / tau)
    } else {
      ## dplR: fail early
      ##stop("error in tau estimation")
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
  rhotrue <- mean(rhovec)
  return(rhotrue)
}






plot_redfit <- function(redf.dat, times, site, start_end, max_period = 20) {
  
  freq_plot <- which(redf.dat[["freq"]] > 1 / max_period)
  
  par(tcl = 0.5, mar = rep(2.2, 4), mgp = c(1.1, 0.1, 0), xaxs = "i")
  
  plot(1/redf.dat[["freq"]][freq_plot], redf.dat[["gxxc"]][freq_plot], 
       ylim = range(redf.dat[["ci99"]]/2, redf.dat[["gxxc"]][freq_plot]),
       type = "n", ylab = "Spectrum", xlab = "Period (days)",
       #main = paste0(window, "-day segment starting ", df$timeBgn[which(times == idx)]),
       main = paste0(site," top, ", start_end[1], " - ", start_end[2],
                     "\n~", 
                     floor((max(times)-min(times))/redf.dat[["params"]][["n50"]]),
                     "-day windows, nsim =", redf.dat[["params"]][["nsim"]],
                     ", rhopre =", signif(redf.dat[["params"]][["rhopre"]], 3)
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
  legend("bottomright", c("dat", "CI99", "CI95", "CI90"), lwd = 2,
         col = c("black", "#D95F02", "#7570B3", "#E7298A"),
         bg = "white", cex = 0.7)
  box()
}
