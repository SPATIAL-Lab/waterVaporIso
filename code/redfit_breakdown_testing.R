#redfitMinls <- function...
# https://github.com/OpenDendro/dplR/blob/master/R/redfit.R


t <- times
x <- values

# params
txOrdered = FALSE
tType = 'time'
tTime <- tType == "time"



# clean up x and t
## lines 192 - 287
{
if (is.double(x)) {
  x2 <- x
} else {
  x2 <- as.numeric(x)
}
np <- as.numeric(length(x2))
tGiven <- !missing(t)
if (tGiven) {
  if (is.double(t)) {
    t2 <- t
  } else {
    t2 <- as.numeric(t)
  }
  if (length(t2) != np) {
    stop("lengths of 't' and 'x' must match")
  }
} else {
  t2 <- as.numeric(seq_len(np))
}
naidx <- is.na(x2)
if (tGiven) {
  naidx <- naidx | is.na(t2)
}
if (any(naidx)) {
  goodidx <- which(!naidx)
  t2 <- t2[goodidx]
  x2 <- x2[goodidx]
  nporig <- np
  np <- as.numeric(length(x2))
  nna <- nporig - np
  warning(sprintf(ngettext(nna,
                           "%.0f NA value removed",
                           "%.0f NA values removed",
                           domain = "R-dplR"), nna), domain = NA)
}
MIN_POINTS <- 3
if (np < MIN_POINTS) {
  stop(gettextf("too few points (%.0f), at least %.0f needed",
                np, MIN_POINTS, domain = "R-dplR"), domain = NA)
}
duplT <- FALSE
if (tGiven && !txOrdered) {
  idx <- order(t2)
  t2 <- t2[idx]
  x2 <- x2[idx]
  dupl <- duplicated(t2)
  if (any(dupl)) {
    duplT <- TRUE
    if (tTime) {
      warning("Duplicate times in 't', averaging data")
    } else {
      warning("Duplicate ages in 't', averaging data")
    }
    if (verbose) {
      if (tTime) {
        cat(gettext("Number of duplicates by time,\n",
                    domain = "R-dplR"), file = stderr())
      } else {
        cat(gettext("Number of duplicates by age,\n",
                    domain = "R-dplR"), file = stderr())
      }
      cat(gettext("'k' duplicates means 'k + 1' total observations:\n",
                  domain = "R-dplR"), file = stderr())
      dtable <- table(t2[dupl])
      if (tTime) {
        dtable <- data.frame(time = as.numeric(names(dtable)),
                             duplicates = as.vector(dtable))
      } else {
        dtable <- data.frame(age = as.numeric(names(dtable)),
                             duplicates = as.vector(dtable))
      }
      write.table(dtable, row.names = FALSE, file = stderr())
    }
    notdupl <- !dupl
    nunique <- sum(notdupl)
    xnew <- numeric(nunique)
    currentid <- 1
    currentstart <- 1
    for (k in 2:np) {
      if (notdupl[k]) {
        xnew[currentid] <- mean(x2[currentstart:(k - 1)])
        currentid <- currentid + 1
        currentstart <- k
      }
    }
    if (currentid == nunique) {
      xnew[nunique] <- mean(x2[currentstart:np])
    }
    t2 <- t2[notdupl]
    x2 <- xnew
    np <- as.numeric(nunique)
    if (np < MIN_POINTS) {
      stop(gettextf("too few points (%.0f), at least %.0f needed",
                    np, MIN_POINTS, domain = "R-dplR"), domain = NA)
    }
  }
}
}

## lines 291 - 299
## dplR: The rest of the function assumes that t2 is age, not time ## DOES THIS MESS SOMETHING UP IF THE ASSUMPTION IS WRONG??
## see line 1590 in redfit for a correction eplanation 
if (tTime) {
  t2 <- -rev(t2)
  x2 <- rev(x2)
}
if (tGiven) {
  difft <- diff(t2)
} else {
  difft <- rep.int(1.0, np)
}

## lines 300 - 317:   define params from initial function input
## lines 318 - 325:   find spectrum


## lines 326 - 341:   estimate rho!!!
## dplR: estimate lag-1 autocorrelation coefficient unless prescribed
if (is.null(rhopre) || rhopre < 0) {
  rho <- redfitGetrho(t2, x2, dn50, nseg, segskip, lmfitfun) # This is where the function was breaking. Fix it by just putting in an initial rho estimate.
  ## make sure that tau is non-negative
  if (rho > 1) {
    warning(gettext("redfitGetrho returned rho = %f, forced to zero",
                    rho, domain = "R-dplR"),
            domain = NA)
    rho <- 0
  }
} else {
  rho <- rhopre 
}
## dplR: determine tau from rho.
## Avoids the rho -> tau -> rho mess of REDFIT.
tau <- as.numeric(-avgdt / log(rho))

# to get an estimate of rho, use lines 277 - 295 in this script to get an idea of what value to use

#
#


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

###

redfitTauest <- function(t, x) {
  np <- length(t)
  ## Correct time direction; assume that ages are input
  ## dplR: Correction of time direction is done by modifying this
  ## function and redfitMinls, not by explicitly reversing (and
  ## multiplying by one)
  #tscal <- -rev(t)
  #xscal <- rev(x)
  ## Scaling of x
  xscal <- x / sd(x)
  ## Scaling of t (=> start value of a = 1/e)
  dt <- (t[np] - t[1]) / (np - 1)
  ## dplR: rhoest() of REDFIT is now an "inline function" of two
  ## lines + comment line:
  ## Autocorrelation coefficient estimation (equidistant data)
  xscalMNP <- xscal[-np]
  rho <- sum(xscalMNP * xscal[-1]) / sum(xscalMNP * xscalMNP)
  if (rho <= 0) {
    rho <- 0.05
    warning("rho estimation: <= 0")
  } else if (rho > 1) {
    rho <- 0.95
    warning("rho estimation: > 1")
  }
  scalt <- -log(rho) / dt
  tscal <- t * scalt
  ## Estimation
  minRes <- redfitMinls(tscal, xscal)
  amin <- minRes[["amin"]]
  mult <- minRes[["nmu"]]
  warnings <- FALSE
  if (mult) {
    warning("estimation problem: LS function has > 1 minima")
    warnings <- TRUE
  }
  if (amin <= 0) {
    warning("estimation problem: a_min =< 0")
    warnings <- TRUE
  } else if (amin >= 1) {
    warning("estimation problem: a_min >= 1")
    warnings <- TRUE
  }
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

#
# CPER whole dataset fails with "estimation problem: a_min >= 1"
# tested above using whole dataset (no segmenting) and amin = 0.383
# code below is using the above functions but with the segmenting
#

n50 = floor((max(times)-min(times))/45)
nseg <- round(np / (n50 + 1) * 2) 
#segkip:
if (n50 == 1) {
  segskip <- 0
} else {
  ## dplR: (ideal, not rounded) difference between starting indices of
  ## consecutive segments
  segskip <- (np - nseg) / (n50 - 1)
  if (segskip < 1) {
    stop("too many segments: overlap of more than nseg - 1 points")
  }
}

redfitGetrho <- function(t, x, n50, nseg, segskip, lmfitfun) {
  np <- as.numeric(length(x))
  nseg2 <- as.numeric(nseg)
  segskip2 <- as.numeric(segskip)
  rhovec <- numeric(n50)
  twkM <- matrix(1, nseg2, 2)
  for (i in as.numeric(seq_len(n50))) {
    ## copy data of (i+1)'th segment into workspace
    iseg <- .Call(dplR.seg50, i, nseg2, segskip2, np) #seg50 is a c function: dplR/src/redfit.c
    # iseg is just  segment (i.e. first segment = indices 1-3139 in data)
    # IMPORTANT: segments are in the index space, whereas tau/rho are in time space. So the correct number of points will be present
    # per data segment, but maybe not the correct about of time.
    iseg <- 12557:15695 #asked Claude to do the seg50 c function for me. 
    twk <-t[iseg]     #t should be the scaled one
    twkM[, 2] <- twk
    xwk <-x[iseg]     #x should be the scaled one
    ## detrend data
    xwk <- do.call(lmfitfun, list(twkM, xwk))[["residuals"]] #not doing this in my run-through; data is already detrended enough I think
    ## estimate and sum rho for each segment
    rho <- redfitTauest(twk, xwk) ### THIS IS EXACTLY WHERE THE FUNCTION FAILS
    ## bias correction for rho (Kendall & Stuart, 1967; Vol. 3))
    rhovec[8] <- (rho * (nseg2 - 1) + 1) / (nseg2 - 4)
  }
  ## average rho
  mean(rhovec)
}

plot(twk, xwk, type = "l")

x <- xwk
t <- twk
