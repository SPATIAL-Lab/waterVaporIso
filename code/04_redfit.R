##
# SPECTRAL FITTING AND SIGNIFICANCE TESTING USING dplR
#
# The redfit function in dplR is a part of Schulz’s REDFIT (version 3.8e) program 
# and estimates the red-noise spectrum of a time series (Schulz & Mudelsee, 2002) 
# with optional testing of that spectrum against a red-noise background using 
# Monte Carlo simulations.
#
# Subset the data first to only the length of time between 1/1/2021 and 6/30/2025 that has the most complete data
# Then, divide the data into seasons
# 


library(dplR)
library(dplyr)
library(lubridate)


source("code/f_redfit.R")


site <- "CPER"
ml <- "top"        #10 or "top"

tz = "GMT"


# load cleaned data
df <- read.csv(paste0("data/residuals/iso_", site, "_top_residuals.csv"))

df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz=tz)



## full data: ignore years that have spotty coverage
plot(df$timeBgn, df$dlta18O_residual, cex = 0.3, pch = 19, main = site) 

# data coverage per year
yearly_coverage <- df %>%
  mutate(year = year(timeBgn)) %>%
  group_by(year) %>%
  summarise(
    n_obs        = n(),
    n_expected   = 365 * 48,          # expected half-hourly obs per year
    pct_coverage = (n_obs / n_expected) * 100
  ) %>%
print()

# manually subset the full dataset to capture the best coverage years
best <- subset(df, timeBgn >= "2021-01-01 00:00:00" & timeBgn <= "2024-07-01 00:00:00") # full data = timeBgn >= "2021-01-01 00:00:00" & timeBgn <= "2025-06-30 00:00:00"

start_end <- c(format(min(best$timeBgn), "%m/%Y"), format(max(best$timeBgn), "%m/%Y"))


##
# get times and values for function
times  <- best$elapsed_days
times  <- times - times[1]

values <- best$dlta18O_residual


##
# get a good pre-estimate of rho via functions taken from redfit
rhotrue <- get_rho(times, values, winlength = 30)


# run redfit with true rho
redf.dat <- redfit(x = values, t = times, 
                   n50 = floor((max(times)-min(times))/30), # number is approx how many days per segment, 50% overlap. n50 = number of segments.
                   nsim = 100, 
                   rhopre = rhotrue,
                   iwin = "hanning", #this is the default
                   verbose = TRUE)


png(paste0("plots/redfit/", site, "_bestcoverageyears.png"), width = 1000, height = 800, res = 150)

plot_redfit(redf.dat, times, site, start_end)

dev.off() #save plot


# with ggplot
{plot_df <- data.frame(
  period = 1 / redf.dat[["freq"]][freq_plot],
  gxxc   = redf.dat[["gxxc"]][freq_plot],
  ci99   = smooth.spline(redf.dat[["ci99"]][freq_plot], spar = 0.8)$y,
  ci95   = smooth.spline(redf.dat[["ci95"]][freq_plot], spar = 0.8)$y,
  ci90   = smooth.spline(redf.dat[["ci90"]][freq_plot], spar = 0.8)$y
)


ggplot(plot_df, aes(x = period)) +
  geom_line(aes(y = gxxc, colour = "dat"),  linewidth = 0.7) +
  geom_line(aes(y = ci99, colour = "CI99"), linewidth = 0.7) +
  geom_line(aes(y = ci95, colour = "CI95"), linewidth = 0.7) +
  geom_line(aes(y = ci90, colour = "CI90"), linewidth = 0.7) +
  scale_colour_manual(
    name   = NULL,
    values = c("dat"  = "black",
               "CI99" = "#D95F02",
               "CI95" = "#7570B3",
               "CI90" = "#E7298A"),
    breaks = c("dat", "CI99", "CI95", "CI90")   # controls legend order
  ) +
  scale_y_continuous(
    limits = range(redf.dat[["ci99"]] / 2, redf.dat[["gxxc"]][freq_plot])
  ) +
  labs(
    title    = paste0(site, " top, ", start_end[1], " - ", start_end[2]),
    subtitle = paste0("~", floor((max(times) - min(times)) / redf.dat[["params"]][["n50"]]),
                      "-day windows, nsim = ", redf.dat[["params"]][["nsim"]],
                      ", rhopre = ",           redf.dat[["params"]][["rhopre"]]),
    x        = "Period (days)",
    y        = "Spectrum"
  ) +
  theme_minimal()+#base_size = 13) +
  theme(
    #panel.grid.minor = element_blank(),
    legend.position  = "right",
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 9, colour = "gray40")
  )}






######################

# subset by season

# seasonal coverage per year
seasonal_coverage <- df %>%
  mutate(year   = year(timeBgn),
         season = case_when(
           month(timeBgn) %in% c(1, 2) ~ "DJF",
           month(timeBgn) %in% c(3, 4, 5)  ~ "MAM",
           month(timeBgn) %in% c(6, 7, 8)  ~ "JJA",
           month(timeBgn) %in% c(9, 10, 11, 12) ~ "SON"
         )) %>%
  group_by(year, season) %>%
  summarise(pct_coverage = (n() / (90 * 48)) * 100) %>%
  arrange(year, factor(season, levels = c("DJF", "MAM", "JJA", "SON"))) %>%
  print()

# manually subset by season
{
JJA_21   <- subset(df, timeBgn >= as.POSIXct("2021-06-01", tz = tz) & timeBgn < as.POSIXct("2021-09-01", tz = tz))
DJF_21   <- subset(df, timeBgn >= as.POSIXct("2021-12-01", tz = tz) & timeBgn < as.POSIXct("2022-03-01", tz = tz))
JJA_22   <- subset(df, timeBgn >= as.POSIXct("2022-06-01", tz = tz) & timeBgn < as.POSIXct("2022-09-01", tz = tz))
DJF_22   <- subset(df, timeBgn >= as.POSIXct("2022-12-01", tz = tz) & timeBgn < as.POSIXct("2023-03-01", tz = tz))
JJA_23   <- subset(df, timeBgn >= as.POSIXct("2023-06-01", tz = tz) & timeBgn < as.POSIXct("2023-09-01", tz = tz))
DJF_23   <- subset(df, timeBgn >= as.POSIXct("2023-12-01", tz = tz) & timeBgn < as.POSIXct("2024-03-01", tz = tz))
JJA_24   <- subset(df, timeBgn >= as.POSIXct("2024-06-01", tz = tz) & timeBgn < as.POSIXct("2024-09-01", tz = tz))
DJF_24   <- subset(df, timeBgn >= as.POSIXct("2024-12-01", tz = tz) & timeBgn < as.POSIXct("2025-03-01", tz = tz))

summer_21 <- subset(df, timeBgn >= as.POSIXct("2021-04-01", tz = tz) & timeBgn < as.POSIXct("2021-10-01", tz = tz))
winter_21 <- subset(df, timeBgn >= as.POSIXct("2021-10-01", tz = tz) & timeBgn < as.POSIXct("2022-04-01", tz = tz))
summer_22 <- subset(df, timeBgn >= as.POSIXct("2022-04-01", tz = tz) & timeBgn < as.POSIXct("2022-10-01", tz = tz))
winter_22 <- subset(df, timeBgn >= as.POSIXct("2022-10-01", tz = tz) & timeBgn < as.POSIXct("2023-04-01", tz = tz))
summer_23 <- subset(df, timeBgn >= as.POSIXct("2023-04-01", tz = tz) & timeBgn < as.POSIXct("2023-10-01", tz = tz))
winter_23 <- subset(df, timeBgn >= as.POSIXct("2023-10-01", tz = tz) & timeBgn < as.POSIXct("2024-04-01", tz = tz))
summer_24 <- subset(df, timeBgn >= as.POSIXct("2024-04-01", tz = tz) & timeBgn < as.POSIXct("2024-10-01", tz = tz))
winter_24 <- subset(df, timeBgn >= as.POSIXct("2024-10-01", tz = tz) & timeBgn < as.POSIXct("2025-04-01", tz = tz))
}

season <- winter_23

start_end <- c(format(min(season$timeBgn), "%m/%Y"), format(max(season$timeBgn), "%m/%Y"))

##
# get times and values for function
times  <- season$elapsed_days
times  <- times - times[1]

values <- season$dlta18O_residual

winlength <- 30

##
# get a good pre-estimate of rho via functions from redfit
rhotrue <- get_rho(times, values, winlength)

# run redfit again with true rho
redf.dat <- redfit(x = values, t = times, 
                   n50 = floor((max(times)-min(times))/winlength),
                   nsim = 100, 
                   rhopre = rhotrue,
                   iwin = "hanning", 
                   verbose = TRUE)

# plot
plot_redfit(redf.dat, times, site, start_end)


##






layout(matrix(c(
  1, 2, 5, 7,
  3, 4, 6, 0
), nrow = 2, byrow = T))

layout.show(5)
par(mfrow = c(1, 1))






### CLAUDE's for loop for seasons:

# ── Options ───────────────────────────────────────────────────────────────────
do_facet  <- TRUE
col_name  <- "dlta18O_residual"   # change to whichever variable you want

# ── Define seasons ────────────────────────────────────────────────────────────
season_list <- list(
  DJF21   = list(start = "2021-12-01", end = "2022-03-01"),
  JJA22   = list(start = "2022-06-01", end = "2022-09-01"),
  DJF22   = list(start = "2022-12-01", end = "2023-03-01"),
  JJA23   = list(start = "2023-06-01", end = "2023-09-01"),
  DJF23   = list(start = "2023-12-01", end = "2024-03-01"),
  JJA24   = list(start = "2024-06-01", end = "2024-09-01"),
  DJF24   = list(start = "2024-12-01", end = "2025-03-01"),
  winter21 = list(start = "2021-10-01", end = "2022-04-01"),
  summer22 = list(start = "2022-04-01", end = "2022-10-01"),
  winter22 = list(start = "2022-10-01", end = "2023-04-01"),
  summer23 = list(start = "2023-04-01", end = "2023-10-01")
)

# ── Functions (source these from your functions script) ───────────────────────
run_redfit_season <- function(season_df, col_name) {
  
  times  <- season_df$elapsed_days
  times  <- times - times[1]
  values <- season_df[[col_name]]
  
  if (length(values) < 10 || sd(values, na.rm = TRUE) == 0) {
    cat("  Skipping — too few points or zero variance\n")
    return(NULL)
  }
  
  # Pre-estimate rho
  np       <- length(values)
  n50      <- max(1, floor((max(times) - min(times)) / 30))
  nseg2    <- as.numeric(round(np / (n50 + 1) * 2))
  segskip2 <- (np - nseg2) / max(1, n50 - 1)
  rhovec   <- numeric(n50)
  
  for (i in seq_len(n50)) {
    iseg <- seg50(i, nseg2, segskip2, np)
    twk  <- times[iseg]
    xwk  <- values[iseg]
    rho  <- tryCatch(redfitTauest(twk, xwk), error = function(e) NA)
    if (!is.na(rho)) {
      rhovec[i] <- (rho * (nseg2 - 1) + 1) / (nseg2 - 4)
    }
  }
  
  rhotrue <- mean(rhovec, na.rm = TRUE)
  if (is.nan(rhotrue) || is.na(rhotrue)) rhotrue <- 0.9
  cat("  rhotrue:", round(rhotrue, 3), "\n")
  
  # Run redfit
  redf.dat <- tryCatch(
    redfit(x = values, t = times,
           n50    = n50,
           nsim   = 100,
           rhopre = rhotrue,
           iwin   = "hanning",
           verbose = FALSE),
    error = function(e) { cat("  redfit failed:", conditionMessage(e), "\n"); NULL }
  )
  
  list(redf = redf.dat, times = times, rhotrue = rhotrue)
}

make_base_plot <- function(redf.dat, times, label) {
  plot(1/redf.dat[["freq"]][freq_plot], redf.dat[["gxxc"]][freq_plot], 
       ylim = range(redf.dat[["ci99"]]/2, redf.dat[["gxxc"]][freq_plot]),
       type = "n", ylab = "Spectrum", xlab = "Period (days)",
       #main = paste0(window, "-day segment starting ", df$timeBgn[which(times == idx)]),
       main = paste0(site, " — ", label,
                     "\n~", floor((max(times) - min(times)) /
                                    redf.dat[["params"]][["n50"]]),
                     "-day windows, nsim =", redf.dat[["params"]][["nsim"]],
                     ", rho =", round(redf.dat[["params"]][["rhopre"]], 3)),
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

# ── Main loop ─────────────────────────────────────────────────────────────────
par(tcl = 0.5, mar = rep(2.2, 4), mgp = c(1.1, 0.1, 0), xaxs = "i")

if (do_facet) {
  # calculate grid dimensions
  n_plots <- length(season_list)
  n_col   <- 4
  n_row   <- ceiling(n_plots / n_col)
  par(mfrow = c(n_row, n_col))
}

for (nm in names(season_list)) {
  
  cat("\n── Season:", nm, "──\n")
  
  seas <- season_list[[nm]]
  season_df <- subset(df, timeBgn >= as.POSIXct(seas$start, tz = "GMT") &
                        timeBgn <  as.POSIXct(seas$end,   tz = "GMT"))
  
  if (nrow(season_df) < 10) {
    cat("  Skipping — not enough data\n")
    if (do_facet) plot.new()   # keep grid layout intact
    next
  }
  
  res <- run_redfit_season(season_df, col_name)
  
  if (is.null(res) || is.null(res$redf)) {
    cat("  Skipping — redfit failed\n")
    if (do_facet) plot.new()
    next
  }
  
  make_base_plot(res$redf, res$times, label = nm)
}

# Reset layout after facet
if (do_facet) par(mfrow = c(1, 1))









#### TESTS ####


## test out wavelet
#
#out.wave <- morlet(y1 = segment_values, x1 = segment_times,
#                   siglvl = 0.75)
#wavelet.plot(out.wave, useRaster=NA, reverse.y = TRUE)
##



## segment data
#
#window <- 600 #days
#
#df$elapsed_days[which(df$timeBgn == "2020-01-01 00:00:00")] #to get idx of a specific date
#idx <-  1050  #window start elapsed day
#
#segment_times <- times[which(times == idx):which(times == (idx + window))] #get times and values for that segment
#segment_values <- values[which(times ==idx):which(times == (idx + window))] #get times and values for that segment
#
#plot(times, values, 
#     #type = "l", 
#     cex = 0.3, pch = 19, 
#     main = paste("Segment starting", df$timeBgn[which(times == idx)]), 
#     xlab = "Elapsed Days", ylab = "iso_pre-zeroed")
##



## clean vs unclean
#
#unclean <- read.csv(paste0("data/iso/iso_", site, "_release2026.csv"))
#clean <- read.csv(paste0("data/iso/clean/iso_", site, "_top_clean.csv"))
#
#unclean <- subset(unclean, verticalPosition %in% max(unclean$verticalPosition))
#
#df <- unclean
#df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
#                     paste0(df$timeBgn, " 00:00:00"), # append midnight
#                     df$timeBgn)
#df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")
#unclean <- df
#
#df <- clean
#df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
#                     paste0(df$timeBgn, " 00:00:00"), # append midnight
#                     df$timeBgn)
#df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")
#clean <- df
#
#unclean_times <- as.numeric(unclean$timeBgn) /60/60/24
#unclean_times <- unclean_times - unclean_times[1]
#clean_times <- as.numeric(clean$timeBgn) /60/60/24
#clean_times <- clean_times - clean_times[1]
#
#unclean_values <- unclean$dlta18OH2o
#clean_values <- clean$dlta18OH2o
#
#times <- clean_times

