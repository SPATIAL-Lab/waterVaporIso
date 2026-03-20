# Psuedo-wavelet with Lomb-Scargle
# Exploratory methods to analyze weekly signals after the annual cycle is taken out (via fitting_01_NEONdata)
# Steps:
#    1. Define window, step, and periods
#    2. Fit periods to each window (using modified L-S) and create a master df
#       - modified L-S = I want discrete frequencies sampled, not continuous 
#    3. Plot heatmap (similar to Wavelet)

library(ggplot2)
library(dplyr)



site <- "HARV"
ml <- "top"        #10 or "top"

# load data from fitting_01_NEONdata
df <- read.csv(paste0("data/output/fitting_NEONdata_results_", site, "_", ml, ".csv"))

df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                         paste0(df$timeBgn, " 00:00:00"), # append midnight
                         df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")

origin <- min(df$timeBgn) #timeBgn must be in posix for this

# get times and values for function
times = df$elapsed_days 
values = df$residuals_phi


# create modified L-S function for discrete periods
lomb_scargle_discrete <- function(times, values, periods) {
  # Normalize (subtract mean)
  y <- values - mean(values)
  n <- length(times) #when windowing, this should just be the window length
  powers <- numeric(length(periods))
  
  for (i in seq_along(periods)) {
    omega <- 2 * pi / periods[i]
    
    # Compute the phase offset tau (Barning/Lomb formulation)
    tan2tau <- sum(sin(2 * omega * times)) / sum(cos(2 * omega * times))
    tau     <- atan(tan2tau) / (2 * omega)
    
    # Shifted time
    ct <- cos(omega * (times - tau))
    st <- sin(omega * (times - tau))
    
    # LS power
    A <- sum(y * ct)^2 / sum(ct^2)
    B <- sum(y * st)^2 / sum(st^2)
    
    var_y       <- sum(y^2) #I've been using 'press' with lsp. This is 'standard'
    powers[i]   <- (A + B) / (2 * var_y)
  }
  data.frame(
    period = periods,
    power  = powers
  )
}

# eventually add white/red noise (probably red?) for significance testing?

# loop through all 20-day windows, stepping every 2 days
# create df at the end with power of each period per window

sliding_ls <- function(times, values, periods,
                       window = 20, step = 2) {
  
  t_start <- seq(min(times), max(times) - window, by = step)
  
  results <- lapply(t_start, function(t0) {
    idx <- times >= t0 & times < (t0 + window)
    
    # Skip windows with too few points to fit. Every point sampled would be = window * 48
    if (sum(idx) < (window * 24) / 2) return(NULL) # want at least 1/2 of the days to be present?
    
    res        <- lomb_scargle_discrete(times[idx], values[idx], periods)
    res$t_mid  <- t0 + window / 2   # label window by its center time
    res
  })
  
  bind_rows(results)
}

my_periods <- c(1, 5:14)

ls_map <- sliding_ls(
  times = times, 
  values = values, 
  periods = my_periods, #days
  window = 20, 
  step = 2)

# all years on one plot:
ggplot(ls_map, aes(x = origin + t_mid*86400, y = factor(period), fill = power)) +
  scale_x_datetime(date_labels = "%m-%Y",
                   date_breaks = "1 year") +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#0d0221", "#1a0a4a", "#2d1b8e", "#5a4fcf",
                "#c084fc", "#f59e0b", "#fde68a"),
    name    = "LS Power"
  ) +
  scale_y_discrete(limits = rev(as.character(sort(my_periods)))) +
  labs(
    x     = "Date (window center)",
    y     = "Period",
    title = "Sliding Window Lomb-Scargle Periodogram"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid    = element_blank(),
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

# facet years:
library(lubridate)

# Add year and day-of-year columns to your results
ls_map$date   <- origin + ls_map$t_mid * 86400
ls_map$year   <- year(ls_map$date)
ls_map$doy    <- yday(ls_map$date)   # day of year (1–365) as shared x-axis

ggplot(ls_map, aes(x = doy, y = factor(period), fill = power)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("gray85", "gray60", "#b39ddb", "#7B2D8B", "darkblue", "darkorange"),
    name    = "LS Power"
  ) +
  scale_y_discrete(limits = rev(as.character(sort(my_periods)))) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")
  ) +
  facet_grid(year ~ ., switch = "y") +   # stack years vertically
  labs(
    x     = NULL,
    y     = "Period",
    title = paste0("Sliding Window Lomb-Scargle Periodogram \n", site, ", ", ml)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid      = element_blank(),
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    #strip.placement = "outside",         # year labels on left outside axis
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

#######################

# original testing with shorter times
# TEST A COUPLE OF MONTHS
#df_sub <- subset(df, date >= as.POSIXct("2022-06-01 00:00", tz="GMT") & 
#                          date <= as.POSIXct("2022-08-01 00:00", tz="GMT"))
# create elapsed column to make the windows easier to work with
#df_sub$elapsed_days <- as.numeric(df_sub$date) /60/60/24 #seconds to days
#df_sub$elapsed_days <- df_sub$elapsed_days - df_sub$elapsed_days[1]
# plot subset
#ggplot(df_sub, aes(x=elapsed_days, y=data)) +
#  geom_line() +
#  theme_minimal() +
#  labs(title=paste0(site, " ", ml, " residuals (subset)"),
#       x="Date", y="Residuals")
# create first window
#df_sub_1to20 <- subset(df_sub, elapsed_days >= 0 & elapsed_days <= 20)
#ggplot(df_sub_1to20, aes(x=elapsed_days, y = data)) +
#  geom_line()

#test lsd function
#results <- lomb_scargle_discrete(
#  times   = df_sub_1to20$elapsed_days,
#  values  = df_sub_1to20$data,
#  periods = my_periods
#)
#print(results)
