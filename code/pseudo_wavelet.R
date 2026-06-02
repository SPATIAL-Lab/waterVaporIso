# Psuedo-wavelet with Lomb-Scargle
# Exploratory methods to analyze weekly signals after the annual cycle is taken out (via fitting_01_NEONdata)
# Steps:
#    1. Define window, step, and periods
#    2. Fit periods to each window (using modified L-S) and create a master df
#       - modified L-S = I want discrete frequencies sampled, not continuous 
#    3. Plot heatmap (similar to Wavelet)

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)


site <- "CPER"
ml <- "top"       #10 or "top"

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


# build modified L-S function for discrete periods
lomb_scargle_discrete <- function(times, values, periods) {
  y <- values - mean(values) #this is used in L-S equation; IS THIS PER SEGMENT OR WHOLE DATASET?
  powers <- numeric(length(periods))
  
  for (i in seq_along(periods)) { #for each specified period, compute the power
    omega <- 2 * pi / periods[i]
    
    # Compute tau (phase offset)
    tan2tau <- sum(sin(2 * omega * times)) / sum(cos(2 * omega * times)) #*2 is already in omega, and it's also needed here
    tau     <- atan(tan2tau) / (2 * omega)
    
    # cos and sin terms
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

# build sliding window function to apply modified L-S to each window
sliding_ls <- function(times, values, periods,
                       window, step) { #chose enough days to resolve 14-day periods, I think wavelet usually does a step of 1
  
  t_start <- seq(min(times), max(times) - window, by = step)
  
  results <- lapply(t_start, function(t0) {
    idx <- times >= t0 & times < (t0 + window)
    
    # Skip windows with too few points to fit. Every point sampled would be = window * 48
    if (sum(idx) < (window * 48) / 2) return(NULL) # want at least 1/2 of the days to be present?
    
   #THIS IS WHERE A HANNING WINDOW WILL GO
    
    res        <- lomb_scargle_discrete(times[idx], values[idx], periods)
    res$t_mid  <- t0 + window / 2   # label window by its center time
    res
  })
  
  bind_rows(results)
}

# define parameters
periods <- c(5:14)
window <- 45 #days
step <- 2 #days

ls_map <- sliding_ls(
  times = times, 
  values = values, 
  periods = periods, #days
  window = window,
  step = step
)


# facet years:
ls_map$date   <- origin + ls_map$t_mid * 86400
ls_map$year   <- year(ls_map$date)
ls_map$doy    <- yday(ls_map$date)   # day of year (1–365) as shared x-axis

ggplot(ls_map, aes(x = doy, y = factor(period), fill = power)) +
  geom_tile(aes(width = step)) + #width should equal step size
  scale_fill_gradientn(
    colours = c("gray85", "gray60", "#b39ddb", "#7B2D8B", "darkblue", "darkorange"),
    name    = "LS Power", 
    limits  = c(0.01, 0.4),
    oob     = scales::squish
  ) +
  scale_y_discrete(limits = rev(as.character(sort(periods)))) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")
  ) +
  facet_grid(year ~ ., switch = "y") +   # stack years vertically
  labs(
    x     = NULL,
    y     = "Period",
    title = paste0(window,"-day Window Lomb-Scargle Periodogram \n", site, ", ", ml)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid      = element_blank(),
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    #strip.placement = "outside",         # year labels on left outside axis
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


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
  scale_y_discrete(limits = rev(as.character(sort(periods)))) +
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
