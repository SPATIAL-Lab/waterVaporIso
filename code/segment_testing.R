# Lomb-Scargle (a least squares method) with:
#    - Segmented data
#    - Discrete periods to fit
#    - Defined step for beginning of each segment
#    - Some sort of significance testing (or False Alarm Probability)


library(ggplot2)
library(dplyr)
library(lubridate)

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
times <- df$elapsed_days 
values <- df$residuals_phi


#segment data

window <- 60 #days
step <- 2 #days
#t_start <- seq(min(times), max(times) - window, by = step) #the starting date for each window

#pick a segment to look at
df$elapsed_days[which(df$timeBgn == "2023-04-01 00:00:00")] #to get idx of a specific date
idx <-  2220  #window start elapsed day   #or use t_start[11] #example: 3 = third window

segment_times <- times[which(times == idx):which(times == (idx + window))] #get times and values for that segment
segment_values <- values[which(times ==idx):which(times == (idx + window))] #get times and values for that segment

threshold <- length(segment_times) > (window * 48)*.5 #are at least 1/2 the points present?

plot(segment_times, segment_values, cex = 0.8, pch = 19, 
     main = paste("Segment starting at day", idx, "\nAre at least 1/2 of the points present?", threshold), 
     xlab = "Elapsed Days", ylab = "iso_pre-zeroed")




#apply LS to that segment, with discrete periods of interest
periods <- 1 #days

lomb_scargle_discrete <- function(segment_times, segment_values, periods) {
  y <- segment_values - mean(segment_values) #this is used in L-S equation
  powers <- numeric(length(periods))
  
  for (i in seq_along(periods)) { #for each specified period, compute the power
    omega <- 2 * pi / periods[i]
    
    # Compute tau 
    tan2tau <- sum(sin(2 * omega * segment_times)) / sum(cos(2 * omega * segment_times)) #*2 is already in omega, and it's also needed here
    tau     <- atan(tan2tau) / (2 * omega)
    
    # cos and sin terms
    ct <- cos(omega * (segment_times - tau))
    st <- sin(omega * (segment_times - tau))
    
    # LS power
    A <- sum(y * ct)^2 / sum(ct^2)
    B <- sum(y * st)^2 / sum(st^2)
    
    var_y       <- sum(y^2) #I've been using 'press' with lsp. This is 'standard'
    #var_y       <- var(y) #this is effectively the same as sum(y^2) but at a different scale
    powers[i]   <- (A + B) / (2 * var_y)
  }
  data.frame(
    period = periods,
    power  = powers
  )
}


results <- lomb_scargle_discrete(segment_times, segment_values, periods)
results

#plot results
ggplot(results, aes(x = period, y = power)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = paste("Lomb-Scargle Periodogram for Segment Starting at Day", idx),
       x = "Period (days)",
       y = "Power")


#compare to lsp function from package 'lomb'
library(lomb)
lsp_result <- lsp(segment_values, 
                  times = segment_times, 
                  from = min(periods), 
                  to = max(periods), 
                  type = "period", 
                  normalize = 'standard')
lsp_result
getpeaks(lsp_result)
