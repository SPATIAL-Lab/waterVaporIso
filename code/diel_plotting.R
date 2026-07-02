# diel plotting
# 
# plot a week or so at a time with shaded areas indicating day and night
# plot without the annual trend removed

library(ggplot2)
library(suncalc)
library(lubridate)
library(httr)
library(jsonlite)

site <- "SCBI"

df <- read.csv(paste0("data/iso/iso_", site, "_release2026.csv"))
#df <- read.csv(paste0("data/met/met_", site, "_release2026.csv"))

df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")

unique(df$verticalPosition)
#df <- subset(df, verticalPosition %in% max(df$verticalPosition))

var <- "dlta18OH2o"

plot(df$timeBgn, df[,which(names(df) == var)], cex = 0.3, pch = 19, col = "blue", ylab = var)



# randomly choose 14 days to plot

random_day <- sample(unique(as.Date(df$timeBgn)), 1)
random_day <- as.POSIXct(random_day, format="%Y-%m-%d %H:%M:%S", tz="GMT")
start_end <- as.POSIXct(seq(random_day, by = "2 weeks", length.out = 2), format="%Y-%m-%d %H:%M:%S", tz="GMT")

weeks <- subset(df, timeBgn >= as.POSIXct(start_end[1]) & timeBgn <= as.POSIXct(start_end[2]))


# not random
#start_day <- as.POSIXct("2021-07-06 00:00:00", tz = "GMT")
#start_end <- as.POSIXct(seq(start_day, by = "2 weeks", length.out = 2), format="%Y-%m-%d %H:%M:%S", tz="GMT")
#weeks <- subset(df, timeBgn >= as.POSIXct(start_end[1]) & timeBgn <= as.POSIXct(start_end[2]))


# remove NAs from line plot
weeks <- weeks[!is.na(weeks[,which(names(df) == var)]), ]

# basic plot
plot(weeks$timeBgn, weeks[,which(names(df) == var)], cex = 0.3, pch = 19, col = "blue", ylab = var)

ggplot(weeks, aes(x = timeBgn, y = weeks[,which(names(df) == var)], color = factor(verticalPosition))) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("lightblue", "blue"), name = "ML") +
  labs(y = var)
 


{
# plot with day and night shading
# pull lat/long metadata

response <- GET(paste0("https://data.neonscience.org/api/v0/sites/", site))
meta     <- fromJSON(content(response, as = "text"))

lat <- meta$data$siteLatitude
lon <- meta$data$siteLongitude


# get sunrise/sunset for each date in the range 
dates <- seq(as.Date(min(weeks$timeBgn)),
             as.Date(max(weeks$timeBgn)),
             by = "day")

sun <- getSunlightTimes(
  date     = dates,
  lat      = lat,
  lon      = lon,
  keep     = c("sunrise", "sunset"),
  tz       = "GMT"
)

# build rectangles
night_rects <- data.frame(
  xmin = c(min(weeks$timeBgn), sun$sunset),
  xmax = c(sun$sunrise[1], c(sun$sunrise[-1], max(weeks$timeBgn)))
)

# clamp to plot range
night_rects$xmin <- pmax(night_rects$xmin, min(weeks$timeBgn))
night_rects$xmax <- pmin(night_rects$xmax, max(weeks$timeBgn))

# drop any zero-width rectangles
night_rects <- night_rects[night_rects$xmax > night_rects$xmin, ]


# plot
ggplot() +
  geom_rect(data = night_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "gray30", alpha = 0.3) +
  geom_line(data = weeks, 
            aes(x = timeBgn, y = weeks[,which(names(df) == var)], color = factor(verticalPosition)), 
            linewidth = 0.8) +
  #geom_point(data = weeks,
  #          aes(x = timeBgn, y = weeks[,which(names(df) == var)], color = factor(verticalPosition)), 
  #          size = 0.9) +
  scale_color_manual(values = c("lightblue", "blue"), name = "ML") +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%b %d") +
  labs(x = NULL, y = var,
       title = paste0(site, ", Start: ", dates[1])) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )
}

ggplot() +
  geom_rect(data = night_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "gray30", alpha = 0.3) +
  #geom_line(data = weeks, 
  #          aes(x = timeBgn, y = weeks[,which(names(df) == var)], color = factor(verticalPosition)), 
  #          linewidth = 0.8) +
  geom_point(data = weeks,
             aes(x = timeBgn, y = weeks[,which(names(df) == var)], color = factor(verticalPosition)), 
             size = 0.9) +
  scale_color_manual(values = c("lightblue", "blue"), name = "ML") +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%b %d") +
  labs(x = NULL, y = var,
       title = paste0(site, ", Start: ", dates[1])) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# save plot
ggsave(paste0("media/diel/", site, "_", dates[1], ".png"), width = 1000, height = 751, units = "px", dpi = 96)

