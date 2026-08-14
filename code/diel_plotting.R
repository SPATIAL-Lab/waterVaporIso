# diel plotting
# 
# plot a week or so at a time with shaded areas indicating day and night
# plot with the annual trend removed

library(ggplot2)
library(suncalc)
library(lubridate)
library(httr)
library(jsonlite)
library(dplyr)

site <- "CPER"

iso <- read.csv(paste0("data/residuals/iso_", site, "_top_residuals.csv"))
pres <- read.csv(paste0("data/residuals/pres_", site, "_top_residuals.csv"))
temp <- read.csv(paste0("data/residuals/temp_", site, "_top_residuals.csv"))


for (nm in c("iso", "pres", "temp")) {
  df <- get(nm)
  df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                     paste0(df$timeBgn, " 00:00:00"), # append midnight
                     df$timeBgn)
  df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")
  assign(nm, df)
}

names(pres)[names(pres) == "residuals"] <- "pres_residual"
names(temp)[names(temp) == "residuals"] <- "temp_residual"


df <- inner_join(iso[,c(1,9,10,11,12)], pres[c(1,4)], by = "timeBgn")
df <- inner_join(df, temp[c(1,4)], by = "timeBgn")



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
#weeks <- weeks[!is.na(weeks[,which(names(df) == col)]), ]

# basic plot
ggplot(weeks, aes(x = timeBgn, y = dlta18O_residual)) +
  geom_line(size = 0.8)
  #scale_color_manual(values = c("lightblue", "blue"), name = "ML")
 
# variables to plot
names(weeks)

{
var1 <- "dlta18O_residual"
var2 <- "q_residual"
lab1 <- "d18O"
lab2 <- "sp. humidity"
col1 <- "blue"
col2 <- "orange3"
var3 <- "temp_residual"
lab3 <- "temp"
col3 <- "cyan3"
}

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
            aes(x = timeBgn, y = .data[[var1]], color = lab1),
            linewidth = 0.8) +
  geom_line(data = weeks,
            aes(x = timeBgn, y = .data[[var2]]*1000, color = lab2),
            linewidth = 0.8) +
  #geom_line(data = weeks,
  #          aes(x = timeBgn, y = .data[[var3]], color = lab3), 
  #          linewidth = 0.8) +
  scale_color_manual(
    name   = NULL,
    values = c("d18O" = col1, "sp. humidity" = col2)#, "temp" = col3)
  ) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%b %d") +
  labs(x = NULL, 
       y = "z-score",
       title = paste0(site, ", start: ", dates[1], "; correlation = ", signif(cor(weeks[[var1]], weeks[[var2]]), 4))) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank())
}


# save plot
ggsave(paste0("plots/diel/isoq_", site, "_", dates[1], ".png"), width = 1000, height = 751, units = "px", dpi = 96)

