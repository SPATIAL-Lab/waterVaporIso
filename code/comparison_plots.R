##
# 
# variable plots


library(ggplot2)
library(dplyr)
library(lubridate)




site <- "ORNL"

{
# use cleaned values, not those with annual trend removed
iso <- read.csv(paste0("data/iso/clean/iso_", site, "_top_clean.csv"))
met <- read.csv(paste0("data/met/met_", site, "_release2026.csv"))

# top and barometer levels only
met <- subset(met, verticalPosition %in% c(median(met$verticalPosition), max(met$verticalPosition)))

iso$verticalPosition <- NULL 

iso$timeBgn <- ifelse(nchar(iso$timeBgn) == 10,       # length of "YYYY-MM-DD"
                      paste0(iso$timeBgn, " 00:00:00"), # append midnight
                      iso$timeBgn)
iso$timeBgn <- as.POSIXct(iso$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")


# combine iso and met data to line up dates
pres <- pres[,c("timeBgn", "presAtm")]
temp <- temp[,c("timeBgn", "tempAirTop")]

df <- inner_join(iso, pres, by = "timeBgn")
df <- inner_join(df, temp, by = "timeBgn")


# separate into summer and winter
JJA <- df %>% filter(month(timeBgn) %in% c(6, 7, 8))
DJF <- df %>% filter(month(timeBgn) %in% c(12, 1, 2))



# ISO vs TEMP

model <- lm(dlta18OH2o ~ tempAirTop, data = df)
r2 <- round(summary(model)$r.squared, 3)
slope <- round(coef(model)[2], 3) 

# all data
ggplot(df, aes(x = tempAirTop, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "blue") +
  labs(x = "Temperature (C)", y = "dlta18OH2O", title = site) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.5, vjust = 2,
           label = paste0("R^2 = ", r2, "\nslope = ", slope), color = "red") +
  theme_minimal()

ggsave(paste0("plots/simple/", site, "_iso~temp.png"), width = 1000, height = 751, units = "px", dpi = 96)

# winter
model <- lm(dlta18OH2o ~ tempAirTop, data = DJF)
r2 <- round(summary(model)$r.squared, 3)
slope <- round(coef(model)[2], 3) 

ggplot(DJF, aes(x = tempAirTop, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "blue") +
  labs(x = "Temperature (C)", y = "dlta18OH2O", title = paste0(site, ", winter (DJF)")) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.5, vjust = 2,
           label = paste0("R^2 = ", r2, "\nslope = ", slope), color = "red") +
  theme_minimal()

ggsave(paste0("plots/simple/", site, "_iso~temp_winter.png"), width = 1000, height = 751, units = "px", dpi = 96)

# summer
model <- lm(dlta18OH2o ~ tempAirTop, data = JJA)
r2 <- round(summary(model)$r.squared, 3)
slope <- round(coef(model)[2], 3) 

ggplot(JJA, aes(x = tempAirTop, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "blue") +
  labs(x = "Temperature (C)", y = "dlta18OH2O", title = paste0(site, ", summer (JJA)")) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.5, vjust = 2,
           label = paste0("R^2 = ", r2, "\nslope = ", slope), color = "red") +
  theme_minimal()

ggsave(paste0("plots/simple/", site, "_iso~temp_summer.png"), width = 1000, height = 751, units = "px", dpi = 96)



# ISO vs SPECIFIC HUMIDITY

model <- lm(dlta18OH2o ~ q, data = df)
r2 <- round(summary(model)$r.squared, 3)
slope <- round(coef(model)[2], 3) 

# all data
ggplot(df, aes(x = q, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "orange3") +
  labs(x = "q", y = "dlta18OH2O", title = site) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.5, vjust = 2,
           label = paste0("R^2 = ", r2, "\nslope = ", slope), color = "red") +
  theme_minimal()

ggsave(paste0("plots/simple/", site, "_iso~q.png"), width = 1000, height = 751, units = "px", dpi = 96)

# winter
model <- lm(dlta18OH2o ~ q, data = DJF)
r2 <- round(summary(model)$r.squared, 3)
slope <- round(coef(model)[2], 3) 

ggplot(DJF, aes(x = q, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "orange3") +
  labs(x = "q", y = "dlta18OH2O", title = paste0(site, ", winter (DJF)")) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.5, vjust = 2,
           label = paste0("R^2 = ", r2, "\nslope = ", slope), color = "red") +
  theme_minimal()

ggsave(paste0("plots/simple/", site, "_iso~q_winter.png"), width = 1000, height = 751, units = "px", dpi = 96)

# summer
model <- lm(dlta18OH2o ~ q, data = JJA)
r2 <- round(summary(model)$r.squared, 3)
slope <- round(coef(model)[2], 3) 

ggplot(JJA, aes(x = q, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "orange3") +
  labs(x = "q", y = "dlta18OH2O", title = paste0(site, ", summer (JJA)")) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.5, vjust = 2,
           label = paste0("R^2 = ", r2, "\nslope = ", slope), color = "red") +
  theme_minimal()

ggsave(paste0("plots/simple/", site, "_iso~q_summer.png"), width = 1000, height = 751, units = "px", dpi = 96)

}



# ISO vs ATM PRESSURE

model <- lm(dlta18OH2o ~ presAtm, data = df)
r2 <- round(summary(model)$r.squared, 3)
slope <- round(coef(model)[2], 3) 

# all data
ggplot(df, aes(x = presAtm, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "lightblue") +
  labs(x = "presAtm", y = "dlta18OH2O", title = site) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.5, vjust = 2,
           label = paste0("R^2 = ", r2, "\nslope = ", slope), color = "red") +
  theme_minimal()

