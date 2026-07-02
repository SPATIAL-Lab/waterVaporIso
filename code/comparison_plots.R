# variable plots

library(ggplot2)

site <- "SCBI"

# use raw values, not those with annual trend removed
iso <- read.csv(paste0("data/iso/iso_", site, "_release2026.csv"))
met <- read.csv(paste0("data/met/met_", site, "_release2026.csv"))

#subset level
ml <- "top"   # choose measurement level: 1, "pres", or "top"

#reduce to chosen measurement level
if (ml == 1) {
  iso <- subset(iso, verticalPosition %in% 10)
  met <- subset(met, verticalPosition %in% 10)
  rh <- subset(rh, verticalPosition %in% 0)
} else if (ml == "top") {
  iso <- subset(iso, verticalPosition %in% max(iso$verticalPosition))
  met <- subset(met, verticalPosition %in% max(met$verticalPosition))
  rh <- subset(rh, verticalPosition %in% max(rh$verticalPosition))
} else if (ml == "pres"){
  iso <- subset(iso, verticalPosition %in% max(iso$verticalPosition)) #top ML for pressure
  met <- subset(met, !verticalPosition %in% c(10, max(met$verticalPosition))) #not top or bottom, leftover should be where the barometer is
  rh <- subset(rh, verticalPosition %in% max(rh$verticalPosition))
} else {
  print("no ml")
}

# iso vs temp (top)
model <- lm(iso$dlta18OH2o ~ met$tempAirTop)
r2 <- round(summary(model)$r.squared, 3)

ggplot(iso, aes(x = met$tempAirTop, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "blue") +
  labs(x = "Temperature (C)", y = "dlta18OH2O", title = paste0(site, ", ", ml)) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = -20, y = -20, label = paste0("R^2 = ", r2), color = "red") +
  theme_minimal()


# iso vs radiLwOut
model <- lm(iso$dlta18OH2o ~ met$radiLwOut)
r2 <- round(summary(model)$r.squared, 3)

ggplot(iso, aes(x = met$radiLwOut, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "blue") +
  labs(x = "radiLwOut", y = "dlta18OH2O", title = paste0(site, ", ", ml)) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = 200, y = -20, label = paste0("R^2 = ", r2), color = "red") +
  theme_minimal()


# iso vs temp (bottom)
model <- lm(iso$dlta18OH2o ~ met$veloXaxsYaxsErth)
r2 <- round(summary(model)$r.squared, 3)

ggplot(iso, aes(x = met$veloXaxsYaxsErth, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "blue") +
  labs(x = "veloXaxsYaxsErth", y = "dlta18OH2O", title = paste0(site, ", ", ml)) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = 200, y = -20, label = paste0("R^2 = ", r2), color = "red") +
  theme_minimal()


# iso vs RH
model <- lm(iso$dlta18OH2o ~ rh$RHMean)
r2 <- round(summary(model)$r.squared, 3)

ggplot(iso, aes(x = rh$RHMean, y = dlta18OH2o)) +
  geom_point(size = 0.8, color = "blue") +
  labs(x = "RH", y = "dlta18OH2O", title = paste0(site, ", ", ml)) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = 0, y = -5, label = paste0("R^2 = ", r2), color = "red") +
  theme_minimal()
