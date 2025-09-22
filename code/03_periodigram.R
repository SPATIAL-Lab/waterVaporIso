# Lomb-Scargle periodigram

#install.packages("lomb")
library(lomb)

wd <- getwd()

sitech <- "YELL" #choose site

df <- read.csv(paste0(wd, "/data/iso_", sitech, "_release2024.csv"))

#remove NAs from isotope data
df <- df[!is.na(df$data.isoH2o.dlta18OH2o.mean),]

#choose measurement level
unique(df$verticalPosition)
ml <- 10
df <- subset(df, verticalPosition %in% ml)

#in lsp, x must be only 2 columns (time, data)
df <- df[,c("timeBgn", "data.isoH2o.dlta18OH2o.mean")]

#convert timeBgn to numeric
df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                           paste0(df$timeBgn, " 00:00:00"), # append midnight
                           df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")
df$timeBgn <- as.numeric(df$timeBgn)


lsp(df, 
    from = 1/(365*24), #starting (lowest) frequency ( = 1 cycle per year (converted to hours))
    to = 1,  #highest frequency to analyze (1 = 1 cycle per hour)
    type = "frequency", 
    ofac = 4, #oversampling factor (usually 2-4)
    alpha = 0.05, 
    plot = T)

