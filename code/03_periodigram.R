# Lomb-Scargle periodigram

#install.packages("lomb")
library(lomb)

wd <- getwd()

sitech <- "YELL" # choose site

df <- read.csv(paste0(wd, "/data/iso_", sitech, "_release2024.csv"))

#remove NAs from isotope data
df <- df[!is.na(df$data.isoH2o.dlta18OH2o.mean),]

#choose measurement level
unique(df$verticalPosition)
ml <- 10
df <- subset(df, verticalPosition %in% ml)

#subset only relevant data (time, data)
df <- df[,c("timeBgn", "data.isoH2o.dlta18OH2o.mean")]

#create numeric dates
df$timeBgn <- ifelse(nchar(df$timeBgn) == 10,       # length of "YYYY-MM-DD"
                           paste0(df$timeBgn, " 00:00:00"), # append midnight
                           df$timeBgn)
df$timeBgn <- as.POSIXct(df$timeBgn, format="%Y-%m-%d %H:%M:%S", tz="GMT")
df$hours <- as.numeric(df$timeBgn) /60/60  # seconds to hours
df$elapsed <- df$hours - df$hours[1] # elapsed hours

plot(x = df$timeBgn, y = df$data.isoH2o.dlta18OH2o.mean, pch = 19, cex = 0.3)

mean(diff(df$timeBgn)) # avg time interval (probably used in lsp())

#full data
lsp_full <- lsp(df[,c(4,2)], 
                type = "period", 
                to = (nrow(df)/2))
getpeaks(lsp_full)
summary(lsp_full)

#one year of data
df_year <- subset(df, timeBgn >= as.POSIXct("2022-01-01 00:00", tz="GMT") & 
                  timeBgn <= as.POSIXct("2023-01-01 00:00", tz="GMT"))
df_year$elapsed <- df_year$hours - df_year$hours[1] # elapsed from start of year
plot(x = df_year$timeBgn, y = df_year$data.isoH2o.dlta18OH2o.mean, pch = 19, cex = 0.3)
mean(diff(df_year$timeBgn))

lsp_year <- lsp(df_year[,c(4,2)], 
            type = "period", 
            to = nrow(df_year)/2, 
            ofac = 1)
getpeaks(lsp_year)
summary(lsp_year)
pershow(lsp_year)

#couple of months of fairly complete data
df_mos <- subset(df, timeBgn >= as.POSIXct("2020-12-25 00:00", tz="GMT") & 
                   timeBgn <= as.POSIXct("2021-03-01 00:00", tz="GMT"))
plot(x = df_mos$timeBgn, y = df_mos$data.isoH2o.dlta18OH2o.mean, pch = 19, cex = 0.3)
df_mos$elapsed <- df_mos$hours - df_mos$hours[1] # elapsed from start of time period
mean(diff(df_mos$timeBgn))

lsp_mos <- lsp(df_mos[,c(4,2)], 
             type = "period", 
             to = nrow(df_mos)/2, 
             ofac = 1)
getpeaks(lsp_mos)
summary(lsp_mos)



