# Lomb-Scargle periodogram

#install.packages("lomb")
library(lomb)

wd <- getwd()

###### Testing the artificial data ######

df<- read.csv(paste0(wd, "/data/test.csv")) # 24 and 168 hr (7 day) cycles
plot(x = df[,1], y = df[,9], pch = 19, cex = 0.3)

#subset
dfsub <- subset(df, elapsed >= 200 & elapsed <= 300)
plot(x = dfsub[,1], y = dfsub[,9], pch = 19, cex = 0.3) # "diurnal" cycle is very clear

mean(diff(df$elapsed)) # this is exactly what lsp uses after it removes NAs
pg <- lsp(df[,c(1,9)], type = "period", normalize = "press") # column 9 is the noisy+gapped data

getpeaks(pg) # two clear peaks where they should be
summary(pg)



# test_amp: noisier data and added a 50-hr cycle (24, 50, and 168 hr). Variable amplitudes in column 2, same amplitude in column 3.
df<- read.csv(paste0(wd, "/data/test_amp.csv"))
plot(x = df[,1], y = df[,2], pch = 19, cex = 0.3)

# test the variable amplitude data:
pg <- lsp(df[,c(1,2)], type = "period",
          normalize = "press") # column 2 = variable amplitudes
getpeaks(pg) # picking up a 90 hr cycle that I didn't create. 24hr is there but small

pg <- lsp(df[,c(1,2)], 
          type = "period", 
          ofac = 2, # changing ofac doesn't change the top 3 peaks too much with the variable amps
          normalize = "press") # press normalizes by variance
getpeaks(pg)


# test the same amplitude data: 
pg <- lsp(df[,c(1,3)], type = "period", 
          normalize = "press") # column 3 = same amplitudes
getpeaks(pg) # reduces the power of the 167 and 50 periods, and significantly lifts up the 24hr period power

pg <- lsp(df[,c(1,3)], 
          type = "period", 
          ofac = 2, # changing ofac raises the 24hr period power even more
          normalize = "press") # press normalizes by variance
getpeaks(pg)



# Normalize amplitude + ofac >1 = possibly can pick out the lower daily periods
library(zoo)
win <- 12 # the smaller the window, the better preserved the amplitudes
local <- rollapply(df[,2], win, sd, fill = NA, align = "center", na.rm = T, partial = T)
x_norm <- df[,2] / local

plot(x = df[,1], y = df[,2], pch = 19, cex = 0.2)
points(x = df[,1], y = x_norm, col = "red", pch = 19, cex = 0.3) 

dfsub <- subset(df, elapsed >= 400 & elapsed <= 500)
plot(x = dfsub[,1], y = dfsub[,2], type = "l")
points(x = dfsub[,1], y = x_norm[801:1001], col = "red", type = "l")

pg <- lsp(x_norm, times = df[,1], 
          type = "period", 
          ofac = 1, 
          normalize = "press") 
getpeaks(pg) 
# higher window (=48) loses the 24hr cycle entirely
# lower window (=12) preserves the 24hr cycle but doesn't amplify it


###### NEON data ######

sitech <- "CPER" # choose site

df <- read.csv(paste0(wd, "/data/iso_", sitech, "_release2024.csv"))

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
df$days <- as.numeric(df$timeBgn) /60/60/24  # seconds to hours


plot(x = df$timeBgn, y = df$data.isoH2o.dlta18OH2o.mean, pch = 19, cex = 0.3)

#full data
lsp_full <- lsp(df[,c(3,2)], #use the days column = 3 for time, column 2 = data
                type = "period", 
                normalize = "press")
getpeaks(lsp_full) 
summary(lsp_full)
pershow(lsp_full)

lsp_full <- lsp(df[,c(3,2)], #use the days column = 3 for time, column 2 = data
                type = "period", 
                normalize = "press", 
                to = 50) #cap the periods to look for



#one year of data
df_year <- subset(df, timeBgn >= as.POSIXct("2022-01-01 00:00", tz="GMT") & 
                  timeBgn <= as.POSIXct("2023-01-01 00:00", tz="GMT"))
plot(x = df_year$timeBgn, y = df_year$data.isoH2o.dlta18OH2o.mean, pch = 19, cex = 0.3)

lsp_year <- lsp(df_year[,c(3,2)], 
            type = "period", 
            normalize = "press", 
            ofac = 1) # 1 is default ofac
getpeaks(lsp_year)
summary(lsp_year)
pershow(lsp_year)

#couple of months
df_mos <- subset(df, timeBgn >= as.POSIXct("2022-05-01 00:00", tz="GMT") & 
                   timeBgn <= as.POSIXct("2022-07-01 00:00", tz="GMT"))
plot(x = df_mos$timeBgn, y = df_mos$data.isoH2o.dlta18OH2o.mean, type = "l")

lsp_mos <- lsp(df_mos[,c(3,2)], 
             type = "period", 
             normalize = "press", 
             ofac = 1)
getpeaks(lsp_mos)
summary(lsp_mos)

lsp_mos <- lsp(df_mos[,c(4,2)], 
               type = "period", 
               #to = nrow(df_mos)/2, 
               ofac = 4) #increase ofac to sample more frequencies
getpeaks(lsp_mos) # diurnal still doesn't come up but 6-10 day cycles do


#closer look at a few days
df_days <- subset(df, timeBgn >= as.POSIXct("2022-02-20 00:00", tz="GMT") & 
                   timeBgn <= as.POSIXct("2022-03-01 00:00", tz="GMT"))
plot(x = df_days$timeBgn, y = df_days$data.isoH2o.dlta18OH2o.mean, type = "l")


lsp_days <- lsp(df_days[,c(3,2)], 
               type = "period",
               normalize = "press",
               ofac = 1)
getpeaks(lsp_days) 


