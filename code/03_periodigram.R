# Lomb-Scargle periodigram

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
pg <- lsp(df[,c(1,9)], type = "period") # column 9 is the noisy+gapped data

getpeaks(pg) # two clear peaks where they should be
summary(pg)



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
df$hours <- as.numeric(df$timeBgn) /60/60  # seconds to hours
df$elapsed <- df$hours - df$hours[1] # elapsed hours

plot(x = df$timeBgn, y = df$data.isoH2o.dlta18OH2o.mean, pch = 19, cex = 0.3)

#full data
lsp_full <- lsp(df[,c(4,2)], 
                type = "period", 
                to = (nrow(df)/2))
getpeaks(lsp_full) #9477/24 = 394 days...
summary(lsp_full)

#one year of data
df_year <- subset(df, timeBgn >= as.POSIXct("2022-01-01 00:00", tz="GMT") & 
                  timeBgn <= as.POSIXct("2023-01-01 00:00", tz="GMT"))
df_year$elapsed <- df_year$hours - df_year$hours[1] # elapsed from start of year
plot(x = df_year$timeBgn, y = df_year$data.isoH2o.dlta18OH2o.mean, pch = 19, cex = 0.3)

lsp_year <- lsp(df_year[,c(4,2)], 
            type = "period", 
            to = nrow(df_year)/2, 
            ofac = 1) # 1 is default ofac
getpeaks(lsp_year)
summary(lsp_year)
pershow(lsp_year)

#couple of months
df_mos <- subset(df, timeBgn >= as.POSIXct("2022-05-01 00:00", tz="GMT") & 
                   timeBgn <= as.POSIXct("2022-07-01 00:00", tz="GMT"))
plot(x = df_mos$timeBgn, y = df_mos$data.isoH2o.dlta18OH2o.mean, pch = 19, cex = 0.3)
df_mos$elapsed <- df_mos$hours - df_mos$hours[1] # elapsed from start of time period

lsp_mos <- lsp(df_mos[,c(4,2)], 
             type = "period", 
             #to = nrow(df_mos)/2, 
             ofac = 1)
getpeaks(lsp_mos) # 243/24 = 10 day period
summary(lsp_mos)

lsp_mos <- lsp(df_mos[,c(4,2)], 
               type = "period", 
               #to = nrow(df_mos)/2, 
               ofac = 4) #increase ofac to sample more frequencies
getpeaks(lsp_mos) # diurnal still doesn't come up but 6-10 day cycles do


#closer look at a few days
df_days <- subset(df, timeBgn >= as.POSIXct("2022-05-20 00:00", tz="GMT") & 
                   timeBgn <= as.POSIXct("2022-06-01 00:00", tz="GMT"))
plot(x = df_days$timeBgn, y = df_days$data.isoH2o.dlta18OH2o.mean, pch = 19, cex = 0.3)

df_days$elapsed <- df_days$hours - df_days$hours[1] # elapsed from start of time period

lsp_days <- lsp(df_days[,c(4,2)], 
               type = "period", 
               ofac = 1)
getpeaks(lsp_days) # no clear diurnal period
