install.packages(c("dplyr","ggplot2","lubridate"))
library(dplyr)
library(ggplot2)
library(lubridate)


weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = c("#N/A", "NA"))

metadata <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv")

weather$dateF <- mdy_hm(weather$Date)

weather$dateET <- mdy_hm(weather$Date, tz = "America/New_York")

weatherCheck <- weather %>%
  filter(is.na(weather$dateET))

#IN ClASS ACTIVITY WORK

weather$dateF [2] %--% weather$dateF [3]
int_length(weather$dateF [2] %--% weather$dateF [3])

test <- weather$dateF[1:18]
test
test[-1]
  # as new var ==> test1 <- weather$dateF[2:18]

timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
}

timeCheck900(weather$dateF)

soilFiles <- list.files("/cloud/project/activity04/soil")
#set up variables to use in for-loop
soilList <- list()

for(i in 1:length(soilFiles)){
  soilList[[i]] <- read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
  }

str(soilList)

soilData <- do.call("rbind", soilList)

#calculate moving average
airMA <- numeric()

for(i in 8-length(weather$AirTemp)){
  airMA[1] <- mean(weather$AirTemp, (i-7))
}


#PROMPT 2

mayJune22 <- weather %>%
  filter(month(dateF > 5)| dateF < june2022 )


