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

#PROMPT 1

jan22Weather <- weather %>%
  filter((month(weather$dateF) == 1)&(year(weather$dateF) == 2022))

#calculate rolling average
airMA <- list()

for(i in 1:8){
  airMA[i] <- mean(jan22Weather$AirTemp[1:i])
  }

#plot air temp for 8 values
 
plot(jan22Weather$dateF[1:8], 
     jan22Weather$AirTemp[1:8], 
     type = "b",
     pch = 19,
     xlab = "Jan 1 2022 Time",
     ylab = "Temperature")
#add rolling average data
points(jan22Weather$dateF[1:8],
       airMA,
       type = "b", 
       pch = 19,
       col= "skyblue1")
legend("topleft",
       c("15 min Air Temp", "Rolling Average"),
       col=c("black", "skyblue1"),
       pch=19, bty= "n")
     
#PROMPT 2

mayJune21 <- weather %>%
  filter((month(weather$dateF) == 5 | month(weather$dateF) == 6) & 
           (year(weather$dateF) == 2021))

mayJune21$sensorIssue <- ifelse(((hour(mayJune21$dateF) > 6) & (hour(mayJune21$dateF) < 20) & (mayJune21$SolRad == 0)),
                         TRUE,
                         FALSE)

ggplot(mayJune21,
       aes(dateF, sensorIssue))+
  geom_point()+
  geom_line()


#Prompt 3
#Discussed in class

#No issues with buildup or accumulation on the sensor because
#during daylight hours when solar radiation is expected there was 
#never a time in which none was found.

#-------------

#Homework Questions

#QUESTION 1

weather$qualityPrecip <- ifelse (weather$AirTemp > 0 & abs(weather$XLevel) < 2 & abs(weather$YLevel) < 2,
                                 weather$Precip,
                                 NA)

missingPrecip <- 0

for(i in 1:length(weather$Precip)){
  ifelse(is.na(weather$Precip[i]),
         missingPrecip <- missingPrecip +1,
         missingPrecip <- missingPrecip)
}

#QUESTION 2

weather$BatFlag <- ifelse(weather$BatVolt < 8.5, 1, 0)

#QUESTION 3

unrealistic <- function(x){
  x$bad <- ifelse(x$AirTemp > 40 | x$AirTemp< -25 | x$SolRad > 1000, TRUE,FALSE)
  obs <- x %>%
    filter(x$bad == TRUE)
  obs
}

unrealistic(weather)

#QUESTION 4

winter21 <- weather %>%
  filter(month(weather$dateF) >= 1 & month(weather$dateF) <= 3 & 
           year(weather$dateF) == 2021)

ggplot(winter21,
       aes(dateF, AirTemp))+
  geom_line()+
  xlab("Date")+
  ylab("Temperature")+
  theme_classic()

#QUESTION 5


