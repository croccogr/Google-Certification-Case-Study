install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot")
install.packages("lubridate")
install.packages("rmarkdown")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)
library(rmarkdown)

dec_2022 <- read_csv("202212_trip_data.csv")
jan_2023 <- read_csv("202301_trip_data.csv")
feb_2023 <- read_csv("202302_trip_data.csv")

str(dec_2022)
str(jan_2023)
str(feb_2023)

last_quart <- bind_rows(dec_2022, jan_2023, feb_2023)
summary(last_quart)


last_quart <-  last_quart %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))
table(last_quart$member_casual)


last_quart$date <- as.Date(last_quart$started_at) #The default format is yyyy-mm-dd
last_quart$month <- format(as.Date(last_quart$date), "%m")
last_quart$day <- format(as.Date(last_quart$date), "%d")
last_quart$year <- format(as.Date(last_quart$date), "%Y")
last_quart$day_of_week <- format(as.Date(last_quart$date), "%A")

last_quart$ride_length <- difftime(last_quart$ended_at,last_quart$started_at)


last_quart$ride_length <- difftime(last_quart$ended_at,last_quart$started_at)
str(last_quart)

#Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(last_quart$ride_length)
last_quart$ride_length <- as.numeric(as.character(last_quart$ride_length))
is.numeric(last_quart$ride_length)

#Remove "bad data": from when bikes were taken out of service.
last_quart_v2 <- last_quart[!(last_quart$start_station_name == "HQ QR" | last_quart$ride_length<0),]


#ANALYSIS

#ride_length (in seconds)
summary(last_quart_v2$ride_length)

# Compare user type
aggregate(last_quart_v2$ride_length ~ last_quart_v2$member_casual, FUN = mean)
aggregate(last_quart_v2$ride_length ~ last_quart_v2$member_casual, FUN = median)
aggregate(last_quart_v2$ride_length ~ last_quart_v2$member_casual, FUN = max)

#average ride time by each day for members vs casual users
aggregate(last_quart_v2$ride_length ~ last_quart_v2$member_casual + last_quart_v2$day_of_week, FUN = mean)


#Order weekdays
last_quart_v2$day_of_week <- ordered(last_quart_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#analyze ridership data by type and weekday
last_quart_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()							 
            ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday)							


#visualize
library(ggplot2)
last_quart_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

last_quart_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
