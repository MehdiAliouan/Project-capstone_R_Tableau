# Project_R
Cyclist_Bike
## installing requied packages
library(tidyverse)
library(lubridate)
library(ggplot2)
setwd("/Users/ElMehdi/Downloads/Projects/Case_Study_Bike")

## STEP 1: COLLECTING DATA
# UPLOAD DIVVY DATASETS(CSV FILES):
Q2_2019<-read_csv("Divvy_Trips_2019_Q2.csv")
Q3_2019<-read_csv("Divvy_Trips_2019_Q3.csv")
Q4_2019<-read_csv("Divvy_Trips_2019_Q4.csv")
Q1_2020<-read_csv("Divvy_Trips_2020_Q1.csv")

## STEP 2: WRANGLING THE DATA AND COMBINE IT INTO SINGLE FILE
colnames(Q2_2019)
colnames(Q3_2019)
colnames(Q4_2019)
colnames(Q1_2020)

# RENAME THE COLUMNS TO MAKE THEM CONSISTENT WITH Q1_2020
(Q4_2019 <- rename(Q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(Q3_2019 <- rename(Q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))


(Q2_2019 <- rename(Q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

# INSPECTION OF DATAFRAME LOOKING FOR LACK OF CONSISTENCY
str(Q2_2019)
str(Q3_2019)
str(Q4_2019)
str(Q1_2020)

# CONVERT RIDE_ID AND RIDEABLE_TYPE TO CHARACTER SO THEY CAN STACK CORRECTLY
Q4_2019 <-  mutate(Q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

Q3_2019 <-mutate(Q3_2019, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))

Q2_2019 <-mutate(Q2_2019, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))

# STACK INDIVIDUEL QUARTER DATAFRAMES INTO ONE BIG DATAFRAME
all_trips<-bind_rows(Q1_2020, Q2_2019, Q3_2019, Q4_2019)

all_trips<-all_trips%>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

## STEP 3: CLEAN UP AND THE DATA TO PREPARE FOR ANALYSIS
#   LET'S INSPECT THE NEW TABLE
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

## there are few problem to fix
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# In the "member_casual" column, we're gonna replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders, we will want to make our dataframe consistent with their current nomenclature
table(all_trips$member_casual)

all_trips<-all_trips%>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber"="member"
                                ,"Customer"="casual"))
table(all_trips$member_casual)

# (2) We will want to add some additional columns of data 
# -- such as day, month, year -- that provide additional opportunities to aggregate the data.
all_trips$date<-as.Date(all_trips$started_at)
all_trips$mounth<-format(as.Date(all_trips$date,"%m"))
all_trips$day<-format(as.Date(all_trips$date, "%d"))
all_trips$year<-format(as.Date(all_trips$date, "%y"))
all_trips$day_of_week<-format(as.Date(all_trips$date, "%A"))

# (3) Adding ride_length calculation
all_trips$ride_length<-difftime(all_trips$ended_at,all_trips$started_at)
str(all_trips)
# Convert "ride_length" from Factor to numeric
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove bad data 
# The dataframe includes a few hundred entries 
# when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# Step 4 ANALYSING THE DATA
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# Average ride time by each day for members vs casual members
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							
            ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday)

# Let's visualize the number of rides by rider type 
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# STEP 5: Save as csv for further analysis and visualization in Tableau

write_csv(all_trips_v2, "all_trips_v2.csv")

#total and average weekly rides by rider type
summary_ride_weekly <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  

write_csv(summary_ride_weekly, "summary_ride_weekly.csv")

#total and average weekly rides by rider type
summary_ride_weekly_type <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday, rideable_type) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  

write_csv(summary_ride_weekly_type, "summary_ride_weekly_type.csv")

#total and avg monthly rides by rider type
summary_month <- all_trips_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>%  
  group_by(month,member_casual) %>%  
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%    
  arrange(month, member_casual)

write_csv(summary_month, "summary_ride_monthly.csv")

#most popular stations
popular_stations <- all_trips_v2 %>%
  mutate(station = start_station_name) %>%
  drop_na(start_station_name) %>%
  group_by(start_station_name, member_casual) %>%
  summarise(number_of_rides=n())

write_csv(popular_stations, "popular_stations.csv")

#total membership types and rideable types
total_riders <- data.frame(table(all_trips_v2$member_casual))
total_types <- data.frame(table(all_trips_v2$rideable_type))

write_csv(total_riders, "total_riders.csv")
write_csv(total_types, "total_types.csv")



