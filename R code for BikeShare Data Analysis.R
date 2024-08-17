# Set working directory
setwd('C:/Users/syedh/documents')

# Installing necessary packages (you only need to do this once)
install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')
install.packages('dplyr')

# Loading the packages
library(tidyverse)
library(janitor)
library(lubridate)
library(dplyr)

# Importing the CSV files
Nov2023 <- read_csv("Divvy_TripData_Monthly/2023_11.csv")
Dec2023 <- read_csv("Divvy_TripData_Monthly/2023_12.csv")
Jan2024 <- read_csv("Divvy_TripData_Monthly/2024_01.csv")
Feb2024 <- read_csv("Divvy_TripData_Monthly/2024_02.csv")
Mar2024 <- read_csv("Divvy_TripData_Monthly/2024_03.csv")
Apr2024 <- read_csv("Divvy_TripData_Monthly/2024_04.csv")
May2024 <- read_csv("Divvy_TripData_Monthly/2024_05.csv")
Jun2024 <- read_csv("Divvy_TripData_Monthly/2024_06.csv")
Jul2024 <- read_csv("Divvy_TripData_Monthly/2024_07.csv")

# Checking the structure of datasets (optional)
str(Nov2023)
str(Dec2023)
str(Jan2024)
str(Feb2024)
str(Mar2024)
str(Apr2024)
str(May2024)
str(Jun2024)
str(Jul2024)

# Merging the datasets into one clean dataframe
df_combined <- bind_rows(Nov2023, Dec2023, Jan2024, Feb2024, Mar2024, Apr2024, May2024, Jun2024, Jul2024)

# Cleaning column names and removing empty rows/columns
df_combined <- df_combined %>%
  clean_names() %>%
  remove_empty(which = c("rows", "cols"))

# Convert datetime columns to POSIXct if not already
df_combined <- df_combined %>%
  mutate(
    started_at = as.POSIXct(started_at, format="%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format="%Y-%m-%d %H:%M:%S")
  )

# Calculate trip duration in hours and minutes
df_combined <- df_combined %>%
  mutate(
    Hours = as.numeric(difftime(ended_at, started_at, units="hours")),
    Minutes = as.numeric(difftime(ended_at, started_at, units="mins"))
  )

# Filter out trips with a duration of zero or negative hours
df <- df_combined %>% filter(Hours > 0)

# Removing rows with N/A values
cyclistic_data <- drop_na(df)

# View the first few rows of the cleaned dataframe
head(cyclistic_data)

# Extract the day of the week, month, and year
cyclistic_data <- cyclistic_data %>%
  mutate(
    started_at = as.POSIXct(started_at, format="%Y-%m-%d %H:%M:%S"),
    day_of_week = wday(started_at, label = TRUE, abbr = FALSE),
    month = month(started_at, label = TRUE, abbr = FALSE),
    year = year(started_at),
    date = as.Date(started_at)
  )

# Calculate basic summary statistics
summary_stats <- cyclistic_data %>%
  summarise(
    avg_trip_duration_hours = mean(Hours, na.rm=TRUE),
    median_trip_duration_hours = median(Hours, na.rm=TRUE),
    total_trips = n(),
    avg_trip_duration_seconds = mean(Minutes * 60, na.rm=TRUE)
  )

# Checking for the different types of members and bikes
unique(cyclistic_data$member_casual)
unique(cyclistic_data$rideable_type)

# Round two of removing null data (just to be thorough)
cyclistic_data <- drop_na(cyclistic_data)

# Creating a data frame to gather the station name, latitude and longitude to perform calculations
cyclistic_data_chunk <- cyclistic_data[, c(5, 9, 10)] #Here 5, 9, 10 are column numbers and before comma rows are specified, since we do not need any rows we left it blank

# Removing duplicates
cyclistic_data_chunk <- cyclistic_data_chunk[!duplicated(cyclistic_data_chunk$start_station_name),] 

# Number of unique stations
NROW(unique(cyclistic_data_chunk))

# Now calculating the length of rides and converting it to minutes

# Acquiring the difference between ending and starting time
cyclistic_data$ride_length <- difftime(cyclistic_data$ended_at, cyclistic_data$started_at)

# Dividing by 60 to acquire data in minutes
cyclistic_data$ride_length <- cyclistic_data$ride_length/60 

# Rounding data to 2 decimal places
cyclistic_data$ride_length <- round(cyclistic_data$ride_length, 2)

# Finding total ride length
summary(cyclistic_data$ride_length)

# Comparing the rider type by calculating mean, median, max, and min ride length for each group
rider_type_summary <- cyclistic_data %>%
  group_by(member_casual) %>%
  summarise(
    avg_ride_length = mean(ride_length),
    median_ride_length = median(ride_length),
    max_ride_length = max(ride_length),
    min_ride_length = min(ride_length)
  )

# Fixing the order of the days of the week and months for analysis
cyclistic_data$day_of_week <- ordered(cyclistic_data$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
cyclistic_data$month <- factor(cyclistic_data$month, 
                               levels=c("November", "December", 
                                        "January", "February", "March", 
                                        "April", "May", "June", "July"))

# Checking the aggregate of ride length by casual vs. members for each statistic
agg_mean <- aggregate(cyclistic_data$ride_length ~ cyclistic_data$member_casual, FUN = mean)
agg_median <- aggregate(cyclistic_data$ride_length ~ cyclistic_data$member_casual, FUN = median)
agg_max <- aggregate(cyclistic_data$ride_length ~ cyclistic_data$member_casual, FUN = max)
agg_min <- aggregate(cyclistic_data$ride_length ~ cyclistic_data$member_casual, FUN = min)

# Using mean function to aggregate data
agg_by_day <- aggregate(cyclistic_data$ride_length ~ cyclistic_data$day_of_week + cyclistic_data$member_casual, FUN=mean) 

# Aggregate rides by day of the week and member type, calculating the number of rides and average duration
cyclistic_data_by_day <- cyclistic_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    num_of_rides = n(),
    avg_duration = mean(ride_length, na.rm = TRUE)
  ) %>%
  arrange(member_casual, day_of_week)

# View the first few rows of the aggregated data
head(cyclistic_data_by_day)

# Turning off scientific notation for simplicity
options(scipen=999)

# Analyzing ridership data by type and weekday
cyclistic_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(num_of_rides = n(), avg_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = num_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge2") + 
  labs(title = "Total Number of Rides by Day", x = "Week Day", y = "Number of Rides")

# Differentiating between weekday and weekend casual rides
total_rides_casual_weekend <- NROW(filter(cyclistic_data, member_casual == "casual" & 
                                            (day_of_week == "Saturday" | day_of_week == "Sunday")))
total_rides_casual_weekend

total_rides_casual_weekday <- NROW(filter(cyclistic_data, member_casual == "casual" & 
                                            !(day_of_week == "Saturday" | day_of_week == "Sunday")))
total_rides_casual_weekday

# Percentage difference between weekday and weekend rides
week <- c("Weekday", "Weekend")
casual_week <- c(total_rides_casual_weekday, total_rides_casual_weekend)
piepercent <- round(100 * casual_week / sum(casual_week), 2)
weekride_casual <- paste(week, piepercent, "%", sep="")

print(weekride_casual)

# Ridership data by type and month
cyclistic_data %>%
  group_by(member_casual, month) %>%
  summarise(num_of_rides = n(), avg_duration = mean(ride_length)) %>%
  arrange(factor(month, levels=c("November", "December", "January", "February", "March", "April", "May", "June", "July")), member_casual) %>%
  ggplot(aes(x = month, y = num_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge2") + 
  labs(title = "Total Number of Rides by Month", x = "Month", y = "Number of Rides") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#Find casual riders in May-July vs rest of the year because traffic is highest between May-July

rides_casual_may_july <- NROW(filter(cyclistic_data, member_casual == "casual" & (month == "May" | month=="June" | month=="July")))
print(rides_casual_may_july)

rides_casual_rest <- NROW(filter(cyclistic_data, member_casual == "casual" & !(month == "May" | month=="June" | month=="July")))
print(rides_casual_rest)

#Percentage of casual

months <- c("July to September", "Rest of the Year")
casual_month <- c(rides_casual_july_sept, rides_casual_rest)
piepercent <- round(100 * casual_month / sum(casual_month), 1)
monthride <- paste(months, piepercent)
monthride_casual <- paste(monthride, "%", sep="")

print(monthride_casual)

# Calculate the percentage of casual riders in May-July vs. the rest of the year

months <- c("May to July", "Rest of the Year")
casual_month <- c(rides_casual_may_july, rides_casual_rest)
piepercent <- round(100 * casual_month / sum(casual_month), 1)
monthride <- paste(months, piepercent)
monthride_casual <- paste(monthride, "%", sep="")

print(monthride_casual)

# Find member riders in May-July vs. the rest of the year

rides_member_may_july <- NROW(filter(cyclistic_data, member_casual == "member" & (month == "May" | month == "June" | month == "July")))
print(rides_member_may_july)

rides_member_rest <- NROW(filter(cyclistic_data, member_casual == "member" & !(month == "May" | month == "June" | month == "July")))
print(rides_member_rest)

# Percentage of members

months <- c("May to July", "Rest of the Year")
member_month <- c(rides_member_may_july, rides_member_rest)
piepercent <- round(100 * member_month / sum(member_month), 1)
monthride <- paste(months, piepercent)
monthride_member <- paste(monthride, "%", sep="")

print(monthride_member)

# Create a new data frame to gather all member riders' data

cyclistic_data_member <- filter(cyclistic_data, member_casual == "member")

# Analyze member type by bike type and month

cyclistic_data_member %>%
  group_by(rideable_type, month) %>%
  summarise(num_of_rides = n(), avg_duration = mean(ride_length)) %>%
  arrange(rideable_type, month) %>%
  ggplot(aes(x = month, y = num_of_rides, fill = rideable_type)) + 
  geom_col(position = "dodge2") + 
  labs(title = "Total Number of Member Rides by Month", 
       x = "Month", 
       y = "Number of Member Rides") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Analyze member type by bike type and day of the week

cyclistic_data_member %>%
  group_by(rideable_type, day_of_week) %>%
  summarise(num_of_rides = n(), avg_duration = mean(ride_length)) %>%
  arrange(rideable_type, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = num_of_rides, fill = rideable_type)) + 
  geom_col(position = "dodge") + 
  labs(title = "Total Number of Member Rides by Day", 
       x = "Day", 
       y = "Number of Rides")

# Create a new data frame for casual riders

cyclistic_data_casual <- filter(cyclistic_data, member_casual == "casual")

# Analyze casual riders' data by rider type and month

cyclistic_data_casual %>%
  group_by(rideable_type, month) %>%
  summarise(num_of_rides = n(), avg_duration = mean(ride_length)) %>%
  arrange(rideable_type, month) %>%
  ggplot(aes(x = month, y = num_of_rides, fill = rideable_type)) + 
  geom_col(position = "dodge2") + 
  labs(title = "Total Number of Casual Rides by Month", 
       x = "Month", 
       y = "Number of Rides") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Analyze casual riders by rider type and day of the week

cyclistic_data_casual %>%
  group_by(rideable_type, day_of_week) %>%
  summarise(num_of_rides = n(), avg_duration = mean(ride_length)) %>%
  arrange(rideable_type, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = num_of_rides, fill = rideable_type)) + 
  geom_col(position = "dodge2") + 
  labs(title = "Number of Casual Riders by Day", 
       x = "Day", 
       y = "Number of Rides")












# Create columns with routes for casual riders

cyclistic_data_casual <- cyclistic_data_casual %>%
  mutate(route = paste(start_station_name, "to", sep=" ")) 

cyclistic_data_casual <- cyclistic_data_casual %>%
  mutate(route = paste(route, end_station_name, sep=" "))

# Find the most popular route for casual riders

popular_casual_ride_route <- cyclistic_data_casual %>%
  group_by(route) %>%
  summarise(num_of_rides = n(), avg_duration_mins = mean(ride_length)) %>%
  arrange(desc(num_of_rides), route, avg_duration_mins)

# Create a data frame for the top 10 routes of casual riders

popular_casual_ride_route_top10 <- head(popular_casual_ride_route, 10)

head(popular_casual_ride_route_top10, 10)

# Separate the top 10 start and end station names

popular_casual_ride_route_top10 <- popular_casual_ride_route_top10 %>%
  separate(route, c("start_station_name", "end_station_name"), sep = " to ")

# New data frame with top 10 station names, number of rides, and average ride duration

popular_casual_ride_route_top10_start <- popular_casual_ride_route_top10[, c(1, 3, 4)]

# Single data frame for all the information to find latitude and longitude

# Creating a data frame for station name
cyclistic_data_station <- cyclistic_data[, c(5, 9, 10)]

casual_top10_stations <- merge(popular_casual_ride_route_top10_start, cyclistic_data_station)

head(casual_top10_stations, 10)

# Similarly, find popular routes for member riders

cyclistic_data_member <- filter(cyclistic_data, member_casual == "member")

# Create a new column with routes for member riders

cyclistic_data_member <- cyclistic_data_member %>%
  mutate(route = paste(start_station_name, "to", sep=" ")) 

cyclistic_data_member <- cyclistic_data_member %>%
  mutate(route = paste(route, end_station_name, sep=" "))

# Find the most popular route for member riders

popular_member_ride_route <- cyclistic_data_member %>%
  group_by(route) %>%
  summarise(num_of_rides = n(), avg_duration_minutes = mean(ride_length)) %>%
  arrange(desc(num_of_rides), route, avg_duration_minutes)

# Top 10 popular member ride routes

popular_member_ride_route_top10 <- head(popular_member_ride_route, 10)

head(popular_member_ride_route_top10, 10)

# Separate top 10 start and end station names for member riders

popular_member_ride_route_top10 <- popular_member_ride_route_top10 %>%
  separate(route, c("start_station_name", "end_station_name"), sep = " to ")

# Create a new data frame with top 10 stations, number of rides, and average ride duration

popular_member_ride_route_top10_start <- popular_member_ride_route_top10[, c(1, 3, 4)]

# Single data frame for all the information to find latitude and longitude

member_top10_stations <- merge(popular_member_ride_route_top10_start, cyclistic_data_station)

head(member_top10_stations, 10)

#Making a pie chart to aggregate information

pie(casual_week, labels = weekride_casual, col = terrain.colors(2), main = "Casual Riders Breakup by Days of Week")

#Similarly for months

pie(casual_month, labels = monthride_casual, col=terrain.colors(2), main = "Casual Riders Breakup by Month")