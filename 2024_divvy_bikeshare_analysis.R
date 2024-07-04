# Load and install tidyverse package
install.packages("tidyverse")
library(tidyverse)

# Use the conflicted package to manage conflicts
library(conflicted)

# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#=====================
# STEP 1: COLLECT DATA
#=====================
#load library readr
library(readr)
# Upload Divvy datasets (csv file which contains data from january to may 2024 recent data) here
all_trips <- read_csv("F:\\my_Data\\Case_study_1_data\\My_final_analysis\\2024_divvy_bikeshare_analysis\\all_trips1-6month_data.csv")
View(all_trips)

#remove unwanted data column '...1' optional(if col'...1') exists
all_trips <- all_trips %>%
  select(-c(...1))

View(all_trips)
#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before
#we can use a command to join them into one file
colnames(all_trips)
# Inspect the dataframes and look for incongruencies
str(all_trips)
# Remove latitude, longitude, started_at,as no longer needed
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
colnames(all_trips)


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips) #List of column names
nrow(all_trips) #How many rows are in data frame?
dim(all_trips) #Dimensions of the data frame?
head(all_trips) #See the first 6 rows of data frame. Also tail(all_trips)
str(all_trips) #See list of columns and data types (numeric, character, etc)
summary(all_trips) #Statistical summary of data. Mainly for numerics


# There are a few problems we will need to fix:
#---------------------------------------------------------------------------------------------------
# (1) The data can only be aggregated at the ride-level, which is too granular. We will want to
add some additional columns of data -- such as day, month, year -- that provide additional
opportunities to aggregate the data.

# (2) We will want to add a calculated field for length of ride since the 2020Q1 data did not have
the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.

# (3) There are some rides where tripduration shows up as negative, including several hundred
rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to
delete these rides.

# (4) During data cleaning process in excel for 2023Q1 data found column rideable_type with 3 values 
"docked_bike", "electric_bike", "classic_bike", but data after 8/2023 found only 2 values "electric_
bike", "classic_bike".
#----------------------------------------------------------------------------------------------------
# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing
#these operations we could only aggregate at the ride level
#-----------------------------------------------------------------------------------
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
#-----------------------------------------------------------------------------------

all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
# Inspect the structure of the columns
str(all_trips)


# Add a "ride_length" calculation to all_trips (in seconds)

#-----------------------------------------------------------------------
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
#-----------------------------------------------------------------------
#creates an column and assign time difference
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)


# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
# Inspect the column ride_length and look for incongruencies
head(all_trips$ride_length, n = 100)
tail(all_trips$ride_length, n = 100)

# Inspected some blanks(0's) in data field
#removed all the NA's/blanks in data
# Remove "bad" data

# The data frame includes a few hundred entries when bikes were taken out of docks and
##checked for quality by Divvy / ride_length was negative

# We will create a new version of the dataframe (v2) since data is being removed
#-----------------------------------------------------------------------------------
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
#-----------------------------------------------------------------------------------
# Data removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0 
                            | all_trips$ride_length == 0),]

library(tidyr)
all_trips_v3 <- all_trips_v2 %>% drop_na()
View(all_trips_v3)

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v3$ride_length) #straight average (total ride length / rides)
median(all_trips_v3$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v3$ride_length) #longest ride
min(all_trips_v3$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v3$ride_length)

# Compare members and casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = mean)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = median)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = max)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = min)


# See the average ride time by each day for members vs casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week,
          FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week,
          FUN = mean)



# Compare electric_bikes and classic_bikes users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$rideable_type, FUN = mean)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$rideable_type, FUN = median)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$rideable_type, FUN = max)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$rideable_type, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$rideable_type + all_trips_v3$day_of_week,
          FUN = mean)
#=============================
#(cos week days sorted above already[optional now]) Notice that the days of the week are out of order. Let's fix that.
all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week,
          FUN = mean)
#=============================
# analyze ridership data by type and weekday
library(dplyr)
all_trips_v3 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using wday()
  group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday) # sorts

# analyze ridership data by bike type and weekday
all_trips_v3 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using wday()
  group_by(rideable_type, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(rideable_type, weekday) # sorts

# Let's visualize the number of rides by rider type
all_trips_v3 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's visualize the number of rides by bike type
all_trips_v3 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(rideable_type, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(rideable_type, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge")


install.packages("ggvis")
library(ggvis)
library(dplyr)
library(lubridate)
# Let's create a visualization for average duration for member_casual
all_trips_v3 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") 


# Let's create a visualization for average duration for rideable_type
all_trips_v3 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(rideable_type, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(rideable_type, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = rideable_type)) +
  geom_col(position = "dodge") 


#======================================
# Remove started_at, ended_at  fields as no longer needed
all_trips_ <- all_trips %>%
  select(-c(started_at, ended_at))
#check the available columns
colnames(all_trips)
str(all_trips)
#======================================
#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location
#accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can
#read more here: https://datatofish.com/export-dataframe-to-csv-in-r/

  
avg_ride_length_member_casual <- aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual +
                        all_trips_v3$day_of_week, FUN = mean)

avg_ride_length_rideable_type <- aggregate(all_trips_v3$ride_length ~ all_trips_v3$rideable_type +
                                              all_trips_v3$day_of_week, FUN = mean)

write.csv(avg_ride_length_member_casual, file = 'F:\\my_Data\\Case_study_1_data\\My_final_analysis\\2024_divvy_bikeshare_analysis\\avg_ride_length_member_casual_Q1Q2.csv')
write.csv(avg_ride_length_rideable_type, file = 'F:\\my_Data\\Case_study_1_data\\My_final_analysis\\2024_divvy_bikeshare_analysis\\avg_ride_length_rideable_type_Q1Q2.csv')


ride <- read_csv("F:\\my_Data\\Case_study_1_data\\My_final_analysis\\2024_divvy_bikeshare_analysis\\avg_ride_length_member_casual.csv")
View(ride)
head(ride)
str(ride)

ride1 <- read_csv("F:\\my_Data\\Case_study_1_data\\My_final_analysis\\2024_divvy_bikeshare_analysis\\avg_ride_length_rideable_type.csv")
View(ride1)
head(ride1)
str(ride1)

#===============================================================================
#save all different types of data aggergated and analysed in csv format
#save a modified data csv file with col (ride_id, rideable_type, long,lat, date, day, month, year, weekday )
write.csv(all_trips_v3, file = 'F:\\my_Data\\Case_study_1_data\\My_final_analysis\\2024_divvy_bikeshare_analysis\\all_trips_Q1Q2-2024_granularData.csv')
read_csv("F:\\my_Data\\Case_study_1_data\\My_final_analysis\\2024_divvy_bikeshare_analysis\\all_trips_granularData.csv")
#===============================================================================