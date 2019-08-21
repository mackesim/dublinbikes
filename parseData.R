" 
  Description:
    Code to read and parse Dublin Bikes usage data. Adapted from the code by George Nidhin.

"
install.packages("tidyverse")
install.packages("jsonlite")
install.packages("lubridate")
install.packages("rjson")
install.packages("readr")
install.packages("dplyr")
library(tidyverse)
library(jsonlite)
library(lubridate)
library(rjson)
library(readr)
library(plyr)
library(dplyr)

################################ Read in JSON and CSV files #################################

# Get into the right directory
setwd("C:/Users/user/Documents/DublinBikes/new_data")

# Get all file names in the directory for JSON files
file_names <- dir()

# Read in all json files, flattening where appropriate
T1<-Sys.time() 
all_df <- do.call(rbind, lapply(file_names, jsonlite::fromJSON, flatten = TRUE))
T2<-Sys.time() 
difftime(T2, T1) 

# Get into the right directory
setwd("C:/Users/user/Documents/DublinBikes/raw-data")

# Get all file names in the directory for CSV files
file_names <- dir()

# Read all files into data frame
T3<-Sys.time() 
df <- do.call(rbind, lapply(file_names, read_csv))
T4<-Sys.time() 
difftime(T4, T3)

# Reorganize columns before combine them
all_df <- all_df[,-c(4:7)]
df <- df %>%
  mutate(available_bikes = bike_stands - available_bike_stands
         )
df <- df[, c(1, 2, 3, 4, 5, 7, 6)]
df <- df[order(df$number),]
geo_df <- read_csv("C:/Users/user/Documents/DublinBikes-George/geo_data/db_geo.csv")
colnames(geo_df)[colnames(geo_df)=="Number"] <- "number"
colnames(geo_df)[colnames(geo_df)=="Name"] <- "name"
colnames(geo_df)[colnames(geo_df)=="Address"] <- "address"

# ADD THE POSITION TO DF
df <- df %>%
  left_join(geo_df, by = "name")
df <- df[,-c(8:9)]
colnames(df)[colnames(df)=="number.x"] <- "number"
colnames(df)[colnames(df)=="address.x"] <- "address"
colnames(df)[colnames(df)=="Latitude"] <- "position.lat"
colnames(df)[colnames(df)=="Longitude"] <- "position.lng"



# Combine the old and new data
all_df<-rbind(df, all_df)

# Save the data as rds
write_rds(all_df, "db_raw_new_data.rds")


################################ Support Functions ###################################

# Function to calculate number of checked in bikes
# Args: diff <- difference in # of bikes from previous period to current period
c_check_in <- function(diff){
  # Check if there was a check in
  if(diff > 0){
    return (diff)
  }
  # else return 0 as there was no check in
  else{
    return (0)
  }
}

# Function to calculate number of checked out bikes
# Args: diff <- difference in # of bikes from previous period to current period
c_check_out <- function(diff){
  # Check if there was a check out
  if(diff < 0){
    return (abs(diff))
  }
  # else return 0 as there was no check out
  else{
    return (0)
  }
}

################################## Parse the data #################################
df <- read_rds("C:/Users/user/Documents/DublinBikes/raw-data/db_raw_new_data.rds")

# Separate Duplicates
dup <- duplicated(df)
dup <- df %>%
  mutate(duplicate = dup) %>%
  filter(dup == TRUE)

df <- distinct(df)

# Calculate difference in number of bikes between periods
df <- df %>%
  # Convert POSIXct to date and split into each col
  mutate(
    last_update = as_datetime(last_update/1000, tz = "GMT"),
    Year = year(last_update),
    Month = month(last_update),
    Day = day(last_update),
    Hour = as.character(hour(last_update)),
    Min = minute(last_update),
    Sec = "00"
  ) %>%  
  # Standardise minutes i.e. group them in 10 minute slots and make new time
  mutate(
    Min = ifelse(Min < 10, "00",
                 ifelse(Min < 20, "10",
                        ifelse(Min < 30, "20",
                               ifelse(Min < 40, "30",
                                      ifelse(Min < 50, "40",
                                             ifelse(Min < 60, "50",
                                                    NA)))))
    ),
    Date = ymd(paste(Year, Month, Day, sep = "-")),
    Time = paste(Hour, Min, Sec, sep = ":"),
    Weekday = weekdays(Date, abbreviate = TRUE)
  ) %>%
  # Sort in order so we can calculate the differences
  group_by(number) %>%
  arrange(Year, Month, Day, Hour, Min, Sec) %>%
  # Calculate differences
  mutate(
    prev_period_diff = 
      available_bike_stands - lag(available_bike_stands, default = available_bike_stands[1])
  ) %>%
  # Apply functions to determine checked in/out 
  rowwise() %>%
  mutate(
    check_in = c_check_in(prev_period_diff),
    check_out = c_check_out(prev_period_diff)
  ) %>%
  # Group results in slots of 10 minutes
  group_by(number, name, address, Date, Time, Weekday) %>%
  # Add up results
  summarise(
    bike_stands = min(bike_stands),
    prev_period_diff = sum(prev_period_diff),
    check_in = sum(check_in),
    check_out = sum(check_out),
    available_stands = last(available_bike_stands)
  ) %>%
  ungroup()

# Rename columns
df <- df %>%
  rename(
    Number = number,
    Name = name,
    Address = address,
    Bike_stands = bike_stands,
    Prev_period_diff = prev_period_diff,
    Check_in = check_in,
    Check_out = check_out,
    Available_stands = available_stands
  )

# Add factor level information to days in the week
days_level <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
df <- df %>%
  mutate(Weekday = factor(Weekday, levels = days_level))

# Write output to rds file so code doesn't have to be re-run
write_rds(df, "db_all_data.rds")

############################# Format data for estimating trips ################################

df <- read_rds("C:/Users/user/Documents/DublinBikes/db_all_data.rds")

# Add available bikes column
df <- df %>%
  mutate(available_bikes = Bike_stands - Available_stands
  )
df <- df[,-c(8:10)]

# Delete false data : available bike < 0 : impossible
df <- df[!(df$available_bikes)<0,]

# As Dublin Bikes working from 5am to 12:30am
# Keep only days with 95% or more of the 118 (144-26) observations per day so > or = to 112
counts <- ddply(df, .(df$Date, df$Number), nrow)
names(counts) <- c("Date", "Station", "Freq")
counts_under112 <- counts[!(counts$Freq) >= 112,]
counts_over112 <- counts[!(counts$Freq) < 112,]

# Days to remove from the data frame
days_to_remove <- distinct(counts_under112, Date)

# Days to keep from the data frame
days_to_keep <- distinct(counts_over112, Date)

# Remove from the dataframe
dtr <- unlist(days_to_remove, use.names = FALSE)
dtr <- as.Date(dtr, origin = "1970-01-01")
dtk <- unlist(days_to_keep, use.names = FALSE)
dtk <- as.Date(dtk, origin = "1970-01-01")
df_final <- df[!(df$Date %in% dtr),]

# Write into rds file
write_rds(df_final, "C:/Users/user/Documents/DublinBikes/db_estimating_trips.rds")
