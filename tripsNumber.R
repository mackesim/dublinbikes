" 
  Code to estimate the number of trips per day using calculation from available bikes.
"

library(tidyverse)
library(lubridate)
library(stringr)
library(ggmap)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)

############################################# IMPORT DATA #############################################

# Read previously processed data
df_final <- read_rds("C:/Users/user/Documents/DublinBikes/db_estimating_trips.rds")
df_final <- df_final %>%
  mutate(diff=NA) %>%
  mutate(absdiff=NA)

# Create a custom order for time
df_final$Time <- factor(df_final$Time, levels = 
            c("0:00:00", "0:10:00", "0:20:00", "0:30:00", "0:40:00", "0:50:00",
              "1:00:00", "1:10:00", "1:20:00", "1:30:00", "1:40:00", "1:50:00",
              "2:00:00", "2:10:00", "2:20:00", "2:30:00", "2:40:00", "2:50:00",
              "3:00:00", "3:10:00", "3:20:00", "3:30:00", "3:40:00", "3:50:00",
              "4:00:00", "4:10:00", "4:20:00", "4:30:00", "4:40:00", "4:50:00",
              "5:00:00", "5:10:00", "5:20:00", "5:30:00", "5:40:00", "5:50:00", 
              "6:00:00", "6:10:00", "6:20:00", "6:30:00", "6:40:00", "6:50:00", 
              "7:00:00", "7:10:00", "7:20:00", "7:30:00", "7:40:00", "7:50:00", 
              "8:00:00", "8:10:00", "8:20:00", "8:30:00", "8:40:00", "8:50:00", 
              "9:00:00", "9:10:00", "9:20:00", "9:30:00", "9:40:00", "9:50:00", 
              "10:00:00", "10:10:00", "10:20:00", "10:30:00", "10:40:00", "10:50:00",
              "11:00:00", "11:10:00", "11:20:00", "11:30:00", "11:40:00", "11:50:00",
              "12:00:00", "12:10:00", "12:20:00", "12:30:00", "12:40:00", "12:50:00", 
              "13:00:00", "13:10:00", "13:20:00", "13:30:00", "13:40:00", "13:50:00", 
              "14:00:00", "14:10:00", "14:20:00", "14:30:00", "14:40:00", "14:50:00", 
              "15:00:00", "15:10:00", "15:20:00", "15:30:00", "15:40:00", "15:50:00", 
              "16:00:00", "16:10:00", "16:20:00", "16:30:00", "16:40:00", "16:50:00", 
              "17:00:00", "17:10:00", "17:20:00", "17:30:00", "17:40:00", "17:50:00", 
              "18:00:00", "18:10:00", "18:20:00", "18:30:00", "18:40:00", "18:50:00", 
              "19:00:00", "19:10:00", "19:20:00", "19:30:00", "19:40:00", "19:50:00", 
              "20:00:00", "20:10:00", "20:20:00", "20:30:00", "20:40:00", "20:50:00", 
              "21:00:00", "21:10:00", "21:20:00", "21:30:00", "21:40:00", "21:50:00", 
              "22:00:00", "22:10:00", "22:20:00", "22:30:00", "22:40:00", "22:50:00", 
              "23:00:00", "23:10:00", "23:20:00", "23:30:00", "23:40:00", "23:50:00"
              ))

notUsed <- c("0:40:00", "0:50:00",
              "1:00:00", "1:10:00", "1:20:00", "1:30:00", "1:40:00", "1:50:00",
              "2:00:00", "2:10:00", "2:20:00", "2:30:00", "2:40:00", "2:50:00",
              "3:00:00", "3:10:00", "3:20:00", "3:30:00", "3:40:00", "3:50:00",
              "4:00:00", "4:10:00", "4:20:00", "4:30:00", "4:40:00", "4:50:00",
              "5:00:00", "5:10:00", "5:20:00")

# Ordering data to calculate the differences between time periods
df_final <- df_final[order(df_final$Date,  df_final$Number, df_final$Time),]

# Get the list of dates from the table
Date <- unique(as.Date(df_final$Date, origin = "1970-01-01"))

# Calculate every difference between two periods
for(i in 2:nrow(df_final)){ # Start at 2 because first row doesn't have previous row
  if(df_final$Number[i] == df_final$Number[i-1]){ # Calculate difference if two consecutive rows are same station
    df_final$diff[i] = df_final$available_bikes[i] - df_final$available_bikes[i-1]
    df_final$absdiff[i] = abs(df_final$available_bikes[i] - df_final$available_bikes[i-1])
  }else if(df_final$Number[i] != df_final$Number[i-1]){ # Else it's a new station so go to the next row
    i = i + 1
  }
}

# Function to calculate every daily interactions and daily trips
overall <- function(){
  result_table <- data.frame(Date=as.Date(character()), X=integer()) 
  for(i in Date){
    table <- df_final[(df_final$Date) == as.Date(i, origin),]
    table <- table %>%
      dplyr::select(Number,Date,Time,absdiff) %>%
      group_by(Number, Date) %>%
      spread(key = Number, value = absdiff) %>%
      ungroup() %>%
      dplyr::select(-c(1:2))
    table[is.na(table)] <- 0
    sum_day = sum(table)
    result_table <- rbind(result_table, c(i,sum_day))
  }
  names(result_table)[1]<-"Date"
  names(result_table)[2]<-"X"
  result_table[1] = as.Date(result_table[[1]], origin)
  result_table <<- result_table
}

# Function to calculate daily interactions of one date
unique_date <- function(day){
  result_day <- data.frame(Date=as.Date(character()), Xd=integer()) 
  table <- df_final[(df_final$Date) == day,]
  table <- table %>%
    dplyr::select(Number,Date,Time,diff) %>%
    group_by(Number, Date) %>%
    spread(key = Number, value = diff) %>%
    ungroup() %>%
    dplyr::select(-c(1:2))
  table[is.na(table)] <- 0
  sum_day = sum(table)
  result_day <- rbind(result_day, c(day,sum_day))
  names(result_day)[1]<-"Date"
  names(result_day)[2]<-"DailyInteractions"
  result_day[1] = result_day[[1]]
  result_day <<- result_day
}

# Import clusters
clusters <- read_rds("C:/Users/user/Documents/DublinBikes/clustering/db_4clustered_stations.rds")
df_final <- merge(df_final, clusters, by = "Number")
df_final <- df_final[,-c(3,6,12)]
df_final <- df_final[,c(1:2,10,3:9)]
colnames(df_final)[colnames(df_final)=="Name.x"] <- "Name"  
df_final$diff[is.na(df_final$diff)] <- 0
df_final$absdiff[is.na(df_final$absdiff)] <- 0

# Divide database by cluster
c1 <- filter(df_final, df_final$cluster == "1" & df_final$diff != 0)
c2 <- filter(df_final, df_final$cluster == "2" & df_final$diff != 0)
c3 <- filter(df_final, df_final$cluster == "3" & df_final$diff != 0)
c4 <- filter(df_final, df_final$cluster == "4" & df_final$diff != 0)
df_without_zero <- rbind(c1, c2, c3, c4)
time_cluster_means <- aggregate(diff ~ cluster + Time, data=df_without_zero, FUN=mean)

# Active stations - COMBINED DAM
each_day <- aggregate(abs(diff) ~ Date + Number, data=df_final, FUN=sum)
names(each_day)[3]<-"Diff"
for(i in 1:nrow(each_day)){
  if(each_day$Diff[i] > 0){
    each_day$Diff[i] = 1
  }else{
    each_day$Diff[i] = 0
  }
}
each_day <- aggregate(Diff ~ Date, data=each_day, FUN=sum)

# Active stations - INTERVAL AGGREGATION MODEL
each_interval <- df_final[,-c(2,6,7,8)]
each_interval <- left_join(each_interval, time_cluster_means, by = c("Time" = "Time", "cluster" = "cluster"))
colnames(each_interval)[colnames(each_interval)=="diff.y"] <- "rebalance" 
each_interval$rebalance[is.na(each_interval$rebalance)] <- 0
each_interval$rebalance <- round(each_interval$rebalance)
each_interval$I <- abs(each_interval$diff.x - each_interval$rebalance)
aggregating <- aggregate(I ~ Date + Time, data=each_interval, FUN=sum)
each_interval$A <- NA
for(i in 1:nrow(each_interval)){
  if(each_interval$diff.x[i] != 0 ){
    each_interval$A[i] = 1
  }else{
    each_interval$A[i] = 0
  }
}
`%notin%` <- Negate(`%in%`)
each_interval <- filter(each_interval, each_interval$Time %notin% notUsed,)
each_interval <- each_interval[,-c(2)]
each_interval <- aggregate(. ~ Date + Time, each_interval, FUN=sum)
each_interval <- each_interval[,-c(1,2,3,4,6)]
each_interval <- each_interval[,c(2,1,3)]
each_interval$A2 <- each_interval$A * each_interval$A

# Active stations - STATION AGGREGATION MODEL
sam_data <- df_final[,-c(2,6,7,8)]
sam_data <- left_join(sam_data, time_cluster_means, by = c("Time" = "Time", "cluster" = "cluster"))
colnames(sam_data)[colnames(sam_data)=="diff.y"] <- "rebalance" 
sam_data$rebalance[is.na(sam_data$rebalance)] <- 0
sam_data$I <- abs(sam_data$diff.x - sam_data$rebalance)
for(i in 1:nrow(sam_data)){
  if(sam_data$diff.x[i] != 0 ){
    sam_data$A[i] = 1
  }else{
    sam_data$A[i] = 0
  }
}
sam_data <- sam_data[,-c(2, 5, 7)]
sam_data <- aggregate(. ~ Date + Number, sam_data, FUN=sum)
sam_data$I <- round(sam_data$I)
sam_data <- sam_data[,-c(1, 2, 3)]
sam_data <- sam_data[,c(2,1,3)]
sam_data$A2 <- sam_data$A * sam_data$A

# Take the outliers for every cluster
c1_outlier_val <- boxplot.stats(c1$diff)$out
c1_outlier_idx <- which(c1$diff %in% c(c1_outlier_val))
c2_outlier_val <- boxplot.stats(c2$diff)$out
c2_outlier_idx <- which(c2$diff %in% c(c2_outlier_val))
c3_outlier_val <- boxplot.stats(c3$diff)$out
c3_outlier_idx <- which(c3$diff %in% c(c3_outlier_val))
c4_outlier_val <- boxplot.stats(c4$diff)$out
c4_outlier_idx <- which(c4$diff %in% c(c4_outlier_val))

# Puting every outliers in files
c1 <- c1[c1_outlier_idx,]
c2 <- c2[c2_outlier_idx,]
c3 <- c3[c3_outlier_idx,]
c4 <- c4[c4_outlier_idx,]

# Combined every outliers
outliers <- rbind(c1, c2, c3, c4)
rm(c1, c2, c3, c4, c1_outlier_idx, c1_outlier_val, c2_outlier_idx, c2_outlier_val,
   c3_outlier_idx, c3_outlier_val, c4_outlier_idx, c4_outlier_val)

# Calculate every mean - difference
outliers <- left_join(outliers, time_cluster_means, by = c("cluster" = "cluster", "Time" = "Time"))
outliers <- outliers[order(outliers$Date,  outliers$Number, outliers$Time),]
outliers <- outliers %>%
  mutate(rebalancing=NA)
for(i in 1:nrow(outliers)){
  if(outliers$diff.x[i] >= 10 | outliers$diff.x[i] <= -10){
    outliers$rebalancing[i] = abs(round(0.5*outliers$diff.x[i] - outliers$diff.y[i]))
  }else if(outliers$diff.x[i] < 10 & outliers$diff.x[i] > -10){
    outliers$rebalancing[i] = abs(round(outliers$diff.x[i] - outliers$diff.y[i]))
  }
}

# Get rebalancing for each day
rebalancing <- aggregate(outliers$rebalancing, by=list(Category=outliers$Date), FUN=sum)
rebalancing$x <- round(rebalancing$x/2)

# Add rebalancing to the result table
result_table <- left_join(result_table, rebalancing, by = c("Date" = "Category"))
colnames(result_table)[colnames(result_table)=="x"] <- "R" 

# Get collisions per day
collisions <- aggregate(df_final$diff, by=list(Category=df_final$Date), FUN=sum)

# Add collisions to the result table
result_table <- left_join(result_table, collisions, by = c("Date" = "Category"))
colnames(result_table)[colnames(result_table)=="x"] <- "C" 

# Add active stations
result_table <- left_join(result_table, each_day, by = c("Date" = "Date"))
colnames(result_table)[colnames(result_table)=="Diff"] <- "A" 

# Function to calculate daily trips
dailytrips <- function(){
  result_table <- result_table %>%
    mutate(Tx = NA) %>%
    mutate(Tx2 = NA) %>%
    mutate(T = NA)
  for(i in 1:nrow(result_table)){
    result_table$Tx[i] = round(result_table$X[i]/2)
    result_table$Tx2[i] = result_table$Tx[i]*result_table$Tx[i]
    result_table$T[i] = round((abs(result_table$X[i]-result_table$R[i])+result_table$C[i])/2)
  }
  result_table <<- result_table
}

# Save result table before performing regression models
write_rds(result_table, "C:/Users/user/Documents/DublinBikes/result_table.rds")
write_rds(each_interval, "C:/Users/user/Documents/DublinBikes/each_interval.rds")
write_rds(sam_data, "C:/Users/user/Documents/DublinBikes/sam_data.rds")
