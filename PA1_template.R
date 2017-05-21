#Reproducible Research: Peer Assessment 1

# Set the work directory
rm(list=ls())
WD <- getwd()
if (!is.null(WD)) setwd(WD)

#Download and unzip data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if (!file.exists("data")) dir.create("data")
if (!file.exists("data/activity.zip")) download.file(url, destfile = "data/activity.zip", mode="wb")
if (!file.exists("data/activity.csv")) unzip("data/activity.zip", exdir="./data")

#1.Code for reading in the dataset and/or processing the data

#11- Read the raw data and discover its statistics
activity_raw <- read.csv("./data/activity.csv", header = TRUE, sep = ',',stringsAsFactors=FALSE)
str(activity_raw)
head(activity_raw)
#tail(activity_raw)
#class(activity_raw)

# load library
library(dtplyr)
library(knitr)
library(lubridate)
library(ggplot2)

#12-Process/transform the data (if necessary) into a format suitable for analysis
#Convert the date format to POSIXct

activity_raw$date <- as.POSIXct(activity_raw$date, format="%Y-%m-%d")
#class(activity_raw$date)

#Date the download
#Datadownloaded <-date()

#Set the language
Sys.setlocale(category = "LC_ALL", locale = "english")

# Compute the weekdays from the date attribute
activity_raw <- data.frame(date=activity_raw$date, 
                           weekday=tolower(weekdays(activity_raw$date)), 
                           steps=activity_raw$steps, 
                           interval=activity_raw$interval)

library(plyr)
activity_raw <-mutate(activity_raw, daytype=ifelse(activity_raw$weekday == "saturday" | activity_raw$weekday == "sunday", "weekend","weekday"))
#head(activity_raw)
#str(activity_raw)

# Create the tidy data.frame
activity_tidy <- data.frame(date=activity_raw$date, 
                       weekday=activity_raw$weekday, 
                       interval=activity_raw$interval,
                       daytype=activity_raw$daytype,
                       steps=activity_raw$steps)
#str(activity_tidy)
#head(activity_tidy)
#rm(activity_raw)
#Check some statistics of the new data frame
#str(activity_tidy)
#head(activity_tidy)
#summary(activity_tidy)
#table(activity_tidy$date)
#table(activity_tidy$weekday)


#  3. What is mean total number of steps taken per day?
#31. Sum steps by day, create Histogram 
library(dplyr)
steps_by_day <- aggregate(steps ~ date, activity_tidy, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

#Copy the plot from the screen to a png file
dev.copy(png, file="./data/Plot1.png") 

#Close the device to view the file
dev.off()

#32 Calculate mean and median
mean(steps_by_day$steps, na.rm=TRUE)
#[1] 10766.19
median(steps_by_day$steps, na.rm=TRUE)
#[1] 10765
#Mean steps are 10766 and median steps are 10765

#Check the results of the mean and median
summary(steps_by_day)

#4- What is the average daily activity pattern?
#Time series plot of the average number of steps taken

#41. Compute the means of steps accross all days for each interval
mean_data <- aggregate(activity_tidy$steps, 
                       by=list(activity_tidy$interval), 
                       FUN=mean, 
                       na.rm=TRUE)

#check mean_data
str(mean_data)

# Rename the attributes
names(mean_data) <- c("interval", "mean")
#Check mean_data with the new attributes
head(mean_data)
str(mean_data)

#42. Create the plot of the average number of steps taken
plot(mean_data$interval, 
     mean_data$mean, 
     type="l", 
     col="red", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals")
     
#Copy the plot from the screen to a png file
dev.copy(png, file="./data/Plot2.png") 

#Close the device to view the file
dev.off()

#5. The 5-minute interval that, on average, contains the maximum number of steps
# Position of the maximum mean
max_pos <- which(mean_data$mean == max(mean_data$mean))
str(max_pos)
#int 104

# We lookup the value of interval at this position
max_interval <- mean_data[max_pos, 1]
str(max_interval)
#int 835
#The 5-minute interval that contains the maximum of steps, on average across all days, is 835

#6. Code to describe and show a strategy for imputing missing data
#61. Count the total number of missing data

NA_total <- sum(is.na(activity_tidy$steps))
str(NA_total)
#int 2304
#The number of NA's is 2304

# Compute the percentage of Na data
percentage_na <- mean(is.na(activity_tidy$steps)) 
percentage_na
#[1] 0.1311475
#About 13% of NA data

#62.Strategy or filling in all of the missing values in the dataset : use the mean/median for that day, or the mean for that 5-minute interval
# Find where are NA
na_position <- which(is.na(activity_tidy$steps))

#str(na_position)
#int [1:2304] 1 2 3 4 5 6 7 8 9 10 ...

#The strategy used is to remplace each NA value by the mean of the steps attribute.
#Create a vector linked to mean

mean_vec <- rep(mean(activity_tidy$steps, na.rm=TRUE), times=length(na_position))

#63. Create a new data set with NA off, each NA replaced by the mean of the steps attribute
activity_tidy[na_position, "steps"] <- mean_vec

#head(activity_tidy)

#Check that there are no missing values
sum(is.na(activity_tidy$step))
#[1] 0

#7. Histogram of the total number of steps taken each day after missing values are imputed
# Compute the total number of steps each day (NA values removed)
sum_data <- aggregate(steps ~ date, activity_tidy, sum)
head(sum_data)


# Compute the histogram of the total number of steps each day
hist(sum_data$steps, main = paste("Total Steps Each Day(NA values removed) "), col="red", xlab="Number of Steps")

#Copy the plot from the screen to a png file
dev.copy(png, file="./data/Plot3.png") 

#Close the device to view the file
dev.off()

#Compute the mean and median
mean(sum_data$steps)
median(sum_data$steps)
summary(sum_data$steps)

#Mean steps are 10766 and median steps are 10766 : the same value for both

#8. Are there differences in activity patterns between weekdays and weekends? 
#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
#81. Create a factor variable that states whether each day is a weekday or weekend see the part of process data with daytype


#82. Create a panel plot containing a time series plot (i.e. type = "l") of the 5- minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

#821. Compute the average number of steps taken, averaged across all daytype variable
mean_data <- aggregate(activity_tidy$steps, 
                       by=list(activity_tidy$daytype, 
                               activity_tidy$weekday, activity_tidy$interval), mean)
# head(mean_data)
#822. Rename the attributes
library(lattice)
names(mean_data) <- c("daytype", "weekday", "interval", "mean")
head(mean_data)

xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       main="Average Steps per Day by Interval",
       layout=c(1,2))

#Copy the plot from the screen to a png file
dev.copy(png, file="./data/Plot4.png") 

#Close the device to view the file
dev.off()
