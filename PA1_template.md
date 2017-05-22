# "Reproducible Research: Peer Assessment 1"
Nsi J.  
20 mai 2017  

# Set the work directory


```r
rm(list=ls()) 
WD <- getwd()
if (!is.null(WD)) setwd(WD)
(echo = TRUE)
```

```
## [1] TRUE
```

# Download and unzip data

```r
url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'

if (!file.exists("data")) dir.create("data")
if (!file.exists("activity.zip")) download.file(url, destfile = "data/activity.zip", mode="wb")
if (!file.exists("activity.csv")) unzip("activity.zip", exdir="./data")
```
#   Code for reading in the dataset and/or processing the data

```r
activity_raw <- read.csv("activity.csv", header = TRUE, 
sep = ',',stringsAsFactors=FALSE)

# Process/transform the data (if necessary) into a format suitable for analysis
activity_raw$date <- as.POSIXct(activity_raw$date, format="%Y-%m-%d")

#Set the language
Sys.setlocale(category = "LC_ALL", locale = "english")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
# Compute the weekdays from the date attribute
activity_raw <- data.frame(date=activity_raw$date, 
                           weekday=tolower(weekdays(activity_raw$date)), 
                           steps=activity_raw$steps, 
                           interval=activity_raw$interval)

# Compute the day type (weekend or weekday)
library(plyr)
activity_raw <-mutate(activity_raw, daytype=ifelse(activity_raw$weekday == "saturday" | activity_raw$weekday == "sunday", "weekend","weekday"))

# Create the tidy data.frame
activity_tidy <- data.frame(date=activity_raw$date, 
                       weekday=activity_raw$weekday, 
                       daytype=activity_raw$daytype, 
                       interval=activity_raw$interval,
                       steps=activity_raw$steps) 
rm(activity_raw)
(echo=TRUE) 
```

```
## [1] TRUE
```
     
#   What is mean total number of steps taken per day?


```r
# Calculate the total number of steps per day using dplyr and group by date

# load library
library(dtplyr)
library(knitr)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:plyr':
## 
##     here
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(ggplot2)  
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
steps_by_day <- aggregate(steps ~ date, activity_tidy, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/total_steps_per_day_histogram-1.png)<!-- -->

```r
#Copy the plot from the screen to a png file
dev.copy(png, file="Plot1.png") 
```

```
## png 
##   3
```

```r
#Close the device to view the file
dev.off()
```

```
## png 
##   2
```

```r
#32 Calculate mean and median
mean(steps_by_day$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
#[1] 10766.19
median(steps_by_day$steps, na.rm=TRUE)
```

```
## [1] 10765
```

```r
#[1] 10765
#Mean steps are 10766 and median steps are 10765

#Check the results of the mean and median
summary(steps_by_day)
```

```
##       date                         steps      
##  Min.   :2012-10-02 00:00:00   Min.   :   41  
##  1st Qu.:2012-10-16 00:00:00   1st Qu.: 8841  
##  Median :2012-10-29 00:00:00   Median :10765  
##  Mean   :2012-10-30 16:43:01   Mean   :10766  
##  3rd Qu.:2012-11-16 00:00:00   3rd Qu.:13294  
##  Max.   :2012-11-29 00:00:00   Max.   :21194
```

```r
(echo=TRUE)
```

```
## [1] TRUE
```
#  What is the average daily activity pattern?  

```r
# Compute the means of steps accross all days for each interval
mean_data <- aggregate(activity_tidy$steps, 
                       by=list(activity_tidy$interval), 
                       FUN=mean, 
                       na.rm=TRUE)

# Check mean_data
str(mean_data)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ Group.1: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ x      : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
# Rename the attributes
names(mean_data) <- c("interval", "mean")

#Check mean_data with the new attributes
# head(mean_data)
# str(mean_data)

#  Create the plot of the average number of steps taken
plot(mean_data$interval, 
     mean_data$mean, 
     type="l", 
     col="red", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals")
```

![](PA1_template_files/figure-html/Time_series_plot_the_average_number_steps_taken-1.png)<!-- -->

```r
#  Copy the plot from the screen to a png file
dev.copy(png, file="Plot2.png") 
```

```
## png 
##   3
```

```r
# Close the device to view the file
dev.off()
```

```
## png 
##   2
```

```r
#  The 5-minute interval that, on average, contains the maximum number of steps
#  Position of the maximum mean
max_pos <- which(mean_data$mean == max(mean_data$mean))
#str(max_pos)
#int 104

# We lookup the value of interval at this position
max_interval <- mean_data[max_pos, 1]

#str(max_interval)
#int 835
#  The 5-minute interval that contains the maximum of steps, on average across all days, is 835
(echo=TRUE)
```

```
## [1] TRUE
```

```r
# Count the total number of missing data

NA_total <- sum(is.na(activity_tidy$steps))
str(NA_total)
```

```
##  int 2304
```

```r
#int 2304
#The number of NA's is 2304

#  Compute the percentage of Na data
percentage_na <- mean(is.na(activity_tidy$steps)) 
percentage_na
```

```
## [1] 0.1311475
```

```r
#[1] 0.1311475

#About 13% of NA data

# Strategy or filling in all of the missing values in the dataset : use the mean/median for that day, or the mean for that 5-minute interval

# Find where are NA
na_position <- which(is.na(activity_tidy$steps))

# str(na_position)
# int [1:2304] 1 2 3 4 5 6 7 8 9 10 ...

# The strategy used is to remplace each NA value by the mean of the steps attribute.

# Create a vector linked to mean

mean_vec <- rep(mean(activity_tidy$steps, na.rm=TRUE), times=length(na_position))

# Create a new data set with NA off, each NA replaced by the mean of the steps attribute
activity_tidy[na_position, "steps"] <- mean_vec

# head(activity_tidy)

# Check that there are no missing values
sum(is.na(activity_tidy$step))
```

```
## [1] 0
```

```r
#[1] 0
(echo=TRUE)
```

```
## [1] TRUE
```
# Histogram of the total number of steps taken each day after missing values are imputed  

```r
# Compute the total number of steps each day (NA values removed)
sum_data <- aggregate(steps ~ date, activity_tidy, sum)
# head(sum_data)


# Compute the histogram of the total number of steps each day
hist(sum_data$steps, main = paste("Total Steps Each Day(NA values removed) "), col="red", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/Compute _total_number_steps_each_day_NA_values_remove-1.png)<!-- -->

```r
#  Copy the plot from the screen to a png file
dev.copy(png, file="Plot3.png") 
```

```
## png 
##   3
```

```r
#  Close the device to view the file
dev.off()
```

```
## png 
##   2
```

```r
#  Compute the mean and median
mean(sum_data$steps)
```

```
## [1] 10766.19
```

```r
median(sum_data$steps)
```

```
## [1] 10766.19
```

```r
summary(sum_data$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

```r
#  #Mean steps are 10766 and median steps are 10766 : the same value for both
(echo=TRUE)
```

```
## [1] TRUE
```

#  Are there differences in activity patterns between weekdays and weekends?  

```r
#  Create a factor variable that states whether each day is a weekday or weekend see the part of process data with daytypecreated there


#  Create a panel plot containing a time series plot (i.e. type = "l") of the 5- minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

#  Compute the average number of steps taken, averaged across all daytype variable
mean_data <- aggregate(activity_tidy$steps, 
            by=list(activity_tidy$daytype, 
            activity_tidy$weekday, activity_tidy$interval), mean)
# head(mean_data)
#822. Rename the attributes
library(lattice)

names(mean_data) <- c("daytype", "weekday", "interval", "mean")
head(mean_data)
```

```
##   daytype  weekday interval     mean
## 1 weekday   friday        0 8.307244
## 2 weekday   monday        0 9.418355
## 3 weekend saturday        0 4.672825
## 4 weekend   sunday        0 4.672825
## 5 weekday thursday        0 9.375844
## 6 weekday  tuesday        0 0.000000
```

```r
xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       main="Average Steps per Day by Interval",
       layout=c(1,2))
```

![](PA1_template_files/figure-html/Comparing_average_number_steps_taken_per_5_minute_interval_across_weekdays_weekends-1.png)<!-- -->

```r
#Copy the plot from the screen to a png file
dev.copy(png, file="Plot4.png") 
```

```
## png 
##   3
```

```r
#Close the device to view the file
dev.off()
```

```
## png 
##   2
```

```r
(echo=TRUE)
```

```
## [1] TRUE
```
