---
title: "Activity Monitoring data analysis"
output: html_document
---




```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
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

## Loading and preprocessing the data

Load the data (i.e. read.csv())

```r
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, destfile="repdata%2Fdata%2Factivity.zip")
unzip("repdata%2Fdata%2Factivity.zip")
activity <- read.csv("activity.csv", na.strings = "NA")
```

##What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
total_steps_day <- tapply(activity$steps,activity$date,FUN = sum, na.rm = TRUE)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(total_steps_day, na.rm = TRUE)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)


3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_first <- mean(total_steps_day)
median_first <- median(total_steps_day)
```

Mean = 9354.23

Median = 10395.00

##What is the average daily activity pattern?
1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avg_interval <- tapply(activity$steps,activity$interval,FUN = mean, na.rm = TRUE)
avg_interval <- data.frame(as.numeric(as.character(names(avg_interval))),avg_interval)
names(avg_interval) <- c("interval","avg.steps")
ggplot(avg_interval, aes(x=interval,y=avg.steps)) + 
  geom_line() 
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max <- max(avg_interval$avg.steps)
max_interval <- avg_interval[which(avg_interval$avg.steps == max),1]
```

Maximun number of steps are in interval # 835

Reflecting the interval in below graph


```r
ggplot(avg_interval, aes(x=interval,y=avg.steps)) + 
  geom_line() +
  scale_x_continuous(breaks = max_interval)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)



##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy: I will replace NA values with average step value for that interval

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity[which(is.na(activity$steps)),1] <- avg_interval[as.character(activity[which(is.na(activity$steps)),3]),2]
```


4.a Make a histogram of the total number of steps taken each day. 

```r
total_steps_day <- tapply(activity$steps,activity$date,FUN = sum)
hist(total_steps_day, na.rm = TRUE)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

4.b Calculate and report the mean and median total number of steps taken per day. 

```r
mean_second <- mean(total_steps_day)
median_second <- median(total_steps_day)
```

Mean = 10766.19

Median = 10766.19

4.c Do these values differ from the estimates from the first part of the assignment? 

YES

4.d What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean has increased by 1411.96 steps

Median has increased by 371.19 steps

##Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day



```r
activity$dayname <- weekdays(as.POSIXct(activity$date)) 
activity <- mutate(activity, weekend = ifelse(activity$dayname %in% c("Saturday","Sunday"), "weekend", "weekday"))
activity <- mutate(activity, weekend = as.factor(weekend))
activity <- activity %>% group_by(interval,weekend) %>% mutate(avg.steps = mean(steps))
```

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
ggplot(activity, aes(x=interval, y=avg.steps)) + 
  geom_line(col = "blue") +
  facet_grid(weekend ~ .)  
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)



