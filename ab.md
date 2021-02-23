---
title: "Reproducible Research: Peer Assessment 1"
author: "Revathi S"
date: "2/23/2021"
output: 
  html_document: 
    keep_md: yes
---



## Loading and preprocessing the data


```r
## Loading and preprocessing the data
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

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 4.0.3
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(ggplot2)
library(httpuv)

activity <- read.csv("activity.csv", header=TRUE, na.strings = "NA")

# Clean up date class
activity$date <- ymd(activity$date)

# Remove NA
activity1 <- na.omit(activity)
```

## What is mean total number of steps taken per day?

```r
# Summarize data for ggplot
activity2 <- summarize(group_by(activity1,date),daily.step=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
mean.activity <- as.integer(mean(activity2$daily.step))
median.activity <- as.integer(median(activity2$daily.step))

# Plot histogram
plot.steps.day <- ggplot(activity2, aes(x=daily.step)) + 
  geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..)) + 
  geom_vline(xintercept=mean.activity, colour="red", linetype="dashed", size=1) +
  geom_vline(xintercept=median.activity, colour="green" , linetype="dotted", size=1) +
  labs(title="Histogram of Number of Steps taken each day", y="Frequency", x="Daily Steps") 
plot.steps.day
```

![](ab_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Mean total number of steps taken per day is 10766.  
Median total number of steps taken per day is 10765.

## What is the average daily activity pattern?

```r
# Prepare data for ggplot
activity3 <- activity1 %>% group_by(interval) %>% summarize(mean.step=mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# Plot average number of steps by 5-minute interval
plot.step.interval <- ggplot(activity3, aes(x=interval,y=mean.step)) + 
  geom_line(color="red") + 
  labs(title="Average Number of Steps Taken vs 5-min Interval", y="Average Number of Steps", x="5-min Interval Times Series")
plot.step.interval
```

![](ab_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
optimal <- which.max(activity3$mean.step)
optimal.step <- activity3$interval[optimal]
```

Maximum number of steps is coming from 835th 5-min interval 

## Imputing missing values

```r
# Total number of missing values in the dataset
sum(is.na(activity))
```

```
## [1] 2304
```

Total number of missing values in the dataset is 2304.


```r
impute.activity <- activity
impute.activity$steps[is.na(impute.activity$steps)] <- mean(impute.activity$steps,na.rm=TRUE)
impute.activity$steps <- as.numeric(impute.activity$steps)
impute.activity$interval <- as.numeric(impute.activity$interval)
colSums(is.na(impute.activity))
```

```
##    steps     date interval 
##        0        0        0
```

```r
# Summarize data by date
impute.activity2 <- summarize(group_by(impute.activity,date),daily.step=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
mean.impute   <- as.integer(mean(impute.activity2$daily.step))

median.impute <- as.integer(median(impute.activity2$daily.step))

# Plot histogram
plot.steps.day <- ggplot(impute.activity2, aes(x=daily.step)) + 
  geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..)) + 
  geom_vline(xintercept=mean.impute, colour="red", linetype="dashed", size=1) +
  geom_vline(xintercept=median.impute, colour="green" , linetype="dotted", size=1) +
  labs(title="Histogram of Number of Steps taken each day (impute)", y="Frequency", x="Daily Steps")
plot.steps.day
```

![](ab_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Mean total number of steps taken per day (after impute) is 10766.

Median total number of steps taken per day (after impute) is 10766.

## Are there differences in activity patterns between weekdays and weekends?

```r
impute.activity$day <- ifelse(weekdays(impute.activity$date) %in% c("Saturday","Sunday"), "weekday", "weekend")
# Preparing data for ggplot
impute.df <- impute.activity %>% group_by(interval,day) %>% summarise(mean.step=mean(steps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
# Plot Average steps across weekday/weekend vs 5-min interval Time Series
plot.weekday.interval <- ggplot(impute.df, aes(x=interval, y=mean.step, color=day)) + 
  facet_grid(day~.) +
  geom_line() + 
  labs(title="Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", y="Average Number of Steps", x="5-min Interval Times Series")
plot.weekday.interval
```

![](ab_files/figure-html/unnamed-chunk-6-1.png)<!-- -->








