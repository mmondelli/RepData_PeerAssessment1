---
title: "Reproducible Research: Peer Assessment 1"
author: "Maria Luiza Mondelli"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

To start, first we need to read the data:


```r
setwd('~/Dropbox/repositories/RepData_PeerAssessment1/') # Please, change if needed
unzip("activity.zip")
data <- read.csv("activity.csv")
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

and perform some data cleaning to remove missing values from steps:


```r
clean.data <- data[!is.na(data$steps),]
head(clean.data)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

## What is mean total number of steps taken per day?

According to the data, the following is a summary of the number of steps per day.

```r
steps.day <- aggregate(steps ~ date, clean.data, FUN=sum)
head(steps.day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
A histogram representing these data is presented below:


```r
hist(steps.day$steps, main="Total Number of Steps per Day", 
     xlab="Steps per Day", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The mean and median number of total steps taken per day are (respectively):


```r
mean(steps.day$steps); median(steps.day$steps)
```

```
## [1] 10766.19
```

```
## [1] 10765
```

## What is the average daily activity pattern?

The following plot shows the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
avg.activity <- aggregate(steps ~ interval, clean.data, FUN=mean)
plot(avg.activity$interval, avg.activity$steps, type="l", 
     xlab="Interval", ylab="Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

The following 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:


```r
max.steps <- avg.activity$interval[which.max(avg.activity$steps)]
avg.activity[which(avg.activity$interval == max.steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

The number of missing values in the dataset is:


```r
sum(is.na(data))
```

```
## [1] 2304
```

Now, each missing value will be replaced with the mean of the 5-minute interval averaged across all days.


```r
data.imputed <- data
for(i in 1:nrow(data.imputed)) {
  if (is.na(data.imputed[i,]$steps)) {
    data.imputed[i,]$steps <- avg.activity$steps[which(avg.activity$interval == data.imputed[i,]$interval)]
  }
}
head(data.imputed)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

A histogram representing the new dataset is presented below:


```r
new.steps.day <- aggregate(steps ~ date, data.imputed, FUN=sum)
hist(new.steps.day$steps, main="Total Number of Steps per Day (Imputed)", 
     xlab="Steps per Day", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(new.steps.day$steps); median(new.steps.day$steps)
```

```
## [1] 10766.19
```

```
## [1] 10766.19
```

We can see that the distribution was similar to the previous one, but now it includes a larger amount of records, since the missing values were filled. The mean value remained the same as that calculated earlier, but de median value has changed. This is expected, since all previously missing values now contain the mean value of the 5-minute interval averaged across all days. 

## Are there differences in activity patterns between weekdays and weekends?

To answer that, we need to create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. We will use the new dataset created in te previous step (in which the missing values were filled).  
(As you can notice, in he second line I used 'domingo' and 'sábado' because my computer is set to portuguese. I'm not sure if this can impact a future evaluation. Then, if necessary, please change these values to 'sunday' and 'saturday')


```r
data.imputed$days <- weekdays(as.POSIXct(data.imputed$date, format="%Y-%m-%d"))
data.imputed$day_type <- data.imputed$days == "domingo" | data.imputed$days == "sábado"
data.imputed$day_type <- as.factor(data.imputed$day_type)
levels(data.imputed$day_type) = c("weekday","weekend")
str(data.imputed)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ days    : chr  "segunda" "segunda" "segunda" "segunda" ...
##  $ day_type: Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Now, we can plot the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(plyr)
library(lattice)

average.steps <- ddply(data.imputed, .(interval, day_type), 
                       summarize, 
                       steps = mean(steps))

xyplot(steps ~ interval | day_type, 
       data = average.steps, 
       layout = c(1, 2), 
       type = "l", 
       xlab="Intervals", 
       ylab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

As we can notice, the plots indicate different patterns of activity. During the weekdays, we have more activity early in the day while during weekends the activity record seem to be more consistent across time.
