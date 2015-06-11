Reproducible Research - Assignment 1
========================================================

## Loading and preprocessing the data

Reading and preprocessing the Activity Monitoring Dataset. 

Preprocessing involves removing missing data rows.


```r
temp <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
unzip(temp)
activity = read.csv2("activity.csv",sep=",")
data = na.omit(activity)
head(data)
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

1.Calculate the total number of steps taken per day


```r
totSteps = aggregate(data$steps,list(data$date),FUN="sum")
totSteps
```

```
##       Group.1     x
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

2.Make a histogram of the total number of steps taken each day


```r
hist(totSteps$x,main="Histogram of steps taken", xlab="Steps Taken")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3.Calculate and report the mean and median of the total number of steps taken per day


```r
mean(totSteps$x)
```

```
## [1] 10766.19
```

```r
median(totSteps$x)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
tsData = aggregate(data$steps,list(data$interval),FUN="mean")
plot(x=tsData$Group.1,y=tsData$x,type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

830-835 interval


```r
tsData[which.max(tsData$x),]
```

```
##     Group.1        x
## 104     835 206.1698
```

## Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(subset(activity,is.na(activity$steps)))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I have used the mean for the 5-minute interval to be substituted in place of missing values.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
imputedActivity=activity
imputedActivity$steps[is.na(imputedActivity$steps)] = round(aggregate(data$steps,list(data$interval),FUN="mean")$x)
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
ImptotSteps = aggregate(imputedActivity$steps,list(imputedActivity$date),FUN="sum")
hist(ImptotSteps$x,main="Histogram of steps taken (Imputed data)",xlab="Steps Taken")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
mean(ImptotSteps$x)
```

```
## [1] 10765.64
```

```r
median(ImptotSteps$x)
```

```
## [1] 10762
```

There seems to be not much change with the inclusion of imputed data. In other words, the imputation seems to work great that it does not deviate the mean and median of teh dataset.

## Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
imputedActivity$week = ifelse(weekdays(as.Date(imputedActivity$date))=="Sunday" | weekdays(as.Date(imputedActivity$date))=="Saturday" ,"Weekend","Weekday")
imputedActivity$week = as.factor(imputedActivity$week)
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice)
aggImputedActivity = aggregate(imputedActivity$steps,list(imputedActivity$interval,imputedActivity$week),FUN="mean")
xyplot(x~Group.1 | Group.2, data=aggImputedActivity, layout=c(1,2),type="l",xlab="Time Intervals",ylab="Average Steps Taken")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
