# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

```r
library(dplyr)
DF <- read.table(unz("activity.zip", "activity.csv"), header=T, sep= ",")
DF$date <- as.Date(as.character(DF$date),"%Y-%m-%d")
```


## What is mean total number of steps taken per day?


```r
DF_date <- summarize(group_by(DF,date),total = sum(steps,na.rm = T))
hist(DF_date$total,main = "Total number of steps per day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)\


### Mean of total steps taken per day

```r
mean(DF_date$total,na.rm = T)
```

```
## [1] 9354.23
```

### Median of total steps taken per day

```r
median(DF_date$total,na.rm = T)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

```r
  DF_interval <- summarize(group_by(DF,interval),mean = mean(steps,na.rm = T))
  plot(DF_interval$interval,DF_interval$mean,type="l",xlab = "5-minute interval", 
       ylab = "Average number of steps", main = "Average number of steps by 5-minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)\


### 5 minute interval whith on average highest total number of steps taken 

```r
  as.character(DF_interval[which.max(DF_interval$mean),1])
```

```
## [1] "835"
```

## Imputing missing values
### Total number of missing values

```r
sum(is.na(DF$steps))
```

```
## [1] 2304
```

### filling all the missing values with the average of that interval

```r
DF_new <- merge(DF,DF_interval,by="interval")
DF_new$steps <- ifelse(is.na(DF_new$steps),DF_new$mean,DF_new$steps)
DF_new <- DF_new[,1:3]
```

### What is mean total number of steps taken per day with filled missing values?

```r
DF_new_date <- summarize(group_by(DF_new,date),total = sum(steps,na.rm = T))
hist(DF_new_date$total,main = "Total number of steps per day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)\


### Mean of total steps taken per day with filled missing values

```r
mean(DF_new_date$total)
```

```
## [1] 10766.19
```

### median of total steps taken per day with filled missing values

```r
median(DF_new_date$total)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
library(ggplot2)
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
DF_new$DayOfTheWeek <- factor((weekdays(DF_new$date) %in% weekdays), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
DF_new_interval <- summarize(group_by(DF_new,interval,DayOfTheWeek),mean = mean(steps,na.rm = T))
ggplot(DF_new_interval,aes(interval,mean))+geom_line()+facet_grid(DayOfTheWeek ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)\
