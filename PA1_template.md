# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. Unzip 'activity.zip' file;
2. Read 'activity.csv' file into a data frame;
3. Exclude NAs.


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
data_preprocessed <- data[!is.na(data$steps), ]
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day.


```r
total_steps_per_day <- aggregate(x = data_preprocessed$steps, by = list(data_preprocessed$date), FUN = sum)
names(total_steps_per_day) <- c("Date", "Total.steps")
```

2. Make a histogram of the total number of steps taken each day.


```r
library("ggplot2")
g <- ggplot(data = total_steps_per_day, aes(Total.steps))
print(
    g
    + geom_histogram(bins = 5, col = "white", fill = "cadetblue", alpha = 0.7)
    + xlab("Total steps per day")
    + ylab("Number of days")
    + ggtitle("Total number of steps taken each day")
    + theme(axis.title = element_text(face="bold"), plot.title = element_text(face="bold"))
)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day.


```r
mean(total_steps_per_day$Total.steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_per_day$Total.steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
average_steps_per_day <- aggregate(x = data_preprocessed$steps, by = list(data_preprocessed$interval), FUN = mean)
names(average_steps_per_day) <- c("Interval", "Average.steps")
g <- ggplot(data = average_steps_per_day, aes(Interval, Average.steps))
print(
    g
    + geom_line(color = "cadetblue")
    + xlab("Interval")
    + ylab("Average number of steps")
    + ggtitle("Average number of steps taken per day")
    + theme(axis.title = element_text(face="bold"), plot.title = element_text(face="bold"))
)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_steps_per_day <- aggregate(x = data_preprocessed$steps, by = list(data_preprocessed$interval), FUN = max)
names(max_steps_per_day) <- c("Interval", "Max.steps")
max_steps_per_day[max_steps_per_day$Max.steps == max(max_steps_per_day$Max.steps), "Interval"]
```

```
## [1] 615
```

## Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
The strategy to fill missing values is to use the mean for that 5-minute interval.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data_rm_na <- data
for (i in which(sapply(data_rm_na, is.na))) {
    data_rm_na[i, "steps"] <- as.integer(mean(data_rm_na[data_rm_na$interval == data_rm_na[i, "interval"], "steps"], na.rm = TRUE))
}
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_steps_per_day_rm_na <- aggregate(x = data_rm_na$steps, by = list(data_rm_na$date), FUN = sum)
names(total_steps_per_day_rm_na) <- c("Date", "Total.steps")

g <- ggplot(data = total_steps_per_day_rm_na, aes(Total.steps))
print(
    g
    + geom_histogram(bins = 5, col = "white", fill = "cadetblue", alpha = 0.7)
    + xlab("Total steps per day")
    + ylab("Number of days")
    + ggtitle("Total number of steps taken each day")
    + theme(axis.title = element_text(face="bold"), plot.title = element_text(face="bold"))
)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(total_steps_per_day_rm_na$Total.steps)
```

```
## [1] 10749.77
```

```r
median(total_steps_per_day_rm_na$Total.steps)
```

```
## [1] 10641
```

## Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.



```r
data_rm_na$weekday <- factor(
    (weekdays(as.Date(data_rm_na$date)) %in% c("Saturday", "Sunday")),
    levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))
average_steps_per_day_rm_na <- aggregate(x = data_rm_na$steps, by = list(data_rm_na$interval, data_rm_na$weekday), FUN = mean)
names(average_steps_per_day_rm_na) <- c("Interval", "Weekday", "Average.steps")
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
g <- ggplot(average_steps_per_day_rm_na, aes(Interval, Average.steps))
print(
    g
    + geom_line(color = "cadetblue")
    + facet_grid(Weekday ~ .)
    + xlab("Interval")
    + ylab("Average number of steps")
    + ggtitle("Average number of steps taken per day")
    + theme(axis.title = element_text(face="bold"), plot.title = element_text(face="bold"))
)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
