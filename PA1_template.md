Course Project Assignment 1

----------------------------------------------

# Loading and preprocessing the data

* Read in activity.csv file, clear the NAs


```r
unzip("activity.zip")
```

```
## Warning in unzip("activity.zip"): error 1 in extracting from zip file
```

```r
data <- read.csv("activity.csv")
data2 <- data[!is.na(data$steps),]
```

# What is mean total number of steps taken per day?

* Calculate the daily total number of steps, the mean, and median


```r
data2$date <- as.character(data2$date)
Date <- levels(as.factor(data2$date))

step.sum <- vector(mode = "numeric", length = length(Date))
step.record <- vector(mode = "numeric", length = length(Date))
step.mean <- vector(mode = "numeric", length = length(Date))
step.median <- vector(mode = "numeric", length = length(Date))

for(i in 1:length(Date)){
    step.sum[i] <- sum(data2$steps[data2$date == Date[i]])
    step.record[i] <- sum(data2$date == Date[i]) 
    step.mean[i] <- step.sum[i]/step.record[i]
    step.median[i] <- median(data2$steps[data2$date == Date[i]])
}
```

* Histogram of total number of steps each day


```r
plot1 <- hist(step.sum, breaks = 25, col = "lightblue", main = "Histogram of the Total Number of Steps Each Day", xlab = "Total number of steps each day", ylab = "Frequency", xlim = c(0,25000))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
print(plot1)
```

```
## $breaks
##  [1]     0  1000  2000  3000  4000  5000  6000  7000  8000  9000 10000
## [12] 11000 12000 13000 14000 15000 16000 17000 18000 19000 20000 21000
## [23] 22000
## 
## $counts
##  [1]  2  0  1  1  1  2  1  2  5  2 10  6  6  4  2  5  0  1  0  0  1  1
## 
## $density
##  [1] 3.773585e-05 0.000000e+00 1.886792e-05 1.886792e-05 1.886792e-05
##  [6] 3.773585e-05 1.886792e-05 3.773585e-05 9.433962e-05 3.773585e-05
## [11] 1.886792e-04 1.132075e-04 1.132075e-04 7.547170e-05 3.773585e-05
## [16] 9.433962e-05 0.000000e+00 1.886792e-05 0.000000e+00 0.000000e+00
## [21] 1.886792e-05 1.886792e-05
## 
## $mids
##  [1]   500  1500  2500  3500  4500  5500  6500  7500  8500  9500 10500
## [12] 11500 12500 13500 14500 15500 16500 17500 18500 19500 20500 21500
## 
## $xname
## [1] "step.sum"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```

```r
dev.copy(png, file = "plot1.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

* Summary of daily step records


```r
StepSummary <- data.frame(date = Date, StepSum = step.sum, StepRecords = step.record, StepMean = step.mean, StepMedian = step.median)

summary(StepSummary)
```

```
##          date       StepSum       StepRecords     StepMean      
##  2012-10-02: 1   Min.   :   41   Min.   :288   Min.   : 0.1424  
##  2012-10-03: 1   1st Qu.: 8841   1st Qu.:288   1st Qu.:30.6979  
##  2012-10-04: 1   Median :10765   Median :288   Median :37.3785  
##  2012-10-05: 1   Mean   :10766   Mean   :288   Mean   :37.3826  
##  2012-10-06: 1   3rd Qu.:13294   3rd Qu.:288   3rd Qu.:46.1597  
##  2012-10-07: 1   Max.   :21194   Max.   :288   Max.   :73.5903  
##  (Other)   :47                                                  
##    StepMedian
##  Min.   :0   
##  1st Qu.:0   
##  Median :0   
##  Mean   :0   
##  3rd Qu.:0   
##  Max.   :0   
## 
```

# What is the average daily activity pattern?

* Average Steps in 5 Minutes Interval Across All Days


```r
t <- as.ts(data2$interval[data2$date == Date[1]])

interval.step <- vector(mode = "numeric", length = length(t))
for(i in 1:288){
    interval.step[i] <- sum(data2$steps[data2$interval == t[i]])/53
}

MeanStep <- data.frame(interval = t, steps = interval.step)

peak.time <- t[which(interval.step == max(interval.step))]
  
plot(t, interval.step, type = "l", main = "Average Steps in 5 Minutes Interval Across All Days", col = "blue", xlab ="Time in a day (24 hours)", ylab = "Average steps")

abline(v = peak.time, col = "red")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
dev.copy(png, file = "daily-frequency.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

The peak time is at 835 of the day. 


# Imputing missing values

* Calculate and report the total number of missing values in the dataset


```r
na.num <- sum(is.na(data$steps))
```
The total numer of rows with NAs are 2304

* Replace NA values with interval-mean, and forming a new dataset


```r
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
na.subset <- data[is.na(data$steps),]
na2 <- merge(na.subset, MeanStep, by = "interval")
Na2Mean <- data.frame(steps = na2$steps.y, date = na2$date, interval = na2$interval)
data3 <- merge(data2, Na2Mean, all = T)
data3 <- arrange(data3, date, interval)
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day


```r
Date.n <- levels(as.factor(data3$date))

step.sum.n <- vector(mode = "numeric", length = length(Date.n))
step.record.n <- vector(mode = "numeric", length = length(Date.n))
step.mean.n <- vector(mode = "numeric", length = length(Date.n))
step.median.n <- vector(mode = "numeric", length = length(Date.n))

for(i in 1:length(Date.n)){
    step.sum.n[i] <- sum(data3$steps[data3$date == Date.n[i]])
    step.record.n[i] <- sum(data3$date == Date.n[i]) 
    step.mean.n[i] <- step.sum.n[i]/step.record.n[i]
    step.median.n[i] <- median(data3$steps[data3$date == Date.n[i]])
}

plot2 <- hist(step.sum.n, breaks = 25, col = "pink", main = "Histogram of the Total Number of Steps Each Day with NA Replaced", xlab = "Total number of steps each day", ylab = "Frequency", xlim = c(0,25000))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
print(plot2)
```

```
## $breaks
##  [1]     0  1000  2000  3000  4000  5000  6000  7000  8000  9000 10000
## [12] 11000 12000 13000 14000 15000 16000 17000 18000 19000 20000 21000
## [23] 22000
## 
## $counts
##  [1]  2  0  1  1  1  2  1  2  5  2 18  6  6  4  2  5  0  1  0  0  1  1
## 
## $density
##  [1] 3.278689e-05 0.000000e+00 1.639344e-05 1.639344e-05 1.639344e-05
##  [6] 3.278689e-05 1.639344e-05 3.278689e-05 8.196721e-05 3.278689e-05
## [11] 2.950820e-04 9.836066e-05 9.836066e-05 6.557377e-05 3.278689e-05
## [16] 8.196721e-05 0.000000e+00 1.639344e-05 0.000000e+00 0.000000e+00
## [21] 1.639344e-05 1.639344e-05
## 
## $mids
##  [1]   500  1500  2500  3500  4500  5500  6500  7500  8500  9500 10500
## [12] 11500 12500 13500 14500 15500 16500 17500 18500 19500 20500 21500
## 
## $xname
## [1] "step.sum.n"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```

```r
dev.copy(png, file = "plot2.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
When the NAs in step are replaced by mean steps of that spefic interval, the result is centralization of the histogram (towards the peak).

* Summary of daily step records


```r
StepSummary.n <- data.frame(date = Date.n, StepSum = step.sum.n, StepRecords = step.record.n, StepMean = step.mean.n, StepMedian = step.median.n)

summary(StepSummary.n)
```

```
##          date       StepSum       StepRecords     StepMean      
##  2012-10-01: 1   Min.   :   41   Min.   :288   Min.   : 0.1424  
##  2012-10-02: 1   1st Qu.: 9819   1st Qu.:288   1st Qu.:34.0938  
##  2012-10-03: 1   Median :10766   Median :288   Median :37.3826  
##  2012-10-04: 1   Mean   :10766   Mean   :288   Mean   :37.3826  
##  2012-10-05: 1   3rd Qu.:12811   3rd Qu.:288   3rd Qu.:44.4826  
##  2012-10-06: 1   Max.   :21194   Max.   :288   Max.   :73.5903  
##  (Other)   :55                                                  
##    StepMedian    
##  Min.   : 0.000  
##  1st Qu.: 0.000  
##  Median : 0.000  
##  Mean   : 4.474  
##  3rd Qu.: 0.000  
##  Max.   :34.113  
## 
```

# Are there differences in activity patterns between weekdays and weekends?

* Make a new factor variable 


```r
weekdays <- c("星期一", "星期二", "星期三", "星期四", "星期五")

data3$weekday <- factor(weekdays(as.Date(data3$date)) %in% weekdays, levels = c(TRUE, FALSE), labels = c("weekday", "weekend"))
```

* Calculate the mean step in each group and each interval


```r
data.w <- tbl_df(data3)
data.w.mean <- summarize(group_by(data.w, weekday, interval), mean(steps))
colnames(data.w.mean) <- c("weekday", "interval", "mean.step")
```

* Make a facet plot to compare between weekday and weekend


```r
library(ggplot2)
g <- ggplot(data.w.mean, aes(x = interval, y = mean.step, color = weekday))
g + geom_line(size = 1) + facet_grid(weekday~.) + labs(x = "Time interval in a day", y = "Mean steps in 5 minutes interval", title = "Mean Steps in Each Time Interval During Weekday and Weekend")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

```r
dev.copy(png, file = "compare.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

