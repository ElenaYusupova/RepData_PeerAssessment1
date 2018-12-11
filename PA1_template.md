---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




## Loading and preprocessing the data

In the first step I downloaded and imported the data. The main preprocessing as to ensure that variables will be in the right format.

```r
sDataDir <- "Y:/R_ElkaTraining/Coursera/Data_Science/5_Reproducible Research/Week2"
if (!file.exists("projectdata.zip")) {
    
    projectdata<- download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
                                "projectdata.zip")
    
}

if (!dir.exists("Data") && !file.exists(file.path(sDataDir, "activity_monitoring_data.csv"))) {
    
    # Unzip files to Data directory
    unzip("projectdata.zip", exdir = "Data")
}

# Read in the data 
sDataDir <- file.path(sDataDir, "Data")
mdata<- read.csv(file.path(sDataDir, "activity.csv"), header = T) 

# First we ensure that the data variables are in the right format
mdata$steps <- as.numeric(mdata$steps)
mdata$date <- as.Date(mdata$date)
```

## What is mean total number of steps taken per day?

In the next step I calculate how many steps are taken on average every day. The results are presented also in histogram.   

```r
asteps <- tapply(mdata$steps,mdata$date,sum)
par(mfrow = c(1,1))
hist(asteps, xlab = "Count of Steps", main = "Histogram of the total number of steps taken each day", 
     breaks=8)
```

![](RR_Markup_Document_files/figure-html/step1-1.png)<!-- -->

```r
mean(asteps, na.rm=T)
```

```
## [1] 10766.19
```

```r
median(asteps,na.rm=T)
```

```
## [1] 10765
```
The average number of steps taken per day is 10766, median is 10765.  

## What is the average daily activity pattern?

In this part I create a time series plot of the average number of steps taken in 5-minute interval 
across all days


```r
astepsper5min <- tapply(mdata$steps,mdata$interval,mean,na.rm=T) 

plot(names(astepsper5min), astepsper5min, type="l", xlab="Interval", ylab="Steps", main="Average number of steps taken in each 5min interval")
```

![](RR_Markup_Document_files/figure-html/step2-1.png)<!-- -->

```r
which.max(astepsper5min)
```

```
## 835 
## 104
```
Interval 835 has on average, across all the days in the dataset, the highest number of steps

## Imputing missing values
The dataset contains a number of missing observiations. 

```r
length(which(is.na(mdata$steps)))
```

```
## [1] 2304
```
In particular there are 2304 missing observations. 

As the next step I replaced all missing values with the average number of steps taken in that respective interval on other days   

```r
mdata2 <- mdata

mdata2$steps[which(is.na(mdata2$steps))] <- sapply(mdata2$interval[which(is.na(mdata2$steps))], function(x) {
    # x <- mdata2$interval[which(is.na(mdata2$steps))][1]
    
    return(astepsper5min[paste0(x)])
})
```
New dataset mdata2 will contain imputed data replacing missing values. 

After the imputations I created histogram with the original and new dataset to compare the results. 

```r
asteps2 <- tapply(mdata2$steps,mdata2$date,sum)
par(mfrow = c(1,2), mar = c(3, 4, 4,1.5), oma=c(0,0,2,0))
hist(asteps, xlab = "Count of Steps", main = "Original data", breaks=8)
hist(asteps, xlab = "Count of Steps", main = "Data with imputed missing values", breaks=8)
par(title(main="Histogram of the total number of steps taken each day", outer=T, cex=1.5))
```

![](RR_Markup_Document_files/figure-html/step5-1.png)<!-- -->

```
## named list()
```

```r
#Now we compare the mean and median
df <- NULL
df$col1 <- c(1, 2)
df$col2 <- c(4, 5)
df <- as.data.frame(df)
rownames(df) <- c("mean", "median")
colnames(df) <- c("Original data", "Dataset with imputations")
df[1, 1] <- mean(asteps, na.rm=T)
df[2,1]  <- median(asteps,na.rm=T)
df[1,2] <- mean(asteps2)
df[2,2]  <- median(asteps2)
df
```

```
##        Original data Dataset with imputations
## mean        10766.19                 10766.19
## median      10765.00                 10766.19
```
Imputing values increases the total number of steps in the dataset but does not impact the average number of steps taken each day

## Are there differences in activity patterns between weekdays and weekends?

In this part I first created a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
After I calculated the average number of steps taken in 5 minutes interval across weekdays and weekends.
I used this object to make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
mdata2$DayOfWeek <- "weekend"
mdata2$DayOfWeek[which(weekdays(mdata2$date) %in% c("Monday", "Tuesday", "Wednesday","Thursday","Friday"))] <- "weekday" 
astepsper5minByWeekday <- aggregate(steps ~ interval + DayOfWeek, mdata2, mean) 

#Create chart
library(ggplot2)
g <- ggplot(astepsper5minByWeekday, aes(interval,steps))
g + geom_line(colour="#000090") + facet_wrap(~ DayOfWeek, ncol=1) + labs(y = "Number of steps") +labs(title = "Average number of steps taken in each 5min interval")
```

![](RR_Markup_Document_files/figure-html/step6-1.png)<!-- -->



