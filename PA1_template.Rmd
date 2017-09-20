



##Reproducible Research Course Project 1
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=TRUE)
```

###Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

###Loading and Preprocessing the data
```{r}
activity <- read.csv("activity.csv", header = TRUE, sep = ",")
```

###What is mean total number of steps taken per day?
1.Make a histogram of the total number of steps taken each day  
2.Calculate and report the mean and median total number of steps taken per day
```{r}
sum <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
hist(sum, xlab = "sum of steps per day", main = "histogram of steps per day")
mean<-round(mean(sum))
median<-round(median(sum))
print(c("The mean is",mean))
print(c("The median is",median))
```

###What is the average daily activity pattern?
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  
2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
mean_interval <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
plot(mean_interval ~ unique(activity$interval), type="l", xlab = "Interval",ylab="Average Steps",main="Average Steps over 5 minute intervals",col="blue",lwd=3)
mean_interval[which.max(mean_interval)]
print("The interval with the highest average is 835th inverval with 206 steps")
```

###Imputing missing values
1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  
2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
3.Create a new dataset that is equal to the original dataset but with the missing data filled in.  
4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

#####Total number of missing values
```{r}
summary(activity)
missing<- sum(is.na(activity$steps))
print(c("The total number of missing values is ",missing))
```

#####Filling in the missing values in the new dataset and Making a Histogram
```{r}
activity2 <- activity 
for (i in 1:nrow(activity)){
    if(is.na(activity$steps[i])){
        activity2$steps[i]<- mean_interval[[as.character(activity[i, "interval"])]]
    }
}

sum2 <- tapply(activity2$steps, activity2$date, sum, na.rm=TRUE)
hist(sum2, xlab = "sum of steps per day", main = "histogram of steps per day")
```

#####Mean and Median of new dataset and difference compared to original data
```{r}
mean2<-round(mean(sum2))
median2<-round(median(sum2))
print(c("The new mean is",mean2))
print(c("The new median is",median2))
compare<-NULL
compare <- rbind(compare, data.frame(mean = c(mean, mean2), median = c(median, median2)))
rownames(compare) <- c("with NA's", "without NA's")
print(compare)
```

###Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.  
2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  

```{r}
activity2$weekday <- c("weekday")
activity2[weekdays(as.Date(activity2[, 2])) %in% c("Saturday", "Sunday", "samedi", "dimanche", "saturday", "sunday", "Samedi", "Dimanche"), ][4] <- c("weekend")
table(activity2$weekday == "weekend")
activity2$weekday <- factor(activity2$weekday)
activity2_weekend <- subset(activity2, activity2$weekday == "weekend")
activity2_weekday <- subset(activity2, activity2$weekday == "weekday")
mean_activity2_weekday <- tapply(activity2_weekday$steps, activity2_weekday$interval, mean)
mean_activity2_weekend <- tapply(activity2_weekend$steps, activity2_weekend$interval, mean)
```

```{r}
library(lattice)
df_weekday <- data.frame(interval = unique(activity2_weekday$interval), avg = as.numeric(mean_activity2_weekday), day = rep("weekday", length(mean_activity2_weekday)))
df_weekend <- data.frame(interval = unique(activity2_weekend$interval), avg = as.numeric(mean_activity2_weekend), day = rep("weekend", length(mean_activity2_weekend)))
df_final <- rbind(df_weekday, df_weekend)
xyplot(avg ~ interval | day, data = df_final, layout = c(1, 2), 
       type = "l", ylab = "Number of steps",col="blue",lwd=3)
```

