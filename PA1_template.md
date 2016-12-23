---
title: "Untitled"
author: "Ashley Purcell"
date: "December 22, 2016"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Read Data

```{r}
activity <- read.csv("activity.csv", header = TRUE)
```

## What is the mean total number of steps taken per day?

```{r}
totalstepsperday <- tapply(activity$steps, activity$date, FUN = sum,  na.rm = TRUE)
totalstepsperday
```

###1. Make a histogram of the total number of steps taken each day.

```{r}
totalstepseachday <- aggregate(activity$steps, list(activity$date), sum)
colnames(totalstepseachday) <- c("date", "steps")

library(ggplot2)
ggplot(data = totalstepseachday, aes(x=steps)) + geom_histogram(fill = "#000000") + ggtitle("Steps Taken Per Day") + labs(x = "Number of Steps per Day", y = "Number of Times in a Day")
````

###2. Calculate and report the mean and median total number of steps taken per day.

````{r}
stepmean <- mean(totalstepsperday)
stepmean
````

````{r}
stepmedian <- median(totalstepsperday)
stepmedian
````

##What is the average daily activity pattern?

###1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

````{r}
fiveminutemean <- aggregate(activity$steps, by = list(interval = as.factor(activity$interval)), FUN = mean, na.rm = TRUE)

fiveminutemean$interval <- as.integer(levels(fiveminutemean$interval)[fiveminutemean$interval])

colnames(fiveminutemean) <- c("interval", "steps")

ggplot(fiveminutemean, aes(x = interval, y = steps)) + geom_line()
````

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``````{r}
maxinterval <- fiveminutemean[which.max(fiveminutemean$steps), ]
maxinterval
````

##Input Missing Values
Note that there are a number of days/intervals where there are missing values. The presence of missing days may introduce bias into some calculations or summaries of the data.

###1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

````{r}
valuesNA <- sum(is.na(activity$steps))
valuesNA
````

###2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

````{r}
stepinterval <- aggregate(steps~interval, activity, FUN = mean)
head(activity)
````

````{r}
for (i in 1:nrow(activity)){
     tmp <- activity$steps[i]
     if(is.na(tmp)){
         for(j in 1:nrow(stepinterval)){
             if(activity$interval[i] == stepinterval$interval[j]){
                 activity$steps[i] = stepinterval$steps[j]
                 break
             }
         }
     }  
 }

head(data)

head(activity)
````

###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

````{r}
newsteps <- aggregate(activity$steps, list(activity$date), sum)
colnames(newsteps) <- c("date", "steps")

library(ggplot2)
ggplot(newsteps, aes(x = steps)) + geom_histogram(fill = "#000000") +
  ggtitle("Steps Taken Per Day") + labs(x = "Number of Steps Per Day", y = "Number of Times in a Day")
````

###4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

````{r}
newmean <- mean(newsteps$steps)
newmean
````

````{r}
newmedian <- median(newsteps$steps)
newmedian
````

````{r}
newstepinterval <- aggregate(activity$steps, by = list(interval = as.factor(activity$interval)), FUN = mean, na.rm = TRUE)

newstepinterval$interval <- as.integer(levels(newstepinterval$interval)[newstepinterval$interval])

colnames(newstepinterval) <- c("interval", "steps")

ggplot(newstepinterval, aes(x = interval, y = steps)) + geom_line()
````

####The mean and median estimates did not change. 

##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

###1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

````{r}
week_day <- function(date_val) {
    wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
    if  (!(wd == 'Saturday' || wd == 'Sunday')) {
        x <- 'Weekday'
    } else {
        x <- 'Weekend'
    }
    x
}
````

###2. Make a panel plot containing a time series plot (i.e. type = "1") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

````{r}
activity$day_type <- as.factor(sapply(activity$date, week_day))

library(ggplot2)

stepsperdayplot <- aggregate(steps ~ interval+day_type, activity, mean)

# Create the plot
comparisonplot <- ggplot(stepsperdayplot, aes(interval, steps)) +
    geom_line(stat = "identity", aes(color = day_type)) +
    theme_gray() +
    facet_grid(day_type ~ ., scales="fixed", space="fixed") +
    labs(x="Interval", y=expression("Number of Steps")) +
    ggtitle("Number of Steps Per Interval by Day Type")
print(comparisonplot)
````
