---
title: "PA1_template.md"
author: "Caesar Hermogeno"
date: "May 3, 2016"
output: html_document
---

Reproducible Research: Peer Assessment 1

Loading the data for processing

unzip(zipfile="activity.zip")

mydata <- read.csv("activity.csv")

names(mydata)

str(mydata)

head(mydata)

What is mean total number of steps taken per day?

library(ggplot2)

totalnum.steps <- tapply(mydata$steps, mydata$date, FUN = sum, na.rm = TRUE)

qplot(totalnum.steps, binwidth = 1000, xlab = "total number of steps taken each day")

mean(totalnum.steps, na.rm = TRUE)

median(totalnum.steps, na.rm = TRUE)

What is the average daily activity pattern?

library(ggplot2)

averages <- aggregate(x = list(steps = mydata$steps), by = list(interval = mydata$interval), 
                      FUN = mean, na.rm = TRUE)
ggplot(mydata = averages, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + 
  ylab("average number of steps taken")
  
On average across all the days in the dataset, the 5-minute interval contains the maximum number of steps?
  
averages[which.max(averages$steps), ]

Imputing missing values

There are many days/intervals where there are missing values (coded as  NA ). The presence of missing days may introduce bias into some calculations or summaries of the data.

missing <- is.na(mydata$steps)

table(missing)

All of the missing values are filled in with mean value for that 5-minute interval.

# Replace each missing value with the mean value of its 5-minute interval

fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps)) 
    filled <- c(steps) else filled <- (averages[averages$interval == interval, "steps"])
    return(filled)
}

filled.mydata <- mydata

filled.mydata$steps <- mapply(fill.value, filled.mydata$steps, filled.mydata$interval)


Using the filled data set, create a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.

totalnum.steps <- tapply(filled.mydata$steps, filled.mydata$date, FUN=sum)

qplot(totalnum.steps, binwidth=1000, xlab="total number of steps taken each day")

mean(totalnum.steps)

median(totalnum.steps)

Are there differences in activity patterns between weekdays and weekends?

Let's find the day of the week for each measurement in the dataset. 

weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}

filled.mydata$date <- as.Date(filled.mydata$date)

filled.mydata$day <- sapply(filled.mydata$date, FUN=weekday.or.weekend)

Let's make a panel plot containing plots of average number of steps taken on weekdays and weekends.

averages <- aggregate(steps ~ interval + day, mydataa = filled.mydata, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
  xlab("5-minute interval") + ylab("Number of steps")


    
    


