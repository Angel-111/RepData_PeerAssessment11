# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Task 1: mean and median total steps per day
------

#### Get averages by day
```{r}
steps <-read.csv ("~/Downloads/activity.csv")
stepdays<-tapply(steps$steps, steps$date, sum)
hist(stepdays, main="Steps per day", xla="steps", yla="number of days")
```


## What is mean total number of steps taken per day?

#### Mean and median: 
```{r}
mean(stepdays, na.rm=T)
median(stepdays, na.rm=T)
```


## What is the average daily activity pattern?

### Plot time series:
```{r}
stepmin<-aggregate(steps$steps, list(steps$interval), mean, na.rm=T)
names(stepmin)<-c("interval", "steps")
plot(stepmin$interval, stepmin$steps, type="l")
```

### Get the interval with the most steps:
```{r}
stepmin[order(stepmin$steps),][288,]$interval
```

## Imputing missing values

Since some timepoints very reliably have zero, the proper way to do this is probably a two-part model: first determine whether a give point should be zero, and if not, impute based on interval and day. 

Actually, we should probably use multiple methods based on the surrounding data. If one or two points are missing, linear interpolation from the adjacent points is probably a good bet. If they're missing in large chunks, that's not a good bet. 

But to be lazy, let's just predict based on time. Time isn't linear and is only vaguely resembles quadratic, so I'll make it into 3 hour bins and treat as categorical. 

### number of missing data points:

```{r}
length(which(is.na(steps$steps)))
```

### steps ~ time model:

```{r}
#note: R incorrectly thinks the HHMM time is an integer, but we can work with that instead of having to convert.
steps$hour = floor(steps$interval/100)
steps$period=floor(steps$hour/3)
steps$period<-factor(steps$period)
levels(steps$period)<-c("0-2", "3-5", "6-8", "9-11", "12-14", "15-17", "18-20", "21-23")
mod<-lm(steps ~ period, data=steps)
mod
```

Now imputing (the instructions say to create a new dataset, but I'm just adding a variable to the existing dataset, which lends itself to more useful comparisons):

```{r}
steps$stepsi<-steps$steps
steps$stepsi[is.na(steps$steps)]<-predict(mod, newdata=steps[is.na(steps$steps),])
```

```{r}
stepdaysi<-tapply(steps$stepsi, steps$date, sum, na.rm=T)
stepdaysi
hist(stepdaysi, main="Steps per day (with imputed data)", xla="steps", yla="number of days", col="#ff99ff")
```
*mean (with imputed data)*:
```{r}
mean(stepdaysi, na.rm=T)
```
*median  (with imputed data)*:
```{r}
median(stepdaysi, na.rm=T)
```



## Are there differences in activity patterns between weekdays and weekends?

First, convert the date variable from factor to string, and then to a date object. Then we can get weekday/weekend status. 

```{r}
steps$ddate<-as.character(steps$date)
steps$ddate<-as.Date(steps$ddate, format="%Y-%m-%d")
steps$weekday<-weekdays(steps$ddate)
steps$weekend<-F
steps$weekend[steps$weekday %in% c("Saturday", "Sunday")]<-T
stepmin.i.weekdays<-aggregate(steps$stepsi[!steps$weekend], list(steps$interval[!steps$weekend]), mean, na.rm=T)
stepmin.i.weekends<-aggregate(steps$stepsi[steps$weekend], list(steps$interval[steps$weekend]), mean, na.rm=T)
names(stepmin.i.weekdays)<-c("interval", "steps")
names(stepmin.i.weekends)<-c("interval", "steps")
par(mfrow = c(2,1), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
plot(stepmin.i.weekends$interval, stepmin.i.weekends$steps, pch="", ylab="Steps", xlab="", main="weekend", type="l", ylim=c(0,220), col="blue")
plot(stepmin.i.weekdays$interval, stepmin.i.weekdays$steps, pch="", ylab="Steps", xlab="", main="weekday", type="l",  ylim=c(0,220), col="darkred")
```