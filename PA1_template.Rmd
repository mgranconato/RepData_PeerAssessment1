# Reproducible Research: Peer Assessment 1
by Marcello Granconato
[Git hub repository](https://github.com/mgranconato/RepData_PeerAssessment1)  

## Loading and preprocessing the data

Use ggplot2 
```{r, results='hide'}
library(ggplot2)
```

**Unzip and loading raw data**

```{r} 
activity <- read.csv(unzip('activity.zip'))
```


## What is mean total number of steps taken per day?

**Histogram of the total number of steps taken each day**

```{r, results='hide'}
# create a new data frame for the histogram
activityByDate <- data.frame(rowsum(activity$steps, activity$date))
names(activityByDate) <- 'TotalSteps'
qplot(activityByDate$TotalSteps, binwidth = 1000,xlab = "Total Steps by Date", ylab = "Frequency")  

#calculate mean and median
stepsMean <- mean(activityByDate$TotalSteps,na.rm=TRUE)
stepsMedian <- median(activityByDate$TotalSteps,na.rm=TRUE)
```


**Mean and median total number of steps taken per day**
  
  Mean: `r as.integer(stepsMean)`  
  Median: `r stepsMedian`


## What is the average daily activity pattern?

**Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

```{r}
# create a new data frame for the histogram
activityByInterval <- aggregate(steps ~ interval, data=activity, FUN=mean)
names(activityByInterval) <- c('interval', 'StepsMean')

#Time series plot
qplot(activityByInterval$interval, 
      activityByInterval$StepsMean, 
      geom='line',xlab = "5-minute Interval", ylab = "steps mean")
```


**5-minute interval, on average across all the days in the dataset which contains the maximum number of steps**

```{r, results='hide'}
# subset the data frame using  the max number of step, which.max(activityByInterval$steps)
# store the values in 2 variables
intervalMaxSteps <- activityByInterval[which.max(activityByInterval$StepsMean),]$interval
intervalMaxMeanSteps <- activityByInterval[which.max(activityByInterval$StepsMean),]$StepsMean

```

  5-minute interval with the maximum number of step: `r intervalMaxSteps`  
  Number of average steps at the maximum 5-minute inteval: `r intervalMaxMeanSteps`


## Imputing missing values

```{r, results='hide'}
activityNA <- sum(is.na(activity$steps))

```

The total number of missing values is: `r activityNA`  

```{r results='hide'}
# new dataframe with NA values replaced by interval averages.

# create a new dataframe with a column with the average of the interval
activity2 <- merge(activity, activityByInterval, by='interval')

#replace the NA value with the average
activity2$steps[is.na(activity2$steps)] <- 
    as.integer(activity2$StepsMean[is.na(activity2$steps)])
```

```{r, results='hide'}
# create a the data frame for the histogram with the filled NA values
activity2ByDate <- data.frame(rowsum(activity2$steps, activity2$date))
names(activity2ByDate) <- 'TotalSteps'
qplot(activity2ByDate$TotalSteps, binwidth = 1000, main = "Hystogram with Filled data",xlab = "Total Steps by Date", ylab = "Frequency")  

#calculate mean and median
steps2Mean <- mean(activity2ByDate$TotalSteps,na.rm=TRUE)
steps2Median <- median(activity2ByDate$TotalSteps,na.rm=TRUE)
```


**Mean and median total number of steps taken per day on the activity dataframe with NA filled**
  
  Mean with "filled data": `r as.integer(steps2Mean)`  
  Median with "filled data": `r as.integer(steps2Median)`

The value are different from the dataset with NA, there is an increase in the difference between mean and median


## Are there differences in activity patterns between weekdays and weekends?

**Create a new factor variable with two levels - "weekday" and "weekend"**  

```{r, results='hide'}
# add to the activity2 dataframe a dayweektype column abd fill it with the value 'weekday'
# we will change the value to 'weekend" on the right date searching the wday (number from 0 to 6 (sunday to saturday)
# value in date

activity2$dayweektype <- 'weekday'
activity2$dayweektype[as.POSIXlt(activity2$date)$wday %in% c(6,0)] <- 'weekend'
activity2$dayweektype <- as.factor(activity2$dayweektype)

dataFinalPlot <- 
    with(activity2, 
         aggregate(steps, list(dayweektype, interval), mean))
names(dataFinalPlot) <- c('dayweektype', 'interval', 'StepsMean')
```

**Plot containing a time series plot split by "weekday" and "weekend"**  

```{r, results='hide'}
finalPlot <- qplot(interval, 
           StepsMean, data = dataFinalPlot, geom = 'line', main = "Weekday vs Weekend pattern", xlab = "5-minute interval",ylab = 'Number of steps') 

finalPlot + facet_wrap(~ dayweektype,ncol=1)

```


