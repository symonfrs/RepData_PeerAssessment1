#Reproducible Research
# Week1 Project Assignment

setwd("B:\\Studies\\Coursera\\Data Science Specialization\\Reproducible Research\\Week1\\Project\\repdata-data-activity")
getwd()

#Loading the Activity data
activitydat <- read.csv("activity.csv", header = TRUE, colClasses=c("numeric", "character", "numeric"))

#Formating the Activity data
activitydat$date<-as.Date(activitydat$date)
activitydat$interval<-as.factor(activitydat$interval)

# What is mean total number of steps taken per day?

totalStepsperDay <- aggregate(steps ~ date, activitydat, sum)
colnames(totalStepsperDay) <- c("date","steps")
head(totalStepsperDay)

#Histogram of the total # of steps per day
library("ggplot2")
g <- ggplot(totalStepsperDay, aes(x=steps))
g <- g + geom_histogram(fill = "brown", binwidth = 1000) +
    xlab("Total number of Steps taken per day") +
    ylab("Frequency") +
    ggtitle('Histogram of Total number of Steps per Day')
print(g)

# Mean and median of number of steps per day
stepsMean   <- mean(totalStepsperDay$steps, na.rm=TRUE)
stepsMedian <- median(totalStepsperDay$steps, na.rm=TRUE)
stepsMean
stepsMedian

# What is the average daily activity pattern?
###1.	Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
###the average number of steps taken, averaged across all days (y-axis)

avgStepsper5min <- aggregate(activitydat$steps, 
                             by = list(interval = activitydat$interval),
                             FUN=mean, na.rm=TRUE)
avgStepsper5min$interval <- as.integer(levels(avgStepsper5min$interval)[avgStepsper5min$interval])
colnames(avgStepsper5min) <- c("interval", "steps")

g <- ggplot(avgStepsper5min, aes(x=interval, y=steps))
g <- g + geom_line(color="blue", size=1) +  
    labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
    theme_bw()
print(g)
maxInterval <- avgStepsper5min[which.max(avgStepsper5min$steps),]

#	Calculate and report the total number of missing values in the dataset (i.e. the total
#   number of rows with NAs)

rowsWithNA<-activitydat[is.na(activitydat$steps),]
numrowsWithNA<-length(rowsWithNA$steps)
numrowsWithNA


#	Devise a strategy for filling in all of the missing values in the dataset. The strategy
#   does not need to be sophisticated. For example, you could use the mean/median for that
#   day, or the mean for that 5-minute interval, etc.

rowsWithNA["steps"] <- lapply("steps", function(x) avgStepsper5min[[x]][match(rowsWithNA$interval, avgStepsper5min$interval)])

#	Create a new dataset that is equal to the original dataset but with the missing data 
#   filled in.
rowsWithoutNA<-activitydat[!is.na(activitydat$steps),]
filledActivitydat<-rbind(rowsWithoutNA,filledRowsWithNA)
sum(is.na(filledActivitydat$steps))
str(filledActivitydat)

#	Make a histogram of the total number of steps taken each day and Calculate and report 
#   the mean and median total number of steps taken per day. Do these values differ from 
#   the estimates from the first part of the assignment? What is the impact of imputing 
#   missing data on the estimates of the total daily number of steps?

# What is mean total number of steps taken per day?

totalStepsperDayNew <- aggregate(steps ~ date, filledActivitydat, sum)
colnames(totalStepsperDayNew) <- c("date","steps")

#Histogram of the total # of steps per day
library("ggplot2")
g <- ggplot(totalStepsperDayNew, aes(x=steps))
g <- g + geom_histogram(fill = "lightgreen", binwidth = 1000) +
    xlab("Total number of Steps taken per day") +
    ylab("Frequency") +
    ggtitle('Histogram of Total number of Steps per Day')
print(g)

# Mean and median of number of steps per day
stepsMeanNew   <- mean(totalStepsperDayNew$steps, na.rm=TRUE)
stepsMedianNew <- median(totalStepsperDayNew$steps, na.rm=TRUE)

stepsMean
stepsMedian
stepsMeanNew
stepsMedianNew

#Are there differences in activity patterns between weekdays and weekends?
#For this part the weekdays() function may be of some help here. Use the dataset with the
#filled-in missing values for this part.
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.
#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
#interval (x-axis) and the average number of steps taken, averaged across all weekday 
#days or weekend days (y-axis). 

#Augument data with day of the week

filledActivitydat$weekday<-as.factor(weekdays(filledActivitydat$date))
filledActivitydat$weekdayType<-"Weekday"
filledActivitydat[filledActivitydat$weekday=="Saturday",]$weekdayType<-"Weekend"
filledActivitydat[filledActivitydat$weekday=="Sunday",]$weekdayType<-"Weekend"

filledActivitydatWeekday<-subset(filledActivitydat,weekdayType=="Weekday")
filledActivitydatWeekend<-subset(filledActivitydat,weekdayType=="Weekend")

StepsperIntervalWeekday <- aggregate(filledActivitydatWeekday$steps, 
                             by = list(weekdayType=filledActivitydatWeekday$weekdayType,interval = filledActivitydatWeekday$interval),
                             FUN=mean, na.rm=TRUE)
StepsperIntervalWeekday$interval <- as.integer(levels(StepsperIntervalWeekday$interval)[StepsperIntervalWeekday$interval])
colnames(StepsperIntervalWeekday) <- c("weekdayType", "interval","steps")

StepsperIntervalWeekend <- aggregate(filledActivitydatWeekend$steps, 
                                     by = list(weekdayType=filledActivitydatWeekend$weekdayType,interval = filledActivitydatWeekend$interval),
                                     FUN=mean, na.rm=TRUE)
StepsperIntervalWeekend$interval <- as.integer(levels(StepsperIntervalWeekend$interval)[StepsperIntervalWeekend$interval])
colnames(StepsperIntervalWeekend) <- c("weekdayType", "interval","steps")


StepsperInterval<-rbind(StepsperIntervalWeekday, StepsperIntervalWeekend)

#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends:
g <- ggplot(StepsperInterval, aes(x=interval, y=steps))  
g <- g + geom_line(color="purple") + 
    facet_wrap(~ weekdayType, nrow=2, ncol=1) +
    labs(x="Interval", y="Number of steps") +
    theme_bw()
print(g)



###########################################################################################
