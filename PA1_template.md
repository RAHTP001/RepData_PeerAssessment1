# Downloading file from given website 
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        activity<- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",activity)
        unzip(activity)
        unlink(activity)
}
activity<- read.csv("activity.csv", header = TRUE)
## Looking at "activity" file

head(activity)

summary(activity)

#Calculating the total number of steps taken per day

stepsperday<-aggregate(steps~date, activity, sum)

#Ploting Histogram
hist(stepsperday$steps,col= "red", xlab="Total Number of Steps per day", ylab="Number of Days",
     main="Steps taken each day")
#calculating Mean and Median of "steps per day"

mean_step<-mean(stepsperday$steps)
# Mean of steps with NA
mean_step

median_steps<-median(stepsperday$steps)
#Median of steps with NA 
median_steps

##What is the average daily activity pattern?
# time series plot

avgstepsbyinterval<-aggregate(steps~interval, activity, mean)

with(avgstepsbyinterval, plot(interval, steps, type = "l",col= "blue"))

avgstepsbyinterval[which.max(avgstepsbyinterval[,2]),1]

#Imputing missing values
#Calculate and report the total number of missing values in the dataset???s)

missingvalues<-is.na(activity[,1])
# filling missing values with median
med<-median(avgstepsbyinterval$steps)
activityNew<-activity
#New data set with missing values replace by Median
activityNew[missingvalues,1]<-med
head(activityNew)
# 4) Ploting histogram of the total number of steps taken each day 
# after imputing missing values
totalstepsperday1<-aggregate(steps~date, activityNew, sum)
hist(totalstepsperday1$steps,col = "blue", xlab="Total Steps per day", ylab="Days", 
     main="Number of Steps taken each day")
#calculating the mean and median total number of steps per day 
#we first find total number of steps per day

totalstepsperday1<-aggregate(steps~date, activityNew, sum)
Newmean_stepsimput<-mean(totalstepsperday1$steps)
#Mean Steps without NA
mean_stepsimput

#Median

med_stepsimput<-median(totalstepsperday1$steps)
#Median Steps without NA
med_stepsimput

##differences in activity patterns between weekdays and weekends?
activityNew$date<-as.Date(activityNew$date)
library(dplyr)

activityNew2<-activityNew%>%
        mutate(dayType= ifelse(weekdays(activityNew$date)=="Saturday" 
                               | weekdays(activityNew$date)=="Sunday",
                               "Weekend", "Weekday"))
head(activityNew2)

##plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken,
#averaged across all weekday days or weekend days (y-axis)
averageStepsperDaytypeAndinterval<-activityNew2 %>%
        group_by(dayType, interval) %>%
        summarize(averageStepByDay=sum(steps))

head(averageStepsperDaytypeAndinterval)

##
library(lattice)
with(averageStepsperDaytypeAndinterval, 
     xyplot(averageStepByDay ~ interval | dayType, 
            type = "l", col= "red",      
            main = "Total Number of Steps within Intervals by dayType",
            xlab = "Daily Intervals",
            ylab = "Average Number of Steps"))


