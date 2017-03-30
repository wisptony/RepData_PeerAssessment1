#this is the code chunk for reading data
activity <- read.csv(file = "activity.csv", header = T, stringsAsFactors = F)
#What is mean total number of steps taken per day? (Ignore the missing values)
#1. Calculate the total number of steps taken per day?
activity_1 <- activity[!is.na(activity$steps),]
aggregate_steps <- aggregate(steps~date, data=activity_1, FUN = sum)
#2. Make a histogram of steps taken each day
hist(aggregate_steps$steps, main = "Histogram of Steps Taken Each Day", 
     xlab = "Steps Taken Per Day", ylab=" Frequency")
#3. Calculate and report mean and median of the total number of steps taken each day
aggregate <- data.frame(mean=mean(aggregate_steps$steps), median=median(aggregate_steps$steps))
aggregate
#What is the average daily activity pattern
#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#   and the average number of steps taken, averaged across all days (y-axis)
head(activity)
unique(activity$interval)
average_steps <- aggregate(steps~interval, data=activity_1, FUN = mean)
plot(average_steps$interval, average_steps$steps, type = "l", xlab="5 minutes interval",
     ylab = "average steps taken across all days")
#2. Which 5-minute interval, on average across all the days in the dataset, 
#   contains the maximum number of steps?
average_steps[which.max(average_steps$steps),]

#Imputing Missing Values
#1. Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with NAs)
sum(is.na(activity))
#2. Devise a strategy for filling in all of the missing values in the dataset. 
#   The strategy does not need to be sophisticated. For example, you could use 
#   the mean/median for that day, or the mean for that 5-minute interval, etc.
#   My strategy is to replace NA values with the mean of the interval
#3. Create a new dataset that is equal to the original dataset but with the missing
#   data filled in (new activity)
#sapply it to new activity!!!!
summary(newactivity)
class(which(is.na(activity)))

#4. Make a histogram of the total number of steps taken each day and Calculate 
#   and report the mean and median total number of steps taken per day. 
#   Do these values differ from the estimates from the first part of the a
#   ssignment? What is the impact of imputing missing data on the estimates of 
#   the total daily number of steps?
newactivity <- activity
for(i in 1:17568){
        if(is.na(newactivity$steps[i])){
                newactivity$steps[i] <- average_steps$steps[which(newactivity$interval[i]==average_steps$interval)]
        }
}
summary(newactivity)
head(average_steps)
newaggregate_steps <- aggregate(steps~date, data =  newactivity, FUN = sum)
head(newactivity)
head(newaggregate_steps)
hist(newaggregate_steps$steps, main = "Histogram of Steps Taken Each Day", 
     xlab = "Steps Taken Per Day", ylab=" Frequency")
summary(newactivity)
#Are there differences in activity patterns between weekdays and weekends?


