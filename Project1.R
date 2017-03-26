#this is the code chunk for reading data
activity <- read.csv(file = "activity.csv", header = T, stringsAsFactors = F)
#What is mean total number of steps taken per day? (Ignore the missing values)
#1. Calculate the total number of steps taken per day?
activity_1 <- activity[!is.na(activity$steps),]
aggregate_steps <- aggregate(x = activity_1$steps, by = list(activity_1$date), FUN = sum)
#2. Make a histogram of steps taken each day
hist(aggregate_steps$x, main = "Histogram of Steps Taken Each Day", 
     xlab = "Steps Taken Per Day", ylab=" Frequency")
#3. Calculate and report mean and median of the total number of steps taken each day
data.frame(mean=mean(aggregate_steps$x), median=median(aggregate_steps$x))

#What is the average daily activity pattern
#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#   and the average number of steps taken, averaged across all days (y-axis)
head(activity)
unique(activity$interval)
average_steps <- aggregate(x = activity_1$steps, by = list(activity_1$interval), FUN = mean)
plot(average_steps$Group.1, average_steps$x, type = "l", xlab="5 minutes interval",
     ylab = "average steps taken across all days")
#2. Which 5-minute interval, on average across all the days in the dataset, 
#   contains the maximum number of steps?
average_steps$Group.1[which.max(average_steps$x)]
24*12*5
24*60
plot(x = 1:288, y = average_steps$Group.1)
average_steps$Group.1/5
