### Load necessary libraries
library(dplyr)
library(lubridate)
library(stringr)
library(lattice)


### Loading and preprocessing the data
# Unzip the data.  
# Note: The zip files containing the data must be in the local directory
unzip("activity.zip")

# Read in the .csv file
data <- read.csv("activity.csv",colClasses=c(NA,"Date",NA),col.names=c("Steps","Date","Interval"))
# Reorder the columns
data <- select(data,Date,Interval,Steps)


### What is mean total number of steps taken per day?
# Calculate the number of steps taken per day using the dplyr package
dataByDate <- group_by(data,Date)
dataByDateSum <- dataByDate %>%
                        filter(!Steps %in% NA) %>%
                        summarize(TotalSteps=sum(Steps))
# Create a histogram for the total number of steps per day
hist(dataByDateSum$TotalSteps)
# Calculate the mean and median of the total number of steps per day
mean(dataByDateSum$TotalSteps)
median(dataByDateSum$TotalSteps)


### What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# Calculate the mean number of steps per interval
dataByInt <- group_by(data,Interval)
dataByIntMean <- dataByInt %>%
                            filter(!Steps %in% NA) %>%
                            summarize(TotalSteps=mean(Steps))
# Convert Interval into a date format
intervalDate <- strptime(sprintf("%04d", as.numeric(dataByIntMean$Interval)), format="%H%M")
dataByIntMean <- cbind(dataByIntMean,intervalDate)

# Create the plot
plot (dataByIntMean$intervalDate,dataByIntMean$TotalSteps, typ="l",xlab = "Time of Day (HH:MM)",ylab = "Mean Number of Steps")

# Determine which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
dataByIntMean[(dataByIntMean$TotalSteps == max(dataByIntMean$TotalSteps)),]


### Imputing missing values
# Check for NAs in the entire data DF.
sum(is.na(data))
# Check for NAs in the Steps variable.
sum(is.na(data$Steps))
# Since out latter output equals check on entire DF then NAs only exist in Steps variable

## Strategy to for filling in NA values
# Create a copy of the original data DF that was read in
dataNew <- data
# Create a logical vector on dataByInt$Steps showing location of NAs
missingSteps <- is.na(dataNew$Steps)
# Create a vector containing mean steps minus NAs
meanSteps <- dataByIntMean[,2]
# Add the interval names to the new vector
names(meanSteps) <- dataByIntMean[,1]
# Round the mean values in meanSteps and overwrite the missing values in the data DF.  Create a new dataset.
dataNew[missingSteps,3] <- round(meanSteps[as.numeric(names(meanSteps)) %in% data[missingSteps,2]])

# Calculate the number of steps taken per day (reuse code from above)
dataNewByDate <- group_by(dataNew,Date)
dataNewByDateSum <- dataNewByDate %>%
    filter(!Steps %in% NA) %>%
    summarize(TotalSteps=sum(Steps))
# Create a histogram for the total number of steps per day
hist(dataNewByDateSum$TotalSteps)
# Calculate the mean and median of the total number of steps per day
mean(dataNewByDateSum$TotalSteps)
median(dataNewByDateSum$TotalSteps)


### Are there differences in activity patterns between weekdays and weekends?
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

# Create a new variable in dataNew to indicate day of week
dayOfWeek <- weekdays(dataNew$Date)
# Create character vectors containing weekdays and weekend days.
weekend <- c("Saturday","Sunday")
weekday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
# Create logical vectors identifying weekdays vs weekend days.
isWeekend <- dayOfWeek %in% weekend
isWeekday <- dayOfWeek %in% weekday
# Overwrite weekend days and weekdays accordingly.
dayOfWeek[isWeekend] <- c("weekend")
dayOfWeek[isWeekday] <- c("weekday")
# Add new variable to dataNew DF
dataNew <- cbind(dataNew,dayOfWeek)

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
dataNewWeekday <- dataNew[dataNew$dayOfWeek %in% c("weekday"),]
dataNewWeekend <- dataNew[dataNew$dayOfWeek %in% c("weekend"),]
# Calculate the mean number of steps per interval for weekend and weekday
# Weekday
dataNewWeekdayByInt <- group_by(dataNewWeekday,Interval)
dataNewWeekdayByIntMean <- dataNewWeekdayByInt %>%
                                summarize(TotalSteps=mean(Steps))
# Weekend
dataNewWeekendByInt <- group_by(dataNewWeekend,Interval)
dataNewWeekendByIntMean <- dataNewWeekendByInt %>%
                                summarize(TotalSteps=mean(Steps))
# Convert Interval into a date format - weekday
intervalDateWeekday <- strptime(sprintf("%04d", as.numeric(dataNewWeekdayByIntMean$Interval)), format="%H%M")
dataNewWeekdayByIntMean <- cbind(dataNewWeekdayByIntMean,intervalDateWeekday)
# Convert Interval into a date format - weekend
intervalDateWeekend <- strptime(sprintf("%04d", as.numeric(dataByIntMean$Interval)), format="%H%M")
dataNewWeekendByIntMean <- cbind(dataNewWeekendByIntMean,intervalDateWeekend)

# Create plots
par(mfrow = c(2, 1))
plot (dataNewWeekdayByIntMean$intervalDateWeekday,dataNewWeekdayByIntMean$TotalSteps, typ="l",xlab = "Time of Day (HH:MM)",ylab = "Mean Number of Steps")
plot (dataNewWeekendByIntMean$intervalDateWeekend,dataNewWeekendByIntMean$TotalSteps, typ="l",xlab = "Time of Day (HH:MM)",ylab = "Mean Number of Steps")






# Convert the intervals into hours and minutes
#intToHHMM <- function (x) {  sprintf("%02d:%02d", x %/% 100, x %% 100)}
#intToHHMM(data)
#interval <- intToHHMM(dataByIntSum$Interval)

# Try2
# Create a new column which combines date with interval
#data <- cbind (data, ymd_hm(paste(data$Date, str_pad(data$Interval, 
#                                                     4, pad = "0"))))
#names(data) <- c("Date","Interval","Steps","DateInterval")

# Calculate the mean number of steps per ignoring NAs
#mean(data$steps,na.rm = TRUE)

#dataByIntSum$IntervalCalc <- strptime(sprintf("%04d", as.numeric(dataByIntSum$Interval)), format="%H%M")

#dataNew <- mutate(dataNew, DayOfWeek = weekdays(dataNew$Date))