# R scripts for the assignment

dataSource <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
dataZip  <- "activity.zip"
dataFile  <- "activity.csv"


if (!file.exists(dataFile)) {
   download.file(dataSource, destfile=dataZip)
   unzip(dataZip)
}
act <- read.csv("activity.csv", header=TRUE, colClasses=c("integer","Date","integer"))

# function to scale the intervals from original encoding to minutes encoding
IntervalRescale <- function(x) {
   (x %/% 100)*60+ (x %% 100)
}

# function to reverse minutes encoding back to original encoding
ReverseScale <- function(x) {
   (x %/% 60)*100 + (x %% 60)   
}

act$interval <- IntervalRescale(act$interval)

actFiltered <- act[complete.cases(act),]

filteredDaySum <- aggregate(actFiltered['steps'], actFiltered['date'], sum)
hist(filteredDaySum$steps,breaks=10, xlab="Total number of steps", main="")

meanTotalSteps <- mean(filteredDaySum$steps)
medianTotalSteps  <- median(filteredDaySum$steps)


filteredIntervalMean <- aggregate(actFiltered['steps'], actFiltered['interval'],mean)
plot(filteredIntervalMean$interval, filteredIntervalMean$steps,type="l",
     xlab="interval", ylab="steps", main="Average Steps During a Day")

maxStepInterval1 <- with(filteredIntervalMean, interval[which.max(steps)])
maxStepInterval2  <- ReverseScale(maxStepInterval1)

missingDataNum <- sum(is.na(act$steps))
                      

act1 <- act

# helper function to calculate interval mean
IntervalMean <- function(interval) {
   filteredIntervalMean$steps[filteredIntervalMean$interval==interval]
}

act1$steps[is.na(act1$steps)] <- sapply(act1[is.na(act1$steps),'interval'],IntervalMean)

filledDaySum <- aggregate(act1['steps'], act1['date'], sum)
hist(filledDaySum$steps, breaks=10, xlab="Total number of steps", main="")

(meanTotalFilledSteps <- mean(filledDaySum$steps))
(medianTotalFilledSteps  <- median(filledDaySum$steps))


act1$weekday <- lapply(act1[,'date'],
                       function(x) {
                          if (weekdays(x) %in% c("Saturday", "Sunday"))
                             "weekend"
                          else
                             "weekday"})
weekdayAct <- act1[act1$weekday == "weekday",]
weekendAct <- act1[act1$weekday == "weekend",]
weekdayIntervalMean <- aggregate(weekdayAct['steps'], weekdayAct['interval'],mean)
weekendIntervalMean <- aggregate(weekendAct['steps'], weekendAct['interval'],mean)
weekdayIntervalMean$weekday <- "weekday"
weekendIntervalMean$weekday <- "weekend"
combinedIntervalMean <- rbind(weekdayIntervalMean, weekendIntervalMean)

library(ggplot2)
qplot(interval, steps, data=combinedIntervalMean, facets = weekday ~ ., geom="line")
