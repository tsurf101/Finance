library(ggplot2)

setwd("~/Serafin_Documents/Cornell/Spring_2020/ORIE_4740_Statistical_Data_Mining/Project/ORIE4710_FINAL-master/") #Tom S - PLEASE DO NOT DELETE
data  <- read.table("Cleaned_Data/data.csv", header=TRUE, sep=",") # Correct formatting 
#data <- read.csv("~/Desktop/orie 5270/4740 lab/data.txt") # Zoe 

#basic statistical analysis
#most frequent crime types
names(sort(table(data$group),decreasing = TRUE))

# top 5 most frequent crime types 
names(sort(table(data$group),decreasing = TRUE))[1:5]

#plot frequency of all types of crime
par(mar=c(4,14,1,1))
barplot(summary(data$group),las=2, col="#69b3a2",horiz=T)

# plotting bar plot of top 10 only, yoou can't view all of these in our report
barplot(sort(table(data$group),decreasing = TRUE)[1:15], main = "Group vs. Count", las=2, col="#69b3a2",horiz=T)

#plot number of crimes based on hour
par(mar=c(6,6,4,3))
hist(data$hour,xlab = "hour",ylab = "Frequency",col="cyan",main="Crime count by hour")

#plot number of crimes based on district
barplot(summary(data$district),las=2, col="cyan", main = "Crime count per district", xlab = "District", ylab = "Count")

#number of crimes based on day of week. Results are similar. Firday a little more and Sunday the lowest
as.data.frame(table(data$day_of_week))
barplot(summary(data$day_of_week),las=2, col="cyan") #probably don't need to show the graph

sort(summary(data$day_of_week), decreasing = TRUE)

# more descriptive figure showing how much crime decays per on day of the week. 
sort(summary(data$day_of_week), decreasing = TRUE)

plot(sort(summary(data$day_of_week), decreasing = TRUE), type="b", pch=21, col="red", ylab="Count", main= "Crime Count per weekday")

library(dplyr)
OCCURRED_ON_DATE
raw_data <- read.table("Raw_Dataset/raw_crime.csv", header=TRUE, sep=",") 
dates <- raw_data %>% select(OCCURRED_ON_DATE) 

interv <- as.POSIXct(raw_data$OCCURRED_ON_DATE)

# Extract the date:
dates_test <- as.Date(interv)

# Test if you've got a holiday:
sum(dates_test %in% holidays)
list <- dates_test %in% holidays
sum(list)

library(rlist)

final_result <- list.remove(raw_data$OCCURRED_ON_DATE, which(test))

length(final_result) - length(list)

test_2 <- as.Date(final_result)

days_of_the_week <- weekdays(test_2)
factor_days_of_the_week <- days_of_the_week

library(lubridate)
startDate <- dmy("15-Jun-2015")
endDate <- dmy("27-Feb-2020")

myDates <-seq(from = startDate, to = endDate, by = "days")
head(myDates)
sum_of_weeks <- c(1:7)
  
sum_of_weeks[1] <- length(which(factor_days_of_the_week == "Sunday"))
sum_of_weeks[2] <- length(which(factor_days_of_the_week == "Monday"))
sum_of_weeks[3] <- length(which(factor_days_of_the_week == "Tuesday"))
sum_of_weeks[4] <- length(which(factor_days_of_the_week == "Wednesday"))
sum_of_weeks[5] <- length(which(factor_days_of_the_week == "Thursday"))
sum_of_weeks[6] <- length(which(factor_days_of_the_week == "Friday"))
sum_of_weeks[7] <- length(which(factor_days_of_the_week == "Saturday"))

sum_of_weeks
sort(sum_of_weeks, decreasing = TRUE)

plot(sort(sum_of_weeks, decreasing = TRUE), type="b", pch=21, col="red", ylab="Count", main= "Crime Count per weekday (Holidays Removed)")

Tuesdsort(summary(data$day_of_week), decreasing = TRUE)

sort(sum_of_weeks, decreasing = TRUE)




