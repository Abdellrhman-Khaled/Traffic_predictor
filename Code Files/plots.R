# working directory
setwd("C:/Users/salma/OneDrive/Desktop/uni/year 4/semester 2/big data")

dataset <- read.csv("Traffic_dataset_with_temperature_and_visibility.csv")
par(mfrow = c(4,3))

#plot 1 - Frequency of Traffic Situations
counts <- table(dataset$Traffic.Situation)
barplot(counts,
        main = "Frequency of Traffic Situations",
        xlab = "Traffic Situation",
        ylab = "Count",
        col = "lightblue")

#Observation: we can see that most of the time over the month the traffic volume was normal and that is the dominant condition
#high and low are less frequent and their frequency is merely the same
#the distribution is skewed toward normal

#---------------------------------------------------------------------------

#plot 2 - Vehicle Counts per Traffic Situation
agg <- aggregate(cbind(CarCount, BikeCount, BusCount, TruckCount) ~ Traffic.Situation, data = dataset, sum)
mat <- t(as.matrix(agg[, -1]))
colnames(mat) <- agg$Traffic.Situation
barplot(mat,
        beside = TRUE,
        col = c("red", "green", "blue", "purple"),
        main = "Vehicle Counts by Traffic Situation",
        xlab = "Traffic Situation",
        ylab = "Total Count")
legend("top",
       legend = rownames(mat),
       fill = c("red", "green", "blue", "purple"),
       inset = 0.05)

#Observation: we can see that the car counts are in every traffic situation and that's occupying most of the traffic no matter the traffic situation is,
#we can also see that bike and bus counts are almost the same in every situation and occupies the same percentage
#for the truck count, in heavy-high-low situations are almost the same proportion but increases largely in normal 

#---------------------------------------------------------------------------

#plot 3 - histogram of total vehicles count
#visualize traffic volume distribution
hist(dataset$Total, 
     breaks = 20, 
     main = "Distribution of Total Vehicle Count", 
     xlab = "Total Vehicles",
     col = "skyblue")

#observations: The distribution is right-skewed, with a peak frequency between 30 and 80 total vehicles, indicating that this range is the most common total traffic volume.
#The frequency gradually decreases as the total vehicle count increases

#---------------------------------------------------------------------------

#plot 4 - Visibility vs Total Vehicle Count
#to see whether low visibility impacts traffic volume
plot(dataset$Visibility.in.metres, dataset$Total,
     main = "Visibility vs Total Traffic Volume",
     xlab = "Visibility (metres)",
     ylab = "Total Vehicle Count",
     col = "orange",
     pch = 19)

#observations: the relationship is weak
#Data is spread across all visibility levels, with higher counts at higher visibility.

#---------------------------------------------------------------------------

#plot 5 - Pie Chart of Vehicle Type Proportions
#What percentage of traffic each vehicle type contributes overall
sums <- colSums(dataset[, c("CarCount", "BikeCount", "BusCount", "TruckCount")])
lbls <- paste(names(sums), "=", round(sums / sum(sums) * 100, 1), "%")
pie(sums, labels = lbls, main = "Overall Vehicle Type Proportions", col = rainbow(length(sums)))

#observations: car count is the dominant vehicle type
#all others are roughly equal but minor contribution in traffic.

#---------------------------------------------------------------------------

#plot 6 - Box plot of Vehicle Count by Day of the Week
#to understand variation across weekdays
dataset$Day.of.the.week <- factor(dataset$Day.of.the.week,
                                  levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday",
                                             "Thursday", "Friday"))
boxplot(Total ~ Day.of.the.week, data = dataset,
        main = "Vehicle Counts by Day of the Week",
        xlab = "Day of Week",
        ylab = "Vehicle Count",
        col = "lightgray")

#Observation: all days are similar in total vehicle counts except for friday
#vehicle counts are relatively consistent across days
#friday shows the highest median and upper whisker, indicating peak traffic
#outliers are minimal, suggesting stable daily patterns with occasional peaks.

#---------------------------------------------------------------------------

#plot 7 - bar plot of average vehicle count by day group (weekends vs weekdays)
#to see if vehicle count is higher in either group
agg_daygroup <- aggregate(Total ~ DayGroup, data = dataset, mean)
barplot(agg_daygroup$Total,
        names.arg = agg_daygroup$DayGroup,
        main = "Average Vehicle Count by Day Group",
        xlab = "Day Group", ylab = "Average Count", ylim = c(0,120),
        col = "maroon")

#observation: average traffic count on weekends and weekdays is almost the same


#---------------------------------------------------------------------------

#plot 8 - Scatter plot to display relation between Temperature and Total Vehicle Counts
#to see whether high temperature impacts traffic volume or reduces it
plot(dataset$Temperature, dataset$Total,
     main = "Temperature vs Total Vehicle Count",
     xlab = "Temperature (°C)",
     ylab = "Total Count",
     col = "red",
     pch = 19)

#observations: Warmer temperatures are associated with higher traffic volumes, possibly due to increased activity.

#---------------------------------------------------------------------------

#convert time format as i need it
dataset$Time <- strptime(dataset$Time, format = "%I:%M:%S %p")
dataset$Hour <- as.numeric(format(dataset$Time, "%H"))

#plot 9 - average vehicle count by hour
agg_hour <- aggregate(Total ~ Hour, data = dataset, mean)
plot(agg_hour$Hour, agg_hour$Total, type = "l", col = "blue",
     xlab = "Hour of Day", ylab = "Average Vehicle Count",
     main = "Average Vehicle Count by Hour")

#observations: we can see that there are 2 peaks of vehicle counts from 5-10 and 15-20
#we can assume that they are the hours of people going to work, school or college, etc and the hours of going back home.
#The bimodal pattern reflects morning and evening rush hours.

#---------------------------------------------------------------------------

#plot 10 - Bar plot of average vehicle count by time of day
agg_timeofday <- aggregate(Total ~ TimeGroup, data = dataset, mean)
barplot(agg_timeofday$Total,
        names.arg = agg_timeofday$TimeGroup,
        main = "Average Vehicle Count by Time of Day",
        xlab = "Time of Day", ylab = "Average Count",
        col = "orange")

#observation: morning, afternoon and evening have the highest average vehicle counts, indicating peak traffic periods.
#while night and late night are lower, showing reduced traffic outside daytime hours.

#---------------------------------------------------------------------------

#plot 11 - box plot to see car, bike, bus, truck count over the entire month
boxplot(dataset$CarCount, dataset$BikeCount, dataset$BusCount, dataset$TruckCount,
        main = "Vehicle Count over the Month",
        ylab = "count",
        names = c('Car', 'Bike', 'Bus', 'Truck'),
        col = c("skyblue", "lightgreen", "lightpink", "lightgray"))

#observations: we can see that car count shows the highest median and IQR, with values ranging from 50 to 150, indicating a significant presence of cars over the month.
#the outlier in car count suggests rare instances of extremely high car traffic.
#bike, bus, and truck couns have much lower medians and IQRs, indicating these vehicle types are less frequent and more consistent in number.
#cars dominate the traffic volume

#--------------------------------------------------------------------------
