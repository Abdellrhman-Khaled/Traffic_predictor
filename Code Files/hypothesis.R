# working directory
setwd("C:/Users/salma/OneDrive/Desktop/uni/year 4/semester 2/big data")

trafficDF <- read.csv("Traffic_dataset_with_temperature_and_visibility.csv")

#using anova to test: 
#hypothesis: Days of the week affect traffic count.
#h0: there is no significant difference in the mean traffic count across different days of the week
#h1: at least one day of the week has a mean traffic count different from the others

days_model <- aov((Total ~ Day.of.the.week), data = trafficDF)

summary(days_model)

TukeyHSD(days_model)

#result = h0,, no significant difference ya guyyssssss 


#parse time w kda 3shan yb2a 24 fa n3mlo 4 time groups eli homa morning, afternoon, evening and night

  
#timegroups khalas created

#using anova to test: 
#hypothesis: Time groups affect traffic count.
#h0: there is no significant difference in the mean traffic count across different time groups
#h1: at least one time group has a mean traffic count different from the others
  
trafficDF$TimeGroup <- factor(trafficDF$TimeGroup, levels = c("Morning", "Afternoon", "Evening", "Night"))

time_model <- aov((Total ~ TimeGroup), data = trafficDF)

summary(time_model)

TukeyHSD(time_model)

#result
# fe significant difference f Night-Morning, Night-Afternoon, Night-Evening 3shan el night dayman olayel awi 



#creating day groups 
trafficDF$DayGroup <- ifelse((trafficDF$Day.of.the.week == "Saturday" | trafficDF$Day.of.the.week == "Sunday"),"Weekend", "Weekday")

#write.csv(trafficDF, "Traffic_dataset_with_temperature_and_visibility.csv", row.names = FALSE)

#t test: 
#using t test to test: 
#hypothesis: Day group affects traffic count.
#h0: There is NO difference in the mean traffic count between weekdays and weekends
#h1: There is a difference in the mean traffic count between weekdays and weekends
t_test_result <- t.test(Total ~ DayGroup, data = trafficDF, var.equal = TRUE)

print(t_test_result)
#-0.4727

#result = h0,, no significant difference between them,, 115 w 113

#manual as well 3shan n compare w n prove enaha the same zy el automatic

weekday <- trafficDF$Total[trafficDF$DayGroup == "Weekday"]
weekend <- trafficDF$Total[trafficDF$DayGroup == "Weekend"]

pooled_var <- function(x,y){
  nx <- length(x)
  ny <- length(y)
  stdx <- sd(x)
  stdy <- sd(y)
  num <- (nx-1)*stdx^2 + (ny-1)*stdy^2
  denom <- nx+ny-2
  (num/denom) * (1/nx + 1/ny)
}

mx <- mean(weekday)

my <- mean(weekend)

mx - my

pooled <- pooled_var(weekday,weekend)

manual_t_test <- (mx-my)/ sqrt(pooled)

print(manual_t_test)
#-0.4727

#manually performing the student t test and just calling the function give the same result,, -0.4727
#will show that fl documentation b screenshots b2a

#cancelled 3shan too many pairwise comparisions
#using anova to test: 
#hypothesis: Time of the day affects traffic count.
#h0: there is no significant difference in the mean traffic count across different times of the day
#h1: at least one time interval has a mean traffic count different from the others

#hour_model <- aov((Total ~ Time), data = trafficDF)

#summary(hour_model)

#TukeyHSD(hour_model)

