# working directory

setwd("C:/Users/salma/OneDrive/Desktop/uni/year 4/semester 2/big data")

# we need to add 2 columns lel dataset,, im thinking visibility and temperature are nice

#data preprocessing: 

# code to add temp (AI generated 3shan y3melha b relation)

library(dplyr)
library(lubridate)

# Read CSV
trafficDF <- read.csv("Traffic_dataset_with_temperature_and_visibility.csv")

# Parse Time manually using strptime
trafficDF$time_parsed <- as.POSIXct(strptime(trafficDF$Time, "%I:%M:%S %p"))

# Extract hour with decimals (including minutes/seconds)
trafficDF$hour <- as.numeric(format(trafficDF$time_parsed, "%H")) + 
  as.numeric(format(trafficDF$time_parsed, "%M")) / 60 +
  as.numeric(format(trafficDF$time_parsed, "%S")) / 3600

# Temperature generation function
time_to_temp <- function(hour) {
  temp_base <- 21 + 9 * sin(2 * pi * (hour - 6) / 24)
  temp <- temp_base + runif(n = length(hour), min = -1.5, max = 1.5)
  temp <- round(pmin(pmax(temp, 10), 35), 1)
}

# Apply temperature function
trafficDF$Temperature <- time_to_temp(trafficDF$hour)

# Clean up helper columns
trafficDF <- trafficDF %>% select(-time_parsed, -hour)

# Save output
write.csv(trafficDF, "Traffic_dataset_with_temperature_and_visibility.csv", row.names = FALSE)


# adding random visibility values bs in a range,, if traffic situation is low, hkhly el range mn 7000 to 8999,, 
# if traffic is normal then mn 5000 to 6999 
# if traffic is heavy then mn 3000 to 4999

generate_visibility <- function(Traffic.Situation) {
  ifelse(Traffic.Situation == "low", visibility <- runif(n = nrow(trafficDF),min = 7000,max = 8999),ifelse(Traffic.Situation == "normal", visibility <- runif(n = nrow(trafficDF), min = 5000, max = 6999),ifelse(Traffic.Situation == "high", visibility <- runif(n = nrow(trafficDF), min = 3000, max = 4999), visibility <- runif(n = nrow(trafficDF), min = 2000, max = 3999)))) 
}

generate_visibility_based_on_temp <- function(temperature) {
  ifelse(temperature >=26 & temperature <=35.9 , visibility <- runif(n = nrow(trafficDF),min = 8000,max = 9999),ifelse(temperature >=21 & temperature <=25.9, visibility <- runif(n = nrow(trafficDF), min = 6000, max = 7999),ifelse(temperature >=16 & temperature <=20.9, visibility <- runif(n = nrow(trafficDF), min = 4000, max = 5999), visibility <- runif(n = nrow(trafficDF), min = 2000, max = 3999)))) 
}


trafficDF$Visibility = round(generate_visibility(trafficDF$Traffic.Situation),2)

trafficDF$Visibility.in.metres = round(generate_visibility_based_on_temp(trafficDF$Temperature),2)

#trafficDF$Visibility.extra.big = round(generate_visibility_based_on_temp(trafficDF$Temperature),2)

trafficDF$TimeParsed <- parse_date_time(trafficDF$Time, orders = "I:%M:%S %p")

# Extract hour (in 24-hour format) and minute
trafficDF$Hour <- hour(trafficDF$TimeParsed)
trafficDF$Minute <- minute(trafficDF$TimeParsed)

trafficDF$TimeGroup <- ifelse(
  ((trafficDF$Hour >= 5 & trafficDF$Hour < 12) | (trafficDF$Hour == 12 & trafficDF$Minute == 0))
  , "Morning", 
  ifelse(
    ((trafficDF$Hour >= 12 & trafficDF$Hour < 17) | (trafficDF$Hour == 17 & trafficDF$Minute == 0))
    , "Afternoon", 
    ifelse(((trafficDF$Hour >= 17 & trafficDF$Hour < 21) | (trafficDF$Hour == 21 & trafficDF$Minute == 0))
           , "Evening"
           ,"Night"))) 

write.csv(trafficDF, "Traffic_dataset_added_vars.csv", row.names = FALSE)

#checking for null values:
colSums(is.na(trafficDF))
#showed 0 nulls, no data cleaning required






