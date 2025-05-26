# working directory
setwd("C:/Users/salma/OneDrive/Desktop/uni/year 4/semester 2/big data")

trafficDF <- read.csv("Traffic_dataset_with_temperature_and_visibility.csv")
#---------------------------------------------------------------------------



#linear regression: 
#A linear regression is a statistical model that analyzes the relationship 
#between a response variable (often called y) and one or more variables and their 
#interactions (often called x or explanatory variables).

# el dependent variable eli i want to predict -> traiffic situation
# el independent variables dol eli hn use to predict -> time, temp, visibility, count

library(nnet)

# Capitalize AM/PM
trafficDF$Time <- gsub("am", "AM", trafficDF$Time)
trafficDF$Time <- gsub("pm", "PM", trafficDF$Time)

# Now safely parse to minutes since midnight
trafficDF$time_minutes <- as.numeric(format(strptime(trafficDF$Time, format = "%I:%M:%S %p"), "%H")) * 60 +
  as.numeric(format(strptime(trafficDF$Time, format = "%I:%M:%S %p"), "%M"))

trafficDF$Traffic.Situation.Num <- as.numeric(factor(trafficDF$Traffic.Situation, levels = c("low", "normal", "high", "heavy")))
model <- lm(Traffic.Situation.Num ~ time_minutes + Total + Temperature + Visibility.in.metres, data = trafficDF)

model

summary(model)

#according to the coeffs: 
# visibility has the greatest coeff -> 6.128e-05 ,, so it has the highest effect 3l traffic situation,, A 1-unit increase in visibility leads to an increase of 0.00006128 in the log-odds of the outcome eli hwa traffic situation
# next is total -> 1.409e-02,, A 1-unit increase in total leads to an increase of 0.01409 in the log-odds of the outcome eli hwa traffic situation
# then, time ->  -1.294e-04,, A 1-unit increase in time leads to a decrease of 0.0001294 in the log-odds of the outcome eli hwa traffic situation
# lastly for temp -> -3.667e-02 ,, A 1-unit increase in temp leads to a decrease of 0.03667 in the log-odds of the outcome eli hwa traffic situation

#---------------------------------------------------------------------------

predict_data_dummy <- data.frame(time_minutes = 360, Total = 150, Temperature = 19.5,Visibility.in.metres = 6900.11)
predicted_traffic_situation <- predict(model, newdata = predict_data_dummy)

cat("before rounding:", predicted_traffic_situation)
cat("after rounding:", round(predicted_traffic_situation))
predicted_traffic_situation_category <- ifelse((round(predicted_traffic_situation) == 1), "low", ifelse((round(predicted_traffic_situation) == 2),"normal", ifelse((round(predicted_traffic_situation) == 3), "high","heavy")))
cat("predicted traffic situation:", predicted_traffic_situation_category, "\n")

#---------------------------------------------------------------------------

predict_data_real <- data.frame(time_minutes = 360, Total = 194, Temperature = 21.8, Visibility.in.metres = 6242.47)
predicted_traffic_situation <- predict(model, newdata = predict_data_real)

cat("before rounding:", predicted_traffic_situation)
cat("after rounding:", round(predicted_traffic_situation))
predicted_traffic_situation_category <- ifelse((round(predicted_traffic_situation) == 1), "low", ifelse((round(predicted_traffic_situation) == 2),"normal", ifelse((round(predicted_traffic_situation) == 3), "high","heavy")))
cat("predicted traffic situation:", predicted_traffic_situation_category, "\n")

#---------------------------------------------------------------------------
#metrics for linear regression: mean absolute error and root mean squared error

library(Metrics)

actual_row <- trafficDF[
  trafficDF$time_minutes == 360 &
    trafficDF$Total == 194 &
    trafficDF$Temperature == 21.8 &
    trafficDF$Visibility.in.metres == 6242.47,
]

actual <- actual_row$Traffic.Situation.Num

# mean absolute error -> Average absolute difference between predicted and actual values
mae(actual, predicted_traffic_situation)
# the lower the better

# mean squared error ->  Average squared difference between the predicted values and the actual values
mse(actual, predicted_traffic_situation)
# the lower the better

# root mean squared error -> Square root of MSE; penalizes larger errors more than MAE
rmse(actual, predicted_traffic_situation)
# the lower the better

#---------------------------------------------------------------------------

#logistic regression   
# talama el logistic true/false lemme google ezay mmkn a3melha,, ana 3yza low,normal yb2o haga wahda w high,heavy yb2o haga wahda,, false y3ni "not congested" ,, true y3ni "congested" 

trafficDF$binarytraffic <- ifelse(trafficDF$Traffic.Situation == "low" | trafficDF$Traffic.Situation == "normal",0, 1)

#idk lw i should use this bsaraha
#to treat it as categorical and not just a number
#trafficDF$binarytraffic <- as.factor(trafficDF$binarytraffic)  
#table(trafficDF$binarytraffic)


trafficlogit <- glm(binarytraffic ~ time_minutes + Total + Temperature + Visibility.in.metres, data = trafficDF)

summary(trafficlogit)

#according to the coeffs: 
# total has the greatest coeff -> 6.721e-03,, so it has the highest effect 3l traffic situation,, A 1-unit increase in total leads to an increase of 0.006721 in the log-odds of the outcome eli hwa traffic situation
# next is visibility -> 2.338e-05,, A 1-unit increase in visibility leads to an increase of 0.00002338 in the log-odds of the outcome eli hwa traffic situation
# then, time -> 1.013e-05,, A 1-unit increase in time leads to an increase of 0.00001013 in the log-odds of the outcome eli hwa traffic situation
# lastly for temp -> -1.703e-02 ,, A 1-unit increase in temp leads to a decrease of 0.01703 in the log-odds of the outcome eli hwa traffic situation


# predicting 3la dummy data: time_minutes = 360, Total = 150, Temperature = 19.5, Visibility.in.metres = 6900.11
binarytraffic_dummy <- trafficlogit$coefficients[[1]] + trafficlogit$coefficients[[2]] * 360 + trafficlogit$coefficients[[3]] * 150 + trafficlogit$coefficients[[4]] * 19.5 + trafficlogit$coefficients[[5]] * 6900.11

binarytraffic_dummy


# predicting 3la real data: time_minutes = 360, Total = 194, Temperature = 21.8, Visibility.in.metres = 6242.47
binarytraffic_real <- trafficlogit$coefficients[[1]] + trafficlogit$coefficients[[2]] * 360 + trafficlogit$coefficients[[3]] * 194 + trafficlogit$coefficients[[4]] * 21.8 + trafficlogit$coefficients[[5]] * 6242.47

binarytraffic_real


print(ifelse(round(binarytraffic) == 1 , "1 -> Congested", "0 -> Not Congested"))


# predict el table kolo
predicted_probs <- predict(trafficlogit, newdata = trafficDF, type = "response")

# Convert probabilities to class labels: 0 or 1
predicted_classes <- ifelse(round(binarytraffic) == 1, 1, 0)

trafficDF$binarytraffic <- factor(trafficDF$binarytraffic, levels = c(0, 1))
predicted_classes <- factor(predicted_classes, levels = c(0, 1))

# confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = trafficDF$binarytraffic)
print(confusion_matrix)

TP <- confusion_matrix["1", "1"]
TN <- confusion_matrix["0", "0"]
FP <- confusion_matrix["1", "0"]
FN <- confusion_matrix["0", "1"]

accuracy  <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)
f1_score  <- 2 * (precision * recall) / (precision + recall)

cat("Accuracy:", accuracy * 100, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

