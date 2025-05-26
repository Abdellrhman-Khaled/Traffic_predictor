library(e1071)
#data <- read.csv("D:\\Nour Bahgat\\University\\yearwork\\year 4\\DA Project\\Traffic_dataset_with_temperature_and_visibility.csv")

setwd("C:/Users/salma/OneDrive/Desktop/uni/year 4/semester 2/big data")

data <- read.csv("Traffic_dataset_with_temperature_and_visibility.csv")

# Convert categorical variables to factors
data$Traffic.Situation <- as.factor(data$Traffic.Situation)
data$Time <- strptime(data$Time, format = "%I:%M:%S %p")
data$Hour <- as.numeric(format(data$Time, "%H"))
data$TimeGroup <- as.factor(data$TimeGroup)

data_subset <- data[, c("Total", "Temperature", "TimeGroup", "Traffic.Situation")]
set.seed(123)  # For reproducibility
ind <- sample(2, nrow(data_subset), prob = c(0.7, 0.3), replace = TRUE)
train.data <- data_subset[ind == 1, ]
test.data <- data_subset[ind == 2, ]

# Fit the Naive Bayes classifier
classifier <- naiveBayes(Traffic.Situation ~ Total + Temperature + TimeGroup, data = train.data)
Output <- predict(classifier, test.data)
Tab <- table(Output, test.data$Traffic.Situation)
Tab

accuracy <- sum(diag(Tab)) / sum(Tab)
cat("Naive Bayes Accuracy:", accuracy * 100)
