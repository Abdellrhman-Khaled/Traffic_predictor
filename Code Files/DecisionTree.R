# Install and load the 'party' package
install.packages("party")
library(party)

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

tree <- ctree(Traffic.Situation ~ Total + Temperature + TimeGroup, data = train.data)
plot(tree, main = "Tree for Traffic Situation Prediction", type = "simple")
testPred <- predict(tree, newdata = test.data)
table(testPred, test.data$Traffic.Situation)

confusion_matrix <- table(testPred, test.data$Traffic.Situation)
confusion_matrix

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Decision Tree Accuracy:", accuracy * 100)



