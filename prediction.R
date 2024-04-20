# Filter data for AMD
amd_data <- data[data$Company == "Advanced Micro Devices, Inc.", ]

# Impute missing values with mean for AMD
amd_data$Close <- ifelse(is.na(amd_data$Close), mean(amd_data$Close, na.rm = TRUE), amd_data$Close)

# Ensure 'amd_data' is sorted by date
amd_data <- amd_data[order(amd_data$Date), ]

# Feature engineering: Create lagged variables for previous closing prices
lagged_prices <- c(1, 2, 3, 4, 5)  # Lagged periods
for (lag in lagged_prices) {
  amd_data[[paste0("Close_lag", lag)]] <- lag(amd_data$Close, lag)
}
# Remove rows with NA values in lagged variables
amd_data <- na.omit(amd_data)

# Split the data into training and testing sets
train_size <- 0.8  # 80% of data for training
train_index <- 1:round(nrow(amd_data) * train_size)
train_data <- amd_data[train_index, ]
test_data <- amd_data[-train_index, ]

# Define features and target variable
features <- c("Close_lag1", "Close_lag2", "Close_lag3", "Close_lag4", "Close_lag5")
target <- "Close"

# Train the Random Forest model for AMD
rf_model <- randomForest(as.formula(paste(target, "~", paste(features, collapse = " + "))),
                         data = train_data, ntree = 500)


# Make predictions on the test data
predictions <- predict(rf_model, test_data)

# Calculate accuracy metrics
accuracy_metrics <- function(actual, predicted) {
  mae <- mean(abs(actual - predicted))  # Mean Absolute Error (MAE)
  mse <- mean((actual - predicted)^2)   # Mean Squared Error (MSE)
  rmse <- sqrt(mse)                     # Root Mean Squared Error (RMSE)
  return(c(MAE = mae, MSE = mse, RMSE = rmse))
}

# Calculate accuracy metrics
metrics <- accuracy_metrics(test_data$Close, predictions)
print(metrics)

# Plot actual vs. predicted values
plot(test_data$Close, predictions, 
     main = "Actual vs. Predicted Close Prices",
     xlab = "Actual Close Price", ylab = "Predicted Close Price",
     col = "blue", pch = 20)
abline(0, 1, col = "red", lwd = 2)  # Add diagonal line for reference
legend("top", legend = c("Actual vs. Predicted"), 
       col = c("blue", "red"), pch = c(20, NA), lty = c(NA, 1), cex = 0.8, bty = "n")

