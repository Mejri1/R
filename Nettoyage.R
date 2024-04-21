# Check for missing values
missing_values <- sum(is.na(data))
missing_values

# Remove rows with missing values
data <- data[complete.cases(data), ]


#Check Date Format: Confirm that the 'Date' column is in the correct date format.
library(lubridate)
is.Date(data$Date)

data$Date <- as.Date(data$Date)

# Check unique values in the 'Date' column
length(unique(data$Date)) == nrow(data)

#Check for Negative Values: Ensure that there are no negative values in numerical columns where it's not appropriate (e.g., 'Open', 'High', 'Low', 'Close', 'Adj.Close', 'Volume').
any(data$Open < 0)
any(data$High < 0)
any(data$Low < 0)
any(data$Close < 0)
any(data$Adj.Close < 0)
any(data$Volume < 0)

#Check Consistency: Ensure that 'High' values are greater than or equal to 'Low' values
any(data$High < data$Low)


# Summary statistics for the 'Close' column after cleaning
summary(data$Close)

# Display the cleaned data
data

# Summary statistics for the 'Close' column
summary(data)

# Boxplot to visualize outliers
boxplot(data$Close, main = "Boxplot of Close Prices")

# Calculate the Interquartile Range (IQR)
Q1 <- quantile(data$Close, 0.25)
Q3 <- quantile(data$Close, 0.75)
IQR <- Q3 - Q1

# Define the lower and upper bounds for outlier detection
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- data$Close[data$Close < lower_bound | data$Close > upper_bound]

# Remove outliers
data_clean <- data[!(data$Close %in% outliers), ]

# Summary statistics for the cleaned dataset
summary(data_clean$Close)

# Boxplot of the cleaned dataset
boxplot(data_clean$Close, main = "Boxplot of Close Prices (Cleaned)")

