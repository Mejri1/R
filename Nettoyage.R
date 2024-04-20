# Check for missing values
missing_values <- sum(is.na(data))
missing_values

# Remove rows with missing values
data <- data[complete.cases(data), ]

# Summary statistics for the 'Close' column after cleaning
summary(data$Close)

# Display the cleaned data
data

# Summary statistics for the 'Close' column
summary(data$Close)

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
