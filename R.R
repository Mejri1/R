# Import libraries
library(readr)

# Specify the path to the CSV file
file_path <- "most_active.csv"

# Load the dataset
data <- read.csv(file_path, header = TRUE)

# Display the first few rows of the dataset for verification
head(data)

# Display the structure of the dataset
str(data)

# Display the summary statistics of the dataset
summary(data)

# Description of the dataset columns:
# - Open: Opening price of the stock. The price at which the first transaction of the day occurred.
# - High: The highest price reached by the stock during the trading day.
# - Low: The lowest price reached by the stock during the trading day.
# - Close: Closing price of the stock. The price at which the last transaction of the day occurred.
# - Adj.Close: Adjusted closing price of the stock. It accounts for any distributions and corporate actions that occurred before the opening of the next day.
# - Volume: Trading volume of the stock, i.e., the total number of shares traded during the trading day.
