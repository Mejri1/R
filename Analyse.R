# Load required library
library(ggplot2)

# Convert 'Date' column to Date type
data$Date <- as.Date(data$Date)

# Specify the companies to include in the plots
selected_companies <- c("American Airlines Group Inc.", "Apple Inc.", "Advanced Micro Devices, Inc.", "Amazon.com, Inc.")

# Filter data for selected companies
selected_data <- data[data$Company %in% selected_companies, ]

# Filter data for the last 2 years
six_months_ago <- as.Date("2022-04-111")
selected_data <- selected_data[selected_data$Date >= six_months_ago, ]

# Set up layout for plots
par(mfrow = c(2, 2))

# Create individual plots for each selected company - Closing Price
for (company in selected_companies) {
  subset_data <- selected_data[selected_data$Company == company, ]
  plot(subset_data$Date, subset_data$Close, type = "l", 
       main = paste("Closing Price Curve for", company),
       xlab = "Date", ylab = "Closing Price")
}

# Create individual plots for sales volume for each selected company
for (company in selected_companies) {
  subset_data <- selected_data[selected_data$Company == company, ]
  plot(subset_data$Date, subset_data$Volume / 1e6, type = "l", 
       main = paste("Sales Volume for", company),
       xlab = "Date", ylab = "Volume (Millions)")
}

# Calculate moving averages for each company
ma_periods <- c(10, 20, 50)  # Moving average periods

# Create plots
par(mfrow = c(3, 1))  # Set up plotting layout
for (company in selected_companies) {
  for (period in ma_periods) {
    subset_data <- selected_data[selected_data$Company == company, ]
    ma_column_name <- paste("MA", period, "Days")
    subset_data[[ma_column_name]] <- stats::filter(subset_data$Close, rep(1/period, period), sides=2)
    
    # Plot adjusted close prices and moving averages
    plot(subset_data$Date, subset_data$Close, type = "l", col = "blue", xlab = "Date", ylab = "Price",
         main = paste("Adjusted Close Prices and Moving Average for", period, "Days"))
    lines(subset_data$Date, subset_data[[ma_column_name]], col = "red", lty = "dashed")
    legend("topright", legend = c("Adjusted Close", paste("MA", period, "Days")), 
           col = c("blue", "red"), lty = c("solid", "dashed"), cex = 0.8)
  }
}

# Load necessary libraries
library(lmtest)  # For econometric modeling

# Calculate correlation between Open and Close prices
correlation <- cor(selected_data$Open, selected_data$Close)
print(paste("Correlation between Open and Close prices:", correlation))

# Plot the relationship between Open and Close prices
ggplot(selected_data, aes(x = Open, y = Close)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Open Price", y = "Close Price", title = "Relationship between Open and Close Prices")


# Perform econometric modeling
model <- lm(Close ~ Open, data = selected_data)
print(summary(model))

# Perform heteroscedasticity test
bptest(model)

# Calculate correlation between High and Low columns
correlation <- cor(selected_data$High, selected_data$Low)

# Plot High vs. Low
plot(selected_data$High, selected_data$Low, xlab = "High", ylab = "Low", main = "High vs. Low", pch = 20)

# Print the correlation coefficient
print(correlation)

# Fit an econometric model
model <- lm(Close ~ Open + High + Low + Volume, data = selected_data)

# Summary of the model
summary(model)

library(dplyr)
# Group by 'Company' and calculate total volume
total_volume <- data %>%
  group_by(Company) %>%
  summarise(Total_Volume = sum(Volume)) %>%
  arrange(desc(Total_Volume))  # Sort by total volume in descending order

# Select the topdata# Select the top 10 companies
top_10_companies <- total_volume %>%
  top_n(10, Total_Volume)

# Increase plot size
options(repr.plot.width = 10, repr.plot.height = 6)  # Adjust width and height as needed

# Plot bar chart with custom design
barplot(top_10_companies$Total_Volume, 
        names.arg = top_10_companies$Company,
        xlab = "Company",
        ylab = "Total Volume",
        main = "Top 10 Companies with Highest Total Volume in Last 6 Months",
        border = "black",
        ylim = c(0, max(total_volume$Total_Volume) * 1.2),
        las = 2,  # Rotate company names if needed for better visualization
        cex.names = 0.8,  # Adjust font size for company names
        cex.lab = 1.2,  # Adjust font size for axis labels
        cex.main = 1.5,  # Adjust font size for main title
        width = 0.5)  # Adjust bar width as needed
