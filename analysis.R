## HEADER
# who: James S and Ed H
# what: Food waste data
# when: Last edited 2024-06-25 10:41

# Source the data preparation script & load libraries
source("scripts/data.R")
source("scripts/libraries.R")

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Load the cleaned data
cleaned_data <- read.csv("data/cleaned_data.csv")

# Basic data analysis
summary(cleaned_data)
str(cleaned_data)

# Analysis of Staff and Category
print(table(cleaned_data$Staff))
print(table(cleaned_data$Category))

# Existing bar plot in base R
par(mar=c(5,18,2,2))
barplot(sort(table(cleaned_data$Category)),
        horiz = T, las = 1)
par(mar=c(5,4,2,2))

# Example analysis: count of each category
category_counts <- cleaned_data %>%
  group_by(Category) %>%
  summarise(count = n())

# Create a bar plot of the Category column
ggplot(category_counts, aes(x = reorder(Category, -count), y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribution of Categories", x = "Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("plots/category_distribution.png")

# Convert Quantity to numeric (removing non-numeric characters)
cleaned_data$Quantity <- as.numeric(gsub("[^0-9.]", "", cleaned_data$Quantity))

# Convert Date.Time to Date type
cleaned_data$Date.Time <- as.POSIXct(cleaned_data$Date.Time, format="%d/%m/%Y %H:%M:%S")

# Aggregate data to get total quantity per day for each category
daily_totals <- cleaned_data %>%
  group_by(Date = as.Date(Date.Time), Category) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE))

# Print daily totals
print("Daily Totals:")
print(daily_totals)

# Aggregate data to get total quantity per week for each category
weekly_totals <- cleaned_data %>%
  group_by(Week = floor_date(Date.Time, "week"), Category) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE))

# Print weekly totals
print("Weekly Totals:")
print(weekly_totals)

# Save the daily and weekly totals to CSV files
write.csv(daily_totals, "data/daily_totals.csv", row.names = FALSE)
write.csv(weekly_totals, "data/weekly_totals.csv", row.names = FALSE)

# Create a histogram of the 'Quantity' column if there are valid (non-NA, non-zero) values
if (any(!is.na(cleaned_data$Quantity) & cleaned_data$Quantity > 0)) {
  ggplot(cleaned_data, aes(x = Quantity)) +
    geom_histogram(binwidth = 100, fill = "blue", color = "black") +
    theme_minimal() +
    labs(title = "Histogram of Quantity", x = "Quantity", y = "Frequency")
  
  # Save the plot
  ggsave("plots/quantity_histogram.png")
} else {
  print("No valid data for Quantity histogram.")
}

# Assuming there are two numerical columns 'NET.Sales' and 'TOTAL.Sales'
# Create a scatter plot
ggplot(cleaned_data, aes(x = NET.Sales, y = TOTAL.Sales)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter Plot of NET Sales vs TOTAL Sales", x = "NET Sales", y = "TOTAL Sales")

# Save the plot
ggsave("plots/net_total_sales_scatter_plot.png")
