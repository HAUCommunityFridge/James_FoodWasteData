## HEADER
# who: James S and Ed H
# what: Food waste data
# when: Last edited 2024-06-28 12:23

# Source the data preparation script & load libraries
source("scripts/data.R")
source("scripts/libraries.R")

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Load the cleaned data
cleaned_data <- read.csv("data/cleaned_data.csv")

# Function to update "Other" categories based on ProductId
update_category <- function(data) {
  lookup_table <- data %>%
    filter(Category != "Other") %>%
    select(ProductId, Category) %>%
    distinct()
  
  updated_data <- data %>%
    left_join(lookup_table, by = "ProductId", suffix = c("", "_new")) %>%
    mutate(Category = ifelse(Category == "Other" & !is.na(Category_new), Category_new, Category)) %>%
    select(-Category_new)
  
  return(updated_data)
}

# Update the Category column in the cleaned data
cleaned_data <- update_category(cleaned_data)

# Save the updated cleaned data
write.csv(cleaned_data, "data/cleaned_data_updated.csv", row.names = FALSE)

# Basic data analysis
summary(cleaned_data)
str(cleaned_data)

# Analysis of Category
print(table(cleaned_data$Category))

# Example analysis: count of each category
category_counts <- cleaned_data %>%
  group_by(Category) %>%
  summarise(count = n())

# Create and save bar plot for category distribution
ggplot(category_counts, aes(x = reorder(Category, -count), y = count)) +
  geom_bar(stat = "identity", fill = viridis::viridis(length(unique(category_counts$Category))), color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Categories", x = "Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/category_distribution.png")

# Convert Quantity to numeric and Date.Time to Date type
cleaned_data$Quantity <- as.numeric(gsub("[^0-9.]", "", cleaned_data$Quantity))
cleaned_data$Date.Time <- as.POSIXct(cleaned_data$Date.Time, format = "%d/%m/%Y %H:%M:%S")

# Aggregate data to get total quantity per day for each category
daily_totals <- cleaned_data %>%
  group_by(Date = as.Date(Date.Time), Category) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE), .groups = 'drop')

# Print daily totals
print("Daily Totals:")
print(daily_totals)

# Create and save line plot for daily totals
ggplot(daily_totals, aes(x = Date, y = Total_Quantity, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Daily Total Quantity by Category", x = "Date", y = "Total Quantity") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_viridis_d()

ggsave("plots/daily_totals.png", width = 10, height = 6)

# Aggregate data to get total quantity per week for each category
weekly_totals <- cleaned_data %>%
  group_by(Week = floor_date(Date.Time, "week"), Category) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE), .groups = 'drop')

# Print weekly totals
print("Weekly Totals:")
print(weekly_totals)

# Create and save line plot for weekly totals
ggplot(weekly_totals, aes(x = Week, y = Total_Quantity, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Weekly Total Quantity by Category", x = "Week", y = "Total Quantity") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_viridis_d()

ggsave("plots/weekly_totals.png", width = 10, height = 6)

# Save the daily and weekly totals to CSV files
write.csv(daily_totals, "data/daily_totals.csv", row.names = FALSE)
write.csv(weekly_totals, "data/weekly_totals.csv", row.names = FALSE)

# Aggregate data to get total quantity by Tender (Returning user vs First time user) over time
tender_daily_totals <- cleaned_data %>%
  group_by(Date = as.Date(Date.Time), Tender) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE), .groups = 'drop')

# Print tender daily totals
print("Tender Daily Totals:")
print(tender_daily_totals)

# Create and save line plot for tender daily totals
ggplot(tender_daily_totals, aes(x = Date, y = Total_Quantity, color = Tender, group = Tender)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Daily Total Quantity by Tender Type", x = "Date", y = "Total Quantity") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_viridis_d()

ggsave("plots/tender_daily_totals.png", width = 10, height = 6)
