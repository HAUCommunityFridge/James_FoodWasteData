<div align="center">
  <h1>Community Fridge Food Waste Data Dashboard</h1>
  <p>This project creates a Data Dashboard to visualise the food waste saved through the community fridge initiative.</p>
  <p><img src="Files/img/Fridge_readme.png" alt="Food Waste Dashboard Banner" style="border-radius: 50%; width: 150px; height: 150px; object-fit: cover;"></p>
</div>

[Check out our Dashboard](https://haucommunityfridge.github.io/James_FoodWasteData/FoodWasteDataDashboard.html)

## Description
The Community Fridge Food Waste Data Dashboard visualises the magnitude of food saved through the community fridge initiative. It provides insights into user interactions and food distribution patterns, helping to understand the impact and efficiency of the community fridge project.

## Table of Contents
- [Usage](#usage)
- [Configuration](#configuration)
- [Dashboard Metrics](#dashboard-metrics)
- [Github Links](#github-links)
- [Script Details](#script-details)

## Usage
To create the dashboard:

1. **Clone the Repository**:
   - Clone this repository to your local machine using:
     ```bash
     git clone https://github.com/HAUCommunityFridge/James_FoodWasteData.git
     ```

2. **Load the Data**:
   - Ensure your data is in the correct format and placed in the data directory. The data should be in an HTML file named ePOS system report.html.

3. **Run the Script**:
   - Open RStudio or any R environment.
   - Open the script file FoodWasteDataDashboard.qmd.
   - Source the script to load, clean, and visualise the data:
   ```{r}
   source("FoodWasteDataDashboard.qmd")
   ```

4. **View the Dashboard**:
   - The dashboard will be generated and can be viewed in your browser.

### Configuration

1. **Ensure Your Data is Correct**:
   - Make sure your data is in the correct format (HTML) and located in the data directory.

2. **Update the Script Configuration**:
   - Open the FoodWasteDataDashboard.qmd file in a text editor or RStudio.
   - Ensure the path to your data file is correct. The script reads from data/ePOS system report.html.

3. **Run the Script**:
   - Open your R environment and source the script by typing:
   ```{r}
   source("FoodWasteDataDashboard.qmd")
   ```

4. **Check the Output**:
   - The script will process the data and generate visualizations in the form of an interactive dashboard.

## Dashboard Metrics

- **Items:** The count of individual food products available or taken from the community fridge.
- **Unique Users:** The cumulative number of new users participating in the initiative.
- **Category:** The type of food (e.g., Meat, Poultry, etc.).
- **Saved:** The food taken from the community fridge, saving it from being discarded or wasted.

## Github Links

- **Link To My Github Repos:**
  [https://github.com/Jamess200?tab=repositories](https://github.com/Jamess200?tab=repositories)

- **Link To Org Github Repos:**
  [https://github.com/HAUCommunityFridge](https://github.com/HAUCommunityFridge)

## Script Details

The script performs the following steps to create the dashboard:

```yaml
---
title: "Community Fridge Food Waste Data Dashboard"
logo: "FoodWasteDataDashboard_files/Agridat_logo/nl_agridat_hex_tp.png"
author: "Harper Adams University"
format: 
  dashboard:
    theme: cerulean
orientation: columns
---
```

## Setup and Data Loading

### 1. **Import Required Libraries**:
```{r}
library(tidyverse)
library(lubridate)
library(plotly)
library(qrencoder)
library(rvest)
library(rlist)
library(dplyr)
library(XML)
```

2. **Generate the QR Code (Optional)**:
```{r}
### Some form of Error here qr code comes out blury.
# Generate the QR code with a higher resolution
#qr <- qrencoder::qrencode("https://www.harper-adams.ac.uk/community/988/community-fridge-pilot-project/")

# Save the QR code as a high-resolution PNG file
#png("qr_code.png", width = 1000, height = 1000)
#plot(as.raster(qr), col = c("white", "black"), asp = 1, axes = FALSE, xlab = "", ylab = "", main = "")
#dev.off()
```

### 3. **Load the Data**:
```{r}
# Load data
table <- readHTMLTable("data/ePOS system report.html")
table <- list.clean(table, fun = is.null, recursive = FALSE)
table <- data.frame(table[["NULL"]])
table <- table[-nrow(table),]
```

### 4. **Data Cleaning**:
```{r}
# Data Cleaning
table$Category <- as.character(table$Category)
table[!grepl(" ", table$Category), "Category"] <- "Other"

# Save the cleaned data to a CSV file for later use
write.csv(table, "data/cleaned_data.csv", row.names = FALSE)
```

### 5. **Load the Cleaned Data**:
```{r}
# Load the cleaned data
cleaned_data <- read.csv("data/cleaned_data.csv")

# Add a column to identify whether the food is taken or donated
cleaned_data <- cleaned_data %>%
  mutate(FoodType = ifelse(grepl("\\(Take\\)", Product), "Take", "Donate"))
```

### 6. **Update "Other" Categories Based on ProductID**:
```{r}
# Function to update "Other" categories based on ProductID
update_category <- function(data) {
  lookup_table <- data %>%
    filter(Category != "Other") %>%
    dplyr::select(ProductId, Category) %>%
    distinct()
  
  updated_data <- data %>%
    left_join(lookup_table, by = "ProductId", suffix = c("", "_new")) %>%
    mutate(Category = ifelse(Category == "Other" & !is.na(Category_new), Category_new, Category)) %>%
    dplyr::select(-Category_new)
  
  return(updated_data)
}

cleaned_data <- update_category(cleaned_data)
```

### 7. **Convert Data Types**:
```{r}
# Convert Quantity to numeric and Date.Time to Date type
cleaned_data$Quantity <- as.numeric(gsub("[^0-9.]", "", cleaned_data$Quantity))
cleaned_data$Date.Time <- as.POSIXct(cleaned_data$Date.Time, format = "%d/%m/%Y %H:%M:%S")
```

### 8. **Map Original Categories to New Names**:
```{r}
# Create a mapping of original categories to new, more readable names
category_mapping <- c(
  "Bakery, Bread & Baking ingredient" = "Bakery & Bread",
  "Dairy & Dairy alternatives" = "Dairy",
  "Dessert, Confectionery, biscuit & snacks" = "Desserts & Snacks",
  "Dried Pasta, Rice, Noodles & Pulse" = "Pasta & Rice",
  "Drink, Juice & Drink ingredients" = "Drinks",
  "Fruit & Vegetables (Fresh)" = "Fruits & Vegetables",
  "Meat, Poultry, Fish & Egg" = "Meat & Fish",
  "Non food" = "Non-Food",
  "Other" = "Unknown Category",
  "Plant based products" = "Plant-Based",
  "Ready meal, pizza, soup & Salad boxes" = "Ready Meals",
  "Sauces, Pickles, Herbs, Tins & Bottles" = "Sauces & Herbs",
  "Tea Coffee, Dried fruits & Nuts" = "Tea & Coffee"
)

# Update the Category column with new names
cleaned_data <- cleaned_data %>%
  mutate(Category = recode(Category, !!!category_mapping))
```

### 9. **Saved Food Used**:
```{r}
# Filter data for taken food
filtered_data_taken <- cleaned_data %>% filter(FoodType == "Take")

# Calculate the total quantity for taken food
total_quantity_taken <- sum(filtered_data_taken$Quantity, na.rm = TRUE)

# Calculate the average daily quantity for taken food
unique_days_taken <- length(unique(as.Date(filtered_data_taken$Date.Time)))
average_daily_quantity_taken <- total_quantity_taken / unique_days_taken

# Calculate the total number of first-time users who have taken food
total_first_time_users_taken <- sum(filtered_data_taken$Tender == "First time user")

# Round the quantities to 2 decimal places
total_quantity_taken <- round(total_quantity_taken, 2)
average_daily_quantity_taken <- round(average_daily_quantity_taken, 0)
```

### 10. **Value Boxes**:
```{r}
#| component: valuebox
#| title: "Total Number Of Food Items Saved"
#| color: primary
total_quantity_taken
```

```{r}
#| component: valuebox
#| title: "Daily Average Number Of Food Items Saved"
#| color: secondary
average_daily_quantity_taken
```

```{r}
#| component: valuebox
#| title: "Cumulative Number Of Unique Users"
#| color: secondary
total_first_time_users_taken
```

### 11. **Distribution Of Food Items By Category**:
```{r}
# Distribution Of Total Number Of Items By Category For Food Saved
category_totals_taken <- filtered_data_taken %>%
  group_by(Category) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE))

# Identify the 5 categories with the lowest total quantities
lowest_5_categories <- category_totals_taken %>%
  arrange(Total_Quantity) %>%
  slice(1:5) %>%
  pull(Category)

# Recode these categories as "Unknown Category"
filtered_data_taken <- filtered_data_taken %>%
  mutate(Category = ifelse(Category %in% lowest_5_categories, "Unknown Category", Category))

# Recreate the summary for the plot
category_totals_taken <- filtered_data_taken %>%
  group_by(Category) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE))

# Create Plotly bar plot for category distribution
plot_ly(category_totals_taken, x = ~reorder(Category, -Total_Quantity), y = ~Total_Quantity, type = 'bar', 
        marker = list(color = '#007BFF', line = list(color = 'black', width = 1.5))) %>%
  layout(
    title = list(text = 'Distribution Of Total Number Of Food Items By Category Saved', font = list(size = 16, color = 'black')),
    xaxis = list(title = 'Category', titlefont = list(size = 14, color = 'black'), tickfont = list(size = 12, color = 'black'), tickangle = -45),
    yaxis = list(title = 'Total Number Of Items', titlefont = list(size = 14, color = 'black'), tickfont = list(size = 12, color = 'black')),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white'
  )
```

### 12. **Cumulative Number Of Food Items**:
```{r}
# Total quantity per day for taken food
daily_totals_taken <- filtered_data_taken %>%
  group_by(Date = as.Date(Date.Time)) %>%
  summarise(Daily_Quantity = sum(Quantity, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Date) %>%
  mutate(Cumulative_Quantity = cumsum(Daily_Quantity))

# Create a plotly line plot for daily cumulative totals
plot_ly(daily_totals_taken, x = ~Date, y = ~Cumulative_Quantity, type = 'scatter', mode = 'lines+markers',
        line = list(color = 'blue'), marker = list(color = 'blue')) %>%
  layout(
    title = list(text = 'Cumulative Total Number Of Food Items Saved Over Time', font = list(size = 16, color = 'black')),
    xaxis = list(title = 'Date', titlefont = list(size = 14, color = 'black'), tickfont = list(size = 12, color = 'black')),
    yaxis = list(title = 'Cumulative Number Of Items', titlefont = list(size = 14, color = 'black'), tickfont = list(size = 12, color = 'black')),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white'
  )
```

### 13. **Unique Users Over Time**:
```{r}
# Count new users over time for taken food
new_users_daily_taken <- filtered_data_taken %>%
  filter(Tender == "First time user") %>%
  group_by(Date = as.Date(Date.Time)) %>%
  summarise(New_Users = n(), .groups = 'drop') %>%
  arrange(Date) %>%
  mutate(Cumulative_New_Users = cumsum(New_Users))

# Create interactive Plotly line plot for new users over time
plot_ly(new_users_daily_taken, x = ~Date, y = ~Cumulative_New_Users, type = 'scatter', mode = 'lines+markers',
        line = list(color = 'orange'), marker = list(symbol = 'square', color = 'orange')) %>%
  layout(
    title = list(text = 'Unique Users Over Time For Saved Food', font = list(size = 16, color = 'black')),
    xaxis = list(title = 'Date', titlefont = list(size = 14, color = 'black'), tickfont = list(size = 12, color = 'black')),
    yaxis = list(title = 'Cumulative Unique Users', titlefont = list(size = 14, color = 'black'), tickfont = list(size = 12, color = 'black')),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white'
  )
```

### 14. **About**:
```{r}
## Dashboard Information
<p style="font-size: 20px;">
**Dashboard Information:**
<p style="font-size: 18px;">
Welcome to the Food Waste Data Dashboard. This dashboard provides insights into the amount of food saved through our community fridge initiative, focusing on the food used from our comuity fridge.
</p>

<p style="font-size: 18px;">
Scan our QR code to learn more about the community fridge project.
</p>
<img src="QR.png" style="width: 200px; height: 200px;">
```

```{r}
## Key Metrics
### Overview
<p style="font-size: 20px;">
**Overview:**
<p style="font-size: 18px;">
- **Items:** The count of individual food products available or taken from the community fridge.<br>  
- **Unique Users:** The cumulative number of new users participating in the initiative.<br>
- **Category:** The type of food (e.g., Meat, Poultry, etc.).<br>
- **Saved:** The food taken from the community fridge saving it from being discarded or wasted out.<br>
</p>
```

```{r}
### Github
<p style="font-size: 20px;">
**Link To My Github Repos:**
<p style="font-size: 18px;">
https://github.com/Jamess200?tab=repositories
</p>
**Link To Org Github Repos:**
<p style="font-size: 18px;">
https://github.com/HAUCommunityFridge
</p>
```