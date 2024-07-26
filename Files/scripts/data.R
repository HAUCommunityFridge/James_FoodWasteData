# Load data
table <- readHTMLTable("data/ePOS system report.html")
table <- list.clean(table, fun = is.null, recursive = FALSE)
table <- data.frame(table[["NULL"]])
table <- table[-nrow(table),]

# table[!grepl(" ",table$Category), ] <- "Other"

# Data Cleaning
table$Category <- as.character(table$Category)
table[!grepl(" ", table$Category), "Category"] <- "Other"

# Save the cleaned data to a CSV file for later use
write.csv(table, "data/cleaned_data.csv", row.names = FALSE)
