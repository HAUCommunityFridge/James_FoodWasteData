table <- readHTMLTable("data/ePOS system report.html")
table <- list.clean(table, fun = is.null, recursive = FALSE)
table <- data.frame(table[["NULL"]])
table <- table[-nrow(table),]

table[!grepl(" ",table$Category), ] <- "Other"


