## HEADER
# who: James S and Ed H
# what: Food waste data
# when: Last edited 2024-06-24

source("scripts/libraries.R")
source("scripts/data.R")

table(table$Staff)
table(table$Category)

par(mar=c(5,18,2,2))
barplot(sort(table(table$Category)),
        horiz = T, las = 1)
par(mar=c(5,4,2,2))

