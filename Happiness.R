library(haven)
path = file.path("C:/Users/Cyrus/Downloads/DANA 4830", "Project", "Happiness-Sustainable-Behaviour.sav")
h = read_sav(path)
#Export the csv
write.csv(h,"C:\\Users\\Cyrus\\Downloads\\DANA 4830\\Project\\Uncleanhset.csv", row.names = TRUE)
#Export the Excel
library("writexl")
write_xlsx(h,"C:\\Users\\Cyrus\\Downloads\\DANA 4830\\Project\\Uncleanhset.xlsx")

setwd("C:/Users/Cyrus/Downloads/DANA 4830/Project")
getwd()
h <- read.csv(file = 'UncleanDataset.csv')
h <- h[, -c(1:3,78:89)]
str(h)
summary(h)

#Accuracy
library(tidyverse)
for (i in seq_along(h)) {
  print(colnames(h)[i])
  print(table(h[i]))
}
#Row 318 M05 4.5; Row 285 E04 6.5; Row 246 III.9.8 4
table(h$M05); table(h$E04); table(h$III.9.8)
h$M05[h$M05 %in% c(4.5)] <- 5; table(h$M05)
h$E04[h$E04 %in% c(6.5)] <- 7; table(h$E04)
h$III.9.8[h$III.9.8 %in% c(4)] <- 0; table(h$III.9.8)

sort(rowSums(is.na(h)))
h30 <- h[which(rowSums(is.na(h)) < 30), ]

#Total Null Values
sum(is.na(h))

#h Dimensions
dim(h)

#Check Number of missing values in the each row --Have to think about this
length(which(rowSums(is.na(h))*100/80 > 14))

#removing the Rows with greater than 30 missing values
h <- h[which(rowSums(is.na(h)) < 30), ]

#Number of missing values in each row
NAcol <- which(colSums(is.na(h)) > 0);NAcol
sort(colSums(sapply(h[NAcol], is.na)), decreasing = TRUE)

#Replacing the missing values with 0 becaue those homes don't have Hybrid car
#4 value is out of range, will replace that with 0 as well because most of the homes don't have Hybrid car
table(h$III.9.8)
h$III.9.8 <- ifelse(is.na(h$III.9.8), 0, h$III.9.8)
h$III.9.8 <- ifelse(h$III.9.8 != 1, 0, h$III.9.8)

#Replacing the NA in flights with 0 becasue NA mean people haven't take any flight this year
table(h$flights)
h$flights <- ifelse(is.na(h$flights), 0, h$flights)

h$X <- NULL
str(h)
