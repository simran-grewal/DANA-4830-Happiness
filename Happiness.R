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

#Questionnaire partioning
h1 <- h[c(1:18)]
h2 <- h[c(19:53)]
h3 <- h[c(54:74)]

#Total Null Values
sum(is.na(h1)); sum(is.na(h2)); sum(is.na(h3))

#h Dimensions
dim(h1); dim(h2); dim(h3)

sort(rowSums(is.na(h1)))
sort(rowSums(is.na(h2)))
sort(rowSums(is.na(h3)))

#Number of missing values in each column
NAcol <- which(colSums(is.na(h)) > 0);NAcol
sort(colSums(sapply(h[NAcol], is.na)), decreasing = TRUE)

#Part 3 Missing Values Replacement based on the sorting results
#Replacing the NA from III.9.1 to III.9.8 with 0
table(h$III.9.1)
h$III.9.1 <- ifelse(is.na(h$III.9.1), 0, h$III.9.1)
table(h$III.9.2)
h$III.9.2 <- ifelse(is.na(h$III.9.2), 0, h$III.9.2)
table(h$III.9.3)
h$III.9.3 <- ifelse(is.na(h$III.9.3), 0, h$III.9.3)
table(h$III.9.4)
h$III.9.4 <- ifelse(is.na(h$III.9.4), 0, h$III.9.4)
table(h$III.9.5)
h$III.9.5 <- ifelse(is.na(h$III.9.5), 0, h$III.9.5)
table(h$III.9.6)
h$III.9.6 <- ifelse(is.na(h$III.9.6), 0, h$III.9.6)
table(h$III.9.7)
h$III.9.7 <- ifelse(is.na(h$III.9.7), 0, h$III.9.7)
table(h$III.9.8)
h$III.9.8 <- ifelse(is.na(h$III.9.8), 0, h$III.9.8)
#h$III.9.8 <- ifelse(h$III.9.8 != 1, 0, h$III.9.8)

#Replacing the NA in flights with 0
table(h$flights)
h$flights <- ifelse(is.na(h$flights), 0, h$flights)

#Replacing the NA in water with mean
summary(h$water)
#mean = 212
h$water <- ifelse(is.na(h$water), 212, h$water)
summary(h$water)

#Replacing the NA in income with mean 
summary(h$income)
#mean = 25
h$income <- ifelse(is.na(h$income), 25, h$income)
summary(h$income)

#Replacing the NA in petrol with mean 
summary(h$petrol)
#mean = 552
h$petrol <- ifelse(is.na(h$petrol), 552, h$petrol)
summary(h$petrol)

#Replacing the NA in children with 0 
table(h$children)
h$children <- ifelse(is.na(h$children), 0, h$children)
table(h$children)

#Replacing the NA in transport with mode
table(h$transport)
#mode = 4
h$transport <- ifelse(is.na(h$transport), 4, h$transport)
table(h$transport)

#Replacing the NA in age with mean
summary(h$age)
#mean = 34
h$age <- ifelse(is.na(h$age), 34, h$age)
summary(h$age)

#Replacing the NA in job with mode
table(h$job)
#mode = 1
h$job <- ifelse(is.na(h$job), 1, h$job)
table(h$job)

#Replacing the NA in home with mode
table(h$home)
#mode = 2
h$home <- ifelse(is.na(h$home), 2, h$home)
table(h$home)

#Replacing the NA in edu with mode
table(h$edu)
#mode = 4
h$edu <- ifelse(is.na(h$edu), 4, h$edu)
table(h$edu)

#Replacing the NA in adult with mode
table(h$adult)
#mode = 4
h$adult <- ifelse(is.na(h$adult), 4, h$adult)
table(h$adult)

#Replacing the NA in sex with mode
table(h$sex)
#mode = 1
h$sex <- ifelse(is.na(h$sex), 1, h$sex)
table(h$sex)
