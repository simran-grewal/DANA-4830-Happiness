library(haven)
library(ggplot2)
library(dplyr)

data <- read.csv('./Documents/Dimentionality Reduction/Group Project/Happiness-Sustainable-Behaviour.csv')
data$X <- NULL
head(data)
str(data)

#Total Null Values
sum(is.na(data))

#Data Dimensions
dim(data)

table(data$III.9.8)


#Check Number of missing values in the each row --Have to think about this
# length(which(rowSums(is.na(data))*100/80 > 14))

#removing the Rows with greater than 30 missing values
# data <- data[which(rowSums(is.na(data)) < 30), ]
# 




#Number of missing values in each row
NAcol <- which(colSums(is.na(data)) > 0);NAcol
sort(colSums(sapply(data[NAcol], is.na)), decreasing = TRUE)

#Replacing the missing values with 0 becaue those homes don't have Hybrid car
#4 value is out of range, will replace that with 0 as well because most of the homes don't have Hybrid car
table(data$III.9.8)
data$III.9.8 <- ifelse(is.na(data$III.9.8), 0, data$III.9.8)
data$III.9.8 <- ifelse(data$III.9.8 != 1, 0, data$III.9.8)

#Replacing the NA in flights with 0 becasue NA, means people haven't taken any flight this year
table(data$flights)
data$flights <- ifelse(is.na(data$flights), 0, data$flights)



Not_attempted_q9 <- which(
  is.na(data$III.9.2)
  & is.na(data$III.9.3)
  & is.na(data$III.9.4)
  & is.na(data$III.9.5)
  & is.na(data$III.9.6)
  & is.na(data$III.9.1)
  & is.na(data$III.9.7)
)

#Means 31 people completly skipped this questions
length(Not_attempted_q9)

data[c(Not_attempted_q9),"income"]
table(data$income)


#Number of missing values per row   
sort(rowSums(is.na(data[,c(3:55)])), decreasing = T)


#%age of missing values per row
sort((rowSums(is.na(data[,c(3:55)]))*100)/53, decreasing = T)


#Removing SC_10 column because of unclear question and a lot of missing values
data$SC_10 <- NULL


#Repace missing values for each column in part 1 and part 2 with maximum repeated values
replace_with_max_value <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

getEachColumn <- function(X) {
  X <- ifelse(is.na(X), replace_with_max_value(X), X)
  return(X)
}

data[,c(3:54)] <- lapply(data[3:54], getEachColumn)

#No missing values for part 1 and part 2
sum(is.na(data[,c(3:54)]))


#Checking out of range values
outOfRage <- lapply(data[3:54], function(X) {
  outOfRange <- ifelse(!X %in% c(1:7), 'YES', 'NO')
  if ('YES' %in% outOfRange) {
    return(1)
  }
  return(0)
})

#Column M05 and E04 have out of range values
names(which(outOfRage == 1))



#Part 3

#Transport
table(data$transport)

sum(is.na(data$transport))

data[which(!is.na(data$income) & !is.na(data$transport)), ] %>%
  ggplot(aes(x = as.factor(transport), y = income, color = as.factor(transport))) +
  geom_boxplot() + 
  geom_jitter(alpha = 0.3)



