library(haven)

# path = file.path("./Documents/Dimentionality Reduction/Group Project/Happiness-Sustainable-Behaviour.sav")
# dataset = read_sav(path)
# str(dataset)
# 
# write.csv(
#   dataset,
#   './Documents/Dimentionality Reduction/Group Project/Happiness-Sustainable-Behaviour.csv'
# )

data <- read.csv('./Documents/Dimentionality Reduction/Group Project/Happiness-Sustainable-Behaviour.csv')
head(data)
str(data)

#Total Null Values
sum(is.na(data))

#Data Dimensions
dim(data)

table(data$III.9.8)


#Check Number of missing values in the each row --Have to think about this
length(which(rowSums(is.na(data))*100/80 > 14))

#removing the Rows with greater than 30 missing values
data <- data[which(rowSums(is.na(data)) < 30), ]





#Number of missing values in each row
NAcol <- which(colSums(is.na(data)) > 0);NAcol
sort(colSums(sapply(data[NAcol], is.na)), decreasing = TRUE)

#Replacing the missing values with 0 becaue those homes don't have Hybrid car
#4 value is out of range, will replace that with 0 as well because most of the homes don't have Hybrid car
table(data$III.9.8)
data$III.9.8 <- ifelse(is.na(data$III.9.8), 0, data$III.9.8)
data$III.9.8 <- ifelse(data$III.9.8 != 1, 0, data$III.9.8)

#Replacing the NA in flights with 0 becasue NA mean people haven't take any flight this year
table(data$flights)
data$flights <- ifelse(is.na(data$flights), 0, data$flights)

data$X <- NULL
str(data)
