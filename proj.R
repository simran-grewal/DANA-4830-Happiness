#setup
setwd("/Users/DD/Desktop/Class")
x <- read.csv("Dana 4830/project/project.csv", TRUE, ',')
x <- x[ , -c(1:2)] #removes first 2 id columns
x <- x[ , -c(75:86)] #remove columns from agebin and beyond
x <- x[ , -c(28)]  #remove sc10(composting)



percentmiss = function(x) {sum(is.na(x)) / length(x)*100}
#using the above function
#participants
missing = apply(x, 1, percentmiss)
table(missing)

replacepeople = subset(x, missing <= 5) #subset rows with less than 5% missing values
nopeople = subset(x, missing > 5) #subset rows with more than 5% missing values (we dont touch this)


#column
missingc = apply(x, 2, percentmiss)
missingc


replaceall = replacepeople[ , -c(53:73)] #subset the columns we want
nocolumn = replacepeople[ , c(53:73)] #the demographics (dont want)

#run mice to replace missing values from participants with less than 5% missing value
library(mice)
tempnomiss = mice(replaceall) 
replaced = complete(tempnomiss, 1)
summary(replaced)

#combine back together, column first
allcolumns = cbind(replaced, nocolumn)
allrows = rbind(allcolumns, nopeople)
summary(allrows)



#ignore below
View(h[ , -c(1:2)])

missingc = apply(h[ , -c(1:2)], 2, percentmiss)
table(missingc)

sort(rowSums(is.na(h)))
data <- data[which(rowSums(is.na(h)) < 30), ]


names(x)

#library(haven)
#path = file.path("/Users/DD/Desktop/Class/Dana 4830/project/Happiness-Sustainable-Behaviour.sav")
# = read_sav(path)
#View(h)
#data <- h[ , -c(1:2)]
#x <- data[ , -c(75:86)] 
#x <- x[ , -c(28)] # final set
##write.csv(h, "project.csv", row.names = FALSE)
#summary(x)