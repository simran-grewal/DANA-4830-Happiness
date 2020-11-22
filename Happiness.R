library(haven)
library(ggplot2)
library(dplyr)
library(GPArotation)
library(stats)
library(psych)
library("factoextra")

data <- read.csv('./Documents/Dimentionality Reduction/Group Project/Happiness-Sustainable-Behaviour.csv')
data$X <- NULL
head(data)
str(data)

#Total Null Values
sum(is.na(data))

#Data Dimensions
dim(data)

table(data$III.9.8)


#Number of missing values in each row
NAcol <- which(colSums(is.na(data)) > 0);NAcol
sort(colSums(sapply(data[NAcol], is.na)), decreasing = TRUE)

#Replacing the missing values with 0 becaue those homes don't have Hybrid car
#4 value is out of range, will replace that with 0 as well because most of the homes don't have Hybrid car
table(data$III.9.8)
data$III.9.8 <- ifelse(is.na(data$III.9.8), 0, data$III.9.8)
data$III.9.8 <- ifelse(data$III.9.8 != 1, 0, data$III.9.8)

#Replacing the NA in flights with 0 becasue NAs means people haven't taken any flight this year
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


#Removing SC_10 column because of unclear question and a lot of missing values
data$SC_10 <- NULL


#Number of missing values per row for part 1 and part 2 quiz only
sort(rowSums(is.na(data[,3:54])), decreasing = T)


#Columns 21 to 54 belongs to part2 questions
###Replacing missing values in the part2 questions with the neutral value
data[21:54] <- lapply(data[21:54], function(X) {
  X <- ifelse(is.na(X), 4, X)
  return(X)
})

#So, No NAs in part 2 questions
sum(is.na(data[21:54]))


#Repace missing values for each column in part 1 with maximum repeated values
replace_with_max_value <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

getEachColumn <- function(X) {
  X <- ifelse(is.na(X), replace_with_max_value(X), X)
  return(X)
}

##Part2 values are already replaces
data[,c(3:54)] <- lapply(data[3:54], getEachColumn)

#No missing values for part 1 and part 2
sum(is.na(data[,c(3:54)]))



#Checking out of range values
outOfRange <- lapply(data[3:54], function(X) {
  isInRange <- ifelse(!X %in% c(1:7), 'YES', 'NO')
  if ('YES' %in% isInRange) {
    return(1)
  }
  return(0)
})

#Column M05 and E04 have out of range values
names(which(outOfRange == 1))

table(data$M05);table(data$E04)

data$M05 <- ifelse(data$M05 == 4.5, 5, data$M05)

data$E04 <- ifelse(data$E04 == 6.5, 6, data$E04)


################Removing Outliers#######################

#Part 1
#Mahalanobis distance
distances <-
  mahalanobis(x = data[3:20],
              center = colMeans(data[3:20]) ,
              cov = cov(data[3:20]))
cutoff <-
  qchisq(0.999, ncol(data[3:20]))
cat("cutoff = ", cutoff)
cat("Number of outliers = ", dim(data[3:20][distances > cutoff, ])[1])

data <- data[distances < cutoff, ]
cat("Number of rows left after removing outliers = ", dim(data)[1], " ")

#Part 2
#Mahalanobis distance
distances <-
  mahalanobis(x = data[21:54],
              center = colMeans(data[21:54]) ,
              cov = cov(data[21:54]))
cutoff <-
  qchisq(0.999, ncol(data[21:54]))
cat("cutoff = ", cutoff)
cat("Number of outliers = ", dim(data[21:54][distances > cutoff, ])[1])

data <- data[distances < cutoff, ]
cat("Number of rows left after removing outliers = ", dim(data)[1], " ")

#Export Cleaned DataSet
write.csv(data, "./Documents/Dimentionality Reduction/Group Project/CleanedDataFile.csv", row.names=FALSE)



###############################PCA######################

#PCA for part 1 quiz
pca_part1 <-
  princomp(data[3:20], cor = T, scores = T)
pca_part1

summary(pca_part1)
pca_part1$loadings
fviz_eig(pca_part1)
names(pca_part1)
pca_part1$scores
eig.val <- get_eigenvalue(pca_part1)
eig.val


#PCA for part 2 quiz
pca_part2 <-
  princomp(data[21:54], cor = T, scores = T)

pca_part2
summary(pca_part2)
pca_part2$loadings
fviz_eig(pca_part2)
pca_part2$scores
eig.val <- get_eigenvalue(pca_part2)
eig.val




#####################################FA#####################################
nofactors1 = fa.parallel(data[3:20], fm="ml", fa="fa")
nofactors1$fa.values#eigen values

nofactors2 = fa.parallel(data[21:54], fm="ml", fa="fa")
nofactors2$fa.values#eigen values

sum(nofactors1$fa.values > 0.7) ##new kaiser criterion
sum(nofactors2$fa.values > 0.7) ##new kaiser criterion



####FA part 1 ########
EFA.model.one <- fa(data[3:20], nfactors=2, rotate = "oblimin", fm = "ml")
fa.diagram(EFA.model.one)


######FA part 2 ######
EFA.model.two <- fa(data[21:54], nfactors=3, rotate = "oblimin", fm = "ml")
fa.diagram(EFA.model.two)

efa2new <- data[, c(21:48,50:54)]
EFA.model.two.new <- fa(efa2new, nfactors=3, rotate = "oblimin", fm = "ml")
fa.diagram(EFA.model.two.new)

#Fit indices
#Comparative fix index (CFI) = 0.8934938 (<0.90, poor)
EFA.model.one
#RMSR: 0.05
#RMSEA: 0.064
#NNFI/TLI: 0.868
EFA.model.one$STATISTIC
EFA.model.one$dof
EFA.model.one$null.chisq
EFA.model.one$null.dof
1 - ((279.3556-118)/(1744.852-153))
#CFI: 0.8986366

EFA.model.two.new
#RMSR: 0.04
#RMSEA: 0.066
#NNFI/TLI: 0.847
EFA.model.two.new$STATISTIC
EFA.model.two.new$dof
EFA.model.two.new$null.chisq
EFA.model.two.new$null.dof
1 - ((1064.346-432)/(5601.906-528))
#CFI: 0.8753729

#Reliability
#part1
#f1 for ML1; f2 for ML2
names(data[, c(3:20)])
f1p1 = c(3:8, 11, 15:16, 18:20)
f2p1 = c(9:10, 12:14, 17)
psych::alpha(data[ , f1p1])
#raw alpha of factor 1: 0.86
psych::alpha(data[ , f2p1])
#raw alpha of factor 2: 0.68

#part2
#efa2new <- data[, c(21:48,50:54)]
names(efa2new)
f1p2 = c()
f2p2 = c()
f3p2 = c()
psych::alpha(data[ , f1p2])
#raw alpha of factor 1: 
psych::alpha(data[ , f2p2])
#raw alpha of factor 2: 
psych::alpha(data[ , f3p2])
#raw alpha of factor 3: 





