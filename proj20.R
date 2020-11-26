#setup
rm(list = ls())
setwd("/Users/DD/Desktop/Class")
x <- read.csv("Dana 4820/proj/loan_data_set.csv", TRUE, ',')
summary(x)
#remove loan id
x <- x[ , -c(1)]

table(x$Loan_Status)
percentmiss = function(x) {sum(is.na(x)) / length(x)*100}
missing = apply(x, 1, percentmiss)
table(missing)
#84 rows with 1/12 = 8.33% missing data. aka 84 rows with missing 1 value 
#1 row with 2/12 = 16.67% missing data. aka 1 row with missing 2 values
str(x)
View(x)

table(x$Credit_History)
#credit history is binary, but not coded as factor
x$Credit_History = factor(x$Credit_History, levels = c(0,1)) 


#detect multicollinearity among 4 non-categorical variables
cor = cor(x[ , c(6,7,8,9)], use = 'pairwise.complete.obs')
View(cor)
symnum(cor)
#no multicolinearity among our int/num variables


#interaction
x$Loan_Status <- as.integer(x$Loan_Status)
#i couldnt figure out how to run interaction plot without first changing loan status to int

#interaction.plot(x$ApplicantIncome, x$CoapplicantIncome, x$LoanAmount)
#is there a simpler way? or its usu done like this?
with(x,{interaction.plot(Gender, Married, Loan_Status)}) #yes?
with(x,{interaction.plot(Gender, Dependents, Loan_Status)}) #yes?
with(x,{interaction.plot(Gender, Education, Loan_Status)}) #yes?
with(x,{interaction.plot(Gender, Self_Employed, Loan_Status)})#yes?
with(x,{interaction.plot(Gender, ApplicantIncome, Loan_Status)})#yes?
with(x,{interaction.plot(Gender, CoapplicantIncome, Loan_Status)})#yes?
with(x,{interaction.plot(Gender, LoanAmount, Loan_Status)})#yes?
with(x,{interaction.plot(Gender, Loan_Amount_Term, Loan_Status)})#yes?
with(x,{interaction.plot(Gender, Credit_History, Loan_Status)})
with(x,{interaction.plot(Gender, Property_Area, Loan_Status)})#yes?

with(x,{interaction.plot(Married, Dependents, Loan_Status)})
with(x,{interaction.plot(Married, Education, Loan_Status)})
with(x,{interaction.plot(Married, Self_Employed, Loan_Status)})#yes?
with(x,{interaction.plot(Married, ApplicantIncome, Loan_Status)})#yes?
with(x,{interaction.plot(Married, CoapplicantIncome, Loan_Status)})#yes?
with(x,{interaction.plot(Married, LoanAmount, Loan_Status)})#yes?
with(x,{interaction.plot(Married, Loan_Amount_Term, Loan_Status)})#yes?
with(x,{interaction.plot(Married, Credit_History, Loan_Status)})
with(x,{interaction.plot(Married, Property_Area, Loan_Status)})#yes?

with(x,{interaction.plot(Education, Self_Employed, Loan_Status)})#yes?
with(x,{interaction.plot(Education, ApplicantIncome, Loan_Status)})#yes?
with(x,{interaction.plot(Education, CoapplicantIncome, Loan_Status)})#yes?
with(x,{interaction.plot(Education, LoanAmount, Loan_Status)})#yes?
with(x,{interaction.plot(Education, Loan_Amount_Term, Loan_Status)})#yes?
with(x,{interaction.plot(Education, Credit_History, Loan_Status)})
with(x,{interaction.plot(Education, Property_Area, Loan_Status)})#yes?

with(x,{interaction.plot(Self_Employed, ApplicantIncome, Loan_Status)})#yes?
with(x,{interaction.plot(Self_Employed, CoapplicantIncome, Loan_Status)})#yes?
with(x,{interaction.plot(Self_Employed, LoanAmount, Loan_Status)})#yes?
with(x,{interaction.plot(Self_Employed, Loan_Amount_Term, Loan_Status)})#yes?
with(x,{interaction.plot(Self_Employed, Credit_History, Loan_Status)})
with(x,{interaction.plot(Self_Employed, Property_Area, Loan_Status)})#yes?

with(x,{interaction.plot(ApplicantIncome, CoapplicantIncome, Loan_Status)})
with(x,{interaction.plot(ApplicantIncome, LoanAmount, Loan_Status)})
with(x,{interaction.plot(ApplicantIncome, Loan_Amount_Term, Loan_Status)})
with(x,{interaction.plot(ApplicantIncome, Credit_History, Loan_Status)})
with(x,{interaction.plot(ApplicantIncome, Property_Area, Loan_Status)})

with(x,{interaction.plot(CoapplicantIncome, LoanAmount, Loan_Status)})
with(x,{interaction.plot(CoapplicantIncome, Loan_Amount_Term, Loan_Status)})
with(x,{interaction.plot(CoapplicantIncome, Credit_History, Loan_Status)})
with(x,{interaction.plot(CoapplicantIncome, Property_Area, Loan_Status)})

with(x,{interaction.plot(LoanAmount, Loan_Amount_Term, Loan_Status)})
with(x,{interaction.plot(LoanAmount, Credit_History, Loan_Status)})
with(x,{interaction.plot(LoanAmount, Property_Area, Loan_Status)})

with(x,{interaction.plot(Loan_Amount_Term, Credit_History, Loan_Status)})
with(x,{interaction.plot(Loan_Amount_Term, Property_Area, Loan_Status)})

with(x,{interaction.plot(Credit_History, Property_Area, Loan_Status)})

#7 splitting
set.seed(100)
library(caTools)
sample_size <- sample.split(x, SplitRatio = 7/10)
train <-subset(x, sample_size == T)
test <-subset(x, sample_size == F)


##Step 4-stepwise
#The intercept  is the expected mean value of Y when all X=0
library(lmtest)
intercept.only.model <- lm(Loan_Status ~., data = x)
summary(intercept.only.model)

full.model.clean <- lm(Loan_Status ~., data = x)

lm.step <- step(intercept.only.model, direction = 'both', scope = formula(full.model.clean))
lm.step.res <- resid(lm.step) # why this no work? forgot what this line is for

lm.step.one <- lm(Loan_Status ~ Married + Education + CoapplicantIncome + LoanAmount + 
                    Loan_Amount_Term + Credit_History + Property_Area, x)
summary(lm.step.one)
