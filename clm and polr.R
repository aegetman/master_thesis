#loading the required packages
library(lme4) #for doing the IRT analyses
library(lattice) #for making the dot plot & qq-plot
library(ggpubr) #for making the box plot 
library(dplyr) #for calculating the time variable (using mutate function)
library(Rcpp)
library(MASS) #for polr function ==> ordinal logistic regression
library(car)
library(broom)
library(ordinal) #for clm function ==> ordinal logistic regression
library(ggeffects) #for ggpredict function

##import data & look at data (saved as a csv-file)##
# select directory with data in R
setwd("C:\\Users\\aleks\\Downloads") ##or use the File menu (change dir)
stat <- read.csv("ratings.txt", sep=";")#reads in data
str(stat)## shows some attributes and first scores for each variable


################### PREP ANALYSIS #########################

#Age categories as factors
stat$Age <- factor(stat$Age, levels=c(1, 18, 25, 35, 45, 50, 56), labels=c("Under 18", "18-24", "25-34", "35-44", "45-49", "50-55", "56+"))

#Occupation categories as factors
stat$Occupation <- factor(stat$Occupation, levels=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20), labels=c("Other", "Academic/ educator", "Artist", "Clerical/admin", "College/grad student", "Customer service", "Doctor/health care", "Executive/managerial", "Farmer", "Homemaker", "K-12 student", "Lawyer", "Programmer", "Retired", "Sales/marketing", "Scientist", "Self-employed", "Technician/engineer", "Tradesman/craftsman", "Unemployed", "Writer"))

#Creat a subset
stat1<-stat %>% filter(UserID<100) 

#Making factors
stat1$UserID <-as.factor(stat1$UserID)
stat1$MovieID <-as.factor(stat1$MovieID)
stat1$Rating<- factor(stat1$Rating, levels=c(1,2,3,4,5), ordered=T)

#Make testing and training set
d.test<-sample(nrow(stat1), nrow(stat1)*.7)
training<-stat1[d.test, ]
test<-stat1[-d.test, ]
################# SEARCHING FOR THE MODEL ##########################

#### With polr function ####

fit1<-polr(Rating~Gender+Genre+Occupation+Age,data=training, Hess = TRUE)
summary(fit1)

#Compute confusion table and misclassification error
predictrating = predict(fit1,test)
table(test$Rating, predictrating)
mean(as.character(dtest$Rating) != as.character(predictrating))
#########################################################################################

### CLM function from ordinal package to get ordinal logistic regression ####
#fit2<- clm(Rating~Gender+Genre+Occupation+Age,data=training, link="logit")
fit2<- clm(Rating~Gender,data=training, link="logit")
summary(fit2)
#To get commulative probabilities
exp(coef(fit2))/(1+exp(coef(fit2)))
#To get the odds
exp(coef(fit2))


cbind(test$Rating, predict(fit2, test, type = "prob")$fit)

predict(fit2, newdata=test, type="prob")
#Automatically gives CI
ggpredict(fit2, terms = c("Gender","Genre","Occupation","Age"))
ggpredictions_ols1<- ggpredict(fit2, terms = c("Gender"))

ggpredictions_ols1$x = factor(ggpredictions_ols1$x)
levels(ggpredictions_ols1$x) = c("object", "subject")
colnames(ggpredictions_ols1)[c(1, 5)] = c("Position", "Rating")

ggpredictions_ols1














