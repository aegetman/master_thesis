#loading the required packages
library(lme4) #for doing the IRT analyses
library(lattice) #for making the dot plot & qq-plot
library(ggpubr) #for making the box plot 
library(dplyr) #for calculating the time variable (using mutate function)
library(Rcpp)
library(MASS)
library(car)
library(broom)
library(ordinal)


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

#Converting into numeric
stat$Rating <- as.numeric(stat$Rating)

#Selecting training set
stat1<-stat %>% filter(UserID<100) 
#Selecting testing set
stat2<-stat %>% filter(UserID>100) 
stat3<-stat2 %>% filter(UserID<200)

#Making factors
stat1$UserID <-as.factor(stat1$UserID)
stat1$MovieID <-as.factor(stat1$MovieID)


stat3$UserID <-as.factor(stat3$UserID)
stat3$MovieID <-as.factor(stat3$MovieID)


################# SEARCHING FOR THE MODEL ##########################

## Just Adding everything up##
fit1<-lm(Rating~Gender+Age+Occupation+Genre,data=stat1)
summary(fit1)
PRESS1<-sum((residuals(fit1) / (1 - lm.influence(fit1)$hat))^2)
MSE1<- summary(fit1)$sigma^2
RMSE1<- sqrt(mean(resid(fit1) ^ 2))
#MSEP1<- mean((predict(fit1, newdata = stat3) - stat3$Rating)^2)

### Checking model assumptions for the full model ###
fit1.res <- residuals(fit1)
fit1.stdres <- stdres(fit1)
fit1.fittedvalues <- fitted.values(fit1)
par(mfrow = c(2,2))
### Normality assumption ###
qqnorm(fit1.stdres, main="")
qqline(fit1.stdres)
### Index against residuals ###
plot(fit1.res, xlab = "Index", ylab = "Residual")
### Residuals against fitted values ###
plot(fit1.fittedvalues, fit1.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit1.res ~ fit1.fittedvalues), col = "red")
### Standardized residuals against index ###
plot(fit1.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
mtext("Figure 1: Base Model without transformations", side = 3, line = -2, outer = TRUE, font=2)
dev.off()



##Apply ordinal logistic regression
stat1$Rating<- as.factor(stat1$Rating)
stat3$Rating<- as.factor(stat3$Rating)

fit2<-polr(Rating~Gender+Genre+Occupation+Age,data=stat1, Hess = TRUE)
summary(fit2)
## store table
(ctable <- coef(summary(fit2)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit2)) # default method gives profiled CIs
confint.default(fit2)

#NOTE:evaluation of the model not possible
PRESS2<-sum((residuals(fit2) / (1 - lm.influence(fit2)$hat))^2)
MSE2<-summary(fit2)$sigma^2
RMSE2<-sqrt(mean(resid(fit2) ^ 2))
#MSEP2<-mean((predict(fit2, newdata = stat3) - stat3$Rating)^2)

#Does not work
predict(fit2, newdata = stat3)



############Classicial fitting ##################

stat1$Rating<-as.numeric(stat1$Rating)
stat2$Rating<-as.numeric(stat2$Rating)

##Interaction between Gender+Genre
fit3<-lm(Rating~Gender:Genre+Age+Occupation,data=stat1)
summary(fit3)

PRESS3<-sum((residuals(fit3) / (1 - lm.influence(fit3)$hat))^2)
MSE3<- summary(fit3)$sigma^2
RMSE3<- sqrt(mean(resid(fit3) ^ 2))
#Does not work
MSEP3<- mean((predict(fit3, newdata = stat3) - stat3$Rating)^2)
#Makes sense to include
anova(fit1, fit3)

##Interaction between Age+Genre
fit4<-lm(Rating~Gender+Age:Genre+Occupation,data=stat1)
summary(fit4)

PRESS4<-sum((residuals(fit4) / (1 - lm.influence(fit4)$hat))^2)
MSE4<- summary(fit4)$sigma^2
RMSE4<- sqrt(mean(resid(fit4) ^ 2))
#Does not work
MSEP4<- mean((predict(fit4, newdata = stat3) - stat3$Rating)^2)
#Makes sense to include
anova(fit1, fit4)
anova(fit3, fit4)

##Interaction between Occupation+Genre
fit5<-lm(Rating~Gender+Age+Occupation:Genre,data=stat1)
summary(fit5)

PRESS5<-sum((residuals(fit5) / (1 - lm.influence(fit5)$hat))^2)
MSE5<- summary(fit5)$sigma^2
RMSE5<- sqrt(mean(resid(fit5) ^ 2))
#Does not work
MSEP5<- mean((predict(fit5, newdata = stat3) - stat3$Rating)^2)
#Makes sense to include
anova(fit1, fit5)
anova(fit4, fit5)

######################## MODEL VALIDATION WITH THREE MODELS ###################
PRESS <- c(PRESS1,PRESS3, PRESS4, PRESS5)
MSE <- c(MSE1, MSE3, MSE4, MSE5)
RMSE <- c(RMSE1, RMSE3, RMSE4, RMSE5)
#MSEP <- c(MSEP1, MSEP3, MSEP4, MSEP5)
### Results ###
n <- dim(stat3)[1]
validation.results <- data.frame(rbind(PRESS/n, MSE, RMSE), row.names = c("PRESS/n", "MSE", "RMSE"))
names(validation.results) <- c("model1", "model3", "model4", "model5")
validation.results

##Recall
# MSEP (Mean squared error of prediction on test data)
#RMSE (Root mean squared error of training set)
#MSE (Mean squared error of training set)
#PRESS (Prediction residual sum of squares)

#########################################################################################

stat1$Rating<- as.factor(stat1$Rating)
stat3$Rating<- as.factor(stat3$Rating)
test<- clm(Rating~Gender+Genre+Occupation+Age,data=stat1, link="logit")
summary(test)
exp(coef(test))
predict(test, newdata=stat3, type="prob")
ggpredict(test, newdata=stat3)















