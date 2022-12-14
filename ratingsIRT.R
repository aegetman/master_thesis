#loading the required packages
library(lme4) #for doing the IRT analyses
library(lattice) #for making the dot plot & qq-plot
library(ggpubr) #for making the box plot 
library(dplyr) #for calculating the time variable (using mutate function)
library(Rcpp)
library(eirm)
library(ggplot2)
library(tidyverse)


##import data & look at data (saved as a csv-file)##
# select directory with data in R
setwd("C:\\Users\\Usuario\\Downloads\\ml-1m\\ml-1m") ##or use the File menu (change dir)
stat <- read.csv("ratings.txt", sep=";")#reads in data
str(stat)## shows some attributes and first scores for each variable




#Reformating dataset for polytomous analysis
stat_poly <- polyreformat(data=stat, id.var = "UserID", long.format = FALSE, 
                         var.name = "MovieID", val.name = "Rating")

# Sort the data by user and movie id
stat_poly <- stat_poly[order(stat_poly$UserID, stat_poly$MovieID),]

#miniset4 <- stat_poly %>% filter(UserID==21, MovieID==1)
stat1<-stat %>% filter(UserID<100) 
stat_poly1<-stat_poly %>% filter(UserID<100) 



#preparing your data for the analysis
stat1$UserID <-as.factor(stat1$UserID)
stat1$MovieID <-as.factor(stat1$MovieID)
stat1$Occupation <-as.factor(stat1$Occupation)
stat1$Age <-as.factor(stat1$Age)

stat_poly1$UserID <-as.factor(stat_poly1$UserID)
stat_poly1$MovieID <-as.factor(stat_poly1$MovieID)
stat_poly1$Occupation <-as.factor(stat_poly1$Occupation)
stat_poly1$Age <-as.factor(stat_poly1$Age)

#EIRT model for rating (polytomous variable)
#mod1 <- eirm(formula = "polyresponse ~ -1  + Gender + Occupation + Genre + (1|MovieID)", data = stat_poly1)
mod1 <- eirm(formula = "polyresponse ~ -1 + Gender + Age + Genre + polycategory + (1|UserID) ", data = stat_poly1)
print(mod1)

rating_pred = predict(mod1$model, newdata = stat_poly1, type = "response") 
stat_poly1$rating_pred_IRT <- rating_pred
#miniset <- stat_poly1 %>% filter(UserID==68, MovieID==1) 
#miniset6<- stat_poly1 %>% filter(UserID==28, MovieID==1) 


############################Predicted probabilities with formula###################################

base_exp <- function(i) {
  base_ex <-   ifelse(stat1$Gender[i]=="F",mod1$parameters$Easiness[1],mod1$parameters$Easiness[2])+
    ifelse(stat1$Age[i]==18,mod1$parameters$Easiness[3],
           ifelse(stat1$Age[i]==25,mod1$parameters$Easiness[4],
                  ifelse(stat1$Age[i]==35,mod1$parameters$Easiness[5],
                         ifelse(stat1$Age[i]==45,mod1$parameters$Easiness[6],
                                ifelse(stat1$Age[i]==50,mod1$parameters$Easiness[7],
                                       ifelse(stat1$Age[i]==56,mod1$parameters$Easiness[8],0))))))+
    ifelse(stat1$Genre[i]=="Adventure",mod1$parameters$Easiness[9],
           ifelse(stat1$Genre[i]=="Comedy",mod1$parameters$Easiness[10],
                  ifelse(stat1$Genre[i]=="Crime",mod1$parameters$Easiness[11],
                         ifelse(stat1$Genre[i]=="Documentary",mod1$parameters$Easiness[12],
                                ifelse(stat1$Genre[i]=="Drama",mod1$parameters$Easiness[13],
                                       ifelse(stat1$Genre[i]=="Horror",mod1$parameters$Easiness[14],
                                              ifelse(stat1$Genre[i]=="Other",mod1$parameters$Easiness[15],
                                                     ifelse(stat1$Genre[i]=="Thriller",mod1$parameters$Easiness[16],0))))))))+
    mod1$persons$UserID$`(Intercept)`[stat1$UserID[i]]
}


item_prof = list()
pred_rating_1 = list()
pred_rating_2 = list()
pred_rating_3 = list()
pred_rating_4 = list()
pred_rating_5 = list()

#for(i in 1:200){
for(i in 1:nrow(stat1)){
  num1 = 1
  num2 = exp(base_exp(i))
  num3 = exp(2*base_exp(i)+mod1$parameters$Easiness[17])
  num4 = exp(3*base_exp(i)+2*mod1$parameters$Easiness[17]+mod1$parameters$Easiness[18])
  num5 = exp(4*base_exp(i)+3*mod1$parameters$Easiness[17]+2*mod1$parameters$Easiness[18]+mod1$parameters$Easiness[19])
  
  denom = num1 + num2 + num3 + num4 + num5
  
  item_prof[i] = num2
  pred_rating_1[i] = num1/denom
  pred_rating_2[i] = num2/denom
  pred_rating_3[i] = num3/denom
  pred_rating_4[i] = num4/denom
  pred_rating_5[i] = num5/denom
  
}



stat1$item_profile = as.numeric(item_prof)
stat1$prob_rating1 = as.numeric(pred_rating_1)
stat1$prob_rating2 = as.numeric(pred_rating_2)
stat1$prob_rating3 = as.numeric(pred_rating_3)
stat1$prob_rating4 = as.numeric(pred_rating_4)
stat1$prob_rating5 = as.numeric(pred_rating_5)


stat1$pred_rating <- stat1$prob_rating1 + stat1$prob_rating2*2 + stat1$prob_rating3*3 + + stat1$prob_rating4*4 + stat1$prob_rating5*5


ggplot(stat1, aes(x=item_profile)) + 
  geom_line(aes(y = prob_rating1), color = "darkred") +
  geom_line(aes(y = prob_rating2), color="steelblue") + 
  geom_line(aes(y = prob_rating3), color="purple") +
  geom_line(aes(y = prob_rating4), color="green") +
  geom_line(aes(y = prob_rating5), color="red") + 
  geom_histogram(aes(y=..density..), alpha=0.5, position="identity", fill = "pink")

ggplot(stat1, aes(x=item_profile))+geom_histogram()

max(stat1$prob_rating1[1],stat1$prob_rating2[1],stat1$prob_rating3[1],stat1$prob_rating4[1],stat1$prob_rating5[1])



df <- stat1 %>%
  select(item_profile, prob_rating1, prob_rating2,prob_rating3,prob_rating4,prob_rating5) %>%
  gather(key = "variable", value = "value", -item_profile)


ggplot(df, aes(x = item_profile, y = value)) + 
  geom_line(aes(color = variable), size = 1.3) + 
  geom_histogram(aes(y=..density..), alpha=0.5, position="identity", fill = "pink")+
  ggtitle("Movie profile vs predicted probability") + ylab("Predicted probability") + xlab("Movie profile") + theme_light()





#The previous would calculate the same ratings for all movies of a specific user because the model only considers user's 
#variables. It could be useful to get initial estimates for the user profile towards the system. Think about it more





hist(stat_poly1$rating_pred_IRT[stat_poly1$polycategory=='cat_2'])

hist(x = stat_poly1$rating_pred_IRT[stat_poly1$polycategory=='cat_2'], freq = FALSE)
lines(x = density(x = stat_poly1$rating_pred_IRT[stat_poly1$polycategory=='cat_2']), col = "red")



ggplot(stat_poly1, aes(x=rating_pred_IRT, fill=polycategory)) +
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity')


rating_pred = predict(mod1$model, newdata = stat1, type = "response") 
stat1$rating_pred_IRT <- rating_pred


###################### MATCHING ANALYSIS ##################

#model 1: the Rasch model (items fixed)
#model1<- glmer (Matching~-1+ MovieID+(1|UserID ), stat, family=binomial(link="logit"))  
#summary(model1)

#model 2: the Rasch model (items random)
model2<- glmer(Matching ~ 1+ (1|MovieID)+(1|UserID), data=stat1, family=binomial(link="logit")) 
summary(model2)

#converting the mean logit of .3011 to a probability, 100 users
logit =-1.74002
exp(logit)/(1+exp(logit))

#converting the mean logit of .3011 to a probability, 1000 users
logit =-1.7217
exp(logit)/(1+exp(logit))


#exploring the random effects
RE<-ranef(model2)
dotplot(RE)
qqmath(RE)

RE <- as.data.frame(RE)
RE_user <- RE[RE$grpvar=="UserID",]
RE_item <- RE[RE$grpvar=="MovieID",]
summary(RE_user$condval)
summary(RE_item$condval)

ggplot(RE_user, aes(condval)) + geom_histogram(binwidth = .1) + labs(x="user match with site")
ggplot(RE_item, aes(condval)) + geom_histogram(binwidth = .1) + labs(x="item matchability")

## does the ability depend on the course?
# stat1<-stat[!duplicated(stat$userid),c(1:10)] #make a dataset with only one line per student
# names(RE_user)[names(RE_user) == "grp"] <- "userid" #rename the student indicator in the dataframe with random effects
# stat1 <- merge(stat1,RE_user,by="userid") #combine both datasets
# ggboxplot(stat1, x = "course", add="mean", y = "condval", ylab = "Vaardigheid", xlab = "Studierichting") 
# 
# ## does the difficulty depend on the processing level?
# stat1<-stat[!duplicated(stat$itemid),c(12:15)] #make a dataset with only one line per item
# names(RE_item)[names(RE_item) == "grp"] <- "itemid" #rename the student indicator in the dataframe with random effects
# stat1 <- merge(stat1,RE_item,by="itemid") #combine both datasets
# ggboxplot(stat1, x = "itemtype", add="mean", y = "condval", ylab = "Gemakkelijkheid", xlab = "Type") 

#model 3: Movie genre explanatory model
model3<- glmer(Matching~1+ Genre + (1|MovieID)+(1|UserID), stat1, family=binomial(link="logit"))  
summary(model3)

#model 4: User gender explanatory model
# model4<- glmer (grade~1+ itemtype + (1|itemid)+(1|userid), stat, family=binomial(link="logit"))  
# summary(model4)



#comparing model fit
anova(model2, model3)

