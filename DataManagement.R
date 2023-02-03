
library(dummy)
library(tidyverse)
library(lme4) 
library(lattice) 
library(ggpubr) 
library(dplyr) 
library(Rcpp)
library(eirm)
library(ggplot2)

setwd("C:\\Users\\Usuario\\Documents\\1 Master thesis")

#Import movies dataset
txt <- readLines("movies.dat", encoding = "latin1")
txt_split <- lapply(strsplit(txt, "::"), function(x) as.data.frame(t(x), stringsAsFactors=FALSE))
df <- do.call(rbind, txt_split)
names(df) <- c("MovieID", "Title", "Genres")

#Separate rows by each genre
df_clean <-
  df %>%
  separate_rows(Genres, sep = ";")

df_clean %>% count(Genres, sort = TRUE)
#18 genres in total



# create dummy variables for all unique values in Genres
movies <-
  df_clean %>%
  select(Genres) %>%
  dummy() %>%
  bind_cols(df_clean) %>%
  select(-Genres) %>%
  gather(key, value, -MovieID, -Title) %>%
  filter(value != 0) %>%
  spread(key, value, fill = 0) %>%
  group_by(MovieID, Title) %>%
  mutate_all(funs(as.integer(.) %>% as.logical())) %>%
  ungroup() %>%
  left_join(y = df, by = c("MovieID", "Title")) %>%
  select(MovieID, Title, matches("Genres_"))


#Import users dataset
txt <- readLines("users.dat", encoding = "latin1")
txt_split <- lapply(strsplit(txt, "::"), function(x) as.data.frame(t(x), stringsAsFactors=FALSE))
users <- do.call(rbind, txt_split)
names(users) <- c("UserID", "Gender", "Age", "Occupation", "Zip-code")
  
#Import ratings dataset  
txt <- readLines("ratings.dat", encoding = "latin1")
txt_split <- lapply(strsplit(txt, "::"), function(x) as.data.frame(t(x), stringsAsFactors=FALSE))
ratings <- do.call(rbind, txt_split)
names(ratings) <- c("UserID", "MovieID", "Rating","Timestamp")

#Combine datasets
ratings <- ratings %>% inner_join(movies) %>% inner_join(users)

write.csv(ratings, file = "ratings_complete.csv")


#############################################################################33
setwd("C:\\Users\\Usuario\\Documents\\1 Master thesis")
stat <- read.csv("ratings_complete.csv")

stat.cs <- as.data.frame(stat %>% group_by(UserID) %>% slice_sample(n=10) %>% ungroup())
#miniset4 <- stat_poly %>% filter(UserID==21, MovieID==1)


#Reformating dataset for polytomous analysis
stat_poly.cs <- polyreformat(data=stat.cs, id.var = "UserID", long.format = FALSE, 
                             var.name = "MovieID", val.name = "Rating")

# Sort the data by user and movie id
stat_poly.cs <- stat_poly.cs[order(stat_poly.cs$UserID, stat_poly.cs$MovieID),]



stat.cs$UserID <-as.factor(stat.cs$UserID)
stat.cs$MovieID <-as.factor(stat.cs$MovieID)
stat.cs$Occupation <-as.factor(stat.cs$Occupation)
stat.cs$Age <-as.factor(stat.cs$Age)

stat_poly.cs$UserID <-as.factor(stat_poly.cs$UserID)
stat_poly.cs$MovieID <-as.factor(stat_poly.cs$MovieID)
stat_poly.cs$Occupation <-as.factor(stat_poly.cs$Occupation)
stat_poly.cs$Age <-as.factor(stat_poly.cs$Age)



#EIRT model for rating (polytomous variable) for dataset of 10 movies samples for 6040 users

mod.cs3 <- eirm(formula = "polyresponse ~ -1 + polycategory + (1|UserID) + Gender + Age +
Gender:Genres_Action + Gender:Genres_Animation	 + Gender:Genres_Documentary	 + 
Gender:Genres_Sci.Fi	 + Gender:Genres_Adventure	 + Gender:Genres_Children.s	 + 
Gender:Genres_Drama	 + Gender:Genres_Thriller	 + Gender:Genres_Comedy	 + 
Gender:Genres_Fantasy	 + Gender:Genres_War	 + Gender:Genres_Crime	 + 
Gender:Genres_Film.Noir	 + Gender:Genres_Western	 + Gender:Genres_Horror	 + 
Gender:Genres_Musical	 + Gender:Genres_Mystery	 + Gender:Genres_Romance + 
               Age:Genres_Action	 + Age:Genres_Animation	 + Age:Genres_Documentary	 + 
               Age:Genres_Sci.Fi	 + Age:Genres_Adventure	 + Age:Genres_Children.s	 + 
               Age:Genres_Drama	 + Age:Genres_Thriller	 + Age:Genres_Comedy	 + 
               Age:Genres_Fantasy	 + Age:Genres_War	 + Age:Genres_Crime	 + 
               Age:Genres_Film.Noir	 + Age:Genres_Western	 + Age:Genres_Horror	 + 
               Age:Genres_Musical	 + Age:Genres_Mystery	 + Age:Genres_Romance", data = stat_poly.cs)
print(mod.cs3)


mod.cs2 <- eirm(formula = "polyresponse ~ -1 + polycategory + (1|UserID) + Gender + Age +
               Age:Genres_Action	 + Age:Genres_Animation	 + Age:Genres_Documentary	 + 
               Age:Genres_Sci.Fi	 + Age:Genres_Adventure	 + Age:Genres_Children.s	 + 
               Age:Genres_Drama	 + Age:Genres_Thriller	 + Age:Genres_Comedy	 + 
               Age:Genres_Fantasy	 + Age:Genres_War	 + Age:Genres_Crime	 + 
               Age:Genres_Film.Noir	 + Age:Genres_Western	 + Age:Genres_Horror	 + 
               Age:Genres_Musical	 + Age:Genres_Mystery	 + Age:Genres_Romance", data = stat_poly.cs)
print(mod.cs2)


mod.cs0 <- eirm(formula = "polyresponse ~ -1 + Age + Gender+ 
Genres_Action + Genres_Animation	 + Genres_Documentary	 + 
Genres_Sci.Fi	 + Genres_Adventure	 + Genres_Children.s	 + 
Genres_Drama	 + Genres_Thriller	 + Genres_Comedy	 + 
Genres_Fantasy	 + Genres_War	 + Genres_Crime	 + 
Genres_Film.Noir	 + Genres_Western	 + Genres_Horror	 + 
Genres_Musical	 + Genres_Mystery	 + Genres_Romance + polycategory + (1|UserID)", 
                data = stat_poly.cs)
print(mod.cs0)

#Previous models have too many parameters, and interactions are not always significant

#Chosen model. Most interactions are significant and it's not that complex

mod.cs <- eirm(formula = "polyresponse ~ -1 + Age + Gender+ 
Gender:Genres_Action + Gender:Genres_Animation	 + Gender:Genres_Documentary	 + 
Gender:Genres_Sci.Fi	 + Gender:Genres_Adventure	 + Gender:Genres_Children.s	 + 
Gender:Genres_Drama	 + Gender:Genres_Thriller	 + Gender:Genres_Comedy	 + 
Gender:Genres_Fantasy	 + Gender:Genres_War	 + Gender:Genres_Crime	 + 
Gender:Genres_Film.Noir	 + Gender:Genres_Western	 + Gender:Genres_Horror	 + 
Gender:Genres_Musical	 + Gender:Genres_Mystery	 + Gender:Genres_Romance + polycategory + (1|UserID)", 
               data = stat_poly.cs)
print(mod.cs)


############################  Predicted probabilities with formula  ###################################

base_exp <- function(i, df.mod, mod.eirt) {
  base_ex <-   ifelse(df.mod$Age[i]==1,mod.eirt$parameters$Easiness[1],
                      ifelse(df.mod$Age[i]==18,mod.eirt$parameters$Easiness[2],
                             ifelse(df.mod$Age[i]==25,mod.eirt$parameters$Easiness[3],
                                    ifelse(df.mod$Age[i]==35,mod.eirt$parameters$Easiness[4],
                                           ifelse(df.mod$Age[i]==45,mod.eirt$parameters$Easiness[5],
                                                  ifelse(df.mod$Age[i]==50,mod.eirt$parameters$Easiness[6],
                                                         ifelse(df.mod$Age[i]==56,mod.eirt$parameters$Easiness[7],0)))))))+
    ifelse(df.mod$Gender[i]=="M",mod.eirt$parameters$Easiness[8],0) +
    ifelse(df.mod$Genres_Action[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[12],mod.eirt$parameters$Easiness[13]),0) +
    ifelse(df.mod$Genres_Animation[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[14],mod.eirt$parameters$Easiness[15]),0) +
    ifelse(df.mod$Genres_Documentary[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[16],mod.eirt$parameters$Easiness[17]),0) +
    ifelse(df.mod$Genres_Sci.Fi[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[18],mod.eirt$parameters$Easiness[19]),0) +
    ifelse(df.mod$Genres_Adventure[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[20],mod.eirt$parameters$Easiness[21]),0) +
    ifelse(df.mod$Genres_Children.s[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[22],mod.eirt$parameters$Easiness[23]),0) +
    ifelse(df.mod$Genres_Drama[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[24],mod.eirt$parameters$Easiness[25]),0) +
    ifelse(df.mod$Genres_Thriller[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[26],mod.eirt$parameters$Easiness[27]),0) +
    ifelse(df.mod$Genres_Comedy[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[28],mod.eirt$parameters$Easiness[29]),0) +
    ifelse(df.mod$Genres_Fantasy[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[30],mod.eirt$parameters$Easiness[31]),0) +
    ifelse(df.mod$Genres_War[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[32],mod.eirt$parameters$Easiness[33]),0) +
    ifelse(df.mod$Genres_Crime[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[34],mod.eirt$parameters$Easiness[35]),0) +
    ifelse(df.mod$Genres_Film.Noir[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[36],mod.eirt$parameters$Easiness[37]),0) +
    ifelse(df.mod$Genres_Western[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[38],mod.eirt$parameters$Easiness[39]),0) +
    ifelse(df.mod$Genres_Horror[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[40],mod.eirt$parameters$Easiness[41]),0) +
    ifelse(df.mod$Genres_Musical[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[42],mod.eirt$parameters$Easiness[43]),0) +
    ifelse(df.mod$Genres_Mystery[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[44],mod.eirt$parameters$Easiness[45]),0) +
    ifelse(df.mod$Genres_Romance[i]==1, 
           ifelse(df.mod$Gender[i]=="F", mod.eirt$parameters$Easiness[46],mod.eirt$parameters$Easiness[47]),0) +
    as.numeric(mod.eirt$persons$UserID$`(Intercept)`[df.mod$UserID[i]])
}

##Function to calculate average predicted rating
avg.pred.rating <- function(i, df.mod, mod.eirt){
  
  num1 = 1
  num2 = exp(base_exp(i, df.mod, mod.eirt))
  num3 = exp(2*base_exp(i, df.mod, mod.eirt)+mod.eirt$parameters$Easiness[9])
  num4 = exp(3*base_exp(i, df.mod, mod.eirt)+2*mod.eirt$parameters$Easiness[9]+mod.eirt$parameters$Easiness[10])
  num5 = exp(4*base_exp(i, df.mod, mod.eirt)+3*mod.eirt$parameters$Easiness[9]+2*mod.eirt$parameters$Easiness[10]+mod.eirt$parameters$Easiness[11])
  
  denom = num1 + num2 + num3 + num4 + num5
  
  item_prof = num2
  pred_rating_1 = num1/denom
  pred_rating_2 = num2/denom
  pred_rating_3 = num3/denom
  pred_rating_4 = num4/denom
  pred_rating_5 = num5/denom
  
  df.preds = data.frame(rating = c(1,2,3,4,5),
                         probability = c(pred_rating_1, pred_rating_2, pred_rating_3, pred_rating_4, pred_rating_5))

  df.preds.sort = df.preds %>% arrange(desc(probability))
  df.preds.sort$weights = c(0.5,0.3,0.2,0,0)

  pred_rating <- as.numeric(df.preds.sort %>% summarise(sump=sum(rating*probability)))
  #pred_rating <- as.numeric(df.preds.sort %>% summarise(sump=sum(rating*weights)))
  return(pred_rating)
  
}


################################ ELO RATING #####################################

mov.out.mod <- function(i) {
  mov <- stat$MovieID[stat$UserID==i]
  mov.in.mod <- stat.cs$MovieID[stat.cs$UserID==i]
  mov.out.mod.us <- setdiff(mov, mov.in.mod)
  return(mov.out.mod.us)
}
mov.out.mod(2)[1]
base_exp(2, stat.cs, mod.cs)
avg.pred.rating(2, stat.cs, mod.cs)

index.movie.user <- function(us.id, mv.id){
  ind <- which(stat$UserID==us.id)[which(stat$MovieID[which(stat$UserID==us.id)]==mv.id)]
  return(ind)
}

#ELO Rating for user 2
#In which row is the first movie the user 2 rated that was not used in the initial likeability estimate?
index.movie.user(2, mov.out.mod(2)[2])
stat$Title[index.movie.user(2, mov.out.mod(2)[2])]

#Expected rating. Whats the predicted rating for the previous movie for that user?
E_Y <- avg.pred.rating(index.movie.user(2, mov.out.mod(2)[2]), stat.cs, mod.cs)
E_Y
#Real rating
Y.r <- stat$Rating[index.movie.user(2, mov.out.mod(2)[2])]
Y.r
#Ability level of user 2 at time 0
Theta_t.1 <- as.numeric(mod.cs$persons$UserID$`(Intercept)`[2])
Theta_t.1

#K step size

usid <- 2
K <- 0.05

Theta_t <- Theta_t.1 + K*(Y.r - E_Y)
Theta_t

mod.cs$persons$UserID$`(Intercept)`[2] <- Theta_t





ln.us <- length(stat$MovieID[stat$UserID==usid])

elo.rating.user <- data.frame(UserID = rep(0,ln.us),
                              MovieID = rep(0,ln.us),
                              Title = rep("",ln.us),
                              Rating = rep(0,ln.us),
                              PredRatingEIRT = rep(0,ln.us),
                              PredRatingELO = rep(0,ln.us),
                              UserLikeability = rep(0,ln.us),
                              stringsAsFactors = FALSE)


elo.rating.user2 <- elo.rating.user

mod.cs$persons$UserID$`(Intercept)`[usid]<-0

for(i in 1:10){
  elo.rating.user$UserID[i] <- usid
  elo.rating.user$MovieID[i] <- stat.cs$MovieID[stat.cs$UserID==usid][i]
  elo.rating.user$Title[i] <- stat.cs$Title[which(stat$UserID==usid & stat$MovieID==stat.cs$MovieID[stat.cs$UserID==usid][i])]
  elo.rating.user$Rating[i] <- stat.cs$Rating[which(stat$UserID==usid & stat$MovieID==stat.cs$MovieID[stat.cs$UserID==usid][i])]
  elo.rating.user$PredRatingEIRT[i] <- avg.pred.rating(index.movie.user(usid, mov.out.mod(usid)[i]), stat.cs, mod.cs)
  elo.rating.user$PredRatingELO[i] <- avg.pred.rating(index.movie.user(usid, mov.out.mod(usid)[i]), stat.cs, mod.cs)
  elo.rating.user$UserLikeability[i] <- mod.cs$persons$UserID$`(Intercept)`[usid]
}

#mod.cs$persons$UserID$`(Intercept)`[usid] <- 0

for(i in 11:ln.us){
  elo.rating.user$UserID[i] <- usid
  elo.rating.user$MovieID[i] <- mov.out.mod(usid)[i-10]
  elo.rating.user$Title[i] <- stat$Title[index.movie.user(usid, mov.out.mod(usid)[i-10])]
  print(elo.rating.user$Title[i])
  elo.rating.user$Rating[i] <- stat$Rating[index.movie.user(usid, mov.out.mod(usid)[i-10])]
  
  theta_temp <- mod.cs$persons$UserID$`(Intercept)`[usid]
  mod.cs$persons$UserID$`(Intercept)`[usid] <- 0
  elo.rating.user$PredRatingEIRT[i] <- avg.pred.rating(index.movie.user(usid, mov.out.mod(usid)[i-10]), stat, mod.cs)
  
  mod.cs$persons$UserID$`(Intercept)`[usid] <- theta_temp
  Theta_t.1 <- as.numeric(mod.cs$persons$UserID$`(Intercept)`[usid])
  Theta_t <- Theta_t.1 + K*(elo.rating.user$Rating[i] - elo.rating.user$PredRatingEIRT[i])
  mod.cs$persons$UserID$`(Intercept)`[usid] <- Theta_t
  elo.rating.user$UserLikeability[i] <- as.numeric(mod.cs$persons$UserID$`(Intercept)`[usid])
  
  elo.rating.user$PredRatingELO[i] <- avg.pred.rating(index.movie.user(usid, mov.out.mod(usid)[i-10]), stat, mod.cs)
}

elo.rating.user$index <- seq_along(elo.rating.user2$UserID)
index <- elo.rating.user$index

ggplot() +
  geom_line(aes(x=index, y=elo.rating.user$UserLikeability), size = 0.8) + theme_light() + 
  ggtitle("User 2 likeability timeline") + ylab("Likeability") + xlab("Iteration")


df.elo <- elo.rating.user %>%
  select(index, Rating, PredRatingEIRT, PredRatingELO) %>%
  gather(key = "variable", value = "value", -index)


ggplot(df.elo, aes(x = index, y = value)) + 
  geom_line(aes(color=variable), size = 0.8) + theme_light() + 
  ggtitle("Ratings for user 2") + ylab("Rating") + xlab("Iteration")

mod.cs$parameters

 

