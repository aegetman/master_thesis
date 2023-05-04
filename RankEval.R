
library(dummy)
library(tidyverse)
library(lme4) 
library(lattice) 
library(ggpubr) 
library(dplyr) 
library(Rcpp)
library(eirm)
library(ggplot2)



#############################################################################33
setwd("C:\\Users\\anprez\\OneDrive - SAS\\Documents")
stat <- read.csv("ratings_complete.csv")
summary(stat)


################ sub-dataset grouped sampling by user (slice 10)#######################



stat.rk <- as.data.frame(stat %>% group_by(UserID) %>% slice_sample(n=10) %>% ungroup())


stat.rk$PredRt <- runif(nrow(stat.rk),3,5)
stat.rk$PredRt2 <- runif(nrow(stat.rk),3,5)


vrlt <- c('PredRt', 'PredRt2')

#Arguments
#df.pred: Dataset grouped by UserID containing ratings and predictions. 
#########It must contain variables UserID and Rating
#pred.var: List of variables containing predictions in quotations
#n.obs: Number of positively rated observations to average
ARC_calc <- function(df.pred, pred.vars, n.obs){
  
  df.pred$PosvRt <- ifelse(df.pred$Rating>=4,1,0)
  
  df.pred.test <- as.data.frame(df.pred %>% 
                                  group_by(PosvRt) %>% 
                                  slice_sample(n=n.obs) %>% 
                                  ungroup() %>% filter(PosvRt==1))
  
  ARC <- rep(0, length(pred.vars))
  i <- 1
  for(vari in pred.vars){
    df.pred <- df.pred %>% arrange(UserID, get(vari)) %>% 
      group_by(UserID) %>%
      mutate(rank = rank(get(vari)))
    
    df.pred.test <- df.pred.test %>% inner_join(df.pred, by = 'X')
    
    ARC[i] <- mean(df.pred.test$rank)
    
    df.pred.test <- df.pred.test %>% select(-rank)
    
    i <- i+1
  }
  return(ARC)
}


ARC_calc(stat.rk, vrlt, 1000)







