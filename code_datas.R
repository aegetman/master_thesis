setwd("C:\\Users\\aleks\\Desktop\\MASTER_PAPERS\\Code\\H_V1") 

stat <- read.csv("ratings_complete.csv")

set.seed(12345)
stat2 <-  stat %>% filter(UserID <= 1000)

statUs <- stat %>% group_by(UserID) %>% summarise(CountUser=n())

statMv <- stat %>% group_by(MovieID) %>% summarise(CountMovie=n())

stat2 <- stat2 %>% inner_join(statUs)
stat2 <- stat2 %>% inner_join(statMv)

stat2$Enough<-ifelse(stat2$CountMovie>=20,1,0)
#Make a filtered dataset
stat3<-stat2[stat2$Enough != 0, ] 
stat3$CountMovie<-NULL
stat3$CountUser<-NULL
stat3$Enough<-NULL
statUs <- stat3 %>% group_by(UserID) %>% summarise(CountUser=n())
stat4 <- stat3 %>% inner_join(statUs)
stat4$Enough<-ifelse(stat4$CountUser>=20,1,0)
stat5<-stat4[stat4$Enough != 0, ] 
stat5$Enough<-NULL
stat5$CountUser<-NULL
statMv <- stat5 %>% group_by(MovieID) %>% summarise(CountMovie=n())
stat6 <- stat5 %>% inner_join(statMv)
stat6$Enough<-ifelse(stat6$CountMovie>=20,1,0)
stat7<-stat6[stat6$Enough != 0, ] 
stat7$CountMovie<-NULL
stat7$Enough<-NULL
statUs <- stat7 %>% group_by(UserID) %>% summarise(CountUser=n())
stat8 <- stat7 %>% inner_join(statUs)
stat8$Enough<-ifelse(stat8$CountUser>=20,1,0)
stat9<-stat8[stat8$Enough != 0, ] 
stat9$Enough<-NULL
stat9$CountUser<-NULL
statMv <- stat9 %>% group_by(MovieID) %>% summarise(CountMovie=n())
stat10 <- stat9 %>% inner_join(statMv)
stat10$Enough<-ifelse(stat10$CountMovie>=20,1,0)
stat11<-stat10[stat10$Enough != 0, ] 
stat11$CountMovie<-NULL
stat11$Enough<-NULL
statUs <- stat11 %>% group_by(UserID) %>% summarise(CountUser=n())
stat12 <- stat11 %>% inner_join(statUs)
statMv <- stat12 %>% group_by(MovieID) %>% summarise(CountMovie=n())
stat13 <- stat12 %>% inner_join(statMv)

#Adding new USER ID and ITEM ID
stat13<-stat13%>%arrange(MovieID, UserID)
statMv <- stat13 %>% group_by(MovieID) %>% summarise(CountMovie=n())
statMv$NewMovieID<-seq(1:1778)
stat13 <- stat13 %>% inner_join(statMv)
statUs <- stat13 %>% group_by(UserID) %>% summarise(CountUser=n())
statUs$NewUserID<-seq(1:980)
stat13 <- stat13 %>% inner_join(statUs)

write_csv(stat13, 
          path = paste0(getwd(),'/fill_dataset.csv'), 
          append = F, col_names = T)
################################################################################
stat2<-stat13
stat2.train <- stat2 %>% sample_frac(0.7)

statMv.train <- stat2.train %>% group_by(MovieID) %>% summarise(CountMovie=n())


stat2.test.universe  <- anti_join(stat2, stat2.train)

statMv.test <- stat2.test.universe %>% group_by(MovieID) %>% summarise(CountMovie=n())

stat2.test.universe$CondMov <- 0
for (i in 1:nrow(stat2.test.universe)) {
  stat2.test.universe$CondMov[i] <- ifelse(stat2.test.universe$MovieID[i] %in% statMv.train$MovieID, 1, 0)
  print(i)
}


stat2.test <- stat2.test.universe %>% filter(CondMov == 1)

testing<-stat2.test

testing$NewMovieID<-NULL
testing$NewUserID<-NULL
testing$CountMovie<-NULL
testing$CountUser<-NULL
testing$CondMov<-NULL

write_csv(testing, 
          path = paste0(getwd(),'/testing_V2.csv'), 
          append = F, col_names = T)
train<-stat2.train
train$NewMovieID<-NULL
train$NewUserID<-NULL
train$CountMovie<-NULL
train$CountUser<-NULL

write_csv(train, 
          path = paste0(getwd(),'/train_V2.csv'), 
          append = F, col_names = T)
