##March Madness - 2018 NCAA Division I men's Basketball Tournament
##Author: Haibin

##Data Preprocessing: feature selection and extraction
# -----------------------------------------------
# -----------------------------------------------

# Features list:
# -----------------------------------------------
##The number of players
# Teams with lower number tends to be more consistent. Consistency can be used as a
# measure of a team's in-season strength.

##Network analysis
# Network analysis component: confidence and performance under pressure, can be measure via a network approach.

##Weighted Wins *
# Weighted Wins: the dot product of a team's win against other NCAA Tournament teams and the inverse
# of those team's seeds. For instance, a team defeated a 15 seed, then the WW score would be 1/15.

##Experience *
# To quantify experience, specifically experience in the NCAA Tournament. Includes the team and the players contributed to the success.

##Common opponents *
# Average results of against the all common opponents for each pairs to predict

setwd("/Users/guanhaibin/desktop/kaggle/Google_NACC_Men")

##load packages
library(tidyverse)
library(dplyr)
library(caret)
library(magrittr)
library(data.table)
library(Rcpp)
library(randomForest)
library(RANN)
library(adabag)
library(plyr)


#read data
test <- read.csv("SampleSubmissionStage2.csv")
seeds<-read.csv("DataFiles/NCAATourneySeeds.csv",stringsAsFactors = FALSE)
teams <- read.csv("DataFiles/Teams.csv", stringsAsFactors = FALSE)
seasons<-read.csv("DataFiles/Seasons.csv",stringsAsFactors = FALSE)
game<-read.csv("DataFiles/GameCities.csv",stringsAsFactors = FALSE)


tourney_compact_result<- read.csv("DataFiles/NCAATourneyCompactResults.csv",stringsAsFactors = FALSE)
regular_compact <- read.csv("DataFiles/RegularSeasonCompactResults.csv", stringsAsFactors = FALSE)
secondary_compact <-read.csv("DataFiles/SecondaryTourneyCompactResults.csv", stringsAsFactors = FALSE)
conference_compact<-read.csv("DataFiles/ConferenceTourneyGames.csv",stringsAsFactors = FALSE)

tourney_stats <- read.csv("DataFiles/NCAATourneyDetailedResults.csv", stringsAsFactors = FALSE)
regular_stats <- read.csv("DataFiles/RegularSeasonDetailedResults.csv", stringsAsFactors = FALSE)
secondary_team<-read.csv("DataFiles/SecondaryTourneyTeams.csv",stringsAsFactors = FALSE)

coach<-read.csv("DataFiles/TeamCoaches.csv",stringsAsFactors = FALSE)
conference<-read.csv("DataFiles/TeamConferences.csv",stringsAsFactors = FALSE)
rank<-read.csv("MasseyOrdinals_thruSeason2018_Day128/MasseyOrdinals_thruSeason2018_Day128.csv",stringsAsFactors = FALSE)

#extract avg and sd rank for each team in each season
rank_mean<-rank%>%filter(RankingDayNum>=130) %>%dplyr::group_by(Season,TeamID,SystemName) %>% dplyr::summarise(rank_mean=mean(OrdinalRank))
rank_mean<-as.data.frame(rank_mean)
topsystem<-table(rank_mean$SystemName)[order(table(rank_mean$SystemName))][-140:-1]
xx<-c("WOL","SAG","RTH","MOR","WLK","POM","DOL","COL","RPI")
rank_mean<-subset(rank_mean,SystemName%in%xx)
team_rank<-rank_mean%>%dplyr::select(Season,TeamID)
team_rank<-unique(as.data.frame(team_rank)[c("Season","TeamID")])
team_rank<-cbind(team_rank, setNames( lapply(xx, function(x) x=NA), xx) )
for (i in 1:nrow(team_rank)){
  a<-rank_mean$rank_mean[which(rank_mean$Season==team_rank$Season[i]&rank_mean$TeamID==team_rank$TeamID[i]&rank_mean$SystemName=='WOL')]
  ifelse(length(a)!=0,team_rank$WOL[i]<-a,NA)
  a<-rank_mean$rank_mean[which(rank_mean$Season==team_rank$Season[i]&rank_mean$TeamID==team_rank$TeamID[i]&rank_mean$SystemName=='SAG')]
  ifelse(length(a)!=0,team_rank$SAG[i]<-a,NA)
  a<-rank_mean$rank_mean[which(rank_mean$Season==team_rank$Season[i]&rank_mean$TeamID==team_rank$TeamID[i]&rank_mean$SystemName=='RTH')]
  ifelse(length(a)!=0,team_rank$RTH[i]<-a,NA)
  a<-rank_mean$rank_mean[which(rank_mean$Season==team_rank$Season[i]&rank_mean$TeamID==team_rank$TeamID[i]&rank_mean$SystemName=='MOR')]
  ifelse(length(a)!=0,team_rank$MOR[i]<-a,NA)
  a<-rank_mean$rank_mean[which(rank_mean$Season==team_rank$Season[i]&rank_mean$TeamID==team_rank$TeamID[i]&rank_mean$SystemName=='WLK')]
  ifelse(length(a)!=0,team_rank$WLK[i]<-a,NA)
  a<-rank_mean$rank_mean[which(rank_mean$Season==team_rank$Season[i]&rank_mean$TeamID==team_rank$TeamID[i]&rank_mean$SystemName=='POM')]
  ifelse(length(a)!=0,team_rank$POM[i]<-a,NA)
  a<-rank_mean$rank_mean[which(rank_mean$Season==team_rank$Season[i]&rank_mean$TeamID==team_rank$TeamID[i]&rank_mean$SystemName=='DOL')]
  ifelse(length(a)!=0,team_rank$DOL[i]<-a,NA)
  a<-rank_mean$rank_mean[which(rank_mean$Season==team_rank$Season[i]&rank_mean$TeamID==team_rank$TeamID[i]&rank_mean$SystemName=='COL')]
  ifelse(length(a)!=0,team_rank$COL[i]<-a,NA)
  a<-rank_mean$rank_mean[which(rank_mean$Season==team_rank$Season[i]&rank_mean$TeamID==team_rank$TeamID[i]&rank_mean$SystemName=='RPI')]
  ifelse(length(a)!=0,team_rank$RPI[i]<-a,NA)
}

rank<-team_rank
#extraxt the seed number and region from seed
seeds$seed<-as.numeric(substr(seeds$Seed,2,3))
seeds$region<-as.character(substr(seeds$Seed,1,1))
seeds$Seed<-NULL

# Join seeds onto the results for team1 and team2
team1_seeds <- dplyr::rename(seeds,Season=Season, T1Seed=seed, Team1=TeamID,T1Region=region)
team2_seeds <- dplyr::rename(seeds,Season=Season, T2Seed=seed, Team2=TeamID,T2Region=region)

# process game so it matchs submission file without seeds (large portion games don't have seeds)
game<-mutate(game,team_id_diff = game$WTeamID - game$LTeamID,
             Team1 = case_when(team_id_diff < 0 ~ WTeamID,
                               team_id_diff > 0 ~ LTeamID),
             Team2 = case_when(team_id_diff > 0 ~ WTeamID,
                               team_id_diff < 0 ~ LTeamID),
             result = if_else(WTeamID == Team1, 1, -1))
game<-game[,c(1,8,9,2,10,5,6)]
team1_train<-merge(game, team1_seeds, by=c("Season", "Team1"),all.x=TRUE)
game_seed<-merge(team1_train, team2_seeds, by=c("Season", "Team2"),all.x=TRUE)
game_seed<-game_seed[,c(1,3,2,4:11)]
game<-na.omit(game_seed)
game<-arrange(game,Season,DayNum)


# process regular_compact so it matchs submission file without seeds (large portion games don't have seeds)
regular_compact$score<-regular_compact$WScore-regular_compact$LScore
regular_compact$score<-as.numeric(regular_compact$score)
regular_compact$WLoc[which(regular_compact$WLoc=='H')]=regular_compact$WTeamID[which(regular_compact$WLoc=='H')]
regular_compact$WLoc[which(regular_compact$WLoc=='A')]=regular_compact$LTeamID[which(regular_compact$WLoc=='A')]
regular_compact <- mutate(regular_compact,team_id_diff = regular_compact$WTeamID - regular_compact$LTeamID,
                          Team1 = case_when(team_id_diff < 0 ~ WTeamID,
                                            team_id_diff > 0 ~ LTeamID),
                          Team2 = case_when(team_id_diff > 0 ~ WTeamID,
                                            team_id_diff < 0 ~ LTeamID),
                          result = if_else(WTeamID == Team1, 1, -1),
                          score = if_else(WTeamID == Team1,score,(-1*score)))
regular_compact<-regular_compact[,c(1,2,7,8,9,11,12,13)]
regular_compact<-regular_compact[,c(1,6,7,2,3,8,5)]
#regular_compact_1<-filter(regular_compact,Team1%in%unique(seeds$TeamID),Team2%in%unique(seeds$TeamID))

team1_train<-merge(regular_compact, team1_seeds, by=c("Season", "Team1"),all.x=TRUE)
regular_compact_seed<-merge(team1_train, team2_seeds, by=c("Season", "Team2"),all.x=TRUE)
regular_compact_seed<-na.omit(regular_compact_seed)
regular_compact_seed<-regular_compact_seed[,c(1,3,2,4:11)]
regular_compact_seed<-arrange(regular_compact_seed,Season,DayNum)

# process secondary_compact so it matchs submission file without seeds (all games don't have seeds)
# secondary_compact$score<-secondary_compact$WScore-secondary_compact$LScore
# process conference_compact so it matchs submission file without seeds (large portion games don't have seeds)
conference_compact <- mutate(conference_compact,team_id_diff = conference_compact$WTeamID - conference_compact$LTeamID,
                             Team1 = case_when(team_id_diff < 0 ~ WTeamID,
                                               team_id_diff > 0 ~ LTeamID),
                             Team2 = case_when(team_id_diff > 0 ~ WTeamID,
                                               team_id_diff < 0 ~ LTeamID),
                             result = if_else(WTeamID == Team1, 1, -1))
conference_compact<-conference_compact[,c(1,7,8,3,2,9)]
#conference_compact<-filter(conference_compact,Team1%in%unique(seeds$TeamID),Team2%in%unique(seeds$TeamID))
team1_train<-merge(conference_compact, team1_seeds, by=c("Season", "Team1"),all.x=TRUE)
conference_compact_seed<-merge(team1_train, team2_seeds, by=c("Season", "Team2"),all.x=TRUE)
conference_compact_seed<-na.omit(conference_compact_seed)
conference_compact_seed<-conference_compact_seed[,c(1,3,2,4:10)]
conference_compact_seed<-arrange(conference_compact_seed,Season,DayNum)

# Keep only the needed files from the results; Season, WTeamID, and LTeamID
results <- dplyr::select(tourney_compact_result,Season, WTeamID, LTeamID,DayNum,WScore,LScore,WLoc)
results$score<-results$WScore-results$LScore
results$score<-as.numeric(results$score)
results$WLoc[which(results$WLoc=='H')]=results$WTeamID[which(results$WLoc=='H')]
results$WLoc[which(results$WLoc=='A')]=results$LTeamID[which(results$WLoc=='A')]
# Rearrange data so it matches submission file
results <- mutate(results,team_id_diff = results$WTeamID - results$LTeamID,
                  Team1 = case_when(team_id_diff < 0 ~ WTeamID,
                                    team_id_diff > 0 ~ LTeamID),
                  Team2 = case_when(team_id_diff > 0 ~ WTeamID,
                                    team_id_diff < 0 ~ LTeamID),
                  result = if_else(WTeamID == Team1, 1, -1),
                  score = if_else(WTeamID == Team1,score,(-1*score)))

# Remove WTeamID, LTeamID, and team_id_diff
results<-dplyr::select(results,1,10,11,4,7,8,12)

# Create Training Set
# Join seeds to training set
team1_train<-merge(results, team1_seeds, by=c("Season", "Team1"),all.x=TRUE)
train<-merge(team1_train, team2_seeds, by=c("Season", "Team2"),all.x=TRUE)
train<-dplyr::select(train,1,3,2,4,5,7,6,8:11)
tourney_compact<-train
tourney_compact<-arrange(tourney_compact,Season,DayNum)


# combine regular_compact_seed with tourney_compact
tourney_regular<-arrange(rbind(tourney_compact,regular_compact_seed),Season,DayNum)

for (i in 1:nrow(tourney_regular)){
  if (tourney_regular$WLoc[i]==tourney_regular$Team1[i]){
    tourney_regular$WLoc[i]="H"
  }else if(tourney_regular$WLoc[i]==tourney_regular$Team2[i]){
    tourney_regular$WLoc[i]="W"
  }else {
    tourney_regular$WLoc[i]="N"
  }
}

# create new data with test data
train<-tourney_regular
train$score<-NULL
train<-filter(train,Season>=2005)



#extract season and teams info from ID of test
test$Season<-as.numeric(substr(test$ID,1,4))
test$Team1<-as.numeric(substr(test$ID,6,9))
test$Team2<-as.numeric(substr(test$ID,11,14))
test$DayNum<-c(0)
test$WLoc<-c('N')

test$ID<-NULL
test$Pred<-NULL
# Join seeds to test set
team1_test<-merge(test, team1_seeds, by=c("Season", "Team1"),all.x=TRUE)
test<-merge(team1_test, team2_seeds, by=c("Season", "Team2"),all.x=TRUE)
test<-test[,c(1,3,2,4:9)]

#fulldata<-rbind(train,test)
#fulldata<-filter(fulldata,Season>=2010)
#fulldata<-arrange(fulldata,Season,DayNum)

##Summary of Extract Features
# -----------------------------------------------
# Feature1: Weighted wins of historical macthes of common opponents
# ---------------------------------------------------------------
train$CWW1_trc_result<-c(0)
train$CWW1_tr_score<-c(0)
train$CWW2_trc_result<-c(0)
train$CWW2_tr_score<-c(0)

#extract features from tourney_compact
test$CWW1_trc_result<-c(0)
test$CWW1_tr_score<-c(0)
test$CWW2_trc_result<-c(0)
test$CWW2_tr_score<-c(0)

for (i in 1:nrow(train)) {
  CWW1_1<-0
  CWW1_2<-0
  CWW2_1<-0
  CWW2_2<-0
  pre_season<-c(2000:train$Season[i]-1)
  t_1_1<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team1==train$Team1[i],tourney_compact$result==1)
  t_1_2<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team2==train$Team1[i],tourney_compact$result==-1)
  t_2_1<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team1==train$Team2[i],tourney_compact$result==1)
  t_2_2<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team2==train$Team2[i],tourney_compact$result==-1)
  r_1_1<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team1==train$Team1[i],regular_compact_seed$result==1)
  r_1_2<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team2==train$Team1[i],regular_compact_seed$result==-1)
  r_2_1<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team1==train$Team2[i],regular_compact_seed$result==1)
  r_2_2<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team2==train$Team2[i],regular_compact_seed$result==-1)
  c_1_1<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team1==train$Team1[i],conference_compact_seed$result==1)
  c_1_2<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team2==train$Team1[i],conference_compact_seed$result==-1)
  c_2_1<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team1==train$Team2[i],conference_compact_seed$result==1)
  c_2_2<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team2==train$Team2[i],conference_compact_seed$result==-1)
  
  common_component<-intersect(c(t_1_1$Team2,t_1_2$Team1),c(t_2_1$Team2,t_2_2$Team1))
  t_1_1<-filter(t_1_1,Team2%in%common_component)
  t_1_2<-filter(t_1_2,Team1%in%common_component)
  t_2_1<-filter(t_2_1,Team2%in%common_component)
  t_2_2<-filter(t_2_2,Team1%in%common_component)
  
  common_component<-intersect(c(r_1_1$Team2,r_1_2$Team1),c(r_2_1$Team2,r_2_2$Team1))
  r_1_1<-filter(r_1_1,Team2%in%common_component)
  r_1_2<-filter(r_1_2,Team1%in%common_component)
  r_2_1<-filter(r_2_1,Team2%in%common_component)
  r_2_2<-filter(r_2_2,Team1%in%common_component)
  
  common_component<-intersect(c(c_1_1$Team2,c_1_2$Team1),c(c_2_1$Team2,c_2_2$Team1))
  c_1_1<-filter(c_1_1,Team2%in%common_component)
  c_1_2<-filter(c_1_2,Team1%in%common_component)
  c_2_1<-filter(c_2_1,Team2%in%common_component)
  c_2_2<-filter(c_2_2,Team1%in%common_component)
  
  CWW1_1=sum(1/(t_1_1$T2Seed))+sum(1/(r_1_1$T2Seed))+sum(1/(c_1_1$T2Seed))
  CWW1_2=sum(1/(t_1_2$T1Seed))+sum(1/(r_1_2$T1Seed))+sum(1/(c_1_2$T1Seed))
  train$CWW1_trc_result[i]=(CWW1_1+CWW1_2)/(nrow(t_1_1)+nrow(t_1_2)+nrow(r_1_1)+nrow(r_1_2)+nrow(c_1_1)+nrow(c_1_2))
  train$CWW1_tr_score[i]=(sum(t_1_1$score)-sum(t_1_2$score)+sum(r_1_1$score)-sum(r_1_2$score))/(nrow(t_1_1)+nrow(t_1_2)+nrow(r_1_1)+nrow(r_1_2))
  
  CWW2_1=sum(1/(t_2_1$T2Seed))+sum(1/(r_2_1$T2Seed))+sum(1/(c_2_1$T2Seed))
  CWW2_2=sum(1/(t_2_2$T1Seed))+sum(1/(r_2_2$T1Seed))+sum(1/(c_2_2$T1Seed))
  train$CWW2_trc_result[i]=(CWW2_1+CWW2_2)/(nrow(t_2_1)+nrow(t_2_2)+nrow(r_2_1)+nrow(r_2_2)+nrow(c_2_1)+nrow(c_2_2))
  train$CWW2_tr_score[i]=(sum(t_2_1$score)-sum(t_2_2$score)+sum(r_2_1$score)-sum(r_2_2$score))/(nrow(t_2_1)+nrow(t_2_2)+nrow(r_2_1)+nrow(r_2_2))
  
}

##fill those whose CWW features are NA, we refill it with further history data


for (i in 1:nrow(test)) {
  CWW1_1<-0
  CWW1_2<-0
  CWW2_1<-0
  CWW2_2<-0
  pre_season<-c(2000:test$Season[i]-1)
  t_1_1<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team1==test$Team1[i],tourney_compact$result==1)
  t_1_2<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team2==test$Team1[i],tourney_compact$result==-1)
  t_2_1<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team1==test$Team2[i],tourney_compact$result==1)
  t_2_2<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team2==test$Team2[i],tourney_compact$result==-1)
  r_1_1<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team1==test$Team1[i],regular_compact_seed$result==1)
  r_1_2<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team2==test$Team1[i],regular_compact_seed$result==-1)
  r_2_1<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team1==test$Team2[i],regular_compact_seed$result==1)
  r_2_2<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team2==test$Team2[i],regular_compact_seed$result==-1)
  c_1_1<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team1==test$Team1[i],conference_compact_seed$result==1)
  c_1_2<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team2==test$Team1[i],conference_compact_seed$result==-1)
  c_2_1<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team1==test$Team2[i],conference_compact_seed$result==1)
  c_2_2<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team2==test$Team2[i],conference_compact_seed$result==-1)
  
  
  common_component<-intersect(c(t_1_1$Team2,t_1_2$Team1),c(t_2_1$Team2,t_2_2$Team1))
  t_1_1<-filter(t_1_1,Team2%in%common_component)
  t_1_2<-filter(t_1_2,Team1%in%common_component)
  t_2_1<-filter(t_2_1,Team2%in%common_component)
  t_2_2<-filter(t_2_2,Team1%in%common_component)
  
  common_component<-intersect(c(r_1_1$Team2,r_1_2$Team1),c(r_2_1$Team2,r_2_2$Team1))
  r_1_1<-filter(r_1_1,Team2%in%common_component)
  r_1_2<-filter(r_1_2,Team1%in%common_component)
  r_2_1<-filter(r_2_1,Team2%in%common_component)
  r_2_2<-filter(r_2_2,Team1%in%common_component)
  
  common_component<-intersect(c(c_1_1$Team2,c_1_2$Team1),c(c_2_1$Team2,c_2_2$Team1))
  c_1_1<-filter(c_1_1,Team2%in%common_component)
  c_1_2<-filter(c_1_2,Team1%in%common_component)
  c_2_1<-filter(c_2_1,Team2%in%common_component)
  c_2_2<-filter(c_2_2,Team1%in%common_component)
  
  CWW1_1=sum(1/(t_1_1$T2Seed))+sum(1/(r_1_1$T2Seed))+sum(1/(c_1_1$T2Seed))
  CWW1_2=sum(1/(t_1_2$T1Seed))+sum(1/(r_1_2$T1Seed))+sum(1/(c_1_2$T1Seed))
  test$CWW1_trc_result[i]=(CWW1_1+CWW1_2)/(nrow(t_1_1)+nrow(t_1_2)+nrow(r_1_1)+nrow(r_1_2)+nrow(c_1_1)+nrow(c_1_2))
  test$CWW1_tr_score[i]=(sum(t_1_1$score)-sum(t_1_2$score)+sum(r_1_1$score)-sum(r_1_2$score))/(nrow(t_1_1)+nrow(t_1_2)+nrow(r_1_1)+nrow(r_1_2))
  
  CWW2_1=sum(1/(t_2_1$T2Seed))+sum(1/(r_2_1$T2Seed))+sum(1/(c_2_1$T2Seed))
  CWW2_2=sum(1/(t_2_2$T1Seed))+sum(1/(r_2_2$T1Seed))+sum(1/(c_2_2$T1Seed))
  test$CWW2_trc_result[i]=(CWW2_1+CWW2_2)/(nrow(t_2_1)+nrow(t_2_2)+nrow(r_2_1)+nrow(r_2_2)+nrow(c_2_1)+nrow(c_2_2))
  test$CWW2_tr_score[i]=(sum(t_2_1$score)-sum(t_2_2$score)+sum(r_2_1$score)-sum(r_2_2$score))/(nrow(t_2_1)+nrow(t_2_2)+nrow(r_2_1)+nrow(r_2_2))
  
}


# -------------------------------------------------------------
# Feature2: History Weighted Wins of each team in Season (InSeason features would be applied for final stage (i.e stage 2))
#extract features from tourney_compact
train$WW1_trc_result<-c(0)
train$WW1_tr_score<-c(0)
train$WW2_trc_result<-c(0)
train$WW2_tr_score<-c(0)


#extract features from tourney_compact
test$WW1_trc_result<-c(0)
test$WW1_tr_score<-c(0)
test$WW2_trc_result<-c(0)
test$WW2_tr_score<-c(0)



for (i in 1:nrow(train)) {
  WW1_1<-0
  WW1_2<-0
  WW2_1<-0
  WW2_2<-0
  pre_season<-c(train$Season[i]-5:train$Season[i]-1)
  t_1_1<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team1==train$Team1[i],tourney_compact$result==1)
  t_1_2<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team2==train$Team1[i],tourney_compact$result==-1)
  t_2_1<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team1==train$Team2[i],tourney_compact$result==1)
  t_2_2<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team2==train$Team2[i],tourney_compact$result==-1)
  r_1_1<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team1==train$Team1[i],regular_compact_seed$result==1)
  r_1_2<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team2==train$Team1[i],regular_compact_seed$result==-1)
  r_2_1<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team1==train$Team2[i],regular_compact_seed$result==1)
  r_2_2<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team2==train$Team2[i],regular_compact_seed$result==-1)
  c_1_1<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team1==train$Team1[i],conference_compact_seed$result==1)
  c_1_2<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team2==train$Team1[i],conference_compact_seed$result==-1)
  c_2_1<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team1==train$Team2[i],conference_compact_seed$result==1)
  c_2_2<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team2==train$Team2[i],conference_compact_seed$result==-1)
  
  WW1_1=sum(1/(t_1_1$T2Seed))+sum(1/(r_1_1$T2Seed))+sum(1/(c_1_1$T2Seed))
  WW1_2=sum(1/(t_1_2$T1Seed))+sum(1/(r_1_2$T1Seed))+sum(1/(c_1_2$T1Seed))
  train$WW1_trc_result[i]=(WW1_1+WW1_2)/(nrow(t_1_1)+nrow(t_1_2)+nrow(r_1_1)+nrow(r_1_2)+nrow(c_1_1)+nrow(c_1_2))
  train$WW1_tr_score[i]=(sum(t_1_1$score)-sum(t_1_2$score)+sum(r_1_1$score)-sum(r_1_2$score))/(nrow(t_1_1)+nrow(t_1_2)+nrow(r_1_1)+nrow(r_1_2))
  
  WW2_1=sum(1/(t_2_1$T2Seed))+sum(1/(r_2_1$T2Seed))+sum(1/(c_2_1$T2Seed))
  WW2_2=sum(1/(t_2_2$T1Seed))+sum(1/(r_2_2$T1Seed))+sum(1/(c_2_2$T1Seed))
  train$WW2_trc_result[i]=(WW2_1+WW2_2)/(nrow(t_2_1)+nrow(t_2_2)+nrow(r_2_1)+nrow(r_2_2)+nrow(c_2_1)+nrow(c_2_2))
  train$WW2_tr_score[i]=(sum(t_2_1$score)-sum(t_2_2$score)+sum(r_2_1$score)-sum(r_2_2$score))/(nrow(t_2_1)+nrow(t_2_2)+nrow(r_2_1)+nrow(r_2_2))
  
}


for (i in 1:nrow(test)) {
  WW1_1<-0
  WW1_2<-0
  WW2_1<-0
  WW2_2<-0
  pre_season<-c(test$Season[i]-5:test$Season[i]-1)
  t_1_1<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team1==test$Team1[i],tourney_compact$result==1)
  t_1_2<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team2==test$Team1[i],tourney_compact$result==-1)
  t_2_1<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team1==test$Team2[i],tourney_compact$result==1)
  t_2_2<-filter(tourney_compact,tourney_compact$Season%in%pre_season,tourney_compact$Team2==test$Team2[i],tourney_compact$result==-1)
  r_1_1<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team1==test$Team1[i],regular_compact_seed$result==1)
  r_1_2<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team2==test$Team1[i],regular_compact_seed$result==-1)
  r_2_1<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team1==test$Team2[i],regular_compact_seed$result==1)
  r_2_2<-filter(regular_compact_seed,regular_compact_seed$Season%in%pre_season,regular_compact_seed$Team2==test$Team2[i],regular_compact_seed$result==-1)
  c_1_1<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team1==test$Team1[i],conference_compact_seed$result==1)
  c_1_2<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team2==test$Team1[i],conference_compact_seed$result==-1)
  c_2_1<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team1==test$Team2[i],conference_compact_seed$result==1)
  c_2_2<-filter(conference_compact_seed,conference_compact_seed$Season%in%pre_season,conference_compact_seed$Team2==test$Team2[i],conference_compact_seed$result==-1)
  
  WW1_1=sum(1/(t_1_1$T2Seed))+sum(1/(r_1_1$T2Seed))+sum(1/(c_1_1$T2Seed))
  WW1_2=sum(1/(t_1_2$T1Seed))+sum(1/(r_1_2$T1Seed))+sum(1/(c_1_2$T1Seed))
  test$WW1_trc_result[i]=(WW1_1+WW1_2)/(nrow(t_1_1)+nrow(t_1_2)+nrow(r_1_1)+nrow(r_1_2)+nrow(c_1_1)+nrow(c_1_2))
  test$WW1_tr_score[i]=(sum(t_1_1$score)-sum(t_1_2$score)+sum(r_1_1$score)-sum(r_1_2$score))/(nrow(t_1_1)+nrow(t_1_2)+nrow(r_1_1)+nrow(r_1_2))
  
  WW2_1=sum(1/(t_2_1$T2Seed))+sum(1/(r_2_1$T2Seed))+sum(1/(c_2_1$T2Seed))
  WW2_2=sum(1/(t_2_2$T1Seed))+sum(1/(r_2_2$T1Seed))+sum(1/(c_2_2$T1Seed))
  test$WW2_trc_result[i]=(WW2_1+WW2_2)/(nrow(t_2_1)+nrow(t_2_2)+nrow(r_2_1)+nrow(r_2_2)+nrow(c_2_1)+nrow(c_2_2))
  test$WW2_tr_score[i]=(sum(t_2_1$score)-sum(t_2_2$score)+sum(r_2_1$score)-sum(r_2_2$score))/(nrow(t_2_1)+nrow(t_2_2)+nrow(r_2_1)+nrow(r_2_2))
  
}


# Feature3: Historical Match results between pairs
# --------------------------------------------------------------
#fulldata$r_avg_result<-c(0)
#fulldata$r_avg_score<-c(0)
#fulldata$s_avg_result<-c(0)
#fulldata$s_avg_score<-c(0)
#fulldata$c_avg_result<-c(0)

#for (i in 1:nrow(fulldata)) {
#  pre_season<-c(1985:fulldata$Season[i])
# r<-filter(regular_compact,regular_compact$Season%in%pre_season,regular_compact$Team1==fulldata$Team1[i],regular_compact$Team2==fulldata$Team2[i])
#  s<-filter(secondary_compact,secondary_compact$Season%in%pre_season,secondary_compact$Team1==fulldata$Team1[i],secondary_compact$Team2==fulldata$Team2[i])
#  c<-filter(conference_compact,conference_compact$Season%in%pre_season,conference_compact$Team1==fulldata$Team1[i],conference_compact$Team2==fulldata$Team2[i])
#  if (nrow(r)!=0){
#    fulldata$r_avg_result[i]=mean(r$result)
#    fulldata$r_avg_score[i]=mean(r$score)
#  }
#  if (nrow(s)!=0){
#    fulldata$s_avg_result[i]=mean(s$result)
#    fulldata$s_avg_score[i]=mean(s$score)
#  }
#  if (nrow(c)!=0){
#    fulldata$c_avg_result[i]=mean(c$result)
#  }
#}
# -----------------------------------------------------------
# Feature4: History Seeds, pre1_1_seed, pre1_2_seed,pre1_3_seed, pre1_4_seed,pre1_5_seed,
# pre2_1_seed, pre2_2_seed,pre2_3_seed,pre2_4_seed,pre2_5_seed

train$pre1_1_seed<-c(0)
train$pre1_2_seed<-c(0)
train$pre1_3_seed<-c(0)


train$pre2_1_seed<-c(0)
train$pre2_2_seed<-c(0)
train$pre2_3_seed<-c(0)


test$pre1_1_seed<-c(0)
test$pre1_2_seed<-c(0)
test$pre1_3_seed<-c(0)


test$pre2_1_seed<-c(0)
test$pre2_2_seed<-c(0)
test$pre2_3_seed<-c(0)


for (i in 1:nrow(train)){
  for (j in 1:3) {
    k<-filter(team1_seeds,Season==train$Season[i]-j,Team1==train$Team1[i])
    if (nrow(k)==0 ){
      train[i,(which(colnames(train)==paste("pre1_",j,"_seed",sep='')))]<-0
    }
    else {
      train[i,(which(colnames(train)==paste("pre1_",j,"_seed",sep='')))]<-k$T1Seed
    }
    q<-filter(team1_seeds,Season==train$Season[i]-j,Team1==train$Team2[i])
    if (nrow(q)==0 ){
      train[i,(which(colnames(train)==paste("pre2_",j,"_seed",sep='')))]<-0
    }
    else {
      train[i,(which(colnames(train)==paste("pre2_",j,"_seed",sep='')))]<-q$T1Seed
    }
  }
}
for (i in 1:nrow(test)){
  for (j in 1:3) {
    k<-filter(team1_seeds,Season==test$Season[i]-j,Team1==test$Team1[i])
    if (nrow(k)==0 ){
      test[i,(which(colnames(test)==paste("pre1_",j,"_seed",sep='')))]<-0
    }
    else {
      test[i,(which(colnames(test)==paste("pre1_",j,"_seed",sep='')))]<-k$T1Seed
    }
    q<-filter(team1_seeds,Season==test$Season[i]-j,Team1==test$Team2[i])
    if (nrow(q)==0 ){
      test[i,(which(colnames(test)==paste("pre2_",j,"_seed",sep='')))]<-0
    }
    else {
      test[i,(which(colnames(test)==paste("pre2_",j,"_seed",sep='')))]<-q$T1Seed
    }
  }
}

# Feature5: Conference

team1_confe<-dplyr::rename(conference,Season=Season,Team1=TeamID,Team1_confe=ConfAbbrev)
team2_confe<-dplyr::rename(conference,Season=Season,Team2=TeamID,Team2_confe=ConfAbbrev)

team1_train<-merge(train, team1_confe, by=c("Season", "Team1"),all.x=TRUE)
train<-merge(team1_train, team2_confe, by=c("Season", "Team2"),all.x=TRUE)

team1_test<-merge(test, team1_confe, by=c("Season", "Team1"),all.x=TRUE)
test<-merge(team1_test, team2_confe, by=c("Season", "Team2"),all.x=TRUE)

train$Team1_Win_confe<-c(0)
train$Team2_Win_confe<-c(0)
test$Team1_Win_confe<-c(0)
test$Team2_Win_confe<-c(0)

Win_confe<-read.csv("DataFiles/ConferenceTourneyGames.csv",stringsAsFactors = FALSE)

for (i in 1:nrow(train)){
  k<-filter(Win_confe,Season==train$Season[i],WTeamID==train$Team1[i],ConfAbbrev==train$Team1_confe[i])
  if (nrow(k)==0 ) {
    train$Team1_Win_confe[i]<-0
  }
  else {
    train$Team1_Win_confe[i]<-nrow(k)
  }
  q<-filter(Win_confe,Season==train$Season[i],WTeamID==train$Team2[i],ConfAbbrev==train$Team2_confe[i])
  if (nrow(q)==0 ) {
    train$Team2_Win_confe[i]<-0
  }
  else {
    train$Team2_Win_confe[i]<-nrow(q)
  }
}

for (i in 1:nrow(test)){
  k<-filter(Win_confe,Season==test$Season[i],WTeamID==test$Team1[i],ConfAbbrev==test$Team1_confe[i])
  if (nrow(k)==0 ){
    test$Team1_Win_confe[i]<-0
  }
  else {
    test$Team1_Win_confe[i]<-nrow(k)
  }
  q<-filter(Win_confe,Season==test$Season[i],WTeamID==test$Team2[i],ConfAbbrev==test$Team2_confe[i])
  if (nrow(q)==0 ){
    test$Team2_Win_confe[i]<-0
  }
  else {
    test$Team2_Win_confe[i]<-nrow(q)
  }
}

# Feature7: Coach (average win ratio)
# Feature8: last match detail performance

# -------------------------------
# reprocess
team1_rank<-dplyr::rename(rank,Season=Season,Team1=TeamID,T1WOL=WOL,T1SAG=SAG,T1RTH=RTH,T1MOR=MOR,T1WLK=WLK,T1POM=POM,T1DOL=DOL,T1COL=COL,T1RPI=RPI)
team2_rank<-dplyr::rename(rank,Season=Season,Team2=TeamID,T2WOL=WOL,T2SAG=SAG,T2RTH=RTH,T2MOR=MOR,T2WLK=WLK,T2POM=POM,T2DOL=DOL,T2COL=COL,T2RPI=RPI)
team1_train<-merge(train, team1_rank, by=c("Season", "Team1"),all.x=TRUE)
train<-merge(team1_train, team2_rank, by=c("Season", "Team2"),all.x=TRUE)



train_orginal<-train
train<-train%>%unite(ID,Season,Team1,Team2,sep="_")
train$DayNum<-NULL
train$seed_diff<-train$T1Seed-train$T2Seed

train<-train[,c(1,2,5,7,8:44,3)]


team1_test<-merge(test, team1_rank, by=c("Season", "Team1"),all.x=TRUE)
test<-merge(team1_test, team2_rank, by=c("Season", "Team2"),all.x=TRUE)

test_orginal<-test
test<-test%>%unite(ID,Season,Team1,Team2,sep="_")
test$DayNum<-NULL
test$seed_diff<-test$T1Seed-test$T2Seed
test<-test[,c(1,2,4,6:43)]


## Machine learning methods
# --------------------------------------------------
# --------------------------------------------------
## Classifier 1
# --------------------------------------------------

train_set1<-filter(train,CWW1_trc_result!=0,CWW2_trc_result!=0,WW1_tr_score!=0,WW2_tr_score!=0)
train_set1$CWW_result<-train_set1$CWW1_trc_result-train_set1$CWW2_trc_result
train_set1$CWW_score<-train_set1$CWW1_tr_score-train_set1$CWW2_tr_score
train_set1$WW_result<-train_set1$WW1_trc_result-train_set1$WW2_trc_result
train_set1$WW_score<-train_set1$WW1_tr_score-train_set1$WW2_tr_score
train_set1<-train_set1[,c(1:4,19:41,43:46,42)]


train_set1$T1Region<-as.factor(train_set1$T1Region)
train_set1$T2Region<-as.factor(train_set1$T2Region)
train_set1$WLoc<-as.factor(train_set1$WLoc)
train_set1$Team1_confe<-as.factor(train_set1$Team1_confe)
train_set1$Team2_confe<-as.factor(train_set1$Team2_confe)
train_set1$result<-as.factor(train_set1$result)

# Imputing missing values using KNN. Also centering and scaling numerial columns
preProcValues_set1<- preProcess(train_set1, method = c("knnImpute","center","scale"))

train_processed<-predict(preProcValues_set1,train_set1)

train_processed$result<-ifelse(train_processed$result=="-1",0,1)

id<-train_processed$ID
train_processed$ID<-NULL
dmy<-dummyVars("~.",data=train_processed)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))
train_transformed$result<-as.factor(train_transformed$result)
train_set1<-train_transformed


train_data<-train_set1
train.rows<-createDataPartition(y=train_data$result,p=0.7,list=FALSE)
train_set1<-train_data[train.rows,]
blender_set1<-train_data[-train.rows,]


test_set1<-filter(test,CWW1_trc_result!=0,CWW2_trc_result!=0,WW1_tr_score!=0,WW2_tr_score!=0)
test_set1$CWW_result<-test_set1$CWW1_trc_result-test_set1$CWW2_trc_result
test_set1$CWW_score<-test_set1$CWW1_tr_score-test_set1$CWW2_tr_score
test_set1$WW_result<-test_set1$WW1_trc_result-test_set1$WW2_trc_result
test_set1$WW_score<-test_set1$WW1_tr_score-test_set1$WW2_tr_score
test_set1$WLOc.H<-c(0)
test_set1$WLoc.N<-c(1)
test_set1$WLoc.W<-c(0)

test_set1<-test_set1[,c(1,46:48,3:4,19:45)]

test_set1$T1Region<-as.factor(test_set1$T1Region)
test_set1$T2Region<-as.factor(test_set1$T2Region)
test_set1$Team1_confe<-as.factor(test_set1$Team1_confe)
test_set1$Team2_confe<-as.factor(test_set1$Team2_confe)

test_processed<-predict(preProcValues_set1,test_set1)
test_set1_id<-test_processed$ID
test_processed$ID<-NULL

dmy<-dummyVars("~.",data=test_processed)
test_transformed <- data.frame(predict(dmy, newdata = test_processed))
test_set1<-test_transformed


outcomeName<-'result'
predictors<-intersect(names(test_set1),names(train_set1))

##feature selection using rfee in caret
fitControl <- trainControl(
  method = "repeatedcv",
  classProbs = TRUE,
  number = 5,
  repeats = 5,
  verboseIter = TRUE)
nn_grid<-expand.grid(size=c(20,15,7,3),decay=0.00147)

feature.names=names(test_set1)
for (f in feature.names) {
  if (class(test_set1[[f]])=="factor") {
    levels <- unique(c(test_set1[[f]]))
    test_set1[[f]] <- factor(test_set1[[f]],
                             labels=make.names(levels))
  }
}
feature.names=names(train_set1)
for (f in feature.names) {
  if (class(train_set1[[f]])=="factor") {
    levels <- unique(c(train_set1[[f]]))
    train_set1[[f]] <- factor(train_set1[[f]],
                              labels=make.names(levels))
  }
}
feature.names=names(blender_set1)
for (f in feature.names) {
  if (class(blender_set1[[f]])=="factor") {
    levels <- unique(c(blender_set1[[f]]))
    blender_set1[[f]] <- factor(blender_set1[[f]],
                                labels=make.names(levels))
  }
}


model_gbm_1<-train(train_set1[,predictors],train_set1[,outcomeName],method='gbm',trControl=fitControl)
model_rf_1<-train(train_set1[,predictors],train_set1[,outcomeName],method='rf',trControl=fitControl)
model_nn_1<-train(train_set1[,predictors],train_set1[,outcomeName],method='nnet',trControl=fitControl,tuneGrid=nn_grid)
model_treebag_1 <- train(train_set1[,predictors],train_set1[,outcomeName], method='treebag', trControl=fitControl)
model_svm_1<- train(train_set1[,predictors],train_set1[,outcomeName], method='svmLinearWeights2', trControl=fitControl)
model_xgbTree_1<-train(train_set1[,predictors],train_set1[,outcomeName], method='xgbTree', trControl=fitControl)
model_avNNet_1<-train(train_set1[,predictors],train_set1[,outcomeName], method='avNNet', trControl=fitControl)
model_regLogistic_1<-train(train_set1[,predictors],train_set1[,outcomeName], method='regLogistic', trControl=fitControl)
model_kknn_1<-train(train_set1[,predictors],train_set1[,outcomeName], method='kknn', trControl=fitControl)
model_dwdRadial_1<-train(train_set1[,predictors],train_set1[,outcomeName], method='dwdRadial', trControl=fitControl)
model_C5_1<-train(train_set1[,predictors],train_set1[,outcomeName], method='C5.0', trControl=fitControl)



blender_set1$gbm<-predict.train(object=model_gbm_1,blender_set1[,predictors],type="prob")[,2]
blender_set1$rf<-predict.train(object=model_rf_1,blender_set1[,predictors],type="prob")[,2]
blender_set1$nn<-predict.train(object=model_nn_1,blender_set1[,predictors],type="prob")[,2]
blender_set1$treebag<-predict.train(object=model_treebag_1,blender_set1[,predictors],type="prob")[,2]
blender_set1$svm<-predict.train(object=model_svm_1,blender_set1[,predictors],type="raw")
blender_set1$xgbTree<-predict.train(object=model_xgbTree_1,blender_set1[,predictors],type="prob")[,2]
blender_set1$avNNet<-predict.train(object=model_avNNet_1,blender_set1[,predictors],type="prob")[,2]
blender_set1$regLogistic<-predict.train(object=model_regLogistic_1,blender_set1[,predictors],type="prob")[,2]
blender_set1$kknn<-predict.train(object=model_kknn_1,blender_set1[,predictors],type="prob")[,2]
blender_set1$dwdRadial<-predict.train(object=model_dwdRadial_1,blender_set1[,predictors],type="prob")[,2]
blender_set1$C5<-predict.train(object=model_C5_1,blender_set1[,predictors],type="prob")[,2]


test_set1$gbm<-predict.train(object=model_gbm_1,test_set1[,predictors],type="prob")[,2]
test_set1$rf<-predict.train(object=model_rf_1,test_set1[,predictors],type="prob")[,2]
test_set1$nn<-predict.train(object=model_nn_1,test_set1[,predictors],type="prob")[,2]
test_set1$treebag<-predict.train(object=model_treebag_1,test_set1[,predictors],type="prob")[,2]
test_set1$svm<-predict.train(object=model_svm_1,test_set1[,predictors],type="raw")
test_set1$xgbTree<-predict.train(object=model_xgbTree_1,test_set1[,predictors],type="prob")[,2]
test_set1$avNNet<-predict.train(object=model_avNNet_1,test_set1[,predictors],type="prob")[,2]
test_set1$regLogistic<-predict.train(object=model_regLogistic_1,test_set1[,predictors],type="prob")[,2]
test_set1$kknn<-predict.train(object=model_kknn_1,test_set1[,predictors],type="prob")[,2]
test_set1$dwdRadial<-predict.train(object=model_dwdRadial_1,test_set1[,predictors],type="prob")[,2]
test_set1$C5<-predict.train(object=model_C5_1,test_set1[,predictors],type="prob")[,2]



final_predictors<-intersect(names(blender_set1)[names(blender_set1)!=outcomeName],names(test_set1))
final_blender_model_1<-train(blender_set1[,final_predictors],blender_set1[,outcomeName],method='avNNet',trControl=fitControl)

test_set1$pred<-predict.train(object=final_blender_model_1,test_set1[,final_predictors],type="prob")[,2]
test_set1_id_result<-cbind(test_set1_id,test_set1$pred)

save(model_nn_1,model_rf_1,model_gbm_1,model_treebag_1,model_svm_1,model_xgbTree_1,final_blender_model_1,
     preProcValues_set1,test_orginal,test,train_orginal,train_set1,test_set1,test_set1_id_result,file="model_1.Rd")


# Method 5: Bayer's Naive

##Classifier 2
# --------------------------------------------------

train_set2<-filter(train,pre1_1_seed!=0,pre1_2_seed!=0,pre1_3_seed!=0,pre2_1_seed!=0,pre2_2_seed!=0,pre2_3_seed!=0)
train_set2<-filter(train_set2,CWW1_trc_result!=0,CWW2_trc_result!=0,WW1_trc_result!=0,WW2_trc_result!=0)
train_set2$CWW_result<-train_set2$CWW1_trc_result-train_set2$CWW2_trc_result
train_set2$CWW_score<-train_set2$CWW1_tr_score-train_set2$CWW2_tr_score
train_set2$WW_result<-train_set2$WW1_trc_result-train_set2$WW2_trc_result
train_set2$WW_score<-train_set2$WW1_tr_score-train_set2$WW2_tr_score
train_set2$seed_diff_1<-train_set2$pre1_1_seed-train_set2$pre2_1_seed
train_set2$seed_diff_2<-train_set2$pre1_2_seed-train_set2$pre2_2_seed
train_set2$seed_diff_3<-train_set2$pre1_3_seed-train_set2$pre2_3_seed
train_set2<-train_set2[,c(1:4,19:41,43:49,42)]


train_set2$T1Region<-as.factor(train_set2$T1Region)
train_set2$T2Region<-as.factor(train_set2$T2Region)
train_set2$WLoc<-as.factor(train_set2$WLoc)
train_set2$Team1_confe<-as.factor(train_set2$Team1_confe)
train_set2$Team2_confe<-as.factor(train_set2$Team2_confe)
train_set2$result<-as.factor(train_set2$result)


preProcValues_set2 <- preProcess(train_set2, method = c("knnImpute","center","scale"))
train_processed<-predict(preProcValues_set2,train_set2)
train_processed$result<-ifelse(train_processed$result=="-1",0,1)
id<-train_processed$ID
train_processed$ID<-NULL
dmy<-dummyVars("~.",data=train_processed)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))
train_transformed$result<-as.factor(train_transformed$result)
train_set2<-train_transformed


train_data<-train_set2
train.rows<-createDataPartition(y=train_data$result,p=0.7,list=FALSE)
train_set2<-train_data[train.rows,]
blender_set2<-train_data[-train.rows,]



# ------test-set2--------------

test_set2<-filter(test,pre1_1_seed!=0,pre1_2_seed!=0,pre1_3_seed!=0,pre2_1_seed!=0,pre2_2_seed!=0,pre2_3_seed!=0)
test_set2<-filter(test_set2,CWW1_trc_result!=0,CWW2_trc_result!=0,WW1_trc_result!=0,WW2_trc_result!=0)
test_set2$CWW_result<-test_set2$CWW1_trc_result-test_set2$CWW2_trc_result
test_set2$CWW_score<-test_set2$CWW1_tr_score-test_set2$CWW2_tr_score
test_set2$WW_result<-test_set2$WW1_trc_result-test_set2$WW2_trc_result
test_set2$WW_score<-test_set2$WW1_tr_score-test_set2$WW2_tr_score
test_set2$seed_diff_1<-test_set2$pre1_1_seed-test_set2$pre2_1_seed
test_set2$seed_diff_2<-test_set2$pre1_2_seed-test_set2$pre2_2_seed
test_set2$seed_diff_3<-test_set2$pre1_3_seed-test_set2$pre2_3_seed

test_set2$WLOc.H<-c(0)
test_set2$WLoc.N<-c(1)
test_set2$WLoc.W<-c(0)

test_set2<-test_set2[,c(1,49:51,3:4,19:48)]


test_set2$T1Region<-as.factor(test_set2$T1Region)
test_set2$T2Region<-as.factor(test_set2$T2Region)
test_set2$Team1_confe<-as.factor(test_set2$Team1_confe)
test_set2$Team2_confe<-as.factor(test_set2$Team2_confe)


test_processed<-predict(preProcValues_set2,test_set2)
test_set2_id<-test_processed$ID
test_processed$ID<-NULL

dmy<-dummyVars("~.",data=test_processed)
test_transformed <- data.frame(predict(dmy, newdata = test_processed))
test_set2<-test_transformed


outcomeName<-'result'
predictors<-intersect(names(test_set2),names(train_set2))


##feature selection using rfee in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 5)


fitControl <- trainControl(
  method = "repeatedcv",
  classProbs = TRUE,
  number = 5,
  repeats = 5,
  verboseIter = TRUE)

rf_grid=expand.grid(mtry=c(8,5,2,1))
nn_grid<-expand.grid(size=c(10,7,3),decay=0.00147)
gbm_grid <- expand.grid(n.trees=c(5,10,20,50),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))

feature.names=names(test_set2)
for (f in feature.names) {
  if (class(test_set2[[f]])=="factor") {
    levels <- unique(c(test_set2[[f]]))
    test_set2[[f]] <- factor(test_set2[[f]],
                             labels=make.names(levels))
  }
}
feature.names=names(train_set2)
for (f in feature.names) {
  if (class(train_set2[[f]])=="factor") {
    levels <- unique(c(train_set2[[f]]))
    train_set2[[f]] <- factor(train_set2[[f]],
                              labels=make.names(levels))
  }
}
feature.names=names(blender_set2)
for (f in feature.names) {
  if (class(blender_set2[[f]])=="factor") {
    levels <- unique(c(blender_set2[[f]]))
    blender_set2[[f]] <- factor(blender_set2[[f]],
                                labels=make.names(levels))
  }
}



model_gbm_2<-train(train_set2[,predictors],train_set2[,outcomeName],method='gbm',trControl=fitControl)
model_rf_2<-train(train_set2[,predictors],train_set2[,outcomeName],method='rf',trControl=fitControl)
model_nn_2<-train(train_set2[,predictors],train_set2[,outcomeName],method='nnet',trControl=fitControl,tuneGrid=nn_grid)
model_treebag_2 <- train(train_set2[,predictors],train_set2[,outcomeName], method='treebag', trControl=fitControl)
model_svm_2<- train(train_set2[,predictors],train_set2[,outcomeName], method='svmLinearWeights2', trControl=fitControl)
model_xgbTree_2<-train(train_set2[,predictors],train_set2[,outcomeName], method='xgbTree', trControl=fitControl)
model_avNNet_2<-train(train_set2[,predictors],train_set2[,outcomeName], method='avNNet', trControl=fitControl)
model_regLogistic_2<-train(train_set2[,predictors],train_set2[,outcomeName], method='regLogistic', trControl=fitControl)
model_kknn_2<-train(train_set2[,predictors],train_set2[,outcomeName], method='kknn', trControl=fitControl)
model_dwdRadial_2<-train(train_set2[,predictors],train_set2[,outcomeName], method='dwdRadial', trControl=fitControl)
model_C5_2<-train(train_set2[,predictors],train_set2[,outcomeName], method='C5.0', trControl=fitControl)



blender_set2$gbm<-predict.train(object=model_gbm_2,blender_set2[,predictors],type="prob")[,2]
blender_set2$rf<-predict.train(object=model_rf_2,blender_set2[,predictors],type="prob")[,2]
blender_set2$nn<-predict.train(object=model_nn_2,blender_set2[,predictors],type="prob")[,2]
blender_set2$treebag<-predict.train(object=model_treebag_2,blender_set2[,predictors],type="prob")[,2]
blender_set2$svm<-predict.train(object=model_svm_2,blender_set2[,predictors],type="raw")
blender_set2$xgbTree<-predict.train(object=model_xgbTree_2,blender_set2[,predictors],type="prob")[,2]
blender_set2$avNNet<-predict.train(object=model_avNNet_2,blender_set2[,predictors],type="prob")[,2]
blender_set2$regLogistic<-predict.train(object=model_regLogistic_2,blender_set2[,predictors],type="prob")[,2]
blender_set2$kknn<-predict.train(object=model_kknn_2,blender_set2[,predictors],type="prob")[,2]
blender_set2$dwdRadial<-predict.train(object=model_dwdRadial_2,blender_set2[,predictors],type="prob")[,2]
blender_set2$C5<-predict.train(object=model_C5_2,blender_set2[,predictors],type="prob")[,2]


test_set2$gbm<-predict.train(object=model_gbm_2,test_set2[,predictors],type="prob")[,2]
test_set2$rf<-predict.train(object=model_rf_2,test_set2[,predictors],type="prob")[,2]
test_set2$nn<-predict.train(object=model_nn_2,test_set2[,predictors],type="prob")[,2]
test_set2$treebag<-predict.train(object=model_treebag_2,test_set2[,predictors],type="prob")[,2]
test_set2$svm<-predict.train(object=model_svm_2,test_set2[,predictors],type="raw")
test_set2$xgbTree<-predict.train(object=model_xgbTree_2,test_set2[,predictors],type="prob")[,2]
test_set2$avNNet<-predict.train(object=model_avNNet_2,test_set2[,predictors],type="prob")[,2]
test_set2$regLogistic<-predict.train(object=model_regLogistic_2,test_set2[,predictors],type="prob")[,2]
test_set2$kknn<-predict.train(object=model_kknn_2,test_set2[,predictors],type="prob")[,2]
test_set2$dwdRadial<-predict.train(object=model_dwdRadial_2,test_set2[,predictors],type="prob")[,2]
test_set2$C5<-predict.train(object=model_C5_2,test_set2[,predictors],type="prob")[,2]



final_predictors<-intersect(names(blender_set2),names(test_set2))
final_blender_model_2<-train(blender_set2[,final_predictors],blender_set2[,outcomeName],method='avNNet',trControl=fitControl)

test_set2$pred<-predict(object=final_blender_model_2,test_set2[,final_predictors],type="prob")[,2]

test_set2_id_result<-cbind(test_set2_id,test_set2$pred)

save(model_nn_2,model_rf_2,model_gbm_2,model_treebag_2,model_svm_2,model_xgbTree_2,final_blender_model_2,
     preProcValues_set2,train_set2,test_set2,test_set2_id_result,file="model_2.Rd")




## Classifier 3
# ---------------------------------------------------
train_set3<-filter(train,WW1_trc_result!=0,WW1_tr_score!=0,WW2_trc_result!=0,WW2_tr_score!=0)
train_set3$WW_result<-train_set3$WW1_trc_result-train_set3$WW2_trc_result
train_set3$WW_score<-train_set3$WW1_tr_score-train_set3$WW2_tr_score
train_set3<-train_set3[,c(1:4,19:41,43:44,42)]

train_set3$T1Region<-as.factor(train_set3$T1Region)
train_set3$T2Region<-as.factor(train_set3$T2Region)
train_set3$WLoc<-as.factor(train_set3$WLoc)
train_set3$Team1_confe<-as.factor(train_set3$Team1_confe)
train_set3$Team2_confe<-as.factor(train_set3$Team2_confe)
train_set3$result<-as.factor(train_set3$result)


preProcValues_set3 <- preProcess(train_set3, method = c("knnImpute","center","scale"))
train_processed<-predict(preProcValues_set3,train_set3)
train_processed$result<-ifelse(train_processed$result=="-1",0,1)
id<-train_processed$ID
train_processed$ID<-NULL
dmy<-dummyVars("~.",data=train_processed)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))
train_transformed$result<-as.factor(train_transformed$result)
train_set3<-train_transformed


train_data<-train_set3
train.rows<-createDataPartition(y=train_data$result,p=0.85,list=FALSE)
train_set3<-train_data[train.rows,]
blender_set3<-train_data[-train.rows,]



#-------test_set3-------------

test_set3<-filter(test,WW1_trc_result!=0,WW1_tr_score!=0,WW2_trc_result!=0,WW2_tr_score!=0)
test_set3$WW_result<-test_set3$WW1_trc_result-test_set3$WW2_trc_result
test_set3$WW_score<-test_set3$WW1_tr_score-test_set3$WW2_tr_score
test_set3$WLOc.H<-c(0)
test_set3$WLoc.N<-c(1)
test_set3$WLoc.W<-c(0)

test_set3<-test_set3[,c(1,44:46,3,4,19:43)]


test_set3$T1Region<-as.factor(test_set3$T1Region)
test_set3$T2Region<-as.factor(test_set3$T2Region)
test_set3$Team1_confe<-as.factor(test_set3$Team1_confe)
test_set3$Team2_confe<-as.factor(test_set3$Team2_confe)


test_processed<-predict(preProcValues_set3,test_set3)
test_set3_id<-test_processed$ID
test_processed$ID<-NULL

dmy<-dummyVars("~.",data=test_processed)
test_transformed <- data.frame(predict(dmy, newdata = test_processed))
test_set3<-test_transformed


outcomeName<-'result'
predictors<-intersect(names(test_set3),names(train_set3))



##feature selection using rfee in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 5)
outcomeName<-'result'
predictors<-intersect(names(train_set3),names(test_set3))

fitControl <- trainControl(
  method = "repeatedcv",
  classProbs = TRUE,
  number = 5,
  repeats = 5,
  verboseIter = TRUE)
rf_grid=expand.grid(mtry=c(8,5,2,1))
nn_grid<-expand.grid(size=c(10,7,3),decay=0.00147)
gbm_grid <- expand.grid(n.trees=c(5,10,20,50),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))

feature.names=names(test_set3)
for (f in feature.names) {
  if (class(test_set3[[f]])=="factor") {
    levels <- unique(c(test_set3[[f]]))
    test_set3[[f]] <- factor(test_set3[[f]],
                             labels=make.names(levels))
  }
}
feature.names=names(train_set3)
for (f in feature.names) {
  if (class(train_set3[[f]])=="factor") {
    levels <- unique(c(train_set3[[f]]))
    train_set3[[f]] <- factor(train_set3[[f]],
                              labels=make.names(levels))
  }
}
feature.names=names(blender_set3)
for (f in feature.names) {
  if (class(blender_set3[[f]])=="factor") {
    levels <- unique(c(blender_set3[[f]]))
    blender_set3[[f]] <- factor(blender_set3[[f]],
                                labels=make.names(levels))
  }
}


model_gbm_3<-train(train_set3[,predictors],train_set3[,outcomeName],method='gbm',trControl=fitControl)
model_rf_3<-train(train_set3[,predictors],train_set3[,outcomeName],method='rf',trControl=fitControl)
model_nn_3<-train(train_set3[,predictors],train_set3[,outcomeName],method='nnet',trControl=fitControl,tuneGrid=nn_grid)
model_treebag_3 <- train(train_set3[,predictors],train_set3[,outcomeName], method='treebag', trControl=fitControl)
model_svm_3<- train(train_set3[,predictors],train_set3[,outcomeName], method='svmLinearWeights2', trControl=fitControl)
model_xgbTree_3<-train(train_set3[,predictors],train_set3[,outcomeName], method='xgbTree', trControl=fitControl)
model_avNNet_3<-train(train_set3[,predictors],train_set3[,outcomeName], method='avNNet', trControl=fitControl)
model_regLogistic_3<-train(train_set3[,predictors],train_set3[,outcomeName], method='regLogistic', trControl=fitControl)
model_kknn_3<-train(train_set3[,predictors],train_set3[,outcomeName], method='kknn', trControl=fitControl)
model_dwdRadial_3<-train(train_set3[,predictors],train_set3[,outcomeName], method='dwdRadial', trControl=fitControl)
model_C5_3<-train(train_set3[,predictors],train_set3[,outcomeName], method='C5.0', trControl=fitControl)



blender_set3$gbm<-predict.train(object=model_gbm_3,blender_set3[,predictors],type="prob")[,2]
blender_set3$rf<-predict.train(object=model_rf_3,blender_set3[,predictors],type="prob")[,2]
blender_set3$nn<-predict.train(object=model_nn_3,blender_set3[,predictors],type="prob")[,2]
blender_set3$treebag<-predict.train(object=model_treebag_3,blender_set3[,predictors],type="prob")[,2]
blender_set3$svm<-predict.train(object=model_svm_3,blender_set3[,predictors],type="raw")
blender_set3$xgbTree<-predict.train(object=model_xgbTree_3,blender_set3[,predictors],type="prob")[,2]
blender_set3$avNNet<-predict.train(object=model_avNNet_3,blender_set3[,predictors],type="prob")[,2]
blender_set3$regLogistic<-predict.train(object=model_regLogistic_3,blender_set3[,predictors],type="prob")[,2]
blender_set3$kknn<-predict.train(object=model_kknn_3,blender_set3[,predictors],type="prob")[,2]
blender_set3$dwdRadial<-predict.train(object=model_dwdRadial_3,blender_set3[,predictors],type="prob")[,2]
blender_set3$C5<-predict.train(object=model_C5_3,blender_set3[,predictors],type="prob")[,2]


test_set3$gbm<-predict.train(object=model_gbm_3,test_set3[,predictors],type="prob")[,2]
test_set3$rf<-predict.train(object=model_rf_3,test_set3[,predictors],type="prob")[,2]
test_set3$nn<-predict.train(object=model_nn_3,test_set3[,predictors],type="prob")[,2]
test_set3$treebag<-predict.train(object=model_treebag_3,test_set3[,predictors],type="prob")[,2]
test_set3$svm<-predict.train(object=model_svm_3,test_set3[,predictors],type="raw")
test_set3$xgbTree<-predict.train(object=model_xgbTree_3,test_set3[,predictors],type="prob")[,2]
test_set3$avNNet<-predict.train(object=model_avNNet_3,test_set3[,predictors],type="prob")[,2]
test_set3$regLogistic<-predict.train(object=model_regLogistic_3,test_set3[,predictors],type="prob")[,2]
test_set3$kknn<-predict.train(object=model_kknn_3,test_set3[,predictors],type="prob")[,2]
test_set3$dwdRadial<-predict.train(object=model_dwdRadial_3,test_set3[,predictors],type="prob")[,2]
test_set3$C5<-predict.train(object=model_C5_3,test_set3[,predictors],type="prob")[,2]




final_predictors<-intersect(names(blender_set3),names(test_set3))
final_blender_model_3<-train(blender_set3[,final_predictors],blender_set3[,outcomeName],method='avNNet',trControl=fitControl)

test_set3$pred<-predict(object=final_blender_model_3,test_set3[,final_predictors],type="prob")[,2]
test_set3_id_result<-cbind(test_set3_id,test_set3$pred)

save(model_nn_3,model_rf_3,model_gbm_3,model_treebag_3,model_svm_3,model_xgbTree_3,final_blender_model_3,
     preProcValues_set3,train_set3,test_set3,test_set3_id_result,file="model_3.Rd")



## Classifier 4
# Feature12: Players (total numbers) 2010:2017
player2010<-read.csv("PlayByPlay_2010/Players_2010.csv",stringsAsFactors = FALSE)
player2011<-read.csv("PlayByPlay_2011/Players_2011.csv",stringsAsFactors = FALSE)
player2012<-read.csv("PlayByPlay_2012/Players_2012.csv",stringsAsFactors = FALSE)
player2013<-read.csv("PlayByPlay_2013/Players_2013.csv",stringsAsFactors = FALSE)
player2014<-read.csv("PlayByPlay_2014/Players_2014.csv",stringsAsFactors = FALSE)
player2015<-read.csv("PlayByPlay_2015/Players_2015.csv",stringsAsFactors = FALSE)
player2016<-read.csv("PlayByPlay_2016/Players_2016.csv",stringsAsFactors = FALSE)
player2017<-read.csv("PlayByPlay_2017/Players_2017.csv",stringsAsFactors = FALSE)
player2018<-read.csv("PlayByPlay_2018/Players_2018.csv",stringsAsFactors = FALSE)

player<-do.call("rbind",list(player2010,player2011,player2012,player2013,player2014,player2015,player2016,player2017,player2018))
train_set4<-filter(train_orginal,Season>=2010)

train_set4$Team1_player<-c(0)
train_set4$Team2_player<-c(0)

test<-test_orginal
test$Team1_player<-c(0)
test$Team2_player<-c(0)

for (i in 1:nrow(train_set4)){
  k<-filter(player,Season==train_set4$Season[i],TeamID==train_set4$Team1[i])
  if (nrow(k)==0 ) {
    train_set4$Team1_player[i]<-0
  }
  else {
    train_set4$Team1_player[i]<-nrow(k)
  }
  q<-filter(player,Season==train_set4$Season[i],TeamID==train_set4$Team2[i])
  if (nrow(q)==0 ) {
    train_set4$Team2_player[i]<-0
  }
  else {
    train_set4$Team2_player[i]<-nrow(q)
  }
}


for (i in 1:nrow(test)){
  k<-filter(player,Season==test$Season[i],TeamID==test$Team1[i])
  if (nrow(k)==0 ) {
    test$Team1_player[i]<-0
  }
  else {
    test$Team1_player[i]<-nrow(k)
  }
  q<-filter(player,Season==test$Season[i],TeamID==test$Team2[i])
  if (nrow(q)==0 ) {
    test$Team2_player[i]<-0
  }
  else {
    test$Team2_player[i]<-nrow(q)
  }
}


train_set4<-filter(train_set4,WW1_trc_result!=0,WW2_trc_result!=0,CWW1_trc_result!=0,CWW2_trc_result!=0)
train_set4$DayNum<-NULL
train_set4$seed_diff<-train_set4$T1Seed-train_set4$T2Seed
train_set4$CWW_result<-train_set4$CWW1_trc_result-train_set4$CWW2_trc_result
train_set4$CWW_score<-train_set4$CWW1_tr_score-train_set4$CWW2_tr_score
train_set4$WW_result<-train_set4$WW1_trc_result-train_set4$WW2_trc_result
train_set4$WW_score<-train_set4$WW1_tr_score-train_set4$WW2_tr_score
train_set4$player_diff<-train_set4$Team1_player-train_set4$Team2_player
train_set4<-train_set4%>%unite(ID,Season,Team1,Team2,sep="_")

train_set4<-train_set4[,c(1,2,5,7,22:43,46:51,3)]

train_set4$T1Region<-as.factor(train_set4$T1Region)
train_set4$T2Region<-as.factor(train_set4$T2Region)
train_set4$WLoc<-as.factor(train_set4$WLoc)
train_set4$Team1_confe<-as.factor(train_set4$Team1_confe)
train_set4$Team2_confe<-as.factor(train_set4$Team2_confe)
train_set4$result<-as.factor(train_set4$result)


test<-test%>%unite(ID,Season,Team1,Team2,sep="_")
test_set4<-filter(test,WW1_trc_result!=0,WW2_trc_result!=0,CWW1_trc_result!=0,CWW2_trc_result!=0)
test_set4$DayNum<-NULL
test_set4$seed_diff<-test_set4$T1Seed-test_set4$T2Seed
test_set4$CWW_result<-test_set4$CWW1_trc_result-test_set4$CWW2_trc_result
test_set4$CWW_score<-test_set4$CWW1_tr_score-test_set4$CWW2_tr_score
test_set4$WW_result<-test_set4$WW1_trc_result-test_set4$WW2_trc_result
test_set4$WW_score<-test_set4$WW1_tr_score-test_set4$WW2_tr_score
test_set4$player_diff<-test_set4$Team1_player-test_set4$Team2_player
test_set4$WLoc.N<-c(1)
test_set4$WLoc.H<-c(0)
test_set4$WLoc.W<-c(0)

test_set4<-test_set4[,c(1,51:53,4,6,21:42,45:50)]

test_set4$T1Region<-as.factor(test_set4$T1Region)
test_set4$T2Region<-as.factor(test_set4$T2Region)
test_set4$Team1_confe<-as.factor(test_set4$Team1_confe)
test_set4$Team2_confe<-as.factor(test_set4$Team2_confe)


preProcValues_set4 <- preProcess(train_set4, method = c("knnImpute","center","scale"))
train_processed<-predict(preProcValues_set4,train_set4)
train_processed$result<-ifelse(train_processed$result=="-1",0,1)
id<-train_processed$ID
train_processed$ID<-NULL
dmy<-dummyVars("~.",data=train_processed)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))
train_transformed$result<-as.factor(train_transformed$result)
train_set4<-train_transformed


train_data<-train_set4
train.rows<-createDataPartition(y=train_data$result,p=0.85,list=FALSE)
train_set4<-train_data[train.rows,]
blender_set4<-train_data[-train.rows,]


test_processed<-predict(preProcValues_set4,test_set4)
test_set4_id<-test_processed$ID
test_processed$ID<-NULL

dmy<-dummyVars("~.",data=test_processed)
test_transformed <- data.frame(predict(dmy, newdata = test_processed))
test_set4<-test_transformed


outcomeName<-'result'
predictors<-intersect(names(test_set4),names(train_set4))


##feature selection using rfee in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3)
outcomeName<-'result'
predictors<-intersect(names(train_set4),names(test_set4))

fitControl <- trainControl(
  method = "repeatedcv",
  classProbs = TRUE,
  number = 5,
  repeats = 5,
  verboseIter = TRUE)
rf_grid=expand.grid(mtry=c(8,5,2,1))
nn_grid<-expand.grid(size=c(20,15,7,3),decay=0.00147)
gbm_grid <- expand.grid(n.trees=c(5,10,20,50),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))
xgbGrid <- expand.grid(
  eta = 0.3,
  max_depth = 1,
  nrounds = 50,
  gamma = 0,
  colsample_bytree = 0.6,
  min_child_weight = 1
)
C5grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )


feature.names=names(test_set4)
for (f in feature.names) {
  if (class(test_set4[[f]])=="factor") {
    levels <- unique(c(test_set4[[f]]))
    test_set4[[f]] <- factor(test_set4[[f]],
                             labels=make.names(levels))
  }
}
feature.names=names(train_set4)
for (f in feature.names) {
  if (class(train_set4[[f]])=="factor") {
    levels <- unique(c(train_set4[[f]]))
    train_set4[[f]] <- factor(train_set4[[f]],
                              labels=make.names(levels))
  }
}
feature.names=names(blender_set4)
for (f in feature.names) {
  if (class(blender_set4[[f]])=="factor") {
    levels <- unique(c(blender_set4[[f]]))
    blender_set4[[f]] <- factor(blender_set4[[f]],
                                labels=make.names(levels))
  }
}


model_gbm_4<-train(train_set4[,predictors],train_set4[,outcomeName],method='gbm',trControl=fitControl)
model_rf_4<-train(train_set4[,predictors],train_set4[,outcomeName],method='rf',trControl=fitControl)
model_nn_4<-train(train_set4[,predictors],train_set4[,outcomeName],method='nnet',trControl=fitControl,tuneGrid=nn_grid)
model_treebag_4 <- train(train_set4[,predictors],train_set4[,outcomeName], method='treebag', trControl=fitControl)
model_svm_4<- train(train_set4[,predictors],train_set4[,outcomeName], method='svmLinearWeights2', trControl=fitControl)
model_xgbTree_4<-train(train_set4[,predictors],train_set4[,outcomeName], method='xgbTree', trControl=fitControl)
model_avNNet_4<-train(train_set4[,predictors],train_set4[,outcomeName], method='avNNet', trControl=fitControl)
model_regLogistic_4<-train(train_set4[,predictors],train_set4[,outcomeName], method='regLogistic', trControl=fitControl)
model_kknn_4<-train(train_set4[,predictors],train_set4[,outcomeName], method='kknn', trControl=fitControl,tuneLength = 20)
model_dwdRadial_4<-train(train_set4[,predictors],train_set4[,outcomeName], method='dwdRadial', trControl=fitControl)
model_C5_4<-train(train_set4[,predictors],train_set4[,outcomeName], method='C5.0', trControl=fitControl,tuneGrid=C5Grid)



blender_set4$gbm<-predict.train(object=model_gbm_4,blender_set4[,predictors],type="prob")[,2]
blender_set4$rf<-predict.train(object=model_rf_4,blender_set4[,predictors],type="prob")[,2]
blender_set4$nn<-predict.train(object=model_nn_4,blender_set4[,predictors],type="prob")[,2]
blender_set4$treebag<-predict.train(object=model_treebag_4,blender_set4[,predictors],type="prob")[,2]
blender_set4$svm<-predict.train(object=model_svm_4,blender_set4[,predictors],type="raw")
blender_set4$xgbTree<-predict.train(object=model_xgbTree_4,blender_set4[,predictors],type="prob")[,2]
blender_set4$avNNet<-predict.train(object=model_avNNet_4,blender_set4[,predictors],type="prob")[,2]
blender_set4$regLogistic<-predict.train(object=model_regLogistic_4,blender_set4[,predictors],type="prob")[,2]
blender_set4$kknn<-predict.train(object=model_kknn_4,blender_set4[,predictors],type="prob")[,2]
blender_set4$dwdRadial<-predict.train(object=model_dwdRadial_4,blender_set4[,predictors],type="prob")[,2]
blender_set4$C5<-predict.train(object=model_C5_4,blender_set4[,predictors],type="prob")[,2]


test_set4$gbm<-predict.train(object=model_gbm_4,test_set4[,predictors],type="prob")[,2]
test_set4$rf<-predict.train(object=model_rf_4,test_set4[,predictors],type="prob")[,2]
test_set4$nn<-predict.train(object=model_nn_4,test_set4[,predictors],type="prob")[,2]
test_set4$treebag<-predict.train(object=model_treebag_4,test_set4[,predictors],type="prob")[,2]
test_set4$svm<-predict.train(object=model_svm_4,test_set4[,predictors],type="raw")
test_set4$xgbTree<-predict.train(object=model_xgbTree_4,test_set4[,predictors],type="prob")[,2]
test_set4$avNNet<-predict.train(object=model_avNNet_4,test_set4[,predictors],type="prob")[,2]
test_set4$regLogistic<-predict.train(object=model_regLogistic_4,test_set4[,predictors],type="prob")[,2]
test_set4$kknn<-predict.train(object=model_kknn_4,test_set4[,predictors],type="prob")[,2]
test_set4$dwdRadial<-predict.train(object=model_dwdRadial_4,test_set4[,predictors],type="prob")[,2]
test_set4$C5<-predict.train(object=model_C5_4,test_set4[,predictors],type="prob")[,2]





final_predictors<-intersect(names(blender_set4),names(test_set4))
final_blender_model_4<-train(blender_set4[,final_predictors],blender_set4[,outcomeName],method='avNNet',trControl=fitControl)

test_set4$pred<-predict(object=final_blender_model_4,test_set4[,final_predictors],type="prob")[,2]

test_set4_id_result<-cbind(test_set4_id,test_set4$pred)

save(model_nn_4,model_rf_4,model_gbm_4,model_treebag_4,model_svm_4,model_xgbTree_4,final_blender_model_4,
     preProcValues_set4,train_set4,test_set4,test_set4_id_result,file="model_4.Rd")



# Classifier 5
# --for test data which doesn't belong to any of above test subset

train_set5<-filter(train_orginal,Season>=2010)

train_set5$Team1_player<-c(0)
train_set5$Team2_player<-c(0)

test<-test_orginal
test$Team1_player<-c(0)
test$Team2_player<-c(0)

for (i in 1:nrow(train_set5)){
  k<-filter(player,Season==train_set5$Season[i],TeamID==train_set5$Team1[i])
  if (nrow(k)==0 ) {
    train_set5$Team1_player[i]<-0
  }
  else {
    train_set5$Team1_player[i]<-nrow(k)
  }
  q<-filter(player,Season==train_set5$Season[i],TeamID==train_set5$Team2[i])
  if (nrow(q)==0 ) {
    train_set5$Team2_player[i]<-0
  }
  else {
    train_set5$Team2_player[i]<-nrow(q)
  }
}


for (i in 1:nrow(test)){
  k<-filter(player,Season==test$Season[i],TeamID==test$Team1[i])
  if (nrow(k)==0 ) {
    test$Team1_player[i]<-0
  }
  else {
    test$Team1_player[i]<-nrow(k)
  }
  q<-filter(player,Season==test$Season[i],TeamID==test$Team2[i])
  if (nrow(q)==0 ) {
    test$Team2_player[i]<-0
  }
  else {
    test$Team2_player[i]<-nrow(q)
  }
}

train_set5<-filter(train_set5)
train_set5$DayNum<-NULL
train_set5$seed_diff<-train_set5$T1Seed-train_set5$T2Seed
train_set5$WW_result<-train_set5$WW1_trc_result-train_set5$WW2_trc_result
train_set5$WW_score<-train_set5$WW1_tr_score-train_set5$WW2_tr_score
train_set5$player_diff<-train_set5$Team1_player-train_set5$Team2_player
train_set5<-train_set5%>%unite(ID,Season,Team1,Team2,sep="_")

train_set5<-train_set5[,c(1,2,5,7,22:43,46:49,3)]

train_set5$T1Region<-as.factor(train_set5$T1Region)
train_set5$T2Region<-as.factor(train_set5$T2Region)
train_set5$WLoc<-as.factor(train_set5$WLoc)
train_set5$Team1_confe<-as.factor(train_set5$Team1_confe)
train_set5$Team2_confe<-as.factor(train_set5$Team2_confe)
train_set5$result<-as.factor(train_set5$result)


test<-test%>%unite(ID,Season,Team1,Team2,sep="_")
test_set5<-filter(test)
test_set5$DayNum<-NULL
test_set5$seed_diff<-test_set5$T1Seed-test_set5$T2Seed
test_set5$WW_result<-test_set5$WW1_trc_result-test_set5$WW2_trc_result
test_set5$WW_score<-test_set5$WW1_tr_score-test_set5$WW2_tr_score
test_set5$player_diff<-test_set5$Team1_player-test_set5$Team2_player
test_set5$WLoc.N<-c(1)
test_set5$WLoc.H<-c(0)
test_set5$WLoc.W<-c(0)

test_set5<-test_set5[,c(1,49:51,4,6,21:42,45:48)]

test_set5$T1Region<-as.factor(test_set5$T1Region)
test_set5$T2Region<-as.factor(test_set5$T2Region)
test_set5$Team1_confe<-as.factor(test_set5$Team1_confe)
test_set5$Team2_confe<-as.factor(test_set5$Team2_confe)


preProcValues_set5 <- preProcess(train_set5, method = c("knnImpute","center","scale"))
train_processed<-predict(preProcValues_set5,train_set5)
train_processed$result<-ifelse(train_processed$result=="-1",0,1)
id<-train_processed$ID
train_processed$ID<-NULL
dmy<-dummyVars("~.",data=train_processed)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))
train_transformed$result<-as.factor(train_transformed$result)
train_set5<-train_transformed


train_data<-train_set5
train.rows<-createDataPartition(y=train_data$result,p=0.7,list=FALSE)
train_set5<-train_data[train.rows,]
blender_set5<-train_data[-train.rows,]


test_processed<-predict(preProcValues_set5,test_set5)
test_set5_id<-test_processed$ID
test_processed$ID<-NULL

dmy<-dummyVars("~.",data=test_processed)
test_transformed <- data.frame(predict(dmy, newdata = test_processed))
test_set5<-test_transformed


outcomeName<-'result'
predictors<-intersect(names(test_set5),names(train_set5))


##feature selection using rfee in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3)
outcomeName<-'result'
predictors<-intersect(names(train_set5),names(test_set5))

fitControl <- trainControl(
  method = "repeatedcv",
  classProbs = TRUE,
  number = 5,
  repeats = 5,
  verboseIter = TRUE)
rf_grid=expand.grid(mtry=c(8,5,2,1))
nn_grid<-expand.grid(size=c(20,15,7,3),decay=0.00147)
gbm_grid <- expand.grid(n.trees=c(5,10,20,50),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))

feature.names=names(test_set5)
for (f in feature.names) {
  if (class(test_set5[[f]])=="factor") {
    levels <- unique(c(test_set5[[f]]))
    test_set5[[f]] <- factor(test_set5[[f]],
                             labels=make.names(levels))
  }
}
feature.names=names(train_set5)
for (f in feature.names) {
  if (class(train_set5[[f]])=="factor") {
    levels <- unique(c(train_set5[[f]]))
    train_set5[[f]] <- factor(train_set5[[f]],
                              labels=make.names(levels))
  }
}
feature.names=names(blender_set5)
for (f in feature.names) {
  if (class(blender_set5[[f]])=="factor") {
    levels <- unique(c(blender_set5[[f]]))
    blender_set5[[f]] <- factor(blender_set5[[f]],
                                labels=make.names(levels))
  }
}



model_gbm_5<-train(train_set5[,predictors],train_set5[,outcomeName],method='gbm',trControl=fitControl)
model_rf_5<-train(train_set5[,predictors],train_set5[,outcomeName],method='rf',trControl=fitControl)
model_nn_5<-train(train_set5[,predictors],train_set5[,outcomeName],method='nnet',trControl=fitControl,tuneGrid=nn_grid)
model_treebag_5 <- train(train_set5[,predictors],train_set5[,outcomeName], method='treebag', trControl=fitControl)
model_svm_5<- train(train_set5[,predictors],train_set5[,outcomeName], method='svmLinearWeights2', trControl=fitControl)
model_xgbTree_5<-train(train_set5[,predictors],train_set5[,outcomeName], method='xgbTree', trControl=fitControl)
model_avNNet_5<-train(train_set5[,predictors],train_set5[,outcomeName], method='avNNet', trControl=fitControl)
model_regLogistic_5<-train(train_set5[,predictors],train_set5[,outcomeName], method='regLogistic', trControl=fitControl)
model_kknn_5<-train(train_set5[,predictors],train_set5[,outcomeName], method='kknn', trControl=fitControl)
model_dwdRadial_5<-train(train_set5[,predictors],train_set5[,outcomeName], method='dwdRadial', trControl=fitControl)
model_C5_5<-train(train_set5[,predictors],train_set5[,outcomeName], method='C5.0', trControl=fitControl)



blender_set5$gbm<-predict.train(object=model_gbm_5,blender_set5[,predictors],type="prob")[,2]
blender_set5$rf<-predict.train(object=model_rf_5,blender_set5[,predictors],type="prob")[,2]
blender_set5$nn<-predict.train(object=model_nn_5,blender_set5[,predictors],type="prob")[,2]
blender_set5$treebag<-predict.train(object=model_treebag_5,blender_set5[,predictors],type="prob")[,2]
blender_set5$svm<-predict.train(object=model_svm_5,blender_set5[,predictors],type="raw")
blender_set5$xgbTree<-predict.train(object=model_xgbTree_5,blender_set5[,predictors],type="prob")[,2]
blender_set5$avNNet<-predict.train(object=model_avNNet_5,blender_set5[,predictors],type="prob")[,2]
blender_set5$regLogistic<-predict.train(object=model_regLogistic_5,blender_set5[,predictors],type="prob")[,2]
blender_set5$kknn<-predict.train(object=model_kknn_5,blender_set5[,predictors],type="prob")[,2]
blender_set5$dwdRadial<-predict.train(object=model_dwdRadial_5,blender_set5[,predictors],type="prob")[,2]
blender_set5$C5<-predict.train(object=model_C5_5,blender_set5[,predictors],type="prob")[,2]


test_set5$gbm<-predict.train(object=model_gbm_5,test_set5[,predictors],type="prob")[,2]
test_set5$rf<-predict.train(object=model_rf_5,test_set5[,predictors],type="prob")[,2]
test_set5$nn<-predict.train(object=model_nn_5,test_set5[,predictors],type="prob")[,2]
test_set5$treebag<-predict.train(object=model_treebag_5,test_set5[,predictors],type="prob")[,2]
test_set5$svm<-predict.train(object=model_svm_5,test_set5[,predictors],type="raw")
test_set5$xgbTree<-predict.train(object=model_xgbTree_5,test_set5[,predictors],type="prob")[,2]
test_set5$avNNet<-predict.train(object=model_avNNet_5,test_set5[,predictors],type="prob")[,2]
test_set5$regLogistic<-predict.train(object=model_regLogistic_5,test_set5[,predictors],type="prob")[,2]
test_set5$kknn<-predict.train(object=model_kknn_5,test_set5[,predictors],type="prob")[,2]
test_set5$dwdRadial<-predict.train(object=model_dwdRadial_5,test_set5[,predictors],type="prob")[,2]
test_set5$C5<-predict.train(object=model_C5_5,test_set5[,predictors],type="prob")[,2]



final_predictors<-intersect(names(blender_set5),names(test_set5))
final_blender_model_5<-train(blender_set5[,final_predictors],blender_set5[,outcomeName],method='avNNet',trControl=fitControl)

test_set5$pred<-predict(object=final_blender_model_5,test_set5[,final_predictors],type="prob")[,2]

test_set5_id_result<-cbind(test_set5_id,test_set5$pred)

save(model_nn_5,model_rf_5,model_gbm_5,model_treebag_5,model_svm_5,model_xgbTree_5,final_blender_model_5,
     preProcValues_set5,train_set5,test_set5,test_set5_id_result,file="model_5.Rd")




# -------conbine test result
test_ID<-as.character(test$ID)
test_ID<-as.data.frame(test_ID)
test_ID$pred<-c(0)
test_result<-as.data.frame(rbind(test_set1_id_result,test_set2_id_result,test_set3_id_result,test_set4_id_result,test_set5_id_result))
colnames(test_result)<-c("ID","pre")
test_result$ID<-as.character(test_result$ID)
test_result$pre<-as.numeric(as.character(test_result$pre))


for (i in 1:nrow(test_ID)){
  k<-filter(test_result,ID==test_ID$test_ID[i])
  if (nrow(k)!=0){
    test_ID$pred[i]<-mean(k$pre)
  }
}


test_left_ID<-filter(test_ID,pred==0)
colnames(test_ID)<-c("id","pred")
write.csv(test_ID,"2018NACC_prediction.csv",row.names = FALSE)
