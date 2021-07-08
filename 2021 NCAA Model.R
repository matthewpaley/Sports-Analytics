rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(reshape2)

teams <- read.csv("MTeams.csv")
seeds <- read.csv("MNCAATourneySeeds.csv")
seas_results <- read.csv("MRegularSeasonCompactResults.csv")
tour_results <- read.csv("MNCAATourneyCompactResults.csv")
seas_detail <- read.csv("MRegularSeasonDetailedResults.csv")
tour_detail <- read.csv("MNCAATourneyDetailedResults.csv")
massey <- read.csv("MMasseyOrdinals.csv")
kenpom <- read.csv("kenpom.csv")
kaggle <- read.csv("kaggle_leaderboard.csv", stringsAsFactors = FALSE)


set.seed(121719)

# Convert Seeds to numeric #
seeds_names <- merge(x = seeds,
                     y = teams[,1:2],
                     by = "TeamID")
seeds_names$Seed <- as.numeric(substr(seeds_names$Seed,2,3))


## Massey Data ##
final_massey <- subset(massey,
                       RankingDayNum==133 & 
                         Season < 2021)
final_massey2 <- subset(massey,
                        RankingDayNum==128 & 
                          Season==2021)
final_massey <- rbind(final_massey,
                      final_massey2)
rm(final_massey2)

# Create avg ranking across all systems
massey <- final_massey %>% 
  group_by(Season, TeamID) %>% 
  summarise(final_rank = mean(OrdinalRank))

rm(final_massey)
names(massey) <- c("season","teamID","final_rank")


## Calculate game by game metrics ##

# Union winning and losing details
# One row per team per game instead of one row per game with both teams on it
winnerHistory <- seas_detail[,c("Season","WTeamID","DayNum",
                                "WScore","NumOT","WFGM","WFGA",
                                "WFGM3","WFGA3","WFTM","WFTA",
                                "WOR","WDR","WAst","WTO","WStl",
                                "WBlk","WPF","LTeamID")]
winnerHistory$Victory <- 1
loserHistory <- seas_detail[,c("Season","LTeamID","DayNum",
                               "LScore","NumOT","LFGM","LFGA",
                               "LFGM3","LFGA3","LFTM","LFTA",
                               "LOR","LDR","LAst","LTO","LStl",
                               "LBlk","LPF","WTeamID")]
loserHistory$Victory <- 0

# Now we normalize the column names before combining the two dataframes
names(winnerHistory) <- c("season","teamID","daynum","score","numot",
                          "fgmade","fgattempt","fgm3","fga3","ftmade",
                          "ftattempt","offreb","defreb","asst","turnover",
                          "steal","block","pfoul","oppID","victory")
names(loserHistory) <- c("season","teamID","daynum","score","numot",
                         "fgmade","fgattempt","fgm3","fga3","ftmade",
                         "ftattempt","offreb","defreb","asst","turnover",
                         "steal","block","pfoul","oppID","victory")

# Combine both winning and losing dataframes into a single df
teamHistory <- rbind(winnerHistory, loserHistory)
rm(winnerHistory, loserHistory)

# Add features
teamHistory$fgm2 <- teamHistory$fgmade - teamHistory$fgm3
teamHistory$fga2 <- teamHistory$fgattempt - teamHistory$fga3
teamHistory$poss <- teamHistory$fgattempt - teamHistory$offreb + teamHistory$turnover + 
  0.475*teamHistory$ftattempt
teamHistory$game_score <- teamHistory$score + 0.4*teamHistory$fgmade - 
  0.7*teamHistory$fgattempt - 0.4*(teamHistory$ftattempt-teamHistory$ftmade) + 
  0.7*teamHistory$offreb + 0.3*teamHistory$defreb + teamHistory$steal + 0.7*teamHistory$asst + 
  0.7*teamHistory$block - 0.4*teamHistory$pfoul - teamHistory$turnover

## Join game by game to Massey rank to get win_pts **
teamHistory <- merge(teamHistory,
                     massey,
                     by.x = c("season","oppID"),
                     by.y = c("season","teamID"))
teamHistory$quadrant1 <- ifelse(teamHistory$victory == 1 & 
                                    teamHistory$final_rank < 51,1,0)
teamHistory$quadrant2 <- ifelse(teamHistory$victory == 1 & 
                                    teamHistory$final_rank > 50 &
                                    teamHistory$final_rank < 101,1,0)

# Calculate win_points
nbr_teams <- nrow(teams)
teamHistory$win_pts <- ifelse(teamHistory$victory == 1, 
                                (nbr_teams - teamHistory$final_rank)/10,0)

## Aggregate game data into season data ##
teamSums <- aggregate(teamHistory[,c("quadrant1","quadrant2","win_pts")],
                      by=list(teamHistory$season,teamHistory$teamID), 
                      function(x) c(sum = sum(x)))

# IF WE ADD FEATURES, FIX THE COLUMN NUMBERS
teamAvgs <- aggregate(teamHistory[,c(5:19,21:24)],
                      by=list(teamHistory$season,teamHistory$teamID), 
                      function(x) c(mean = mean(x))) 
teamAvgs <- merge(teamAvgs,
                  teamSums,
                  by = c("Group.1","Group.2"))
names(teamAvgs)[1:2] <- c("season","teamID")

## Add KenPom Data ##
teamAvgs <- merge(teamAvgs,
                  kenpom[,c(2:3,7,11:27)],
                  by.x = c("season","teamID"),
                  by.y = c("Season","TeamID"))


## Normalize tournament results ##
tour_results$team1 <- ifelse(tour_results$WTeamID > tour_results$LTeamID, 
                             tour_results$LTeamID, 
                             tour_results$WTeamID) 
tour_results$team2 <- ifelse(tour_results$WTeamID > tour_results$LTeamID, 
                             tour_results$WTeamID, 
                             tour_results$LTeamID)
tour_results$team1Victory <- ifelse(tour_results$WTeamID == tour_results$team1, 1, 0) 

# Combine Season Stats With tournament results
train1 <- merge(x = tour_results[,c("Season","DayNum","team1","team2","team1Victory")],
                y = teamAvgs,
                by.x = c("Season","team1"),
                by.y = c("season","teamID"))
traincol <- ncol(train1)

# Change all column names to show team1
names(train1)[6:traincol] <- paste("team1_",names(train1)[6:traincol],sep = "")
train1 <- merge(x = train1,
                y = teamAvgs, 
                by.x = c("Season","team2"),
                by.y = c("season","teamID"))

# Merge back to teamAvgs to get team2 stats
traincol2 <- ncol(train1)
names(train1)[(traincol + 1):traincol2] <- paste("team2_",
                                                 names(train1)[(traincol + 1):traincol2],
                                                 sep = "")

# Add seeds to train1
train1 <- merge(train1, 
                seeds_names[,c("Season","TeamID","Seed")], 
                by.x = c("Season","team1"), 
                by.y = c("Season","TeamID"))
colnames(train1)[colnames(train1)=="Seed"] <- "seed1"

train1 <- merge(train1, 
                seeds_names[,c("Season","TeamID","Seed")], 
                by.x = c("Season","team2"), 
                by.y = c("Season","TeamID"))
colnames(train1)[colnames(train1)=="Seed"] <- "seed2"

# Delta in stats between team1 and team2
team1col <- grep("team1",colnames(train1)[6:(traincol2+2)])
nbrcol <- ncol(train1)

for (i in team1col) {
  col_str <- substr(colnames(train1)[i+5],7,nchar(colnames(train1)[i+5]))
  j <- grep(paste0("team2_",col_str),colnames(train1)[6:(traincol2+2)])
  train1[,nbrcol + 1] <- train1[,i+5] - train1[,j+5]
  names(train1)[nbrcol + 1] <- paste0(col_str,"_diff")
  nbrcol <- nbrcol + 1
}

# Since we have differences in stats, delete the team1 and team2 columns
train1 <- train1[, -grep("team1_", colnames(train1))]
train1 <- train1[, -grep("team2_", colnames(train1))]
train1$seed_diff <- train1$seed1 - train1$seed2
train1$seed1 <- NULL
train1$seed2 <- NULL
train1$victory <- ifelse(train1$team1Victory==1,"yes","no")
train1$victory <- as.factor(train1$victory)



## BUILD MODEL ##

rf <- randomForest(victory ~ .,
                   data=train1[,-c(1:5)],
                   ntree=500,
                   type='ROC',
                   importance= TRUE)

# Feature Importance
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), 
                                Importance=imp[,1])

# Remove negative importance features
non_neg_imp <- featureImportance %>% 
  filter(Importance > 0) %>%
  select(Feature)

ggplot(featureImportance, 
       aes(x=reorder(Feature, Importance), 
           y=Importance)) + 
  geom_bar(stat="identity", fill="#d40511") +
  coord_flip() +
  theme_light(base_size=10) + 
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

# Use Feature Importance to dynamically remove negative Imp
train1final <- train1[,names(train1) %in% non_neg_imp$Feature]
train1final <- cbind(train1[,c("Season","victory")], 
                     train1final)
names(train1final)[1] <- "season"
names(train1final)[2] <- "victory"

## LOOP THROUGH SEASONS TO TEST LOG LOSS##

ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 2,
                     savePred = T,
                     classProb = T,
                     summaryFunction = twoClassSummary)

seasons <- 2014:2019

# for (m in c("rf","LogitBoost","ada","bagFDA","parRF","wsrf","glmboost",
#             "C5.0","xgbLinear","knn","svmLinearWeights")) {
for (m in c("bagFDA")) {
  for (i in seasons) {
    
    print(paste0("Testing ",m," model against ",i," tournament results..."))
    # Make test set the year i
    train_set <- train1final[train1final$season != i,]
    test_set <- train1final[train1final$season == i,]
    
    mod_rf <- train(victory ~ .,
                    data=train_set[,-1],
                    method=m,
                    trControl = ctrl,
                    metric="ROC")
    
    pred_rf <- predict(mod_rf, test_set, type="prob")
    test_set$team1Victory <- ifelse(test_set$victory == "yes",1,0)
    test_set$predicted_rf <- pred_rf[,2]
    test_set$predicted_rf[test_set$predicted_rf == 0] <- 0.000001
    test_set$predicted_rf[test_set$predicted_rf == 1] <- 0.999999
    test_set$predrf <- ifelse(test_set$predicted_rf<0.5,0,1)
    test_set$log_loss <- -((test_set$team1Victory * log(test_set$predicted_rf)) + 
                             ((1-test_set$team1Victory) * log(1-test_set$predicted_rf)))
    log_loss <- mean(test_set$log_loss)
    test_set$accurate <- ifelse(test_set$team1Victory == test_set$predrf, 1, 0)
    accuracy <- sum(test_set$accurate)/nrow(test_set)
    
    # Get Ranking on Kaggle
    kaggle_tmp <- kaggle[kaggle$season == i,]
    kaggle_tmp <- rbind(kaggle_tmp,
                        data.frame(season = i,
                                   subID = 999999,
                                   sub_name = "Bananalytics",
                                   score = log_loss))
    kaggle_tmp$rank <- rank(kaggle_tmp$score, 
                            na.last = NA, 
                            ties.method = "first")
    
    
    # Create data frame to save results
    tmp_results <- data.frame(time_stamp = as.POSIXlt(Sys.time()),
                              caret_model = mod_rf$method,
                              model_desc = mod_rf$modelInfo$label,
                              season = i,
                              log_loss = log_loss,
                              accuracy = accuracy,
                              kaggle_rank = kaggle_tmp$rank[kaggle_tmp$subID == 999999],
                              nbr_submissions = nrow(kaggle_tmp))
    
    tryCatch(if (exists("mod_results")==FALSE) {
      mod_results <- tmp_results
    } else {
      mod_results <- rbind(mod_results, tmp_results)
    },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})

  }
}

write.csv(mod_results, "results by caret model.csv", row.names = FALSE)


## Use for final 2020 model ##

mod_rf <- train(victory ~ .,
                data=train1final,
                method="bagFDA",
                trControl = ctrl,
                metric="ROC")

#### Excel Output ####
# Create dataframe of all possible game combinations w/ team seeds
h2h <- seeds %>% 
  filter(Season==2020) %>% 
  select(Seed, TeamID)

names(h2h) <- c("seed","teamID")
h2h$seed <- as.numeric(substr(h2h$seed,2,3))  

h2h_seeds <- expand.grid(h2h$teamID,
                         h2h$teamID)
names(h2h_seeds) <- c("team1","team2")

h2h_seeds <- merge(h2h,
                   h2h_seeds,
                   by.x = "teamID",
                   by.y = "team1")
names(h2h_seeds) <- c("team1","seed1","team2")

h2h_seeds <- merge(h2h,
                   h2h_seeds,
                   by.x = "teamID",
                   by.y = "team2")
names(h2h_seeds) <- c("team2", "seed2","team1","seed1")

# Combine Season Stats With tournament results
train2 <- merge(h2h_seeds,
                subset(teamAvgs,
                       season == 2020),
                by.x = "team1",
                by.y = "teamID")
traincol <- ncol(train2)
names(train2)[6:traincol] <- paste("team1_",names(train2)[6:traincol],sep = "")

train2 <- merge(train2,
                subset(teamAvgs,
                       season == 2020), 
                by.x = "team2",
                by.y = "teamID")
traincol2 <- ncol(train2)
names(train2)[(traincol + 1):traincol2] <- paste("team2_",
                                                 names(train2)[(traincol + 1):traincol2],
                                                 sep = "")

train2$team2_season.y <- NULL
colnames(train2)[colnames(train2)=="season.x"] <- "season"

# Delta in Seed ranking and season stats
team1col <- grep("team1",colnames(train2)[4:(traincol2 + 2)])
nbrcol <- ncol(train2)
for (i in team1col) {
  col_str <- substr(colnames(train2)[i+3],7,nchar(colnames(train2)[i+3]))
  j <- grep(paste0("team2_",col_str),colnames(train2)[4:(traincol2 + 2)])
  train2[,nbrcol + 1] <- train2[,i+3] - train2[,j+3]
  names(train2)[nbrcol + 1] <- paste0(col_str,"_diff")
  nbrcol <- nbrcol + 1
}
train2 <- train2[, -grep("team1_", colnames(train2))]
train2 <- train2[, -grep("team2_", colnames(train2))]
train2$seed_diff <- train2$seed1 - train2$seed2
train2$seed1 <- NULL
train2$seed2 <- NULL


# Predict 2020 Outcomes

pred2020 <- predict(mod_rf, train2, type="prob")
train2$pred <- pred2020[,2]
results2019 <- train2[,c("team1","team2","pred")]

results2020 <- merge(results2020,
                     teams[,c("TeamID","TeamName")],
                     by.x = "team1",
                     by.y = "TeamID")
colnames(results2020)[colnames(results2020)=="TeamName"] <- "team1Name"
results2020 <- merge(results2020,
                     teams[,c("TeamID","TeamName")],
                     by.x = "team2",
                     by.y = "TeamID")
colnames(results2020)[colnames(results2020)=="TeamName"] <- "team2Name"

results2020 <- merge(results2020,
                     results2020,
                     by.x = c("team1","team2"),
                     by.y = c("team2","team1"))
# Normalize predictions
results2020$finalprediction <- results2020$pred.x/(results2020$pred.x +
                                                     results2020$pred.y)

# Write final csv for Kaggle
kaggle_submission <- results2020[,c("team1","team2","finalprediction")]
kaggle_submission$year <- 2020
kaggle_submission <- kaggle_submission %>% 
  arrange(team1, team2)
kaggle_submission$ID <- paste(kaggle_submission$year,
                              kaggle_submission$team1,
                              kaggle_submission$team2,
                              sep = "_")
kaggle_submission <- kaggle_submission[,c("ID","finalprediction")]
names(kaggle_submission) <- c("ID","Pred")

write.csv(kaggle_submission, "final_kaggle_submission.csv", row.names = FALSE)


# Write final csv for Excel bracket
write.csv(results2020[,c("team1Name.x","team2Name.x","finalprediction")],
          "final_2020_pred_trim_new_2.csv",
          row.names = FALSE)


