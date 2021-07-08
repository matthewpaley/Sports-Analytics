rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(ggExtra)
library(gridExtra)
library(reshape2)
library(pROC)
library(zoo)
library(corrplot)
# library(xgboost)
# library(data.table)

source("CGWP.R")

teams <- read.csv("Teams.csv")
seasons <- read.csv("Seasons.csv")
seeds <- read.csv("NCAATourneySeeds.csv")
seas_results <- read.csv("RegularSeasonCompactResults.csv")
tour_results <- read.csv("NCAATourneyCompactResults.csv")
seas_detail <- read.csv("RegularSeasonDetailedResults.csv")
tour_detail <- read.csv("NCAATourneyDetailedResults.csv")
conferences <- read.csv("Conferences.csv")
team_conferences <- read.csv("TeamConferences.csv")
coaches <- read.csv("TeamCoaches.csv")
massey <- read.csv("MasseyOrdinals.csv")
seas_detail2018 <- read.csv("RegularSeasonDetailedResults.csv")
seeds2018 <- read.csv("actual_2018_teams.csv")
#runs_stats <- read.csv("runs_stats.csv")
cgwp_data <- read.csv("cgwp.csv")

# Functions
scale_01 <- function(x){(x-min(x))/(max(x)-min(x))}

seeds_names <- merge(x = seeds,
                     y = teams[,1:2],
                     by = "TeamID")
seeds_names$Seed <- as.numeric(substr(seeds_names$Seed,2,3))
seeds_names$one_seed <- ifelse(seeds_names$Seed==1,1,0)

#### Massey Data ####

## Pull massey rankings, pivot, and impute missing data
# Manually pulled the SystemNames with data since 2009
final_massey <- subset(massey,
                       RankingDayNum==133 & 
                         SystemName %in% c("BIH","CNG","COL","DCI",
                                           "DOK","DOL","DUN","KPK","KRA",
                                           "LMC","MOR","PIG","POM",
                                           "REW","RPI","RTH","SAG",
                                           "STH","WIL","WLK","WOB","WOL") &
                         Season>2002)
final_massey2 <- subset(massey,
                        RankingDayNum==128 & 
                          SystemName %in% c("BIH","CNG","COL","DCI",
                                            "DOK","DOL","DUN","KPK","KRA",
                                            "LMC","MOR","PIG","POM",
                                            "REW","RPI","RTH","SAG",
                                            "STH","WIL","WLK","WOB","WOL") &
                          Season==2019)
final_massey <- rbind(final_massey,
                      final_massey2)

# Pivot final_massey so SystemNames are columns
massey_tbl <- dcast(final_massey,
                    Season + RankingDayNum + TeamID ~ SystemName)
massey_trim <- massey_tbl[,-c(1:3)]

test <- which(is.na(massey_trim),arr.ind = TRUE)
massey_trim[test] <- rowMeans(massey_trim,na.rm = TRUE)[test[,1]]
massey_tbl <- cbind(massey_tbl[,1:3],
                    massey_trim)


#### Calculate Metrics ####

## Offensive team metrics
# Union winning and losing details
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
offHistory <- rbind(winnerHistory, loserHistory)
offHistory$fgm2 <- offHistory$fgmade - offHistory$fgm3
offHistory$fga2 <- offHistory$fgattempt - offHistory$fga3
offHistory$poss <- offHistory$fgattempt - offHistory$offreb + offHistory$turnover + 
  0.475*offHistory$ftattempt
offHistory$ppp <- offHistory$score / offHistory$poss
offHistory$fgperc <- offHistory$fgmade / offHistory$fgattempt
#offHistory$outcome <- ifelse(offHistory$victory == 1, "W","L")
offHistory$fg2perc <- offHistory$fgm2 / offHistory$fga2 
offHistory$fg3perc <- ifelse(offHistory$fga3 == 0, 0, offHistory$fgm3 / offHistory$fga3)
offHistory$ftperc <- ifelse(offHistory$ftattempt==0,0,offHistory$ftmade / offHistory$ftattempt)
offHistory$game_score <- offHistory$score + 0.4*offHistory$fgmade - 0.7*offHistory$fgattempt -
  0.4*(offHistory$ftattempt-offHistory$ftmade) + 0.7*offHistory$offreb + 0.3*offHistory$defreb +
  offHistory$steal + 0.7*offHistory$asst + 0.7*offHistory$block - 0.4*offHistory$pfoul - 
  offHistory$turnover

## Defensive team metrics
# Bind winner and loser df's to aggregate ranking
winnerHistory <- seas_detail[,c("Season","LTeamID","DayNum",
                                "WScore","NumOT","WFGM","WFGA",
                                "WFGM3","WFGA3","WFTM","WFTA",
                                "WOR","WDR","WAst","WTO","WStl",
                                "WBlk","WPF","WTeamID")]
winnerHistory$Victory <- 1
loserHistory <- seas_detail[,c("Season","WTeamID","DayNum",
                               "LScore","NumOT","LFGM","LFGA",
                               "LFGM3","LFGA3","LFTM","LFTA",
                               "LOR","LDR","LAst","LTO","LStl",
                               "LBlk","LPF","LTeamID")]
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
defHistory <- rbind(winnerHistory, loserHistory)
defHistory$fgm2 <- defHistory$fgmade - defHistory$fgm3
defHistory$fga2 <- defHistory$fgattempt - defHistory$fga3
defHistory$poss <- defHistory$fgattempt - defHistory$offreb + defHistory$turnover + 
  0.475*defHistory$ftattempt
defHistory$ppp <- defHistory$score / defHistory$poss
defHistory$fgperc <- defHistory$fgmade / defHistory$fgattempt
#defHistory$outcome <- ifelse(defHistory$victory == 1, "W","L")
defHistory$fg2perc <- defHistory$fgm2 / defHistory$fga2 
defHistory$fg3perc <- ifelse(defHistory$fga3 ==0,0,defHistory$fgm3 / defHistory$fga3)
defHistory$ftperc <- ifelse(defHistory$ftattempt==0,0,defHistory$ftmade / defHistory$ftattempt)
defHistory$game_score <- defHistory$score + 0.4*defHistory$fgmade - 0.7*defHistory$fgattempt -
  0.4*(defHistory$ftattempt-defHistory$ftmade) + 0.7*defHistory$offreb + 0.3*defHistory$defreb +
  defHistory$steal + 0.7*defHistory$asst + 0.7*defHistory$block - 0.4*defHistory$pfoul - 
  defHistory$turnover

## Calculate off team avgs
offteamAvgs <- aggregate(defHistory[,-c(1:3)],
                         by=list(defHistory$season,defHistory$teamID), 
                         function(x) c(mean = mean(x), sd = sd(x))) #creates grouped dataframe
offteamAvgs <- do.call("data.frame", offteamAvgs) # flatten groups
names(offteamAvgs)[1:2] <- c("season","teamID")
names(offteamAvgs) <- gsub("\\.","_",names(offteamAvgs))
offteamAvgs[,c("oppID_mean","oppID_sd","victory_sd","outcome_mean","outcome_sd")] <- NULL

## Calculate def team avgs
defteamAvgs <- aggregate(offHistory[,-c(1:3)],
                         by=list(offHistory$season,offHistory$teamID), 
                         function(x) c(mean = mean(x), sd = sd(x))) #creates grouped dataframe
defteamAvgs <- do.call("data.frame", defteamAvgs) # flatten groups
names(defteamAvgs)[1:2] <- c("season","teamID")
names(defteamAvgs) <- gsub("\\.","_",names(defteamAvgs))
defteamAvgs[,c("oppID_mean","oppID_sd","victory_sd","outcome_mean","outcome_sd")] <- NULL


### Bind Def Stats with Game by Game results
game_results <- merge(offHistory,
                      offteamAvgs,
                      by.x = c("season","oppID"),
                      by.y = c("season","teamID"))

# Loop through columns and inpute voa
c <- ncol(game_results)
for (i in c(5:19,21:29)) {
  newcol <- paste0("off_",names(game_results)[i],"_voa")
  meancol <- paste0(names(game_results)[i],"_mean")
  sdcol <- paste0(names(game_results)[i],"_sd")
  tryCatch(game_results[,c+1] <- pnorm(game_results[,names(game_results)[i]],
                                       game_results[,meancol],
                                       game_results[,sdcol])
           ,error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  tryCatch(names(game_results)[c+1] <- newcol
           ,error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  c <- c + 1
}

# remove off mean & sd columns
game_results <- game_results[, -grep("_mean", colnames(game_results))]
game_results <- game_results[, -grep("_sd", colnames(game_results))]


# Bind Off Stats With Game by Game results
game_results2 <- merge(defHistory,
                       defteamAvgs,
                       by.x = c("season","oppID"),
                       by.y = c("season","teamID"))

# Loop through columns and inpute voa
c <- ncol(game_results2)
for (i in c(5:19,21:29)) {
  newcol <- paste0("def_",names(game_results2)[i],"_voa")
  meancol <- paste0(names(game_results2)[i],"_mean")
  sdcol <- paste0(names(game_results2)[i],"_sd")
  tryCatch(game_results2[,c+1] <- pnorm(game_results2[,names(game_results2)[i]],
                                        game_results2[,meancol],
                                        game_results2[,sdcol])
           ,error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  tryCatch(names(game_results2)[c+1] <- newcol
           ,error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  c <- c + 1
}

# remove off mean & sd columns
game_results2 <- game_results2[, -grep("_mean", colnames(game_results2))]
game_results2 <- game_results2[, -grep("_sd", colnames(game_results2))]


# merge both game_results df's
game_results <- merge(game_results,
                      game_results2,
                      by = c("season","teamID","daynum"))

game_results <- game_results[, -grep("\\.y", colnames(game_results))]
colnames(game_results) <- gsub("\\.x", "",  colnames(game_results))

# Get quadrant wins
massey_trim$avg_rank <- rowMeans(massey_trim,na.rm = FALSE)
massey_avg <- cbind(massey_tbl[,1:3],
                    massey_trim$avg_rank)
names(massey_avg) <- c("season", "daynum", "teamID", "avg_rank")
game_results2 <- merge(game_results,
                       massey_avg,
                       by.x = c("season", "oppID"),
                       by.y = c("season", "teamID"))
game_results2 <- game_results2[, -grep("\\.y", colnames(game_results2))]
colnames(game_results2) <- gsub("\\.x", "",  colnames(game_results2))
game_results2$quadrant1 <- ifelse(game_results2$victory == 1 & 
                                    game_results2$avg_rank < 51,1,0)
game_results2$quadrant2 <- ifelse(game_results2$victory == 1 & 
                                    game_results2$avg_rank > 50 &
                                    game_results2$avg_rank < 101,1,0)
nbr_teams <- length(unique(game_results2$teamID))
game_results2$win_pts <- ifelse(game_results2$victory == 1, 
                                nbr_teams - game_results2$avg_rank,0)

# Multiply VOA columns by inverse avg_rank
c <- grep("voa",colnames(game_results2))
for (i in c) {
  game_results2[,i] <- game_results2[,i] * (nbr_teams - game_results2$avg_rank)
}


# Get record of last 10 games prior to tournament
game_results2 <- game_results2 %>% 
  arrange(season, teamID, daynum) %>% 
  mutate(last_10 = rollsumr(victory, k = 10, fill = 5)/10)

game_results_last_game <- game_results2 %>% 
  group_by(season, teamID) %>% 
  filter(daynum == max(daynum)) %>% 
  select(season, teamID, daynum, last_10)

game_results <- merge(game_results2[,-which(names(game_results2) %in% c("last_10"))],
                      game_results_last_game[,c("season","teamID","last_10")],
                      by = c("season","teamID"))


# Summarize game results into season results
game_results_mean <- aggregate(game_results[,c(5:19,21:29,78,82)],
                               by=list(game_results$season,game_results$teamID), 
                               function(x) c(mean = mean(x))) 
game_results_sum <- aggregate(game_results[,grep("voa", colnames(game_results))],
                              by=list(game_results$season,game_results$teamID), 
                              function(x) c(sum = sum(x)))
game_results_sum2 <- aggregate(game_results[,c("victory","quadrant1","quadrant2","win_pts")],
                               by=list(game_results$season,game_results$teamID), 
                               function(x) c(sum = sum(x)))
teamAvgs <- merge(game_results_mean,
                  game_results_sum,
                  by = c("Group.1","Group.2"))
teamAvgs <- merge(teamAvgs,
                  game_results_sum2,
                  by = c("Group.1","Group.2"))
names(teamAvgs)[1:2] <- c("season","teamID")


## Summarize game by game data into season stats


# Combine Massey & Season Stats

season_stats <- merge(x = teamAvgs,
                      y = massey_tbl[,names(massey_tbl) != "RankingDayNum"],
                      by.x = c("teamID","season"),
                      by.y = c("TeamID","Season"))

# Combine CGWP & Season stats
season_stats <- merge(season_stats,
                      cgwp_data,
                      by = c("season","teamID"))


# Normalize tournament results
tour_results$team1 <- ifelse(tour_results$WTeamID > tour_results$LTeamID, 
                             tour_results$LTeamID, 
                             tour_results$WTeamID) 
tour_results$team2 <- ifelse(tour_results$WTeamID > tour_results$LTeamID, 
                             tour_results$WTeamID, 
                             tour_results$LTeamID)
tour_results$team1Victory <- ifelse(tour_results$WTeamID == tour_results$team1, 1, 0) 
tour_results$score_diff <- ifelse(tour_results$WTeamID == tour_results$team1, 
                                  tour_results$WScore - tour_results$LScore,
                                  tour_results$LScore - tour_results$WScore)

# Combine Season Stats With tournament results
train1 <- merge(x = tour_results[,c("Season","DayNum","team1","team2","team1Victory")],
                y = season_stats,
                by.x = c("Season","team1"),
                by.y = c("season","teamID"))
traincol <- ncol(train1)
names(train1)[6:traincol] <- paste("team1_",names(train1)[6:traincol],sep = "")
train1 <- merge(x = train1,
                y = season_stats, 
                by.x = c("Season","team2"),
                by.y = c("season","teamID"))
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

# Delta comparing off vs. off
for (i in team1col) {
  col_str <- substr(colnames(train1)[i+5],7,nchar(colnames(train1)[i+5]))
  j <- grep(paste0("team2_",col_str),colnames(train1)[6:(traincol2+2)])
  train1[,nbrcol + 1] <- train1[,i+5] - train1[,j+5]
  names(train1)[nbrcol + 1] <- paste0(col_str,"_diff")
  nbrcol <- nbrcol + 1
}


train1 <- train1[, -grep("team1_", colnames(train1))]
train1 <- train1[, -grep("team2_", colnames(train1))]
train1$seed_diff <- train1$seed1 - train1$seed2
train1$seed1 <- NULL
train1$seed2 <- NULL
train1$victory <- ifelse(train1$team1Victory==1,"yes","no")
train1$victory <- as.factor(train1$victory)


#### Build Model ####

# Check Correlation, Remove Highly Correlated Variables
numeric_cols <- unlist(lapply(train1,is.numeric))
mat <- cor(train1[,numeric_cols])
mat[!rowSums(!is.finite(mat)),]
mat[!is.finite(mat)] <- 0
colnames(mat) <- paste0("f",1:ncol(mat))
rownames(mat) <- paste0("f",1:nrow(mat))
corrplot(mat, diag = FALSE, method = "color")
rem_col <- findCorrelation(mat, 
                           cutoff = 0.9,
                           verbose = FALSE)
train1 <- train1[,-c(rem_col)]
train1 <- train1[!(train1$Season==2018),]


# Start by building a model on everything to get Feature Importance
rf <- randomForest(victory ~ .,
                   data=train1[,-c(1:5)],
                   ntree=200,
                   type='ROC',
                   importance= TRUE)

# Feature Importance
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), 
                                Importance=imp[,1])
non_neg_imp <- featureImportance %>% 
  filter(Importance > 1) %>% 
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
train1final <- cbind(train1[,c(1:5)], 
                     train1final)
train1final$victory <- ifelse(train1final$team1Victory==1,"yes","no")
train1final$victory <- as.factor(train1final$victory)
train1final$team1Victory <- NULL

# Create train and test sets on pre-processed dataset
set.seed(1217)
train_nbr <- createDataPartition(train1final$victory,
                                 p=0.7, 
                                 list=FALSE)
train_set <- train1final[train_nbr,]
test_set <- train1final[-train_nbr,]

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     savePred = T,
                     classProb = T,
                     summaryFunction = twoClassSummary)
tune_grid <- expand.grid(nrounds = 200,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0)

# Retrain models using all features
mod_svm <- train(victory ~ .,
                data=train_set[-c(1:5)],
                method="svmLinear",
                trControl = ctrl,
                tuneLength=10,
                metric="ROC")

mod_rf <- train(victory ~ .,
                data=train_set[-c(1:5)],
                method="rf",
                trControl = ctrl,
                metric="ROC")

mod_knn <- train(victory ~ .,
                 data=train_set[-c(1:5)],
                 method="xgbTree",
                 trControl = ctrl,
                 metric="ROC",
                 tuneGrid = tune_grid,
                 tuneLength = 10)


pred_svm <- predict(mod_svm, test_set, type="prob")
pred_rf <- predict(mod_rf, test_set, type="prob")
pred_knn <- predict(mod_knn, test_set, type="prob")

test_set$team1Victory <- ifelse(test_set$victory == "yes",1,0)
test_set$predicted_rf <- pred_rf[,2]
test_set$predicted_svm <- pred_svm[,2]
test_set$predicted_knn <- pred_knn[,2]
test_set$avg_pred <- (test_set$predicted_rf + test_set$predicted_svm +test_set$predicted_knn)/3

test_set$predrf <- ifelse(test_set$predicted_rf<0.5,0,1)
test_set$predsvm <- ifelse(test_set$predicted_svm<0.5,0,1)
test_set$predknn <- ifelse(test_set$predicted_knn<0.5,0,1)
test_set$avg_pred <- ifelse(test_set$avg_pred<0.5,0,1)

caret::confusionMatrix(test_set$team1Victory,test_set$predrf)
caret::confusionMatrix(test_set$team1Victory,test_set$predsvm)
caret::confusionMatrix(test_set$team1Victory,test_set$predknn)
caret::confusionMatrix(test_set$team1Victory,test_set$avg_pred)

# Use this for the final model

mod_rf <- train(victory ~ .,
                data=train1final[-c(1:5)],
                method="rf",
                trControl = ctrl,
                metric="ROC")


#### Kaggle Output ####

# Generate all possible combinations of matchups
h2h <- seeds %>% 
  filter(Season==2019) %>% 
  select(Seed, TeamID)

names(h2h) <- c("seed","teamID")
h2h$seed <- as.numeric(substr(h2h$seed,2,3))  

h2h_seeds <- expand.grid(h2h$teamID,
                   h2h$teamID)
names(h2h_seeds) <- c("team1","team2")

h2h_seeds <- h2h_seeds %>% 
  arrange(team1, team2) %>% 
  subset(team1 < team2)

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
                subset(season_stats,
                       season == 2019),
                by.x = "team1",
                by.y = "teamID")
traincol <- ncol(train2)
names(train2)[6:traincol] <- paste("team1_",names(train2)[6:traincol],sep = "")

train2 <- merge(train2,
                subset(season_stats,
                       season == 2018), 
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


# Predict 2019 Outcomes

pred2019 <- predict(rf, train2, type="prob")
#pred2019 <- predict(mod_rf, train2, type="prob")
train2$pred <- pred2019[,2]
results2019 <- train2[,c("team1","team2","pred")]
results2019 <- results2019 %>% 
  arrange(team1,team2)

# Kaggle Output
results2019$id <- paste("2019",results2019$team1,results2019$team2,sep = "_")
write.csv(select(results2019,c("id","pred")),"kaggle_submission_1.csv",row.names = FALSE)


#### Excel Output ####
h2h <- seeds %>% 
  filter(Season==2019) %>% 
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
                subset(season_stats,
                       season == 2019),
                by.x = "team1",
                by.y = "teamID")
traincol <- ncol(train2)
names(train2)[6:traincol] <- paste("team1_",names(train2)[6:traincol],sep = "")

train2 <- merge(train2,
                subset(season_stats,
                       season == 2018), 
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


# Predict 2019 Outcomes

#pred2019 <- predict(rf, train2, type="prob")
pred2019 <- predict(mod_rf, train2, type="prob")
train2$pred <- pred2019[,2]
results2019 <- train2[,c("team1","team2","pred")]

results2019 <- merge(results2019,
                     teams[,c("TeamID","TeamName")],
                     by.x = "team1",
                     by.y = "TeamID")
colnames(results2019)[colnames(results2019)=="TeamName"] <- "team1Name"
results2019 <- merge(results2019,
                     teams[,c("TeamID","TeamName")],
                     by.x = "team2",
                     by.y = "TeamID")
colnames(results2019)[colnames(results2019)=="TeamName"] <- "team2Name"

results2019 <- merge(results2019,
                     results2019,
                     by.x = c("team1","team2"),
                     by.y = c("team2","team1"))
results2019$finalprediction <- results2019$pred.x/(results2019$pred.x +
                                                          results2019$pred.y)

# Write final csv
write.csv(results2019[,c("team1Name.x","team2Name.x","finalprediction")],
          "final_2019_pred_trim.csv",
          row.names = FALSE)

