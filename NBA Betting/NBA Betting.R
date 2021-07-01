rm(list=ls())

library(rvest)
library(dplyr)
library(readr)
library(randomForest)
library(caret)
library(reshape2)
library(zoo)
library(nbastatR)
library(stringr)
library(matrixStats)
library(chron)
library(tidyverse)

elo <- as.data.frame(read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/nba-elo/nbaallelo.csv"))

#Site to scrape for the spreads?
#https://www.sportsbookreview.com/betting-odds/nba-basketball/pointspread/

#Scrape Historical NBA Games
NBA_Games <- scrapeNBA(2007,2019)

# Align teams with their historical franchises for Elo consistency
NBA_Games$`Home/Neutral`<- franchiseMatch(NBA_Games, "Home/Neutral")
NBA_Games$`Visitor/Neutral` <- franchiseMatch(NBA_Games, "Visitor/Neutral")

#Create Team master table
teams <- NBA_Games %>%
  select(`Home/Neutral`) %>%
  unique()
teams <- teams %>%
  rename(Team = `Home/Neutral`)%>%
  mutate(franID = match(Team,Team)) %>%
  arrange(franID)

# Get team abbreviations
teams$Abbr <- abbr(teams, "Team")

#Merge to get franIDs
NBA_Games <- merge(NBA_Games,teams, by.x = "Home/Neutral", by.y = "Team") %>%
  select(-Abbr) %>%
  rename(homeID = franID)
NBA_Games <- merge(NBA_Games,teams, by.x = "Visitor/Neutral", by.y = "Team") %>%
  select(-Abbr) %>%
  rename(awayID = franID) %>%
  arrange(Date)

#Get Historical Elo
Elo_Games <- NBA_Games

Elo_Games$homeElo <- 1300
Elo_Games$awayElo <- 1300

teams$elo <- 1300

for(i in 1:(nrow(Elo_Games)-1)) {
  h <- Elo_Games$homeID[i]
  a <- Elo_Games$awayID[i]
  
  Elo_Games$homeElo[i] <- teams$elo[h]
  Elo_Games$awayElo[i] <- teams$elo[a]
  
  tmp <- updateElo(Elo_Games$PTS.1[i], Elo_Games$PTS[i],teams$elo[h], teams$elo[a])
  
  #Elo_Games$homeElo_post[i] <- Elo_Games$homeElo[i] + tmp[1]
  #Elo_Games$awayElo_post[i] <- Elo_Games$awayElo[i] + tmp[2]
  
  teams$elo[h] <- teams$elo[h] + tmp[1]
  teams$elo[a] <- teams$elo[a] + tmp[2]
}

######## Functionize feature engineering
# MODEL Feature Engineering
all_games <- Elo_Games %>%
  rename(home = `Home/Neutral`, 
         away = `Visitor/Neutral`,
         win = HomeWin,
         awayPTS = PTS,
         homePTS = PTS.1)
names(all_games) <- tolower(names(all_games))
all_games$num_date <- as.numeric(as.Date(all_games$date))

all_games$month <- substr(as.character(all_games$date),6,7)
all_games$year <- substr(as.character(all_games$date),1,4)
all_games$season <- all_games$year

all_games$season <- ifelse(as.integer(all_games$month)>8,
                           as.integer(all_games$year)+1, 
                           as.integer(all_games$year))

prev_game <- melt(all_games[,c("num_date","home","away",
                               "win","homeelo","awayelo")],
                  id=c("num_date","win","homeelo","awayelo"))
prev_game <- unique(prev_game)
prev_game <- prev_game %>% 
  group_by(value) %>% 
  arrange(num_date) %>% 
  mutate(rest_days = num_date - lag(num_date, default = first(num_date)),
         elo_change = ifelse(variable == "home", 
                             homeelo - lag(homeelo, default = first(homeelo)), 
                             awayelo - lag(awayelo, default = first(awayelo))))
for(i in 1:nrow(prev_game)) {
  if(prev_game$variable[i] == "away" && prev_game$win[i] == 1) { 
    prev_game$win[i] = 0 
  }
  else if(prev_game$variable[i] == "away" && prev_game$win[i] == 0) { 
    prev_game$win[i] = 1 }
}

prev_game <- prev_game %>% 
  arrange(value,num_date) %>% 
  mutate(last_3 = lag(rollsumr(win, k = 3, fill = NA)/3,default = NA),
         last_5 = lag(rollsumr(win, k = 5, fill = NA)/5,default = NA),
         last_10 = lag(rollsumr(win, k = 10, fill = NA)/10,default = NA),
         last_3_elo = lag(rollsumr(elo_change, k = 3, fill = NA)/3, default = NA),
         last_5_elo = lag(rollsumr(elo_change, k = 5, fill = NA)/5, default = NA),
         last_10_elo = lag(rollsumr(elo_change, k = 10, fill = NA)/10, default = NA))

# Remove NA, split into home and away, change column names, and rbind
prev_game <- prev_game[!is.na(prev_game$last_10),]
prev_game_home <- prev_game[prev_game$variable=="home",]
prev_game_home$variable <- NULL
prev_game_home$win <- NULL
prev_game_home$awayelo <- NULL
colnames(prev_game_home) <- c("num_date","home_elo","home",
                              "home_rest_days", "home_elo_change",
                              "home_last3", "home_last5","home_last10",
                              "home_last3_elo", "home_last5_elo","home_last10_elo")

prev_game_away <- prev_game[prev_game$variable=="away",]
prev_game_away$variable <- NULL
prev_game_away$win <- NULL
prev_game_away$homeelo <- NULL
colnames(prev_game_away) <- c("num_date","away_elo","away",
                              "away_rest_days", "away_elo_change",
                              "away_last3", "away_last5","away_last10",
                              "away_last3_elo", "away_last5_elo","away_last10_elo")

all_games <- merge(all_games,
                   prev_game_home,
                   by = c("num_date","home"))

all_games <- merge(all_games,
                   prev_game_away,
                   by = c("num_date","away"))

all_games$rest_days_diff <- all_games$home_rest_days - all_games$away_rest_days
all_games$last3_diff <- all_games$home_last3 - all_games$away_last3
all_games$last5_diff <- all_games$home_last5 - all_games$away_last5
all_games$last10_diff <- all_games$home_last10 - all_games$away_last10
all_games$elo_diff <- all_games$home_elo - all_games$away_elo
all_games$last3_elo_diff <- all_games$home_last3_elo - all_games$away_last3_elo
all_games$last5_elo_diff <- all_games$home_last5_elo - all_games$away_last5_elo
all_games$last10_elo_diff <- all_games$home_last10_elo - all_games$away_last10_elo
all_games$home_rest_days <- NULL
all_games$home_last3 <- NULL
all_games$home_last5 <- NULL
all_games$home_last10 <- NULL
all_games$away_rest_days <- NULL
all_games$away_last3 <- NULL
all_games$away_last5 <- NULL
all_games$home_last3_elo <- NULL 
all_games$home_last5_elo <- NULL
all_games$home_last10_elo <- NULL
all_games$away_last3_elo <- NULL 
all_games$away_last5_elo <- NULL
all_games$away_last10_elo <- NULL

all_games$win <- as.factor(all_games$win)

##Spread cleaning
allData <- spreadClean(allData, "topodds")
allData <- spreadClean(allData, "bottomodds")
allData <- spreadClean(allData, "bettopodds")
allData <- spreadClean(allData, "betbotodds")
allData <- spreadClean(allData, "botopodds")
allData <- spreadClean(allData, "bobotodds")

allData$topAvg <- rowMeans(allData[,c(35,37,39,41)])
allData$botAvg <- rowMeans(allData[,c(36,38,40,42)])

allData$topSd <- rowSds(as.matrix(allData[,c(35,37,39,41)]))
allData$botSd <- rowSds(as.matrix(allData[,c(36,38,40,42)]))

allData$homeOdds1 <- ifelse(allData$home == allData$topteam, 
                            allData$topodds,allData$bottomodds)
allData$homeOdds3 <- ifelse(allData$home == allData$topteam, 
                            allData$bettopodds,allData$betbotodds)
allData$homeOdds4 <- ifelse(allData$home == allData$topteam, 
                            allData$botopodds,allData$bobotodds)
allData$homeMean <- ifelse(allData$home == allData$topteam, 
                           allData$topAvg,allData$botAvg)
allData$homeSD <- ifelse(allData$home == allData$topteam, 
                         allData$topSd,allData$botSd)

#Don't really need these, they're opposite of home odds
allData$awayOdds1 <- ifelse(allData$away == allData$topteam, 
                            allData$topodds,allData$bottomodds)
allData$awayOdds3 <- ifelse(allData$away == allData$topteam, 
                            allData$bettopodds,allData$betbotodds)
allData$awayOdds4 <- ifelse(allData$away == allData$topteam, 
                            allData$botopodds,allData$bobotodds)
allData$awayMean <- ifelse(allData$away == allData$topteam, 
                           allData$topAvg,allData$botAvg)
allData$awaySD <- ifelse(allData$away == allData$topteam, 
                         allData$topSd,allData$botSd)



## Model ##
last_season <- allData[allData$season == max(allData$season),]
hist_seasons <- allData[allData$season != max(allData$season),]

ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 2,
                     savePred = T,
                     classProb = F)

partition <- createDataPartition(hist_seasons$win, 
                                 p=.7, list = FALSE)

hist_train <- hist_seasons[partition,]
hist_test <- hist_seasons[-partition,]

seasons <- unique(hist_train$season)
hist_train$win <- ifelse(hist_train$win == 0, "no","yes")


# Win Accuracy #

# ,"LogitBoost", "ada",
# "bagFDA","parRF","wsrf",
# "glmboost","C5.0","xgbLinear",
# "knn","svmLinearWeights"

for (m in c("rf")) {
  for (i in seasons) {
    print(paste0("Testing ",m," model against ",i," season results..."))
    # Make test set the year i
    train_set <- hist_train[hist_train$season != i,]
    test_set <- hist_train[hist_train$season == i,]
    
    mod_rf <- train(win ~ elo_diff + rest_days_diff + 
                      last3_elo_diff + last5_elo_diff + 
                      last10_elo_diff + last3_diff + 
                      last5_diff + last10_diff + 
                      homeOdds1 + homeOdds2 +
                      homeOdds3 + homeOdds4 +
                      homeMean + homeSD,
                    data=train_set,
                    method=m,
                    trControl = ctrl,
                    metric="ROC")
    
    pred_rf <- predict(mod_rf, test_set, type="prob")
    
    test_set$home_win <- ifelse(test_set$win == "yes",1,0)
    test_set$predicted_rf <- pred_rf[,2]
    test_set$predicted_rf[test_set$predicted_rf == 0] <- 0.000001
    test_set$predicted_rf[test_set$predicted_rf == 1] <- 0.999999
    test_set$predrf <- ifelse(test_set$predicted_rf<0.5,0,1)
    test_set$log_loss <- -((test_set$home_win * log(test_set$predicted_rf)) + 
                             ((1-test_set$home_win) * log(1-test_set$predicted_rf)))
    log_loss <- mean(test_set$log_loss)
    test_set$accurate <- ifelse(test_set$home_win == test_set$predrf, 1, 0)
    accuracy <- sum(test_set$accurate)/nrow(test_set)
    
    test_set$max_prob <- do.call('pmax',pred_rf)
    test_set$max_prob_rnd <- round(do.call('pmax',pred_rf),1)
    
    # Create data frame to save results
    tmp_results <- data.frame(time_stamp = as.POSIXlt(Sys.time()),
                              caret_model = mod_rf$method,
                              model_desc = mod_rf$modelInfo$label,
                              season = i,
                              log_loss = log_loss,
                              accuracy = accuracy)
    
    tryCatch(if (exists("mod_results")==FALSE) {
      mod_results <- test_set #tmp_results
    } else {
      mod_results <- rbind(mod_results, test_set) #tmp_results)
    },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
    
  }
}



# Predict over the entire dataset
{
  rf <- randomForest(win ~ elo_diff + rest_days_diff + 
                       last3_elo_diff + last5_elo_diff + 
                       last10_elo_diff + last3_diff + 
                       last5_diff + last10_diff +
                       homeOdds1 + homeOdds2 +
                       homeOdds3 + homeOdds4 +
                       homeMean + homeSD,
                     hist_seasons,
                     ntree = 300,
                     importance = TRUE)
  last_season$prediction <- predict(rf, last_season, type = "response")
  confusionMatrix(last_season$prediction, last_season$win)
  
  last_season$max_prob <- do.call('pmax',last_season$prediction)
  test_set$max_prob_rnd <- round(do.call('pmax',pred_rf),1)
  
  # Feature Importance
  imp <- importance(rf, type=1)
  featureImportance <- data.frame(Feature=row.names(imp), 
                                  Importance=imp[,1])
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
  
  write.csv(mod_results, "Win Model Results.csv", row.names = FALSE)
}

## Spreads ##
hist_train$result1 <- ifelse(hist_train$homepts - hist_train$awaypts +
                               round(hist_train$homeMean,0) > 0, "yes",
                             ifelse(hist_train$homepts - hist_train$awaypts +
                                      round(hist_train$homeMean,0) == 0, "push","no"))

## More predicting spreads

for (m in c("rf"#,"LogitBoost","parRF","wsrf","knn"
)) {
  for (i in seasons) {
    
    print(paste0("Testing ",m," model against ",i," season results..."))
    # Make test set the year i
    train_set <- hist_train[hist_train$season != i,]
    test_set <- hist_train[hist_train$season == i,]
    
    mod_rf <- train( ~ elo_diff + rest_days_diff + 
                       last3_elo_diff + last5_elo_diff + 
                       last10_elo_diff + last3_diff + 
                       last5_diff + last10_diff + 
                       homeOdds1 + homeOdds2 +
                       homeOdds3 + homeOdds4 +
                       homeMean + homeSD,
                     data=train_set,
                     method=m,
                     trControl = ctrl,
                     metric="ROC")
    
    pred_rf <- predict(mod_rf, test_set, type="raw")
    table(pred_rf, test_set$result1)
    test_set$predicted_rf <- pred_rf
    pred_prob <- predict(mod_rf, test_set, type = "prob")
    test_set$max_prob <- do.call('pmax',pred_prob)
    test_set$max_prob_rnd <- round(do.call('pmax',pred_prob),1)
    test_set$net_money <- ifelse(test_set$result1=="push",0,
                                 ifelse(test_set$predicted_rf=="push",0,
                                        ifelse(test_set$result1==test_set$predicted_rf,100,-110)))
    test_set$accurate <- ifelse(test_set$result1 == test_set$predicted_rf, 1, 0)
    test_set$caret_model <- mod_rf$method
    test_set$model_desc = mod_rf$modelInfo$label
    
    tryCatch(if (exists("test_results")==FALSE) {
      test_results <- test_set
    } else {
      test_results <- rbind(test_results, test_set)
    },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
    
  }
}

write.csv(test_results, "Spread Model Results.csv", row.names = FALSE)


## Spreads by predicting scores

for (m in c("rf"#,"LogitBoost","parRF","wsrf","knn"
)) {
  for (i in seasons) {
    
    print(paste0("Testing ",m," model against ",i," season results..."))
    # Make test set the year i
    train_set <- hist_train[hist_train$season != i,]
    test_set <- hist_train[hist_train$season == i,]
    
    mod_rf <- train(homediff ~ elo_diff + rest_days_diff + 
                      last3_elo_diff + last5_elo_diff + 
                      last10_elo_diff + last3_diff + 
                      last5_diff + last10_diff + 
                      homeOdds1 + homeOdds2 +
                      homeOdds3 + homeOdds4 +
                      homeMean + homeSD,
                    data=train_set,
                    method=m,
                    trControl = ctrl,
                    metric="RMSE")
    
    pred_rf <- predict(mod_rf, test_set, type="raw")
    table(pred_rf, test_set$homeMean)
    test_set$predicted_rf <- ceiling(pred_rf)
    
    test_set$result <- ifelse((test_set$predicted_rf < test_set$homeMean && test_set$homeMean < 0) ||
                                (test_set$predicted_rf > test_set$homeMean && test_set$homeMean > 0),
                              "yes",
                              ifelse(test_set$predicted_rf == test_set$homeMean || test_set$homeMean == 0,
                                     "push", "no"))
    
    test_set$accurate <- ifelse(test_set$result1 == test_set$predicted_rf, 1, 0)
    test_set$caret_model <- mod_rf$method
    test_set$model_desc = mod_rf$modelInfo$label
    
    tryCatch(if (exists("test_results")==FALSE) {
      test_results <- test_set
    } else {
      test_results <- rbind(test_results, test_set)
    },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
    
  }
}

write.csv(test_results, "Spread Model Results.csv", row.names = FALSE)
