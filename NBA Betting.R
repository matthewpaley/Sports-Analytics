rm(list=ls())

library(rvest)
library(dplyr)
library(readr)
library(randomForest)
library(caret)
library(reshape2)
library(zoo)
library(nbastatR)

elo <- as.data.frame(read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/nba-elo/nbaallelo.csv"))

#Site to scrape for the spreads?
#https://www.sportsbookreview.com/betting-odds/nba-basketball/pointspread/

#Scrape NBA Games
NBA_Games <- scrapeNBA(2007,2019)

#Format Data
NBA_Games <- NBA_Games[which(NBA_Games$Date!="Playoffs"),]
NBA_Games$`Home/Neutral`[NBA_Games$`Home/Neutral` 
                         == "New Orleans/Oklahoma City Hornets"] <- "New Orleans Hornets"
NBA_Games$`Visitor/Neutral`[NBA_Games$`Visitor/Neutral` 
                         == "New Orleans/Oklahoma City Hornets"] <- "New Orleans Hornets"
NBA_Games$Date <- as.Date(NBA_Games$Date,"%a, %b %d, %Y")

# Align teams with their historical franchises for Elo consistency
teams <- NBA_Games %>%
  select(`Home/Neutral`) %>%
  unique()
teams <- teams %>%
  rename(Team = `Home/Neutral`)%>%
  mutate(franID = match(Team,Team)) %>%
  arrange(franID)


teams$Abbr <- teams$Team
teams$Abbr[teams$Abbr == "New Jersey Nets"] <- "NJN"
teams$Abbr[teams$Abbr == "Milwaukee Bucks"] <- "MIL"
teams$Abbr[teams$Abbr == "New York Knicks"] <- "NYK"
teams$Abbr[teams$Abbr == "Washington Wizards"] <- "WAS"
teams$Abbr[teams$Abbr == "Houston Rockets"] <- "HOU"
teams$Abbr[teams$Abbr == "Los Angeles Lakers"] <- "LAL"
teams$Abbr[teams$Abbr == "Minnesota Timberwolves"] <- "MIN"
teams$Abbr[teams$Abbr == "Memphis Grizzlies"] <- "MEM"
teams$Abbr[teams$Abbr == "Portland Trail Blazers"] <- "POR"
teams$Abbr[teams$Abbr == "Charlotte Hornets"] <- "CHO"
teams$Abbr[teams$Abbr == "Los Angeles Clippers"] <- "LAC"
teams$Abbr[teams$Abbr == "Phoenix Suns"] <- "PHO"
teams$Abbr[teams$Abbr == "Utah Jazz"] <- "UTA"
teams$Abbr[teams$Abbr == "Chicago Bulls"] <- "CHI"
teams$Abbr[teams$Abbr == "Toronto Raptors"] <- "TOR"
teams$Abbr[teams$Abbr == "Denver Nuggets"] <- "DEN"
teams$Abbr[teams$Abbr == "Philadelphia 76ers"] <- "PHI"
teams$Abbr[teams$Abbr == "New Orleans Hornets"] <- "NOH"
teams$Abbr[teams$Abbr == "Sacramento Kings"] <- "SAC"
teams$Abbr[teams$Abbr == "New Orleans Pelicans"] <- "NOP"
teams$Abbr[teams$Abbr == "Boston Celtics"] <- "BOS"
teams$Abbr[teams$Abbr == "Golden State Warriors"] <- "GSW"
teams$Abbr[teams$Abbr == "Charlotte Bobcats"] <- "CHA"
teams$Abbr[teams$Abbr == "San Antonio Spurs"] <- "SAS"
teams$Abbr[teams$Abbr == "Oklahoma City Thunder"] <- "OKC"
teams$Abbr[teams$Abbr == "Dallas Mavericks"] <- "DAL"
teams$Abbr[teams$Abbr == "Indiana Pacers"] <- "IND"
teams$Abbr[teams$Abbr == "Detroit Pistons"] <- "DET"
teams$Abbr[teams$Abbr == "Atlanta Hawks"] <- "ATL"
teams$Abbr[teams$Abbr == "Orlando Magic"] <- "ORL"
teams$Abbr[teams$Abbr == "Cleveland Cavaliers"] <- "CLE"
teams$Abbr[teams$Abbr == "Miami Heat"] <- "MIA"
teams$Abbr[teams$Abbr == "Brooklyn Nets"] <- "BRK"
teams$Abbr[teams$Abbr == "Seattle SuperSonics"] <- "SEA"

# currentFranchises <- list()
# currentFranchises[["Hornets"]] <- list("Charlotte Hornets", "New Orleans Hornets")

#Merge to get franIDs
NBA_Games <- merge(NBA_Games,teams, by.x = "Home/Neutral", by.y = "Team") %>%
  select(-Abbr) %>%
  rename(homeID = franID)
NBA_Games <- merge(NBA_Games,teams, by.x = "Visitor/Neutral", by.y = "Team") %>%
  select(-Abbr) %>%
  rename(awayID = franID) %>%
  arrange(Date)

#Get Elo
Elo_Games <- NBA_Games
Elo_Games$homeElo <- 1500
Elo_Games$awayElo <- 1500

teams$startingElo <- 1500
teams$elo <- teams$startingElo

for(i in 1:nrow(Elo_Games)-1) {
  h <- Elo_Games$homeID[i]
  a <- Elo_Games$awayID[i]
  tmp <- updateElo(Elo_Games$PTS.1[i], Elo_Games$PTS[i],teams$elo[h], teams$elo[a])
  Elo_Games$homeElo[i] <- Elo_Games$homeElo[i] + tmp[1]
  Elo_Games$awayElo[i] <- Elo_Games$awayElo[i] + tmp[2]
  teams$elo[h] <- teams$elo[h] + tmp[1]
  teams$elo[a] <- teams$elo[a] + tmp[2]
}



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
for (i in seq_along(all_games)) {
all_games$season[i] <- if(all_games$month[i]>8) {
  all_games$season[i] <- all_games$year[i]+1
}
}

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
  mutate(last_3 = rollsumr(win, k = 3, fill = NA)/3,
         last_5 = rollsumr(win, k = 5, fill = NA)/5,
         last_10 = rollsumr(win, k = 10, fill = NA)/10,
         last_3_elo = rollsumr(elo_change, k = 3, fill = NA)/3,
         last_5_elo = rollsumr(elo_change, k = 5, fill = NA)/5,
         last_10_elo = rollsumr(elo_change, k = 10, fill = NA)/10)

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


## Model ##
last_season <- all_games[all_games$season == max(all_games$season),]
hist_seasons <- all_games[all_games$season != max(all_games$season),]

ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 2,
                     savePred = T,
                     classProb = T)

partition <- createDataPartition(hist_seasons$win, 
                                 p=.7, list = FALSE)

hist_train <- hist_seasons[partition,]
hist_test <- hist_seasons[-partition,]

seasons <- unique(hist_train$season)
hist_train$win <- ifelse(hist_train$win == 0, "no","yes")


# Win Accuracy #
#need new packages for ada, bagFDA
for (m in c("rf","LogitBoost", "ada",
            "bagFDA","parRF","wsrf",
            "glmboost","C5.0","xgbLinear",
            "knn","svmLinearWeights")) {
  for (i in seasons) {
    print(paste0("Testing ",m," model against ",i," season results..."))
    # Make test set the year i
    train_set <- hist_train[hist_train$season != i,]
    test_set <- hist_train[hist_train$season == i,]
    
    mod_rf <- train(win ~ elo_diff + rest_days_diff + 
                      last3_elo_diff + last5_elo_diff + 
                      last10_elo_diff + last3_diff + 
                      last5_diff + last10_diff,
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
    
    # Create data frame to save results
    tmp_results <- data.frame(time_stamp = as.POSIXlt(Sys.time()),
                              caret_model = mod_rf$method,
                              model_desc = mod_rf$modelInfo$label,
                              season = i,
                              log_loss = log_loss,
                              accuracy = accuracy)
    
    tryCatch(if (exists("mod_results")==FALSE) {
      mod_results <- tmp_results
    } else {
      mod_results <- rbind(mod_results, tmp_results)
    },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
    
  }
}

# Predict over the entire dataset
rf <- randomForest(win ~ elo_diff + rest_days_diff + 
                     last3_elo_diff + last5_elo_diff + 
                     last10_elo_diff + last3_diff + 
                     last5_diff + last10_diff,
                   hist_seasons,
                   ntree = 300,
                   importance = TRUE)
last_season$prediction <- predict(rf, last_season, type = "response")
confusionMatrix(last_season$prediction, last_season$win)

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

