library(readr)
library(tidyverse)
library(xgboost)
library(deldir)
library(caret)
options(scipen = 999) # Don't use scientific notation for big numeric Ids


# Load train set
train <- read_csv('nfl-big-data-bowl-2020/train.csv', guess_max = 500000)

######################################################################################
#--------------------------------- Data Cleansing -----------------------------------#
#----------------- With help from Jason Zivkovic and Michael Lopez ------------------#
#- https://www.kaggle.com/jaseziv83/comprehensive-cleaning-and-eda-of-all-variables -#
######################################################################################

dataCleaning <- function(train) {
  
  train <- as.data.frame(train)
  
  ## Fill in Temperature and humidity NAs with mean
  train[is.na(train[, 46]), 46] <- mean(train[, 46], na.rm = TRUE)
  train[is.na(train[, 47]), 47] <- mean(train[, 47], na.rm = TRUE)
  
  ## Remove missing Tony Jefferson Dir/Orientation rows
  train <- train[!is.na(train$Dir),]
  train <- train[!is.na(train$Orientation),]  
  
  ## Standardize Play Direction ##
  train <- train %>% 
    mutate(ToLeft = PlayDirection == "left", 
           IsBallCarrier = NflId == NflIdRusher)
  
  train <- train %>% 
    mutate(TeamOnOffense = ifelse(PossessionTeam == HomeTeamAbbr, "home", "away"),  
           IsOnOffense = Team == TeamOnOffense,  ## Is player on offense?
           YardsFromOwnGoal = ifelse(as.character(FieldPosition) == PossessionTeam,
                                     YardLine, 50 + (50-YardLine)), 
           YardsFromOwnGoal = ifelse(YardLine == 50, 50, YardsFromOwnGoal),  
           X_std = ifelse(ToLeft, 120-X, X) - 10, ## Standardizes X
           Y_std = ifelse(ToLeft, 160/3-Y, Y))    ## Standardized Y   
  
  ## Standardize Player Direction ##   
  train <- train %>% 
    mutate(Dir_std_1 = ifelse(ToLeft & Dir < 90, Dir + 360, Dir), 
           Dir_std_1 = ifelse(!ToLeft & Dir > 270, Dir - 360, Dir_std_1), 
           Dir_std_2 = ifelse(ToLeft, Dir_std_1 - 180, Dir_std_1))
  
  train <- train %>% 
    mutate(X_std_end = S*cos((90-Dir_std_2)*pi/180) + X_std, 
           Y_std_end = S*sin((90-Dir_std_2)*pi/180) + Y_std)    
  
  ## Offensive Formation ##
  train$OffenseFormation[train$OffenseFormation == ""] <- "Unknown"
  
  ## Player Height ##
  train$PlayerHeight <- as.integer(substr(train$PlayerHeight,0,1))*12 + as.integer(substr(train$PlayerHeight,2,3))
  
  ## Team Abbreviations ##
  train$VisitorTeamAbbr[train$VisitorTeamAbbr == "ARI"] <- "ARZ"
  train$HomeTeamAbbr[train$HomeTeamAbbr == "ARI"] <- "ARZ"
  
  train$VisitorTeamAbbr[train$VisitorTeamAbbr == "BAL"] <- "BLT"
  train$HomeTeamAbbr[train$HomeTeamAbbr == "BAL"] <- "BLT"
  
  train$VisitorTeamAbbr[train$VisitorTeamAbbr == "CLE"] <- "CLV"
  train$HomeTeamAbbr[train$HomeTeamAbbr == "CLE"] <- "CLV"
  
  train$VisitorTeamAbbr[train$VisitorTeamAbbr == "HOU"] <- "HST"
  train$HomeTeamAbbr[train$HomeTeamAbbr == "HOU"] <- "HST"
  
  ## Stadium Names ##
  train$Stadium[train$Stadium == "Broncos Stadium At Mile High"] <- "Broncos Stadium at Mile High"
  train$Stadium[train$Stadium == "CenturyField"] <- "CenturyLink Field"
  train$Stadium[train$Stadium == "CenturyLink"] <- "CenturyLink Field"
  train$Stadium[train$Stadium == "EverBank Field"] <- "Everbank Field"
  train$Stadium[train$Stadium == "First Energy Stadium"] <- "FirstEnergy Stadium"
  train$Stadium[train$Stadium == "FirstEnergy"] <- "FirstEnergy Stadium"
  train$Stadium[train$Stadium == "FirstEnergyStadium"] <- "FirstEnergy Stadium"
  train$Stadium[train$Stadium == "Lambeau field"] <- "Lambeau Field"
  train$Stadium[train$Stadium == "Los Angeles Memorial Coliesum"] <- "Los Angeles Memorial Coliseum"
  train$Stadium[train$Stadium == "M & T Bank Stadium"] <- "M&T Bank Stadium"
  train$Stadium[train$Stadium == "M&T Stadium"] <- "M&T Bank Stadium"
  train$Stadium[train$Stadium == "Mercedes-Benz Dome"] <- "Mercedes-Benz Superdome"
  train$Stadium[train$Stadium == "MetLife"] <- "MetLife Stadium"
  train$Stadium[train$Stadium == "Metlife Stadium"] <- "MetLife Stadium"
  train$Stadium[train$Stadium == "NRG"] <- "NRG Stadium"
  train$Stadium[train$Stadium == "Oakland Alameda-County Coliseum"] <- "Oakland-Alameda County Coliseum"
  train$Stadium[train$Stadium == "Paul Brown Stdium"] <- "Paul Brown Stadium"
  train$Stadium[train$Stadium == "Twickenham"] <- "Twickenham Stadium"
  
  ## Locations ##
  train$Location <- trimws(train$Location, "both")
  train$Location[train$Location == "Arlington, Texas"] <- "Arlington, TX"
  train$Location[train$Location == "Baltimore, Maryland"] <- "Baltimore, MD"
  train$Location[train$Location == "Baltimore, Md."] <- "Baltimore, MD"
  train$Location[train$Location == "Charlotte, North Carolina"] <- "Charlotte, NC"
  train$Location[train$Location == "Chicago. IL"] <- "Chicago, IL"
  train$Location[train$Location == "Cincinnati, Ohio"] <- "Cincinnati, OH"
  train$Location[train$Location == "Cleveland"] <- "Cleveland, OH"
  train$Location[train$Location == "Cleveland Ohio"] <- "Cleveland, OH"
  train$Location[train$Location == "Cleveland, Ohio"] <- "Cleveland, OH"
  train$Location[train$Location == "Cleveland,Ohio"] <- "Cleveland, OH"
  train$Location[train$Location == "Detroit"] <- "Detroit, MI"
  train$Location[train$Location == "E. Rutherford, NJ"] <- "East Rutherford, NJ"
  train$Location[train$Location == "East Rutherford, N.J."] <- "East Rutherford, NJ"
  train$Location[train$Location == "Foxborough, Ma"] <- "Foxborough, MA"
  train$Location[train$Location == "Houston, Texas"] <- "Houston, TX"
  train$Location[train$Location == "Jacksonville Florida"] <- "Jacksonville, FL"
  train$Location[train$Location == "Jacksonville, Fl"] <- "Jacksonville, FL"
  train$Location[train$Location == "Jacksonville, Florida"] <- "Jacksonville, FL"
  train$Location[train$Location == "London"] <- "London, England"
  train$Location[train$Location == "Los Angeles, Calif."] <- "Los Angeles, CA"
  train$Location[train$Location == "Miami Gardens, Fla."] <- "Miami Gardens, FLA"
  train$Location[train$Location == "New Orleans"] <- "New Orleans, LA"
  train$Location[train$Location == "New Orleans, La."] <- "New Orleans, LA"
  train$Location[train$Location == "Orchard Park NY"] <- "Orchard Park, NY"
  train$Location[train$Location == "Philadelphia, Pa."] <- "Philadelphia, PA"
  train$Location[train$Location == "Pittsburgh"] <- "Pittsburgh, PA"
  train$Location[train$Location == "Seattle"] <- "Seattle, WA"
  
  ## Stadium Type ##
  outdoor <- c('Outdoor', 'Outdoors', 'Cloudy', 'Heinz Field', 
               'Outdor', 'Ourdoor', 'Outside', 'Outddors', 
               'Outdoor Retr Roof-Open', 'Oudoor', 'Bowl')
  
  indoor_closed <- c('Indoors', 'Indoor', 'Indoor, Roof Closed', 'Indoor, Roof Closed',
                     'Retractable Roof', 'Retr. Roof-Closed', 'Retr. Roof - Closed', 
                     'Retr. Roof Closed', 'Dome', 'Domed, closed', 'Closed Dome', 'Domed', 'Dome, closed')
  
  indoor_open <- c('Indoor, Open Roof', 'Open', 'Retr. Roof-Open', 
                   'Retr. Roof - Open','Domed, Open', 'Domed, open')
  
  convert_stadiums <- function(x) {
    if(x %in% outdoor) {
      "outdoor"
    } else if(x %in% indoor_closed) {
      "indoor closed"
    } else if(x %in% indoor_open) {
      "indoor open"
    } else {
      "unknown"
    }
  }    
  
  train <- train %>% 
    mutate(StadiumType = mapply(convert_stadiums, StadiumType))
  
  ## TURF TYPE ##
  train$Turf[train$Turf == "ARTIFICAL"] <- "ARTIFICIAL"
  train$Turf[train$Turf == "FIELDTURF"] <- "FIELD TURF"
  train$Turf[train$Turf == "FIELDTURF360"] <- "FIELDTURF 360"
  train$Turf[train$Turf == "NATURAL"] <- "GRASS"
  train$Turf[train$Turf == "NATURAL GRASS"] <- "GRASS"
  train$Turf[train$Turf == "NATURALL GRASS"] <- "GRASS"
  train$Turf[train$Turf == "UBU SPORTS SPEED S5-M"] <- "UBU SPEED SERIES-S5-M"
  
  ## Weather ##
  rain <- c('Rainy', 'Rain Chance 40%', 'Showers',
            'Cloudy with periods of rain, thunder possible. Winds shifting to WNW, 10-20 mph.',
            'Scattered Showers', 'Cloudy, Rain', 'Rain shower', 'Light Rain', 'Rain')
  
  overcast <- c('Cloudy, light snow accumulating 1-3"', 'Party Cloudy', 'Cloudy, chance of rain',
                'Coudy', 'Cloudy, 50% change of rain', 'Rain likely, temps in low 40s.',
                'Cloudy and cold', 'Cloudy, fog started developing in 2nd quarter',
                'Partly Clouidy', '30% Chance of Rain', 'Mostly Coudy', 'Cloudy and Cool',
                'cloudy', 'Partly cloudy', 'Overcast', 'Hazy', 'Mostly cloudy', 'Mostly Cloudy',
                'Partly Cloudy', 'Cloudy')
  
  clear <- c('Partly clear', 'Sunny and clear', 'Sun & clouds', 'Clear and Sunny',
             'Sunny and cold', 'Sunny Skies', 'Clear and Cool', 'Clear and sunny',
             'Sunny, highs to upper 80s', 'Mostly Sunny Skies', 'Cold',
             'Clear and warm', 'Sunny and warm', 'Clear and cold', 'Mostly sunny',
             'T: 51; H: 55; W: NW 10 mph', 'Clear Skies', 'Clear skies', 'Partly sunny',
             'Fair', 'Partly Sunny', 'Mostly Sunny', 'Clear', 'Sunny')
  
  snow <- c('Heavy lake effect snow', 'Snow')
  
  none <- c('N/A Indoor', 'Indoors', 'Indoor', 'N/A (Indoors)', 'Controlled Climate')
  
  convert_weather <- function(x) {
    if(x %in% rain) {
      "rain"
    } else if(x %in% overcast) {
      "overcast"
    } else if(x %in% clear) {
      "clear"
    } else if(x %in% snow) {
      "snow"
    } else if( x %in% none) {
      "indoors"
    } else {
      "unknown"
    }
  }    
  
  train <- train %>% 
    mutate(GameWeather = mapply(convert_weather, GameWeather))
  
  ## Wind Speed ##
  train <- train %>% 
    mutate(WindSpeed = ifelse(str_detect(WindSpeed, "E"), WindDirection, 
                              ifelse(str_detect(WindSpeed, "SE"), WindDirection, 
                                     ifelse(str_detect(WindSpeed, "SSW"), WindDirection, WindSpeed)))) %>% 
    mutate(WindSpeed = str_replace(WindSpeed, "Calm", "0"),
           WindSpeed = tolower(WindSpeed))
  
  
  train$WindSpeed <- gsub("mph", "", train$WindSpeed) %>% str_squish()
  train$WindSpeed <- str_extract(train$WindSpeed, "[^-]+")
  train$WindSpeed <- as.numeric(str_extract(train$WindSpeed, "[^ ]+"))
  
  train[is.na(train[, 48]), 48] <- round(mean(train[, 48], na.rm = TRUE), 0)
  train$WindSpeed <- as.character(train$WindSpeed)
  
  ## Wind Direction ##
  train$WindDirection <- toupper(train$WindDirection)
  train$WindDirection[train$WindDirection == "N" | train$WindDirection == "FROM S"] <- "NORTH"
  train$WindDirection[train$WindDirection == "S" | train$WindDirection == "FROM N"] <- "SOUTH"
  train$WindDirection[train$WindDirection == "E" | train$WindDirection == "FROM W"] <- "EAST"
  train$WindDirection[train$WindDirection == "W" | train$WindDirection == "FROM E"] <- "WEST"
  train$WindDirection[train$WindDirection == "FROM SW" | train$WindDirection == "FROM SSW" | train$WindDirection == "FROM WSW" |
                        train$WindDirection == "NE" | train$WindDirection == "NORTH EAST"] <- "NORTHEAST"
  train$WindDirection[train$WindDirection == "FROM SE" | train$WindDirection == "FROM SSE" | train$WindDirection == "FROM ESE" |
                        train$WindDirection == "NW" | train$WindDirection == "NORTH WEST"] <- "NORTHWEST"
  train$WindDirection[train$WindDirection == "FROM NW" | train$WindDirection == "FROM NNW" | train$WindDirection == "FROM WNW" |
                        train$WindDirection == "SE" | train$WindDirection == "SOUTH EAST"] <- "SOUTHEAST"
  train$WindDirection[train$WindDirection == "FROM NE" | train$WindDirection == "FROM NNE" | train$WindDirection == "FROM ENE" |
                        train$WindDirection == "SW" | train$WindDirection == "SOUTH WEST"] <- "SOUTHWEST"
  train$WindDirection[!train$WindDirection %in% 
                        c("NORTH", "SOUTH", "EAST", "WEST", "NORTHEAST", "NORTHWEST", "SOUTHEAST", "SOUTHWEST") ] <- "UNKNOWN" 
  
  
  ######################################################################################
  #-------------------------------- Feature Engineering -------------------------------#
  #-------------------------- With help from Jason Zivkovic ---------------------------#
  #- https://www.kaggle.com/jaseziv83/comprehensive-cleaning-and-eda-of-all-variables -#
  ######################################################################################
  
  
  # create variable to indicate whether the player is on home or away team 
  train <- train %>%
    mutate(PlayerTeam = ifelse(Team == "home", HomeTeamAbbr, VisitorTeamAbbr))
  
  # create variable to indicate whether the player is on offense or defense 
  train <- train %>%
    mutate(OffenseOrDefense = ifelse(Team == "home" & PossessionTeam == HomeTeamAbbr | 
                                       Team == "away" & PossessionTeam == VisitorTeamAbbr,
                                     "Offense", "Defense"))
  
  # create variable to indicate whether the player's team is in the lead, and the margin
  train <- train %>% 
    mutate(TeamMargin = ifelse(Team == "home", HomeScoreBeforePlay - VisitorScoreBeforePlay, VisitorScoreBeforePlay - HomeScoreBeforePlay),
           TeamInLead = ifelse(TeamMargin == 0, "Draw", ifelse(TeamMargin > 0, "Lead", "Behind")))
  
  # create variable to get player age in years based on birthdate
  train <- train %>%
    mutate(PlayerAge = floor(as.double(difftime(Sys.Date(), as.Date(train$PlayerBirthDate,"%m/%d/%Y"), units = "days")/365)))
  
  # convert game clock to seconds remaining in quarter and game
  train <- train %>%
    mutate(SecondsInQuarter = as.numeric(GameClock)/60,
           SecondsInGame = (3600 - (15 * 60 * Quarter) - SecondsInQuarter))
  
  # score diff from home team perspective
  train$h_scoreDiff <- train$HomeScoreBeforePlay - train$VisitorScoreBeforePlay
  
  # start distance from rusher
  rusher_coord_x <- train %>% 
    group_by(GameId,PlayId) %>% 
    filter(NflId==NflIdRusher) %>% 
    select(GameId,PlayId,X_std) %>% 
    rename("rusher_X" = X_std)
  
  train <- merge(train,
                 rusher_coord_x,
                 by.x = c("GameId","PlayId"),
                 by.y = c("GameId","PlayId"),
                 all.x = TRUE)
  
  rusher_coord_y <- train %>% 
    group_by(GameId,PlayId) %>% 
    filter(NflId==NflIdRusher) %>% 
    select(GameId,PlayId,Y_std) %>% 
    rename("rusher_Y" = Y_std)
  
  train <- merge(train,
                 rusher_coord_y,
                 by.x = c("GameId","PlayId"),
                 by.y = c("GameId","PlayId"),
                 all.x = TRUE)
  
  train$dist_from_rusher <- sqrt((train$X_std - train$rusher_X)^2 +
                                   (train$Y_std - train$rusher_Y)^2)
  
  # end distance from rusher
  end_rusher_coord_x <- train %>% 
    group_by(GameId,PlayId) %>% 
    filter(NflId==NflIdRusher) %>% 
    select(GameId,PlayId,X_std_end) %>% 
    rename("end_rusher_X" = X_std_end)
  
  train <- merge(train,
                 end_rusher_coord_x,
                 by.x = c("GameId","PlayId"),
                 by.y = c("GameId","PlayId"),
                 all.x = TRUE)
  
  end_rusher_coord_y <- train %>% 
    group_by(GameId,PlayId) %>% 
    filter(NflId==NflIdRusher) %>% 
    select(GameId,PlayId,Y_std_end) %>% 
    rename("end_rusher_Y" = Y_std_end)
  
  train <- merge(train,
                 end_rusher_coord_y,
                 by.x = c("GameId","PlayId"),
                 by.y = c("GameId","PlayId"),
                 all.x = TRUE)
  
  train$end_dist_from_rusher <- sqrt((train$X_std_end - train$end_rusher_X)^2 +
                                       (train$Y_std_end - train$end_rusher_Y)^2)
  # Min time to tackle
  train$tackleTime <- ifelse(is.infinite(train$dist_from_rusher / train$S),
                             train$dist_from_rusher,
                             train$dist_from_rusher / train$S)
  
  ## Remove plays missing records
  playCounts <- as.data.frame(table(train$PlayId))
  train <- merge(train, playCounts, 
                 by.x = "PlayId", 
                 by.y = "Var1")
  train <- train[train$Freq == 22, ]
  
  # Voronoi
  voronoi <- train %>%
    select(PlayId, X_std, Y_std) 
  
  voronoi$index <- voronoi %>%
    group_by(PlayId) %>%
    group_indices()
  
  vorArea <- c()
  
  for (i in 1:length(unique(voronoi$PlayId))) {
    v2 <- voronoi[voronoi$PlayId == unique(voronoi$PlayId)[i],]
    areas <- deldir(v2$X_std,v2$Y_std)$summary[,8]
    if(length(areas) < 22) {
      areas <- append(areas, mean(areas))
    }
    vorArea <- append(vorArea, areas)
  }
  
  train$vorArea <- vorArea  
  
  # number of players within X range
  def_within_1yd <- train %>%
    group_by(GameId,PlayId) %>%
    filter(OffenseOrDefense=="Defense") %>% 
    filter(dist_from_rusher<=1) %>%
    distinct() %>% 
    select(GameId,PlayId) %>%
    count() %>%
    rename(def_within_1yd = n) 
  
  def_within_3yd <- train %>%
    group_by(GameId,PlayId) %>%
    filter(OffenseOrDefense=="Defense") %>% 
    filter(dist_from_rusher<=3) %>%
    distinct() %>% 
    select(GameId,PlayId) %>% 
    count() %>%
    rename(def_within_3yd = n)  
  
  def_within_5yd <- train %>%
    group_by(GameId,PlayId) %>%
    filter(OffenseOrDefense=="Defense") %>% 
    filter(dist_from_rusher<=5) %>%
    distinct() %>% 
    select(GameId,PlayId) %>% 
    count() %>%
    rename(def_within_5yd = n) 
  
  train <- merge(train,
                 def_within_1yd,
                 by.x = c("GameId","PlayId"),
                 by.y = c("GameId","PlayId"),
                 all.x = TRUE) 
  
  train <- merge(train,
                 def_within_3yd,
                 by.x = c("GameId","PlayId"),
                 by.y = c("GameId","PlayId"),
                 all.x = TRUE)     
  
  train <- merge(train,
                 def_within_5yd,
                 by.x = c("GameId","PlayId"),
                 by.y = c("GameId","PlayId"),
                 all.x = TRUE)    
  
  train$def_within_1yd[is.na(train$def_within_1yd)] <- 0
  train$def_within_3yd[is.na(train$def_within_3yd)] <- 0
  train$def_within_5yd[is.na(train$def_within_5yd)] <- 0
  
  # Aggregate data into 1 row per play    
  ballCarrier <- train %>% 
    group_by(GameId, PlayId) %>%
    filter(NflId==NflIdRusher)
  
  otherPlayers <- train %>% 
    group_by(GameId, PlayId) %>%
    filter(NflId != NflIdRusher) %>%
    summarise(minTackleTime = min(tackleTime),
              meanTackleTime = mean(tackleTime),
              minDistFromRusher = min(dist_from_rusher),
              meanDistFromRusher = mean(dist_from_rusher),              
              minEndDistFromRusher = min(end_dist_from_rusher),
              meanEndDistFromRusher = mean(end_dist_from_rusher),
              meanVorArea = mean(vorArea))
  
  cleanData <- merge(ballCarrier,
                     otherPlayers,
                     by.x = c("GameId","PlayId"),
                     by.y = c("GameId","PlayId"))
  
  ######################################################################################
  #-------------------------------- Data Standardization ------------------------------#
  #------------------------------ And Data Type Conversion ----------------------------#
  ######################################################################################
  
  ## Remove unneeded columns
  remove_cols <- c('GameId', 'PlayId', 'NflId', 'DisplayName',
                   'JerseyNumber', 'Season', 'FieldPosition', 
                   'NflIdRusher', 'PlayDirection', 'TimeHandoff',
                   'TimeSnap', 'PlayerBirthDate', 'PlayerCollegeName',
                   'Location', 'ToLeft', 'IsBallCarrier', 'IsOnOffense',
                   'OffenseOrDefense', 'PlayerTeam', 'tackleTime',
                   'X', 'Y', 'Dir', 'Dir_std_1', 'GameClock',
                   'dist_from_rusher', 'end_dist_from_rusher', 'Freq')    
  
  cleanData <- cleanData %>%
    select(-remove_cols)
  
  ## Convert from num to char
  newChars <- c('YardLine', 'Quarter', 'Down', 'Distance',
                'DefendersInTheBox', 'Yards', 'Week')
  
  for(i in newChars){
    cleanData[[i]] <- as.character(cleanData[[i]])
  }
  
  ## Scale numeric fields
  nums <- cleanData %>%
    select_if(is.numeric)
  
  maxs <- apply(nums, 2, max)
  mins <- apply(nums, 2, min)
  
  scaled <- as.data.frame(scale(nums, center = mins, 
                                scale = maxs - mins))
  
  
  ## Create encoding for char_features ##    
  chars <- colnames(cleanData %>% 
                      select_if(is.character))
  
  outputData <- cleanData %>%
    select_if(is.character)
  
  char_d <- cleanData[, chars]
  char_d <- char_d %>% mutate_all(as.factor)
  
  for(j in chars){
    outputData[[j]] <- as.integer(factor(outputData[[j]], levels = levels(char_d[[j]]))) - 1
  }
  
  
  outputData <- cbind(scaled, outputData)
  
  ## Replace NAs
  outputData <- outputData[complete.cases(outputData), ]
  
}

# Clean the data
train2 <- dataCleaning(train)

# Split the data
partition <- createDataPartition(outputData$Yards, p = 0.75,
                                 list = FALSE,
                                 times = 1)
trainset <- outputData[ partition,]
testset <- outputData[-partition,]

vals <- as.factor(unique(outputData$Yards))
yards <- unique(outputData$Yards)
yards <- as.data.frame(yards)
yards[[2]] <- as.integer(factor(yards[[1]], levels = levels(vals[[1]]))) - 1 

# Train Model
set.seed(33)
nnet <- multinom(Yards ~ ., data = trainset, MaxNWts = 5307)

xgData <- as.matrix(trainset)

xgb <- xgboost(data = xgData, 
               label = trainset$Yards, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = nrow(yards),
               nthread = 3
)

y_pred <- data.frame(matrix(ncol = 199, nrow = nrow(testset)))
x <- c(-99:99)
colnames(y_pred) <- x

df1 <- matrix(0, ncol = 199, nrow = 1)
colnames(df1) <- c(-99:99)


# Loop through plays
for (i in 1:nrow(testset)) {
  
  print(paste("Predicting ", i, " out of ", nrow(testset), ". ", round(i/nrow(testset),2)*100, "% complete.", sep = ""))
  
  predictions <- predict(xgb, as.matrix(testset[1,]), type = "prob") %>%
    as.data.frame() %>%
    select(preds = ".") %>%
    mutate(level = row_number()-1)
  
  temp <- merge(predictions, yards, by.x = "level", by.y = "V2")
  
  temp2 <- as.data.frame(matrix(0, ncol = nrow(temp), nrow = 1))
  temp2[1,] <- temp$preds
  colnames(temp2) <- temp$yards
  
  df2 <- merge(df1, temp2, all.y = TRUE)
  df2 <- df2[,order(as.numeric(names(df2)))]
  df2[is.na(df2)] <- 0
  df2 <- t(apply(df2, 1, function(x)cumsum(x)))
  
  y_pred[1, ] <- df2[1,]
  
  
}
