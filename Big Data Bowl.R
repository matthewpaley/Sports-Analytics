library(tidyverse)
library(ggplot2)
library(gganimate)
library(cowplot)
library(teamcolors)
library(ff)
library(readr)
library(caret)

setwd("/Volumes/My Passport for Mac/Big-Data-Bowl-master/")

#Import Data
gameData <- read.csv("games.csv")
playerData <- read.csv("players.csv")
playData <- read.csv("plays.csv")

datalist = list()
for (i in 1:length(list.files(pattern = "tracking*"))) {
  datalist[[i]] <- as.data.frame(
    read.csv.ffdf(x = NULL,
                  list.files(pattern = "tracking*")[i], header = T))
}
trackingData <- do.call(rbind, datalist)
exampleGame <- list.files(pattern = "tracking*")[1]
game1 <- trackingData[which(trackingData$gameId == substr(list.files(pattern = "tracking*")[1],17,nchar(exampleGame)-4)),]

## Test out the data
game1 %>% count(event) %>% print.data.frame()
td.plays <- game1 %>% filter(event == 'touchdown') %>% pull(playId) %>% unique()
td.plays

## Pass Plays
pass_plays <- playData[which(playData$PassResult!='NA'),] 
pass_plays_master <- merge(trackingData,
                           pass_plays)
pass_plays_WR <- read_csv("/Volumes/My Passport for Mac/Big-Data-Bowl-master/pass_plays_WR.csv")
pass_plays_times2 <- read_csv("pass_plays_times2.csv")

## Build Model
averages <- pass_plays_WR %>%
  filter(PositionAbbr == "WR") %>%
  group_by(nflId, gameId, playId) %>%
  summarise(avgSpeed = mean(s), avgDis = mean(dis), avgDir = mean(dir))

averages <- data.frame(averages)

coords <- matrix(ncol = 2, nrow = nrow(averages))
for (i in 1:nrow(averages)) {
  top <- max(which(pass_plays_WR$playId == averages$playId[i] & 
                      pass_plays_WR$nflId == averages$nflId[i] &
                      pass_plays_WR$gameId == averages$gameId[i]))
  bottom <- min(which(pass_plays_WR$playId == averages$playId[i] & 
                      pass_plays_WR$nflId == averages$nflId[i] &
                      pass_plays_WR$gameId == averages$gameId[i]))
  
  coords[i, 1] <- pass_plays_WR$x[top]-pass_plays_WR$x[bottom]
  coords[i, 2] <- pass_plays_WR$y[top]-pass_plays_WR$y[bottom]
}

coords <- data.frame(coords)

colnames(coords)[1] <- "xDisp"
colnames(coords)[2] <- "yDisp"

averages <- cbind(averages, coords)

coords <- matrix(ncol = 3, nrow = nrow(averages)) 
for (i in 1:nrow(averages)) {
  bottom_wr <- min(which(pass_plays_WR$playId == averages$playId[i] & 
                           pass_plays_WR$nflId == averages$nflId[i] &
                           pass_plays_WR$gameId == averages$gameId[i]))
  
  bottom_ball <- min(which(pass_plays_WR$playId == averages$playId[i] & 
                             is.na(pass_plays_WR$nflId) &
                             pass_plays_WR$gameId == averages$gameId[i]))
  
  coords[i, 1] <- pass_plays_WR$y[bottom_wr]
  coords[i, 2] <- pass_plays_WR$y[bottom_ball]
  coords[i, 3] <- pass_plays_WR$dir[bottom_wr]
}

coords <- data.frame(coords)

colnames(coords)[1] <- "wr_y"
colnames(coords)[2] <- "ball_y"
colnames(coords)[3] <- "wr_dir"

coords$side <- ifelse((coords$wr_y >= coords$ball_y &
                         coords$wr_dir <= 180) |
                        (coords$wr_y < coords$ball_y &
                           coords$wr_dir > 180),
                      "r",
                      "l")

coords <- coords[,-c(1:3)]

averages <- cbind(averages, coords)

colnames(averages)[9] <- "side"
noNAs <- averages[complete.cases(averages),]

plays <- pass_plays_WR %>%
  filter(PositionAbbr == "WR")
plays <- plays[,c(1:3,12)]  
  
routes <- plays %>%
  group_by(nflId, gameId, playId) %>%
  unique()

allIn <- merge(noNAs, routes, by = c("nflId", "gameId", "playId"))
allIn$route_id <- as.factor(allIn$route_id)

scaleData <- allIn[,c(4:8)]
scaleData <- data.frame(scale(scaleData))

modelDF <- allIn

modelDF$avgSpeed <- scaleData$avgSpeed
modelDF$avgDis <- scaleData$avgDis
modelDF$avgDir <- scaleData$avgDir
modelDF$xDisp <- abs(scaleData$xDisp)
modelDF$yDisp <- scaleData$yDisp


training <- modelDF[which(!is.na(modelDF$route_id)),]
testing <- modelDF[which(is.na(modelDF$route_id)),]
training <- training[, -c(1:3)]
testing <- testing[, -c(1:3,10)]

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(13)
fit.lda <- train(route_id~., data=training, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(13)
fit.cart <- train(route_id~., data=training, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(13)
fit.knn <- train(route_id~., data=training, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(13)
fit.svm <- train(route_id~., data=training, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(13)
fit.rf <- train(route_id~., data=training, method="rf", metric=metric, trControl=control)


# summarize accuracy of models
results <- resamples(list(lda = fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

predictions <- predict(fit.knn, testing)
testing$route_id <- predictions

finalData <- rbind(training, testing)
ids <- modelDF[,c(1:3)]
finalData <- cbind(ids, finalData)

#watch original testing routes
watch <- allIn[which(is.na(allIn$route_id)),]







## Analysis - best receiver route combos
# most space between db at time of catch
# best for TDs
# best for 1st downs
# fastest
# best to left side
# best on right side

# Split data into route dataframes

under <- finalData[which(finalData$route_id == 0),]
out <- finalData[which(finalData$route_id == 1),]
slant <- finalData[which(finalData$route_id == 2),]
hitch <- finalData[which(finalData$route_id == 3),]
hook <- finalData[which(finalData$route_id == 4),]
comeback <- finalData[which(finalData$route_id == 5),]
dig <- finalData[which(finalData$route_id == 6),]
corner <- finalData[which(finalData$route_id == 7),]
post <- finalData[which(finalData$route_id == 8),]
go <- finalData[which(finalData$route_id == 9),]
wheel <- finalData[which(finalData$route_id == 11),]

routeSummaries <- finalData[,-c(1:3)] %>%
  group_by(route_id) %>%
  summarise(avgSpeed = mean(avgSpeed), 
            avgDis = mean(avgDis), 
            avgDir = mean(avgDir),
            avgX = mean(xDisp),
            avgY = mean(yDisp))
