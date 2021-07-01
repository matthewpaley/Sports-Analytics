library("nflscrapR")
library("dplyr")
library("car")

###Get NFL PBP data through last season
pbp.data.2009 <-season_play_by_play(2009)
pbp.data.2010 <-season_play_by_play(2010)
pbp.data.2011 <-season_play_by_play(2011)
pbp.data.2012 <-season_play_by_play(2012)
pbp.data.2013 <-season_play_by_play(2013)
pbp.data.2014 <-season_play_by_play(2014)
pbp.data.2015 <-season_play_by_play(2015)
pbp.data.2016 <-season_play_by_play(2016)
pbp.data.2017 <-season_play_by_play(2017)

#Save in an excel file
PBPhist <- rbind(pbp.data.2009,pbp.data.2010,pbp.data.2011,pbp.data.2012,pbp.data.2013,pbp.data.2014,pbp.data.2015,pbp.data.2016,pbp.data.2017)

#Get 2018 data (don't export because updated weekly)
pbp.data.2018 <-season_play_by_play(2018)

#Merge all data
PBPdata <- rbind(PBPhist, pbp.data.2018)

##Get browns 
browns <- PBPdata[PBPdata$posteam == "CLE",]

##Categorize Plays
passes <- subset(browns, PlayType == "Pass")
runs <- subset(browns, PlayType == "Run")

##Get Passers with at least 25 pass attempts
qualified <-  passes %>% group_by(Passer) %>% filter(n()>25)
QBs <- as.data.frame(unique(qualified$Passer))
QBs$AirYards <- sum(qualified$AirYards[qualified$Passer==QBs$`unique(qualified$Passer)`])

AYpA <- data.frame()
Passer <- as.data.frame(unique(qualified$Passer))
for (i in 1:nrow(Passer)) {
  AYpA[i] <- (sum(QBs$AirYards[which(passes$Passer==Passer$`unique(qualified$Passer)`[i])])/sum(QBs$PassAttempt[which(passes$Passer==Passer$`unique(qualified$Passer)`[i])]))
}
  scatterplot(QBs$PassAttempt,QBs$AirYards)
