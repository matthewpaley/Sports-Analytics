library(dplyr)
library(readr)
library(readxl)
library(hexbin)

##import data
Free_throw_log <- read_excel("Basketball Data/Free_Throws.xlsx")
All_nba_shot_log <- read_excel("Basketball Data/All_nba_shot_log.xlsx")
Player_names <- read_excel("Basketball Data/Player_Map.xlsx")
Per_36 <- read_excel("Basketball Data/Per 36.xlsx")
Play_by_Play_New <- read_csv("Basketball Data/Play_by_Play_New.csv")

##Clean Per 36 file
Per_36 <- Per_36 %>% 
  group_by(Player) %>% 
  summarise(MP = sum(MP), FTA = sum(FTA),
            FT = sum(FT), FG = sum(FG), 
            FGA = sum(FGA))

Per_36 <- inner_join(Per_36,Player_names, by=c("Player" = "Name"))
Per_36 <- Per_36[(Per_36$MP/3/36 >= 5),]
Per_36 <- Per_36[(Per_36$FGA > .5),] 
Per_36 <- Per_36[(Per_36$FTA > 1),] ## Minimum attempts per 36 min

##Clean shot log file
All_nba_shot_log <- All_nba_shot_log[(All_nba_shot_log$PTS_TYPE == 3),]
All_nba_shot_log <- All_nba_shot_log[(All_nba_shot_log$CLOSE_DEF_DIST >= 6),]
All_nba_shot_log <- inner_join(All_nba_shot_log,Player_names, by=c("PERSON_ID" = "Player_id"))
All_nba_shot_log <- inner_join(All_nba_shot_log,Per_36, by=c("Name" = "Player"))

All_nba_shot_log <- All_nba_shot_log %>% 
  group_by(PERSON_ID, Name) %>% 
  summarise(FGM = sum(FGM),FGA = sum(FGA.x))

All_nba_shot_log$Pct3 <- All_nba_shot_log$FGM/All_nba_shot_log$FGA

League_3Pct <- sum(All_nba_shot_log$FGM)/sum(All_nba_shot_log$FGA)

##Clean free throw file
Free_throw_log <- Free_throw_log %>% 
  group_by(Player) %>% 
  summarise(FTA = sum(FTA), FT = sum(FT))

Free_throw_log$FTPct <- Free_throw_log$FT/Free_throw_log$FTA
League_FT_Pct <- sum(Free_throw_log$FT)/sum(Free_throw_log$FTA)

##Merge tables
Full_table <- inner_join(Free_throw_log,All_nba_shot_log, by=c("Player" = "Name"))
Full_table$SV_Player_id <- NULL
Full_table$League_FT_Pct <- League_FT_Pct
Full_table$League3Pct <- League_3Pct

##Fouled Scenarios
##And-3: 1 FT vs. 1 3 Pointer (worth 2 points)
  ##Player
    Player_EValFT1 <- Full_table$FTPct * 1
    Player_EVal1 <- Full_table$Pct3 * 2
  ##League
    League_EValFT1 <- Full_table$League_FT_Pct * 1
    League_EVal1 <- Full_table$League3Pct * 2

##2 Pointer: 2 FT vs. 1 3 Pointer (worth 3 points)
  ##Player
    Player_EValFT2 <- Full_table$FTPct * 2
    Player_EVal2 <- Full_table$Pct3 * 3
  ##League
    League_EValFT2 <- Full_table$League_FT_Pct * 2
    League_EVal2 <- Full_table$League3Pct * 3

##3 Pointer: 3 FT vs. 1 3 Pointer (worth 4 points)
  ##Player
    Player_EValFT3 <- Full_table$FTPct * 3
    Player_EVal3 <- Full_table$Pct3 * 4
  ##League
    League_EValFT3 <- Full_table$League_FT_Pct * 3
    League_EVal3 <- Full_table$League3Pct * 4
    
##Add Player scenarios to table    
Full_table$Player_EValFT1 <- Player_EValFT1   
Full_table$Player_EVal1 <- Player_EVal1
Full_table$Player_EValFT2 <- Player_EValFT2   
Full_table$Player_EVal2 <- Player_EVal2
Full_table$Player_EValFT3 <- Player_EValFT3   
Full_table$Player_EVal3 <- Player_EVal3

##Add League scenarios to table
Full_table$League_EValFT1 <- League_EValFT1
Full_table$League_EVal1 <- League_EVal1
Full_table$League_EValFT2 <- League_EValFT2
Full_table$League_EVal2 <- League_EVal2
Full_table$League_EValFT3 <- League_EValFT3
Full_table$League_EVal3 <- League_EVal3

##Play by Play table
PbP <- Play_by_Play_New
Play_by_Play_New <- Play_by_Play_New[c(Play_by_Play_New$Shot_Value == 1),]
Play_by_Play_New$General_Description <- trimws(Play_by_Play_New$General_Description)
Play_by_Play_New <- Play_by_Play_New[c(Play_by_Play_New$General_Description != "Free Throw Flagrant 1 of 1"),]
Play_by_Play_New <- Play_by_Play_New[c(Play_by_Play_New$General_Description != "Free Throw Technical"),]
Play_by_Play_New <- Play_by_Play_New[c(Play_by_Play_New$General_Description != "Free Throw 1 of 1"),]
timeBetween <- 0

##Summed the difference in time between free throw shot 1 and 2 (and 1 and 3)
for(i in 1:nrow(Play_by_Play_New)) {
  if(Play_by_Play_New$General_Description[i] == "Free Throw 1 of 2") {
    timeBetween = timeBetween + (Play_by_Play_New$Wall_Clock_Time[i+1] - 
                                   Play_by_Play_New$Wall_Clock_Time[i])
  }
  else if(Play_by_Play_New$General_Description[i] == "Free Throw 1 of 3") {
    timeBetween = timeBetween + (Play_by_Play_New$Wall_Clock_Time[i+2] - 
                                   Play_by_Play_New$Wall_Clock_Time[i])
  }
  else if(Play_by_Play_New$General_Description[i] == "Free Throw Clear Path 1 of 2") {
    timeBetween = timeBetween + (Play_by_Play_New$Wall_Clock_Time[i+1] - 
                                   Play_by_Play_New$Wall_Clock_Time[i])
  }
  else if(Play_by_Play_New$General_Description[i] == "Free Throw Flagrant 1 of 2") {
    timeBetween = timeBetween + (Play_by_Play_New$Wall_Clock_Time[i+1] - 
                                   Play_by_Play_New$Wall_Clock_Time[i])
  }
  else if(Play_by_Play_New$General_Description[i] == "Free Throw Flagrant 1 of 3") {
    timeBetween = timeBetween + (Play_by_Play_New$Wall_Clock_Time[i+2] - 
                                   Play_by_Play_New$Wall_Clock_Time[i])
  }
}
timePer_FT <- timeBetween/nrow(Play_by_Play_New)
FT_TimePerGame <- timeBetween/length(unique(Play_by_Play_New$Game_id))
FT_TimeSaved <- FT_TimePerGame * 60  ##save up to 6 min 14 seconds per game on average

##End of Game Fouling - not done yet
#PbP$score <- abs(PbP$Home_PTS-PbP$Visitor_PTS)
#PbP <- PbP[PbP$Period == c(4:8),]
#PbP <- PbP[PbP$Play_Clock_Time <= 120,]
#PbP <- PbP[(PbP$General_Description == c("Free Throw 1 of 2", "Free Throw 2 of 2")),]
#for(i in 1:nrow(PbP)) {
#  if(!is.na(PbP$General_Description[i]) && PbP$General_Description[i] == "Free Throw 1 of 2") {
#       if(!is.na(PbP$General_Description[i+1]) && PbP$General_Description[i+1] != "Free Throw 2 of 2") {
#    PbP <- PbP[-i,]
#       }
#     }
#  if(!is.na(PbP$General_Description[i]) && PbP$General_Description[i] == "Free Throw 2 of 2") {
#    if(!is.na(PbP$General_Description[i+1]) && PbP$General_Description[i+1] != "Free Throw 1 of 2") {
#      PbP <- PbP[-i,]
#    }
#  }
#}
#EndFTM <- 0
#EndFTA <- 0
#for(i in 1:nrow(PbP)) {
#  if(PbP$General_Description[i] == "Foul: Personal" && PbP$General_Description[i+1] == "Free Throw 1 of 2") {
#    EndFTM <- EndFTM + PbP$Shot_Outcome[i+1] + PbP$Shot_Outcome[i+2]
#    EndFTA <- EndFTA + 1
#  }
#}
#PbP$EndFTPct <- EndFTM/EndFTA
#PbP$EndFTEVal <- EndFTPct * 2
#PbP$EndFreeShotEVal <- League_3Pct * 2
##




library(grid)
library(jpeg)
library(ggplot2)
#  & PbP$Shot_Outcome != "NULL"
PbP2 <- PbP[(PbP$Shot_Value > 0) &
              PbP$Shot_Distance != "BACKCOURT",]
PbP2$Y_Location <- ifelse(PbP2$Y_Location > 450, 
                          PbP2$Y_Location - 450, 
                          PbP2$Y_Location)
PbP2 <- merge(PbP2, Player_names, by.x = "Player1", by.y = "Player_id")

PbP2$Y_Location <- ifelse(PbP2$Shot_Value == 1,
                          150, 
                          PbP2$Y_Location)

PbP2$Shot_Outcome <- as.numeric(ifelse(PbP2$Shot_Value == 1,
                          ifelse(grepl("missed", PbP2$Description),
                          0,1),
                          ifelse(PbP2$Shot_Outcome == "NULL", 
                                 0, PbP2$Shot_Outcome)))

PbP2$Shot_Distance <- ifelse(PbP2$Shot_Distance == "NULL", "FT", "FT")
PbP2$Shot_Side_of_Ct <- ifelse(PbP2$Shot_Side_of_Ct == "NULL", "FT", "FT")


shotSummary <- PbP2 %>%
  group_by(Name, Shot_Value,
           Shot_Side_of_Ct, 
           Shot_Distance, Season) %>%
  summarize(acc = sum(as.numeric(Shot_Outcome))/n())

PbP3 <- merge(PbP2, shotSummary, by = c("Name", "Season", "Shot_Value", 
                                        "Shot_Side_of_Ct", "Shot_Distance"))

# half court image
courtImg <- "Basketball Data/HalfCourt.jpg"
court <- rasterGrob(readJPEG(courtImg),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# plot using NBA court background and colour by shot zone
ggplot(PbP3[PbP3$Name == "LeBron James" & PbP3$Season == 2016 ,], 
       aes(x=X_Location, y=Y_Location)) + 
  annotation_custom(court, -250, 250, -50, 450) +
  stat_binhex(bins = 25, colour = "gray", alpha = 0.3) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  xlim(-250, 250) +
  ylim(-50, 450)
