## Spread Scraping

library(rvest)
library(xlsx)
library(stringr)
library(stringi)
library(chron)
library(tidyverse)
library(beepr)
library(plyr)

#THIS IS FOR PAST ODDS ONLY
lines <- list()
dailyLines <- list()
availableSpreads <- subset(all_games[which(all_games$season > 2010),]) %>%
  select(date) %>%
  unique() %>%
  arrange(date)
for(i in 1:nrow(availableSpreads)) {
  tryCatch({
  currentDate <- availableSpreads$date[i]
  currDateString <- gsub("-","",currentDate)
  oddsURL <- paste("https://classic.sportsbookreview.com/betting-odds/nba-basketball/?date=", 
                   currDateString, 
                   sep = "")
  oddspage <- read_html(oddsURL)
  node <- html_nodes(oddspage, "div.event-holder.holder-complete")
  games <- length(node)
  if(games > 0) {
    for(j in 1:games) {
      child1 <- html_children(node[j])
      html_children(child1)
      
      child2 <- (html_children(child1)[2])
      html_children(child2)
      
      child3 <- html_children(child2)[3]
      html_children(child3)
      
      child4 <- html_children(child3)[4]
      
      scorenode1 <- html_node(html_children(child4)[1],"span.first.total")
      score1 <- as.integer(html_text(scorenode1))
      
      scorenode2 <- html_node(html_children(child4)[2],"span.total")
      score2 <- as.integer(html_text(scorenode2))
      
      open <- html_children(child1)[8]
      html_children(open)
      
      oddstop <- html_children(open)[1]
      oddsbottom <- html_children(open)[2]
      topodds <- html_text(oddstop)
      bottomodds <- html_text(oddsbottom)
      
      # betonline <- html_children(child1)[13]
      # betTop <- html_children(betonline)[1]
      # betBot <- html_children(betonline)[2]
      # bettopodds <- html_text(betTop)
      # betbotodds <- html_text(betBot)
      
      bovada <- html_children(child1)[14]
      boTop <- html_children(bovada)[1]
      boBot <- html_children(bovada)[2]
      botopodds <- html_text(boTop)
      bobotodds <- html_text(boBot)
      
      # willy <- html_children(child1)[21]
      # willyTop <- html_children(willy)[1]
      # willyBot <- html_children(willy)[2]
      # willytopodds <- html_text(willyTop)
      # willybotodds <- html_text(willyBot)
      # 
      # bookie <- html_children(child1)[35]
      # bookieTop <- html_children(bookie)[1]
      # bookieBot <- html_children(bookie)[2]
      # bookietopodds <- html_text(bookieTop)
      # bookiebotodds <- html_text(bookieBot)
      
      child6 <- html_children(child1)[6]
      html_children(child6)
      top <- html_children(child6)[1]
      bot <- html_children(child6)[2]
      child7 <- html_children(top)[1]
      child8 <- html_children(bot)[1]
      topteam <- html_text(child7)
      botteam <- html_text(child8)
      date <- paste(as.Date(currentDate))
      scoreLine <- as.data.frame(cbind(topteam, botteam, 
                                       score1, score2, 
                                       topodds, bottomodds, 
                                       botopodds, bobotodds,
                                       #bettopodds, betbotodds,
                                       # willytopodds,willybotodds, 
                                       # bookietopodds,bookiebotodds,
                                       date))
      if(ncol(scoreLine) == 9) {
        lines[[j]] <- scoreLine
        dailyLines[[length(dailyLines)+1]] <- lines
      } else { print("missing odds")}
      lines <- list()
    }
  }
  print(i)
  }, error=function(e){})
}

spreads <- t(sapply(dailyLines, '[', 1:max(sapply(dailyLines, length)))) 
spreads <- do.call(rbind, spreads)
#spreads <- lapply(as.data.frame(spreads), as.character)
spreads$date <- as.Date(spreads$date)

# Get team abbreviations
spreads$topAbbr <- abbr(spreads, "topteam")
spreads$botAbbr <- abbr(spreads, "botteam")



# Get elo ratings
spreads <- merge(spreads, teams, by.x = "topAbbr", by.y = "Abbr")  %>%
  select(-c(Team, startingElo, elo))

spreads <- merge(spreads, teams, by.x = "botAbbr", by.y = "Abbr")  %>%
  select(-c(Team, startingElo, elo))
#spreads <- merge(spreads, teams, by.x = "botteam", by.y = "Team")

allData <- merge(all_games,spreads, 
                     by.x = c("awayid", "homeid", "date"),
                     by.y = c("franID.x", "franID.y", "date"))

allData <- allData %>% arrange(date)
