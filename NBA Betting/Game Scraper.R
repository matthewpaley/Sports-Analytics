## Betting Model Functions ##

# 1. Scrape NBA Game Data
scrapeNBA <- function(startDate = ifelse(exists(allData),
                                     max(allData$date) + 1,
                                     Sys.Date()-1),
                      endDate = startDate, 
                      ##playoffStart = 5,
                      fullSeason = FALSE) {
  
  startSeason <- startDate
  endSeason <- endDate
  currentMonth <- as.numeric(format(Sys.Date(),'%m'))
  currentYear <- as.numeric(format(Sys.Date(),'%Y'))
  currentSeason <- ifelse(format(as.Date(startDate),'%m') > 9,
                          currentYear + 1,
                          currentYear)
  
  # If using default Sys.Date() (today), get year for season
  if (startDate == Sys.Date()) {  
    if(as.numeric(format(startDate,'%m')) > 9) {
      startSeason <- as.numeric(format(startDate,'%Y')) + 1
    } else {
      startSeason <- as.numeric(format(startDate,'%Y'))
    }
  }
  
  if (endDate == Sys.Date()) {
    if(as.numeric(format(endDate,'%m')) > 9) {
      endSeason <- as.numeric(format(endDate,'%Y')) + 1
    } else {
      endSeason <- as.numeric(format(endDate,'%Y'))
    }
  }
  
  # If scraping by date, convert date to proper format and get season
  # Adjust the season (october 2019 = 2020 season)
  if (nchar(startDate) == 10) {  
    startDate <- as.Date(startDate)
    if(as.numeric(format(startDate,'%m')) > 9) {
      startSeason <- as.numeric(format(startDate,'%Y')) + 1
    } else {
      startSeason <- as.numeric(format(startDate,'%Y'))
    }
  }
  
  if (nchar(endDate) == 10) {
    endDate <- as.Date(endDate)
    if(as.numeric(format(endDate,'%m')) > 9) {
      endSeason <- as.numeric(format(endDate,'%Y')) + 1
    } else {
      endSeason <- as.numeric(format(endDate,'%Y'))
    }
  }

  #Create list to put season data into
  seasons <- list()
  
  for (i in startSeason:endSeason) {
    
    #Specifying the url for desired website to be scrapped
    if (i != 2012) { #Lockout season started in December 
      oct_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-october.html', sep = "")
      nov_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-november.html', sep = "")
    }
    dec_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-december.html', sep = "")
    jan_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-january.html', sep = "")
    feb_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-february.html', sep = "")
    mar_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-march.html', sep = "")
    apr_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-april.html', sep = "")
    if (i == currentSeason && currentMonth == playoffStart) {
      may_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-may.html', sep = "")
    }
    if (i == currentSeason && currentMonth > playoffStart) {
        jun_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-june.html', sep = "")
    }
    
    #Reading the HTML code from the website
    if (i != 2012) { #Lockout season started in December
      oct_webpage <- read_html(oct_url)
      nov_webpage <- read_html(nov_url)
    }
    dec_webpage <- read_html(dec_url)
    jan_webpage <- read_html(jan_url)
    feb_webpage <- read_html(feb_url)
    mar_webpage <- read_html(mar_url)
    apr_webpage <- read_html(apr_url)
    if (i == currentSeason && format(startDate,'%m') == playoffStart) {
      may_webpage <- read_html(may_url)  
    }
    if (i == currentSeason && format(startDate,'%m') > playoffStart) {
      jun_webpage <- read_html(jun_url) 
    }
    
    #Turn HTML data into data frames for each season month
    if (i != 2012) { #Lockout season started in December
      oct_games <- as.data.frame(html_table(html_nodes(oct_webpage,'table'))[[1]])
      nov_games <- as.data.frame(html_table(html_nodes(nov_webpage,'table'))[[1]])
    }
    dec_games <- as.data.frame(html_table(html_nodes(dec_webpage,'table'))[[1]])
    jan_games <- as.data.frame(html_table(html_nodes(jan_webpage,'table'))[[1]])
    feb_games <- as.data.frame(html_table(html_nodes(feb_webpage,'table'))[[1]])
    mar_games <- as.data.frame(html_table(html_nodes(mar_webpage,'table'))[[1]])
    apr_games <- as.data.frame(html_table(html_nodes(apr_webpage,'table'))[[1]])
    if (i == currentSeason && format(startDate,'%m') == playoffStart) {
      may_games <- as.data.frame(html_table(html_nodes(may_webpage,'table'))[[1]])
    }
    if (i == currentSeason && format(startDate,'%m') > playoffStart) {
      jun_games <- as.data.frame(html_table(html_nodes(jun_webpage,'table'))[[1]])
    }
    
    #Combine all data frames into one data frame for each season
    if(i != 2012 && i != currentSeason) { #Lockout season started in December
      games <- rbind(oct_games,nov_games,dec_games,
                     jan_games,feb_games,mar_games,
                     apr_games,may_games,jun_games)
    } else if (i == 2012) {
      games <- rbind(dec_games,jan_games,
                     feb_games,mar_games,
                     apr_games,may_games,jun_games)
    } else if (i == currentSeason && currentMonth < playoffStart) {
      games <- rbind(dec_games,jan_games,
                     feb_games,mar_games,
                     apr_games)
    } else if (i == currentSeason && currentMonth == playoffStart) {
      games <- rbind(dec_games,jan_games,
                     feb_games,mar_games,
                     apr_games,may_games)
    } else {
      games <- rbind(oct_games,nov_games,dec_games,
                     jan_games,feb_games,mar_games,
                     apr_games,may_games,jun_games)
    } 
    
    colnames(games)[2] <- "startTime"
    colnames(games)[6] <- "PTS.1"
    
    #Add Index
    games <- cbind(rownames(games), games)
    names(games)[names(games)=="rownames(games)"] <- "Index"
    
    #Format time
    games$startTime <- substr(games$startTime, 1, nchar(games$startTime)-1)
    games$startTime <-paste(games$startTime, ":00")
    games$startTime <- times(games$startTime)
    games$gameTime <- as.POSIXct(paste(games$Date, as.character(games$startTime)), 
                                 format = "%a, %b %d, %Y %H:%M:%S", tz = "UTC") + 16*60*60
    games$Date <- as.Date(games$Date,"%a, %b %d, %Y")
   
     #Convert points to numbers for later calculations
    games$PTS <- as.numeric(as.character(games$PTS))
    games$PTS.1 <- as.numeric(as.character(games$PTS.1))
    
    #Use only Regular Season
    for (i in 1:nrow(games)) {
      if (currentYear != currentSeason || 
          (currentYear == currentSeason && currentMonth >= playoffStart)) {
        if (i >= rownames(which(games$`Visitor/Neutral` == "Playoffs"))) {
          games$Type <- "PS"
        } else {
          games$Type <- "RS"
        }
      }
    }
    #games <- games[-c(which(games$`Visitor/Neutral` == "Playoffs"):nrow(games)),]
    
    #Calculate how much the home team won/lost by for future cumulative point differential calculation (done in Excel)
    games$HomeDiff <- games$PTS.1 - games$PTS
    games$HomeWin <- ifelse(games$HomeDiff > 0,1,0)
    
    #remove unneeded columns
    games <- games[,-c(1,3,8:11)]
    
    #Append season to list
    seasons[[i]] <- games
  }
  
  NBA_Games1 <- dplyr::bind_rows(seasons) 
  
  if (fullSeason == FALSE) {
    if (startDate != startSeason) {
      NBA_Games1 <- NBA_Games1[!(NBA_Games1$Date < startDate),]
    }
    if (endDate != endSeason) {
      NBA_Games1 <- NBA_Games1[!(NBA_Games1$Date > endDate),]
    }
  }
  
  NBA_Games1
}

# 2. Get team abbreviations
abbr <- function(df, col) {
  df[[col]] <- 
   ifelse(grepl("Milwaukee", df[[col]]), "MIL", 
   ifelse(grepl("New York", df[[col]]), "NYK", 
   ifelse(grepl("Washington", df[[col]]), "WAS", 
   ifelse(grepl("Houston", df[[col]]), "HOU",
   ifelse(grepl("Lakers", df[[col]]), "LAL", #LA must show team name to avoid confusion
   ifelse(grepl("Minnesota", df[[col]]), "MIN",
   ifelse(grepl("Memphis", df[[col]]), "MEM",
   ifelse(grepl("Portland", df[[col]]), "POR",
   ifelse(grepl("Charlotte", df[[col]]), "CHO",
   ifelse(grepl("Clippers", df[[col]]), "LAC",
   ifelse(grepl("Phoenix", df[[col]]), "PHO",
   ifelse(grepl("Utah", df[[col]]), "UTA",
   ifelse(grepl("Chicago", df[[col]]), "CHI",
   ifelse(grepl("Toronto", df[[col]]), "TOR",
   ifelse(grepl("Denver", df[[col]]), "DEN",
   ifelse(grepl("Philadelphia", df[[col]]), "PHI",
   ifelse(grepl("Sacramento", df[[col]]), "SAC",
   ifelse(grepl("New Orleans", df[[col]]), "NOP",
   ifelse(grepl("Boston", df[[col]]), "BOS",
   ifelse(grepl("Golden State", df[[col]]), "GSW",
   ifelse(grepl("San Antonio", df[[col]]), "SAS",
   ifelse(grepl("Oklahoma City", df[[col]]), "OKC",
   ifelse(grepl("Dallas", df[[col]]), "DAL",
   ifelse(grepl("Indiana", df[[col]]), "IND",
   ifelse(grepl("Detroit", df[[col]]), "DET",
   ifelse(grepl("Atlanta", df[[col]]), "ATL",
   ifelse(grepl("Orlando", df[[col]]), "ORL",
   ifelse(grepl("Cleveland", df[[col]]), "CLE",
   ifelse(grepl("Miami", df[[col]]), "MIA",
   ifelse(grepl("Brooklyn", df[[col]]), "BRK",df[[col]]))))))))))))))))))))))))))))))
  df[[col]]  
}

# 3. Align team with historical franchise
franchiseMatch <- function(df, col) {
  df[[col]] <- ifelse(grepl("New Orleans/Oklahoma City Hornets", df[[col]]), "New Orleans Pelicans",
  ifelse(grepl("New Jersey Nets", df[[col]]), "Brooklyn Nets",
  ifelse(grepl("Charlotte Bobcats", df[[col]]), "Charlotte Hornets",
  ifelse(grepl("Seattle SuperSonics", df[[col]]), "Oklahoma City Thunder", df[[col]]))))
  df[[col]]
}

# 4. Scrape NBA Odds Data
scrapeSpreads <- function(dates = Sys.Date()) {
  lines <- list()
  dailyLines <- list()
  dates <- c(dates)
  for(i in 1:length(dates)) {
    tryCatch({
      currentDate <- dates[i]
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
          top_spread <- html_text(oddstop)
          bottom_spread <- html_text(oddsbottom)
          
          betonline <- html_children(child1)[13]
          betTop <- html_children(betonline)[1]
          betBot <- html_children(betonline)[2]
          bettop_spread <- html_text(betTop)
          betbot_spread <- html_text(betBot)
          
          bovada <- html_children(child1)[14]
          boTop <- html_children(bovada)[1]
          boBot <- html_children(bovada)[2]
          botop_spread <- html_text(boTop)
          bobot_spread <- html_text(boBot)
          
          # willy <- html_children(child1)[33]
          # willyTop <- html_children(willy)[1]
          # willyBot <- html_children(willy)[2]
          # willytop_spread <- html_text(willyTop)
          # willybot_spread <- html_text(willyBot)
          # 
          # bookie <- html_children(child1)[35]
          # bookieTop <- html_children(bookie)[1]
          # bookieBot <- html_children(bookie)[2]
          # bookietop_spread <- html_text(bookieTop)
          # bookiebot_spread <- html_text(bookieBot)
          
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
                                           top_spread, bottom_spread, 
                                           bettop_spread,betbot_spread, 
                                           botop_spread,bobot_spread, 
                                           #willytop_spread,willybot_spread, 
                                           #bookietop_spread,bookiebot_spread, 
                                           date))
          
          if(ncol(scoreLine) == 11) {
            lines[[j]] <- scoreLine
            dailyLines[[length(dailyLines)+1]] <- lines
          }
          lines <- list()
        }
      }
    }, error=function(e){})
  }
  
  spreads <- t(sapply(dailyLines, '[', 1:max(sapply(dailyLines, length)))) 
  spreads <- do.call(rbind, spreads)
  #spreads <- lapply(as.data.frame(spreads), as.character)
  print(spreads$date)
  spreads$date <- as.Date(spreads$date)
  
  # Get team abbreviations
  spreads$topAbbr <- abbr(spreads, "topteam")
  spreads$botAbbr <- abbr(spreads, "botteam")
  
  # Clean data
  oddsCols <- which(grepl("spread",colnames(spreads)))
  for (i in oddsCols) { 
    #spreads$newcol <- getOdds(spreads, colnames(spreads)[i])
    spreads$newcol <- sub('.*(?=.{4}$)', '', spreads[[i]], perl = T) # Make new odds column
    spreads[[i]] <- gsub('.{4}$', '', spreads[[i]]) # Remove odds from spreads column
    spreads[[i]] <- spreadClean(spreads, colnames(spreads)[i])
    spreads$newcol <- spreadClean(spreads, "newcol")
    
    names(spreads)[names(spreads) == "newcol"] <- gsub('spread', 'line', colnames(spreads)[i])
  }
  
  
  # Return spreads data frame
  spreads
}

# 5. Clean Spread data
spreadClean <- function(table, col) {
  table[[col]] <- gsub('½', '.5', table[[col]]) #convert fraction to decimal
  table[[col]] <- str_trim(table[[col]]) #remove blank space
  table[[col]][table[[col]] == "PK"] <- "0" #make pick em' spreads 0
  table[[col]] <- as.numeric(table[[col]])
  table[[col]]
}

# 6. ELO Functions
updateK <- function(MOV, elo_diff) {
  K = 20
  if(MOV > 0) {
    mult = ((MOV+3)^.8)/(7.5+.006*elo_diff)
  } else {
    mult = ((-MOV+3)^.8)/(7.5+.006*-elo_diff)
  }
  return(c(K*mult,K*mult))
}

winLose <- function(homeScore, awayScore) {
  H = 0
  A = 0
  if(homeScore > awayScore) {
    browser()
    H = 1
  } else if(homeScore < awayScore) {
    A = 1
  } else {
    H = .5
    A = .5
  }
  return(c(H,A))
}

calcElo <- function(homeElo, awayElo) {
  homeChange = 1/(1+10^((awayElo - homeElo))/400)
  return(homeChange)
}

updateElo <- function(homeScore, awayScore, homeElo, awayElo) {
  HFA = 100
  eloDiff <- homeElo - awayElo
  MOV = homeScore - awayScore
  homeElo <- homeElo + HFA
  homeChange <- predElo(homeElo, awayElo)
  awayChange <- 1-homeChange 
  tmp <- winLose(homeScore, awayScore)
  H <- tmp[1]
  A <- tmp[2]
  tmp2 <- updateK(MOV, eloDiff)
  homeK <- tmp2[1]
  awayK <- tmp2[2]
  return(c(homeK*(H-homeChange),awayK*(A-awayChange)))
}

currentElo <- function(teamName, df = teams) {
  df[df$Team == teamName, 4]
}

# 7. Feature Engineering
featEng <- function(table)

# 8. Update historical table
updateHist <- function(df1, df2) {
  df2 <- rbind(df2, df1)
  df2 <- df2 %>% 
    distinct(Date, `Visitor/Neutral`, `Home/Neutral`)
}