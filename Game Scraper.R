## Scrape NBA Game Data

#Create list to put season data into
scrapeNBA <- function(startSeason, endSeason) {
  
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
    may_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-may.html', sep = "")
    jun_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-june.html', sep = "")
    
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
    may_webpage <- read_html(may_url)  
    jun_webpage <- read_html(jun_url) 
    
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
    may_games <- as.data.frame(html_table(html_nodes(may_webpage,'table'))[[1]])
    jun_games <- as.data.frame(html_table(html_nodes(jun_webpage,'table'))[[1]])
    
    #Combine all data frames into one data frame for each season
    if(i != 2012) { #Lockout season started in December
      games <- rbind(oct_games,nov_games,dec_games,
                     jan_games,feb_games,mar_games,
                     apr_games,may_games,jun_games)
    } else {
      games <- rbind(dec_games,jan_games,
                     feb_games,mar_games,
                     apr_games,may_games,jun_games)
    }
    games <- games[,-c(2,7:10)]
    
    #Convert points to numbers for later calculations
    games$PTS <- as.numeric(as.character(games$PTS))
    games$PTS.1 <- as.numeric(as.character(games$PTS.1))
    
    #Use only Regular Season
    #games <- games[-c(which(games$`Visitor/Neutral` == "Playoffs"):nrow(games)),]
    
    #Calculate how much the home team won/lost by for future cumulative point differential calculation (done in Excel)
    games$HomeDiff <- games$PTS.1 - games$PTS
    games$HomeWin <- ifelse(games$HomeDiff > 0,1,0)
    
    #Append season to list
    seasons[[i]] <- games
  }
  
  NBA_Games <- dplyr::bind_rows(seasons) 
}
