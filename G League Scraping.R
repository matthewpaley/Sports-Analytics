setwd("~/Documents/Sports Analytics/")
library(rvest)

teams <- c("Austin Spurs", "Austin Toros", "Columbus Riverdragons", 
           "Bakersfield Jam", "Northern Arizona Suns",
           "Canton Charge", "New Mexico Thunderbirds", "Albuquerque Thunderbirds", "Huntsville Flight",
           "Delaware 87ers", 
           "Erie Bayhawks", "Lakeland Magic", 
           "Fort Wayne Mad Ants",
           "Idaho Stampede", "Salt Lake City Stars",
           "Iowa Energy", "Iowa Wolves",
           "Los Angeles D-Fenders", "South Bay Lakers",
           "Maine Red Claws", 
           "Reno Bighorns", "Stockton Kings",
           "Rio Grande Valley Vipers",
           "Santa Cruz Warriors", 
           "Sioux Falls Skyforce", 
           "Springfield Armor", "Grand Rapids Drive", "Springfield Armor", "Anaheim Arsenal",
           "Texas Legends", "Colorado 14ers",
           "Tulsa 66ers", "Oklahoma City Blue", "Ashevile Altitude",
           "Westchester Knicks",
           "Arkansas RimRockers", 
           "Fayetteville Patriots", 
           "Florida Flame", "Charleston Lowgators", "North Charleston Lowgators",
           "Fort Worth Flyers", 
           "Greenville Groove", 
           "Mobile Revelers", 
           "Roanoake Dazzle", 
           "Utah Flash")

abbr <- c("AUS", "AUS", "CBL",
          "BAK", "NAS",
          "CAN", "NMX", "ALB", "HUN",
          "DEL", 
          "ERI", "LKL",
          "FOR", 
          "IDA", "SLS",
          "IOW", "WOL",
          "LOS", "SBL",
          "MAI", 
          "REN", "STK",
          "RIO", 
          "SAN", "DAK",
          "SFL", 
          "SPR", "GRR", "ANA",
          "TEX", "COL",
          "TUL", "OKL", "ASH",
          "WES",
          "ARK", 
          "FAY", 
          "FLO", "CHS", "NCH",
          "FTW", 
          "GRE", 
          "MOB", 
          "ROA", 
          "UTA")

firstYear <- c(2015, 2006, 2002,
               2007, 2017,
               2012, 2011, 2006, 2002,
               2014,
               2009, 2018,
               2008,
               2007, 2017,
               2008, 2018,
               2007, 2018,
               2010,
               2009, 2019,
               2008,
               2013, 2007,
               2007,
               2010, 2015, 2007,
               2011, 2007,
               2006, 2015, 2002,
               2015,
               2006,
               2002,
               2005, 2004, 2002,
               2006,
               2002,
               2002,
               2002,
               2008)
endYear   <- c(2019, 2014, 2005,
               2016, 2019,
               2019, 2011, 2010, 2005,
               2019,
               2017, 2019,
               2019,
               2016, 2019,
               2017, 2019,
               2017, 2019,
               2019,
               2018, 2019,
               2019,
               2019, 2012,
               2019,
               2014, 2019, 2009,
               2019, 2009,
               2014, 2019, 2005,
               2019,
               2007,
               2006,
               2006, 2004, 2003,
               2007,
               2003,
               2003,
               2006,
               2011)
teamInfo <- data.frame(teams, abbr, firstYear, endYear)
teamInfo$numYears <- endYear - firstYear + 1

for (i in 2002:2019) {
  for (j in nrow(teamInfo)) {
    ## Scrape Here
    ## if j == AUS, j <- CBL (2002-2005), j <- AUS (Toros 2006-2013)
    ## if j == CAN, j <- HUN (2002-2005), j <- ALB (2006-2010), j <- NMX (2011)
    ## if j == TUL, j <- ASH (2002-2005)
    ## if j == BAK, j <- NAS (2017-2019)
    ## if j == ERI, j <- LKL (2018-2019)
    ## if j == IDA, j <- SLS (2017-2019)
    ## if j == IOW, j <- WOL (2018-2019)
    ## if j == LOS, j <- SBL (2017-2019)
    ## if j == REN, j <- STK (2019-2019)
    ## if j == SAN, j <- DAK (2007-2012) 

  }
}


for (i in 1:nrow(teamInfo)) {
  for (j in teamInfo$firstYear[i]:teamInfo$endYear[i]) {
    #Specifying the url for desired website to be scrapped
    url <- paste('https://www.basketball-reference.com/gleague/teams/', teamInfo$abbr[i], '/', j, '.html', sep = "")
  
    #Reading the HTML code from the website
    webpage <- read_html(url)
  
    #Turn HTML data into data frames for each team/season
    per36 <- as.data.frame(html_table(html_nodes(webpage,'#nbdl_per_minute'))[1])
    advanced <- as.data.frame(html_table(html_nodes(webpage,'#div_nbdl_advanced'))[1])
    transactions <- as.data.frame(html_table(html_nodes(webpage,'#div_transactions'))[1])
  
  }
}
