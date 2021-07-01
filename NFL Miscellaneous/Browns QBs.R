library(dplyr)
library(na.tools)
library(broom.mixed)
library(lme4)
library(ggplot2)
library(tidyr)
library(plm)
library(stringr)
library(tidyverse)
library(rio)
library(nflfastR)


start<-1999
end<-2020

seasons <- start:end
data <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

datapost <- data  %>% 
  filter(play_type %in% c("run", "pass"),
         penalty == 0,
         season_type == "REG",
         home_team == "CLE" | away_team == "CLE",
         !is.na(down), 
         !is.na(yardline_100)) %>% 
  mutate(in_red_zone = if_else(yardline_100 <= 20, 1, 0),
         in_fg_range = if_else(yardline_100 <= 35, 1, 0),
         run = if_else(play_type == "run", 1, 0),
         pass = if_else(play_type == "pass", 1, 0),
         posteam_grp = posteam) 

prev_play <- datapost %>%
  group_by(game_id, posteam_grp) %>%
  mutate(
    total_runs = if_else(play_type == "run",
                         cumsum(run) - 1, cumsum(run)
    ),
    total_pass = if_else(play_type == "pass",
                         cumsum(pass) - 1, cumsum(pass)
    ),
    previous_play = if_else(posteam_grp == dplyr::lag(posteam_grp),
                            dplyr::lag(play_type), "First play of Drive"
    ),
    previous_play = if_else(is.na(previous_play),
                            replace_na("First play of Drive"), previous_play
    )
  ) 

data_filt <- prev_play %>% ungroup() %>%
  filter(!is_na(epa),
         !is.na(down)) 

browns_home_coach <- data_filt %>% 
  filter(home_team == "CLE") %>% 
  select(home_coach) %>% 
  rename(coach = "home_coach") %>% 
  unique()

browns_away_coach <- data_filt %>% 
  filter(away_team == "CLE") %>% 
  select(away_coach) %>% 
  rename(coach = "away_coach") %>% 
  unique()

browns_coach <- rbind(browns_home_coach, browns_away_coach) %>% 
  unique()

# data_filt$passer_player_id <- ifelse(!is.na(data_filt$passer_player_id) & 
#                                        data_filt$passer_player_id =="32013030-2d30-3033-3438-3535737060f8",
#                                       "00-0034855",
#                                       data_filt$passer_player_id)


rosters <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds'))  %>%
  as_tibble()

qbids<- rosters %>% 
  filter(teamPlayers.position == "QB",
         team.abbr == "CLE") %>% 
  select(teamPlayers.displayName,
         teamPlayers.lastName) %>% 
  mutate(firstInitial = substr(teamPlayers.displayName,1,1),
         pbpName = paste0(firstInitial,".",teamPlayers.lastName)) %>% 
  unique()

qblst <- c(unique(qbids$pbpName))

epa_data<-data_filt %>% 
  select(
    game_id,passer_player_id,rusher_player_id,rusher_player_name,season,
         play_id, epa,posteam, defteam, passer_player_name, complete_pass, yardline_100,
         ydstogo, down, game_seconds_remaining, half_seconds_remaining, yards_after_catch,
         score_differential, run, home_team, away_team, desc, previous_play,in_red_zone
    ) %>%
  mutate(
    down = as.factor(down),
    quarter4 = if_else(game_seconds_remaining < 900,1,0),
    under2minute = if_else(half_seconds_remaining < 120, 1, 0),
    half_seconds_remaining = scale(half_seconds_remaining),
    away = if_else(home_team == posteam, 0, 1),
    prev_play_run = if_else(previous_play=='run',1,0),
    passer_id  = if_else(is.na(passer_player_id ),'',passer_player_id),
    rusher_id  = if_else(is.na(rusher_player_id ),'',rusher_player_id),
    QBplay = if_else( (passer_player_name %in% qblst) , 1 , 0),
    Quarterback = if_else(is.na(passer_player_name),rusher_player_name,passer_player_name),
    seas = as.factor(season),
    t = paste(season,game_id,play_id,sep='_')
    ) %>% 
  filter(
    QBplay == 1
  )

games_data<-read.csv(
  base::url("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv"))%>%
  select(
    -season,-week,-home_team,-away_team)

epa_teams_data <- left_join(epa_data,games_data,
                            by='game_id') %>% 
  select(
    -rusher_id,-passer_id,-rusher_player_id,
    -passer_player_id,-passer_player_name,
    -rusher_player_name
    ) %>% 
  mutate(
    outdoor = if_else(roof=='outdoors' | roof=='open',1,0),
    #open = if_else(roof=='open',1,0),
    closed = if_else(roof=='closed',1,0),
    pos_coach = if_else(home_team==posteam,home_coach,away_coach),
    def_coach = if_else(home_team==defteam,home_coach,away_coach)) %>% 
    filter(pos_coach %in% browns_coach$coach)

epa_teams_data[is.na(epa_teams_data$temp),'temp'] <- 50


plays <- epa_teams_data %>%
  group_by(pos_coach) %>% 
  summarize(num_plays = n()) %>% 
  ungroup()

colnames(plays)[1] <- 'level'

mixed<-lmer(epa ~
              yardline_100 + #situation
              ydstogo + #situation
              ydstogo*down + #situation
              half_seconds_remaining + #situation
              quarter4 + #situation
              down + #situation
              quarter4*down  + #situation
              score_differential + #situation
              score_differential*quarter4 + #situation
              in_red_zone + #situation 
              outdoor + #stadium 
              away + #stadium
              temp + #weather 
              run + #need to adjust for pass or QB run
              seas +  #adjusting for "era"/"epa inflation" 
              (1|Quarterback) + #random effects for QB
              (1|pos_coach)+ #random effects for head coach
              (1|def_coach), data=epa_teams_data) #random effects for opposing head coach


tt<-broom.mixed::tidy(mixed,effects="ran_vals") 
##### Matthew Coach Part
coach<-tt[tt$group=='pos_coach',] %>%
  merge(
    plays,by='level',all.x = TRUE,no.dups=TRUE) %>% 
  filter(
    num_plays > 1) %>%
  mutate(
    t_stat = estimate/std.error
  ); colnames(coach)[1]<-'coach'

coach<-coach[order(-coach$estimate),] %>% head(n=12)

z <- 1.96

coach_filt<-coach %>% filter(
  abs(t_stat) > z
); coach_filt<-coach_filt[order(coach_filt$estimate),]

coach %>%
  ggplot(aes(x=factor(coach, level = coach),estimate)) + 
  geom_point() +
  geom_pointrange(aes(ymin=estimate - z*std.error,
                      ymax=estimate + z*std.error))+
  coord_flip() +theme_bw() +
  labs(x = "Coach",
       y = "Random-effects intercept | Extra EPA/play",
       caption = "Data from nflfastR | EPA models from nflscrapR 
       by Adrian Cadena @adrian_cadem",
       title = "Additional EPA/play generated by Coach. Controlling for Situation, Roof Type, Home/Away, 
       Temperature (Weather), QB, Opposing HC, and Era",
       subtitle = paste(start,'to',end,"- Regular Season - Run and Pass Plays")) 

##### Adrian QB Part
QB<-tt[tt$group=='Quarterback',] %>%
  merge(
    plays,by='level',all.x = TRUE,no.dups=TRUE) %>% 
  filter(
    num_plays > 1) %>%
  mutate(
    t_stat = estimate/std.error
  ); colnames(QB)[1]<-'QB'

QB<-QB[order(-QB$estimate),] %>% head(n=32)

z <- 1.96

QB_filt<-QB %>% filter(
  abs(t_stat) > z
); QB_filt<-QB_filt[order(QB_filt$estimate),]

QB %>%
  ggplot(aes(x=factor(QB, level = QB),estimate)) + 
  geom_point() +
  geom_pointrange(aes(ymin=estimate - z*std.error,
                      ymax=estimate + z*std.error))+
  coord_flip() +theme_bw() +
  labs(x = "Quarterback",
       y = "Random-effects intercept | Extra EPA/play",
       caption = "Data from nflfastR | EPA models from nflscrapR 
       by Adrian Cadena @adrian_cadem",
       title = "Additional EPA/play generated by QB. Controlling for Situation, Roof Type, Home/Away, 
       Temperature (Weather), HC, Opposing HC, and Era",
       subtitle = paste(start,'to',end,"- Regular Season - Run and Pass Plays")) 
