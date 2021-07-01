library(tidyverse)
library(gt)
library(ggrepel)
library(ggimage)
library(nflfastR)

options(scipen = 9999)

games <- readRDS(url("http://www.habitatring.com/games.rds"))
str(games)

pbp_2020 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

rosters <- nflfastR::fast_scraper_roster(2020)

game_data <- games %>% 
  filter(game_id == "2020_01_CLE_BAL") %>% 
  left_join(pbp_2020) %>% 
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr')) 

run_stats <- game_data %>% 
  filter(down <= 4, play_type == 'run') %>%
  group_by(posteam_type, team_nick, rusher) %>%
  rename("player" = "rusher") %>% 
  summarize(
    type = "Rush Attempts",
    avg_epa = mean(epa), 
    epa = sum(epa),
    success_rate = mean(success), 
    first_down_pct = sum(first_down_rush)/n(),
    plays = n()
  ) %>%
  filter(plays > 0,
         !is.na(player)) %>%
  arrange(team_nick, -plays)

target_stats <- game_data %>% 
  filter(down <=4, play_type == 'pass') %>%
  group_by(posteam_type, team_nick, receiver) %>%
  rename("player" = "receiver") %>% 
  summarize(
    type = "Pass Targets",
    avg_epa = mean(epa), 
    epa = sum(epa),
    success_rate = mean(success), 
    first_down_pct = sum(first_down_pass)/n(),
    plays = n()
  ) %>% 
  filter(plays > 0,
         !is.na(player)) %>% 
  arrange(-plays)

pass_stats <- game_data %>% 
  filter(down <=4, play_type == 'pass') %>%
  group_by(posteam_type, team_nick, id, passer) %>%
  rename("player" = "passer") %>% 
  summarize(
    type = "Dropbacks",
    avg_epa = mean(qb_epa), 
    epa = sum(qb_epa),
    success_rate = mean(success), 
    first_down_pct = sum(first_down_pass)/n(),
    plays = n()
  ) %>% 
  filter(plays > 0,
         !is.na(player)) %>% 
  arrange(-plays)

qbs <- game_data %>%
  filter(!is.na(epa), play_type == "pass") %>%
  group_by(id, name) %>%
  summarize(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 0 & n_plays > 0)



adv_stats_player <- rbind(run_stats, pass_stats, target_stats) %>% 
  ungroup() %>% 
  filter(plays > 0) %>% 
  arrange(type,-plays) %>% 
  select(-id, -team_nick)

adv_stats_player_home <- adv_stats_player %>% 
  filter(posteam_type == "home") %>% 
  select(-posteam_type)
  
adv_stats_player_away <- adv_stats_player %>% 
  filter(posteam_type == "away") %>% 
  select(-posteam_type)

home_title <- paste0("Baltimore Ravens Advanced Box Score")

adv_stats_player_home_gt <- adv_stats_player_home %>% 
  gt(groupname_col = "type",
     rowname_col = "player") %>% 
  fmt_percent(columns = vars(first_down_pct), decimals = 1) %>%
  fmt_number(columns = vars(avg_epa, epa, success_rate), decimals = 2) %>%
  tab_header(
    title = home_title,
      #"Home Team Advanced Box Score",
    subtitle = "Week 1 2020"
  ) %>%
  cols_label(
    player = "Player",
    avg_epa = "Avg EPA",
    epa = "EPA",
    success_rate = "Success Rate",
    first_down_pct = "First Down %",
    plays = "Plays"
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  tab_source_note(
    source_note = "Table: Matthew Paley | Data: @nflfastR"
  ) 

adv_stats_player_home_gt

adv_stats_player_away_gt <- adv_stats_player_away %>% 
  gt(groupname_col = "type",
     rowname_col = "player") %>% 
  fmt_percent(columns = vars(first_down_pct), decimals = 1) %>%
  fmt_number(columns = vars(avg_epa, epa, success_rate), decimals = 2) %>%
  tab_header(
    title = "Away Team Advanced Box Score",
    subtitle = "Week 1 2020"
  ) %>%
  cols_label(
    player = "Player",
    avg_epa = "Avg EPA",
    epa = "EPA",
    success_rate = "Success Rate",
    first_down_pct = "First Down %",
    plays = "Plays"
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  tab_source_note(
    source_note = "Table: Matthew Paley | Data: @nflfastR"
  ) 

adv_stats_player_away_gt

adv_stats_type <- game_data %>% 
  filter(down <=4, 
         play_type == 'pass' | play_type == 'run') %>%
  group_by(team_nick, play_type) %>%
  summarize(
    epa = mean(epa), success_rate = mean(success), 
    wpa = mean(wpa), ypc = mean(yards_gained), plays = n()
  ) %>% 
  arrange(-epa)

adv_stats_type


## Win Probability chart
home_color <- teams_colors_logos %>% 
  filter(team_abbr == game_data$home_team[[1]])

away_color <- teams_colors_logos %>% 
  filter(team_abbr == game_data$away_team[[1]])

wp_chart <- game_data %>% 
  ggplot(aes(x = game_seconds_remaining, y = wp, color = team_nick)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c(away_color$team_abbr, home_color$team_abbr),
                     values = c(away_color$team_color, home_color$team_color),
                     guide = FALSE) +
  scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  annotate("text", x = 3000, y = .9, label = game_data$home_team, color = home_color$team_color, size = 4) + 
  annotate("text", x = 3000, y = .1, label = game_data$away_team, color = away_color$team_color, size = 4) +
  geom_vline(xintercept = 900, linetype = "dashed") + 
  geom_vline(xintercept = 1800, linetype = "dashed") + 
  geom_vline(xintercept = 2700, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  labs(
    x = "Time Remaining (seconds)",
    y = "Win Probability",
    title = "Week 1 Win Probability Chart",
    subtitle = "Cleveland Browns vs. Baltimore Ravens",
    caption = "by Matthew Paley") + theme_transparent()

wp_chart




mark_andrews <- game_data %>% 
  filter(down <=4, play_type == 'pass', receiver == "M.Andrews") %>%
  group_by(team_nick, passer) %>%
  rename("player" = "passer") %>% 
  summarize(
    epa = mean(epa), success_rate = mean(success), 
    wpa = mean(wpa), ypc = mean(yards_gained), plays = n()
  ) %>% 
  filter(plays > 0,
         !is.na(player)) %>% 
  arrange(-epa)

mark_andrews

game_data2 <- game_data %>% 
  left_join(rosters)


## Testing knitr
table <- game_data2 %>% 
filter(position %in% c('WR', 'TE', 'RB')) %>%
  group_by(receiver_id, receiver, position) %>%
  summarize(tot_epa = sum(epa), n=n()) %>%
  arrange(-tot_epa) %>%
  ungroup() %>%
  group_by(position) %>%
  mutate(position_rank = 1:n()) %>%
  filter(position_rank <= 50) %>%
  dplyr::rename(Pos_Rank = position_rank, Player = receiver, Pos = position, Tgt = n, EPA = tot_epa) %>%
  select(Player, Pos, Pos_Rank, Tgt, EPA) %>%
  knitr::kable(digits = 0)
