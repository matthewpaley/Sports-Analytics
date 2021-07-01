#Browns Game PBP Animation
library(nflscrapR)
library(dplyr)
library(RCurl)
library(jsonlite)
library(tidyr)
library(sp)
library(ggplot2)
library(data.table)
library(teamcolors)

#Get Data
games2018 <- season_play_by_play(2018)
brownsGameIDs <- unique(browns$GameID)

#Get Individual Game Data
week_7_games <- scrape_game_ids(2018, weeks = 7)

browns_wk7_pbp <- week_7_games %>%
  filter(away_team == "CLE") %>%
  pull(game_id) %>%
  scrape_json_play_by_play()

#Win Prob Chart
# Pull out the Browns and Saints colors:
nfl_teamcolors <- teamcolors %>% filter(league == "nfl")
cle_color <- nfl_teamcolors %>%
  filter(name == "Cleveland Browns") %>%
  pull(primary)
tb_color <- nfl_teamcolors %>%
  filter(name == "Tampa Bay Buccaneers") %>%
  pull(primary)

# Now generate the win probability chart:
browns_wk7_pbp %>%
  filter(!is.na(home_wp),
         !is.na(away_wp)) %>%
  dplyr::select(game_seconds_remaining,
                home_wp,
                away_wp) %>%
  tidyr::gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c("CLE", "TB"),
                     values = c(cle_color, tb_color),
                     guide = FALSE) +
  scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  annotate("text", x = 3000, y = .75, label = "CLE", color = cle_color, size = 8) + 
  annotate("text", x = 3000, y = .25, label = "TB", color = tb_color, size = 8) +
  geom_vline(xintercept = 900, linetype = "dashed", black) + 
  geom_vline(xintercept = 1800, linetype = "dashed", black) + 
  geom_vline(xintercept = 2700, linetype = "dashed", black) + 
  geom_vline(xintercept = 0, linetype = "dashed", black) + 
  labs(
    x = "Time Remaining (seconds)",
    y = "Win Probability",
    title = "Week 6 Win Probability Chart",
    subtitle = "Cleveland Browns vs. Tampa Bay",
    caption = "by Matthew Paley") + theme_bw()
