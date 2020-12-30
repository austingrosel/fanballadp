library(purrr)
library(readr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(lubridate)
library(broom)
library(aws.s3)
library(plotly)

pos_df = df %>% 
  group_by(ScrapeDate) %>%
  mutate(pos_rank = row_number()) %>%
  ungroup() %>%
  mutate(Player = gsub(".", "", Player, fixed = T)) %>%
  mutate(
    Player = as.character(Player),
    Player = case_when(
      Player == "Jeffery Wilson" ~ "Jeff Wilson",
      TRUE ~ Player
    )
  )

# define which seasons shall be loaded
seasons <- 2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
}) %>% filter(week <= 16)

roster = read_csv("https://raw.githubusercontent.com/samhoppen/NFL_Positions/master/nfl_positions_2011_2020.csv")

passes = pbp %>%
  filter(!is.na(passer_player_id), sack == 0) %>%
  mutate(fp = yards_gained * 0.04 + touchdown * 4 - interception) %>%
  dplyr::select(season, play_id, game_id, yardline_100, air_yards, cp, fp, xyac_mean_yardage) %>%
  mutate(season.c = season - median(season),
         xyac_mean_yardage = ifelse(is.na(xyac_mean_yardage), 0, xyac_mean_yardage)) %>%
  filter(!is.na(air_yards), !is.na(cp), !is.na(xyac_mean_yardage))

rushes = pbp %>%
  filter(!is.na(rusher_player_id)) %>%
  mutate(fp = yards_gained/10 + touchdown * 6) %>%
  left_join(., roster %>% select(player_id, position), by = c("rusher_player_id"="player_id")) %>%
  dplyr::select(season, play_id, game_id, yardline_100, fp, position) %>%
  mutate(season.c = season - median(season)) %>%
  filter(!is.na(position))

rec = pbp %>%
  filter(!is.na(receiver_player_id)) %>%
  mutate(fp = complete_pass/2 + yards_gained/10 + touchdown * 6) %>%
  left_join(., roster %>% select(player_id, position), by = c("receiver_player_id"="player_id")) %>%
  dplyr::select(season, desc, play_id, game_id, yardline_100, air_yards, yards_gained, touchdown, position, cp, xyac_median_yardage, fp) %>%
  mutate(season.c = season - median(season),
         xyac_median_yardage = ifelse(is.na(xyac_median_yardage), 0, xyac_median_yardage)) %>%
  filter(!is.na(position), !is.na(cp), !is.na(xyac_median_yardage))

plays = pbp %>%
  filter(season %in% seasons, !game_id %in% '2011120406')

pass_box_score = plays %>%
  filter(!is.na(passer_player_id), !is.na(down), sack != 1, week <= 16) %>%
  rename(player_id=passer_player_id) %>%
  group_by(posteam, player_id, season, week, game_id) %>%
  summarise(
    attempts = n(),
    completions = sum(complete_pass, na.rm = T),
    yards = sum(yards_gained, na.rm = T),
    tds = sum(pass_touchdown),
    ints = sum(interception),
    air_yards = sum(air_yards, na.rm = T),
    yac = sum(yards_after_catch, na.rm = T)
  ) %>%
  left_join(., 
            plays %>%
              filter(!is.na(passer_player_id), !is.na(down), sack == 1) %>%
              rename(player_id = passer_player_id) %>%
              group_by(player_id, season, game_id) %>%
              summarise(sacks = n(), sack_yards = sum(yards_gained, na.rm = T), sack_fum = sum(fumble_lost, na.rm = T)), by = c("player_id", "season", "game_id")
  ) %>%
  distinct(game_id, player_id, .keep_all = T) %>%
  select(posteam, everything()) %>%
  mutate(sacks = ifelse(is.na(sacks), 0, sacks),
         sack_yards = ifelse(is.na(sack_yards), 0, sack_yards),
         sack_fum = ifelse(is.na(sack_fum), 0, sack_fum),
  )

rush_box_score = plays %>%
  filter(play_type == "run", !is.na(down), week <= 16) %>%
  rename(player_id=rusher_player_id) %>%
  group_by(posteam, player_id, season, week, game_id) %>%
  summarise(
    carries = n(),
    yards = sum(yards_gained, na.rm = T),
    tds = sum(rush_touchdown),
    rush_fum = sum(fumble_lost)
  ) %>%
  arrange(posteam, -carries, -yards, -tds) %>%
  left_join(., 
            plays %>%
              filter(play_type == "run") %>%
              group_by(posteam, game_id) %>%
              summarise(team_carries = n()), by = c("posteam", "game_id")
  ) %>%
  distinct(game_id, player_id, .keep_all = T) %>%
  select(posteam, everything())

rec_box_score = plays %>%
  filter(!is.na(receiver_player_id), !is.na(down), week <= 16) %>%
  rename(player_id=receiver_player_id) %>%
  group_by(posteam, player_id, season, week, game_id) %>%
  summarise(
    targets = n(),
    catches = sum(complete_pass),
    yards = sum(yards_gained, na.rm = T),
    tds = sum(pass_touchdown),
    rec_fum = sum(fumble_lost),
    air_yards = sum(air_yards, na.rm = T),
    yac = sum(yards_after_catch, na.rm = T)
  ) %>%
  left_join(., 
            plays %>%
              filter(play_type == "pass", !is.na(down), sack != 1) %>%
              group_by(posteam, game_id) %>%
              summarise(team_targets = n(), team_air_yards = sum(air_yards, na.rm = T)), by = c("posteam", "game_id")
  ) %>%
  distinct(game_id, player_id, .keep_all = T) %>%
  select(posteam, everything())

pass_score = pass_box_score %>%
  ungroup() %>%
  rename(pass_yards = yards, pass_tds = tds, pass_air_yards = air_yards) %>%
  select(season, week, game_id, player_id, posteam, attempts, completions, pass_yards, pass_air_yards, pass_tds, ints, sacks, sack_yards, sack_fum)

rush_score = rush_box_score %>%
  ungroup() %>%
  rename(rush_yards = yards, rush_tds = tds) %>%
  select(season, week, game_id, player_id, posteam, carries, rush_yards, rush_tds, rush_fum)

rec_score = rec_box_score %>%
  ungroup() %>%
  rename(rec_yards = yards, rec_tds = tds, rec_air_yards = air_yards) %>%
  select(season, week, game_id, player_id, posteam, targets, catches, rec_yards, rec_tds, rec_fum, rec_air_yards, yac)

rush_rec_box_score = merge(rush_score, rec_score, by = c("player_id", "posteam", "game_id", "season", "week"), all=T)

final_box_score_game = merge(pass_score, rush_rec_box_score, by = c("player_id", "posteam", "game_id", "season", "week"), all = T)
final_box_score_game[is.na(final_box_score_game)] = 0
final_box_score_game = final_box_score_game %>%
  mutate(fum_lost = rec_fum + rush_fum + sack_fum) %>%
  select(-rec_fum, -rush_fum, -sack_fum) %>%
  #left_join(., plays %>% filter(play_type == "run") %>% group_by(posteam, game_id, spread, total_line, implied_pts) %>% summarise(team_carries = n()), by = c("posteam", "game_id")) %>%
  left_join(., plays %>% filter(play_type == "pass", !is.na(down), sack != 1) %>% group_by(posteam, game_id) %>% summarise(team_targets = n(), team_air_yards = sum(air_yards, na.rm = T)), by = c("posteam", "game_id"))
final_box_score_game[is.na(final_box_score_game)] = 0

games =  pbp %>% 
  select(season, game_id, posteam) %>% 
  distinct() %>%
  filter(!is.na(posteam)) %>% 
  group_by(season, posteam) %>% 
  mutate(game_num = row_number())

final_box_score_game = final_box_score_game %>%
  ungroup() %>%
  mutate(over300_pass = ifelse(pass_yards >= 300, 1, 0),
         over400_pass = ifelse(pass_yards >= 400, 1, 0),
         over100_rush = ifelse(rush_yards >= 100, 1, 0),
         over175_rush = ifelse(rush_yards >= 175, 1, 0),
         over100_rec = ifelse(rec_yards >= 100, 1, 0),
         over175_rec = ifelse(rec_yards >= 175, 1, 0)
  ) %>%
  mutate(half.ppr = pass_yards * 0.04 + pass_tds * 4 - ints + 
           rush_yards * 0.1 + rush_tds * 6 - fum_lost +
           rec_yards/10 + rec_tds * 6 + catches/2,
        full.ppr = pass_yards * 0.04 + pass_tds * 4 - ints + 
           rush_yards * 0.1 + rush_tds * 6 - fum_lost +
           rec_yards/10 + rec_tds * 6 + catches#,
         # yahoo.ppr = pass_yards * 0.04 + pass_tds * 5 - ints * 2 + over300_pass * 3 + over400_pass * 3 +
         #   rush_yards * 0.1 + rush_tds * 6 - fum_lost + over100_rush * 3 + over175_rush * 3 +
         #   rec_yards/10 + rec_tds * 6 + catches/2 + over100_rec * 3 + over175_rush * 3,
  ) %>%
  left_join(., roster %>% select(-player), by = c("player_id")) %>%
  left_join(., games, by = c("season", "game_id", "posteam")) %>%
  select(player_id, full_player_name, position, everything()) %>%
  distinct(player_id, game_id, .keep_all = T) 

results = final_box_score_game %>%
  #filter(position == "QB") %>%
  group_by(player_id, full_player_name, position) %>%
  summarise(fp = round(sum(full.ppr, na.rm = T), 1)) %>%
  arrange(-fp) %>%
  ungroup() %>%
  group_by(position) %>%
  mutate(rank = row_number()) %>%
  ungroup()

cor_df = pos_df %>%
  left_join(., results, by = c("Player"="full_player_name")) %>%
  #mutate(rank = ifelse(is.na(rank), 100, rank)) %>%
  select(ScrapeDate, Position, pos_rank, rank) %>%
  nest(data = !c(ScrapeDate, Position)) %>%
  filter(Position != "TDSP") %>%
  mutate(test = map(data, ~ cor.test(.x$pos_rank, .x$rank, na.rm = T)),
         tidied = map(test, tidy)) %>%
  unnest(tidied) %>%
  select(ScrapeDate, Position, estimate) %>%
  ungroup()

ggplotly(ggplot(cor_df, aes(as.Date(ScrapeDate), estimate, color = Position)) + 
  geom_point() +
  geom_smooth() +
  theme_classic() + ylim(0, 1)
  )

