library(purrr)
library(readr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(lubridate)
library(broom)

data_dir = 'adp/'

csv_files <- fs::dir_ls(data_dir, regexp = "\\.csv$")

df = csv_files %>% 
  map_dfr(read_csv) %>%
  select(Player, ScrapeDate, ADP, Team, `# Picks`, `Position(s)`, Rk) %>% 
  rename(Picks=`# Picks`, Position=`Position(s)`) %>%
  mutate(year = year(ScrapeDate) + yday(ScrapeDate) / 365)

daily_drafts = df %>% group_by(ScrapeDate) %>% summarise(picks = max(Picks))
plot(daily_drafts$ScrapeDate, daily_drafts$picks, type = 'l')

slopes <- df %>%
  nest(-Player) %>%
  mutate(model = map(data, ~ lm(Rk ~ year, .))) %>%
  unnest(map(model, tidy)) %>%
  filter(term == "year")

first_draft = df %>% 
  filter(ScrapeDate >= min(ScrapeDate), ScrapeDate <= min(ScrapeDate) + days(7)) %>%
  mutate(adp.agg = ADP * Picks) %>%
  group_by(Player) %>%
  summarise(adp.agg = sum(adp.agg),
            picks = sum(Picks)
            ) %>%
  mutate(ADP = round(adp.agg/picks, 1)) %>%
  arrange(ADP) %>%
  mutate(Rk = 1:n())

recent_draft = df %>% 
  filter(ScrapeDate >= max(ScrapeDate) - days(7), ScrapeDate <= max(ScrapeDate)) %>%
  mutate(adp.agg = ADP * Picks) %>%
  group_by(Player, Position, Team) %>%
  summarise(adp.agg = sum(adp.agg),
            picks = sum(Picks)) %>%
  mutate(ADP = round(adp.agg/picks, 1)) %>%
  arrange(ADP) %>%
  ungroup() %>%
  mutate(Rk = 1:n())

draft_trend = recent_draft %>%
  left_join(., first_draft %>% select(Player, Rk) %>% rename(start_rank=Rk), by = c("Player")) %>%
  select(-adp.agg) %>%
  mutate(Rk = ifelse(is.na(Rk), 300, Rk),
         start_rank = ifelse(is.na(start_rank), 300, start_rank),
         diff = Rk - start_rank) %>%
  filter(Rk <= 250) %>%
  group_by(Position) %>%
  mutate(rank = order(Rk, decreasing=F),
         pos_rank = paste(Position, rank, sep = '')) %>%
  left_join(., read_csv("rookies.csv"), by = "Player")

draft_trend$Rookie[is.na(draft_trend$Rookie)] = 0

recent_draft_players = recent_draft %>% mutate(max_picks = max(picks)) %>% filter(picks > max_picks - 5) %>% pull(Player)
slopes %>%
  arrange(desc(estimate)) %>%
  filter(Player %in% recent_draft_players) %>%
  head(20) %>%
  inner_join(df, by = "Player") %>%
  mutate(Player = reorder(Player, -estimate)) %>%
  ggplot(aes(year, Rk, color = Player)) +
  geom_point(aes(size = Picks), show.legend = FALSE) +
  geom_smooth(show.legend = F, se = F) +
  facet_wrap(~ Player, scales = "free_y") +
  expand_limits(y = 0) +
  scale_y_reverse() +
  theme_bw()

