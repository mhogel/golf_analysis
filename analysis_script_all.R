library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

##### read in files ----

tourney_list <- readRDS(here::here("tournament_list_curyr.rData"))
tournament_results <- readRDS(here::here("leaderboard_scrape_curyr.rData"))

tourney_levels <- tourney_list %>%
  pull(tournament_names) %>%
  unique()

## Read in predictions

# get all folders
folders <- list.dirs()[grepl("_2021$", list.dirs())]

# find which ones don't have a output folder
drop <- which(folders %>% map_lgl( ~ sum(grepl("package", list.files(here::here(.x)))) == 0))

if(length(drop) > 0) {
  folders <- folders[-drop]
}

all_files <- folders %>%
  map(.f = ~ readRDS(here::here(.x, list.files(here::here(.x))[which(grepl("package", list.files(here::here(.x))))])))

all_results <- all_files %>%
  map_dfr(~ .x$value_calcs %>%
            mutate(tourney = .x$tournament)) %>%
  mutate(tourney = factor(tourney, levels = tourney_levels))

saveRDS(all_results, here::here("all_predictions.rds"))

##### set up data ----

current_results <- tournament_results %>%
  left_join(tourney_list %>%
              select(tournament_names, tournament_ids, tournament_year),
            by = c("tournament_id" = "tournament_ids")) %>%
  group_by(tournament_names, tournament_year) %>%
  mutate(cut = ifelse(!to_par %in% c("CUT", "WD", "DQ"), "Not Cut", to_par),
         across(.cols = c(tot, starts_with("r")), as.numeric)) %>%
  group_by(tournament_names, tournament_year,cut) %>%
  mutate(position = rank(tot, ties.method = "min")) %>%
  group_by(tournament_names, tournament_year) %>%
  mutate(position = ifelse(cut == "CUT", position + 100, ifelse(cut == "WD", position + 500, position)),
         final_rank = rank(position, ties.method = "min"),
         player = tolower(player)) %>%
  filter(tournament_year == 2021) %>%
  ungroup() %>%
  select(tournament_names, player, starts_with("r"), tot, final_rank) %>%
  mutate(tournament_names = factor(tournament_names, levels = tourney_levels))

##### 1) performance vs. odds projections ----

pred_full <- all_results %>%
  filter(category == "Outright") %>%
  group_by(tourney) %>%
  mutate(odds_rank = rank(-Prob, ties.method = "min"),
         model_rank = rank(-Predict, ties.method = "min")) %>%
  left_join(current_results %>%
              select(tournament_names, player, final_rank),
            by = c("tourney" = "tournament_names", "player")) %>%
  filter(!is.na(final_rank))

cor(pred_full$final_rank, pred_full$model_rank)
cor(pred_full$final_rank, pred_full$odds_rank)

##### 2) R1 matches & 3balls ----

r1_matches <- all_results %>%
  filter(category == "R1Matches") %>%
  left_join(current_results %>%
              select(tournament_names, player, r1),
            by = c ("tourney" = "tournament_names", "player")) %>%
  group_by(tourney, site, Tag) %>%
  mutate(return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         max_ev = max(EV),
         max_edge = max(Edge),
         low_score = min(r1),
         tied = sum(r1 == low_score) > 1) %>%
  filter(max_edge > 0.06) %>%
  mutate(result = case_when(
    EV > 0 & tied ~ "P",
    EV > 0 & r1 == low_score ~ "W",
    EV > 0 & r1 != low_score ~ "L"
  ),
  net = case_when(
    result == "W" ~ return,
    result == "L" ~ -100,
    result == "P" ~ 0
  ),
  grp_label = paste0(player, collapse = "_:_")) %>%
  group_by(category, tourney, grp_label, player) %>%
  mutate(rk = rank(Predict, ties.method = "first")) %>%
  filter(rk == 1)

table(r1_matches$result, r1_matches$tourney)
sum(r1_matches$net, na.rm = T)
sum(r1_matches$net, na.rm = T) / (sum(!is.na(r1_matches$net)) * 100)

r1_matches %>%
  filter(result != "P") %>%
  group_by(tourney) %>%
  summarise(w_pct = mean(result == "W"),
            net = sum(net))

r1_3balls <- all_results %>%
  filter(category == "R13Balls") %>%
  left_join(current_results %>%
              select(tournament_names, player, r1),
            by = c("tourney" = "tournament_names", "player")) %>%
  group_by(tourney, Tag) %>%
  mutate(count = n(),
         return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         max_ev = max(EV),
         max_edge = max(Edge),
         low_score = min(r1),
         num_tied = sum(r1 == low_score)) %>%
  filter(max_edge > 0.09,
         count == 3) %>%
  mutate(result = case_when(
    EV == max(EV) & r1 != low_score ~ "L",
    EV == max(EV) & r1 == low_score & num_tied == 1 ~ "W",
    EV == max(EV) & r1 == low_score & num_tied > 1 ~ "P"
  ),
  net = case_when(
    result == "W" ~ return,
    result == "L" ~ -100,
    result == "P" ~ ((1/num_tied) * return) - ((1- 1/num_tied) * 100)
  ))

table(r1_3balls$result, r1_3balls$tourney)
sum(r1_3balls$net, na.rm = T)
sum(r1_3balls$net, na.rm = T) / (sum(!is.na(r1_3balls$net)) * 100)

r1_3balls %>%
  filter(result != "P") %>%
  group_by(tourney) %>%
  summarise(w_pct = mean(result == "W"),
            net = sum(net))

##### 3) Tourney matches ----

tmatches_bodog <- all_results %>%
  filter(category == "TourneyMatches",
         site == "bodog") %>%
  left_join(current_results %>%
              select(tournament_names, player, final_rank),
            by = c("tourney" = "tournament_names", "player")) %>%
  group_by(tourney, Tag) %>%
  mutate(remove = sum(is.na(odds))  > 0,
         return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         max_ev = max(EV),
         max_edge = max(Edge),
         low_score = min(final_rank),
         num_tied = sum(final_rank == low_score)) %>%
  filter(max_edge > 0.06,
         !remove) %>%
  mutate(result = case_when(
    EV == max(EV) & final_rank != low_score ~ "L",
    EV == max(EV) & final_rank == low_score & num_tied == 1 ~ "W",
    EV == max(EV) & final_rank == low_score & num_tied > 1 ~ "P"
  ),
  net = case_when(
    result == "W" ~ return,
    result == "L" ~ -100,
    result == "P" ~ 0
  ),
  grp_label = paste0(player, collapse = "_:_")) %>%
  group_by(category, tourney, grp_label, player) %>%
  mutate(rk = rank(Predict, ties.method = "first")) %>%
  filter(rk == 1)

table(tmatches_bodog$result, tmatches_bodog$tourney)
sum(tmatches_bodog$net, na.rm = T)
sum(tmatches_bodog$net, na.rm = T) / (sum(!is.na(tmatches_bodog$net)) * 100)

tmatches_bodog %>%
  filter(result != "P") %>%
  group_by(tourney) %>%
  summarise(w_pct = mean(result == "W"),
            net = sum(net))

tmatches_365 <- all_results %>%
  filter(category == "TourneyMatches",
         site == "365") %>%
  left_join(current_results %>%
              select(tournament_names, player, final_rank),
            by = c("tourney" = "tournament_names", "player")) %>%
  group_by(tourney, Tag) %>%
  mutate(remove = sum(is.na(odds))  > 0,
         return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         max_ev = max(EV),
         max_edge = max(Edge),
         low_score = min(final_rank, na.rm = T),
         num_tied = sum(final_rank == low_score, na.rm = T)) %>%
  filter(max_edge > 0.06,
         !remove) %>%
  mutate(result = case_when(
    EV == max(EV) & final_rank != low_score ~ "L",
    EV == max(EV) & final_rank == low_score & num_tied == 1 & player != "tie" ~ "W",
    EV == max(EV) & final_rank == low_score & num_tied > 1  & player == "tie" ~ "W",
    EV == max(EV) & final_rank == low_score & num_tied > 1  & player != "tie" ~ "L",
    EV == max(EV) & num_tied == 1  & player == "tie" ~ "L"
  ),
  net = case_when(
    result == "W" ~ return,
    result == "L" ~ -100,
    result == "P" ~ 0
  ))

table(tmatches_365$result, tmatches_365$tourney)
sum(tmatches_365$net, na.rm = T)
sum(tmatches_365$net, na.rm = T) / (sum(!is.na(tmatches_365$net)) * 100)

tmatches_365 %>%
  filter(result != "P") %>%
  group_by(tourney) %>%
  summarise(w_pct = mean(result == "W"),
            net = sum(net))

##### Top results ----

tops <- all_results %>%
  filter(grepl("Top", category),
         site == "bodog",
         Edge >= 0.05) %>%
  left_join(current_results %>%
              filter(!is.na(final_rank)) %>%
              select(tournament_names, player, final_rank),
            by = c ("tourney" = "tournament_names", "player")) %>%
  mutate(spot = as.numeric(gsub("Top", "", category)))

hlines <- tops %>% select(tourney, spot) %>% distinct()

ggplot(tops, aes(x = tourney, y = final_rank, size = Edge)) +
  geom_point() +
  geom_hline(aes(yintercept = spot)) +
  facet_wrap(.~spot, ncol = 2) +
  scale_y_reverse()

r1_matches <- all_results %>%
  filter(category == "R1Matches") %>%

    left_join(current_results %>%
              select(tournament_names, player, r1),
            by = c ("tourney" = "tournament_names", "player")) %>%
  
  group_by(tourney, site, Tag) %>%
  mutate(return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         max_ev = max(EV),
         max_edge = max(Edge),
         low_score = min(r1),
         tied = sum(r1 == low_score) > 1) %>%
  filter(max_edge > 0.06) %>%
  mutate(result = case_when(
    EV > 0 & tied ~ "P",
    EV > 0 & r1 == low_score ~ "W",
    EV > 0 & r1 != low_score ~ "L"
  ),
  net = case_when(
    result == "W" ~ return,
    result == "L" ~ -100,
    result == "P" ~ 0
  ),
  grp_label = paste0(player, collapse = "_:_")) %>%
  group_by(category, tourney, grp_label, player) %>%
  mutate(rk = rank(Predict, ties.method = "first")) %>%
  filter(rk == 1)

r1_3balls <- all_results %>%
  filter(category == "R13Balls") %>%
  left_join(current_results %>%
              select(tournament_names, player, r1),
            by = c("tourney" = "tournament_names", "player")) %>%
  group_by(tourney, Tag) %>%
  mutate(count = n(),
         return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         max_ev = max(EV),
         max_edge = max(Edge),
         low_score = min(r1),
         num_tied = sum(r1 == low_score)) %>%
  filter(max_edge > 0.09,
         count == 3) %>%
  mutate(result = case_when(
    EV == max(EV) & r1 != low_score ~ "L",
    EV == max(EV) & r1 == low_score & num_tied == 1 ~ "W",
    EV == max(EV) & r1 == low_score & num_tied > 1 ~ "P"
  ),
  net = case_when(
    result == "W" ~ return,
    result == "L" ~ -100,
    result == "P" ~ ((1/num_tied) * return) - ((1- 1/num_tied) * 100)
  ))

tmatches_bodog <- all_results %>%
  filter(category == "TourneyMatches",
         site == "bodog") %>%
  left_join(current_results %>%
              select(tournament_names, player, final_rank),
            by = c("tourney" = "tournament_names", "player")) %>%
  group_by(tourney, Tag) %>%
  mutate(remove = sum(is.na(odds))  > 0,
         return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         max_ev = max(EV),
         max_edge = max(Edge),
         low_score = min(final_rank),
         num_tied = sum(final_rank == low_score)) %>%
  filter(max_edge > 0.06,
         !remove) %>%
  mutate(result = case_when(
    EV == max(EV) & final_rank != low_score ~ "L",
    EV == max(EV) & final_rank == low_score & num_tied == 1 ~ "W",
    EV == max(EV) & final_rank == low_score & num_tied > 1 ~ "P"
  ),
  net = case_when(
    result == "W" ~ return,
    result == "L" ~ -100,
    result == "P" ~ 0
  ),
  grp_label = paste0(player, collapse = "_:_")) %>%
  group_by(category, tourney, grp_label, player) %>%
  mutate(rk = rank(Predict, ties.method = "first")) %>%
  filter(rk == 1)

tmatches_365 <- all_results %>%
  filter(category == "TourneyMatches",
         site == "365") %>%
  left_join(current_results %>%
              select(tournament_names, player, final_rank),
            by = c("tourney" = "tournament_names", "player")) %>%
  group_by(tourney, Tag) %>%
  mutate(remove = sum(is.na(odds))  > 0,
         return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         max_ev = max(EV),
         max_edge = max(Edge),
         low_score = min(final_rank, na.rm = T),
         num_tied = sum(final_rank == low_score, na.rm = T)) %>%
  filter(max_edge > 0.06,
         !remove) %>%
  mutate(result = case_when(
    EV == max(EV) & final_rank != low_score ~ "L",
    EV == max(EV) & final_rank == low_score & num_tied == 1 & player != "tie" ~ "W",
    EV == max(EV) & final_rank == low_score & num_tied > 1  & player == "tie" ~ "W",
    EV == max(EV) & final_rank == low_score & num_tied > 1  & player != "tie" ~ "L",
    EV == max(EV) & num_tied == 1  & player == "tie" ~ "L"
  ),
  net = case_when(
    result == "W" ~ return,
    result == "L" ~ -100,
    result == "P" ~ 0
  ))
