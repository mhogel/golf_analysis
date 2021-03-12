library(dplyr)
library(tidyr)
library(purrr)

tourney_folder <- "ArnoldPalmer_2021"

##### read in files ----

tourney_list <- readRDS(here::here("tournament_list_curyr.rData"))
tournament_results <- readRDS(here::here("leaderboard_scrape_curyr.rData"))

files <- list.files(here::here(tourney_folder))
tourney_package <- readRDS(here::here(tourney_folder, files[which(grepl("package", files))]))
tourney_name <- tourney_package$tournament

##### set up data ----

current_results <- tournament_results %>%
  filter(tournament_id == tourney_list$tournament_ids[tourney_list$tournament_names == tourney_name]) %>%
  mutate(cut = ifelse(!to_par %in% c("CUT", "WD"), "Not Cut", to_par),
         across(.cols = c(tot, starts_with("r")), .fns = ~ as.numeric(.))) %>%
  group_by(cut) %>%
  mutate(position = rank(tot, ties.method = "min")) %>%
  ungroup() %>%
  mutate(position = ifelse(cut == "CUT", position + 100, ifelse(cut == "WD", position + 500, position)),
         final_rank = rank(position, ties.method = "min"),
         player = tolower(player)) %>%
  select(player, starts_with("r"), tot, final_rank)

##### 1) performance vs. odds projections ----

pred_full <- tourney_package$value_calcs %>%
  filter(category == "Outright") %>%
  mutate(odds_rank = rank(-Prob, ties.method = "min"),
         model_rank = rank(-Predict, ties.method = "min")) %>%
  left_join(current_results %>%
              select(player, final_rank),
            by = "player") %>%
  filter(!is.na(final_rank))

cor(pred_full$final_rank, pred_full$model_rank)
cor(pred_full$final_rank, pred_full$odds_rank)

##### 2) R1 matches & 3balls ----

r1_matches <- tourney_package$value_calcs %>%
  filter(category == "R1Matches") %>%
  left_join(current_results %>%
              select(player, r1),
            by = "player") %>%
  group_by(site, Tag) %>%
  mutate(return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         max_ev = max(EV),
         max_edge = max(Edge),
         low_score = min(r1),
         tied = sum(low_score == r1) > 1) %>%
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
  ))

table(r1_matches$result)
sum(r1_matches$net, na.rm = T)
sum(r1_matches$net, na.rm = T) / (sum(!is.na(r1_matches$net)) * 100)

r1_3balls <- tourney_package$value_calcs %>%
  filter(category == "R13Balls") %>%
  left_join(current_results %>%
              select(player, r1),
            by = "player") %>%
  group_by(Tag) %>%
  mutate(count = n(),
         return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         max_ev = max(EV),
         max_edge = max(Edge),
         low_score = min(r1),
         num_tied = sum(r1 == low_score)) %>%
  filter(max_ev > 0,
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

table(r1_3balls$result)
sum(r1_3balls$net, na.rm = T)
sum(r1_3balls$net, na.rm = T) / (sum(!is.na(r1_3balls$net)) * 100)

##### 3) Tourney matches ----

tmatches_bodog <- tourney_package$value_calcs %>%
  filter(category == "TourneyMatches",
         site == "bodog") %>%
  left_join(current_results %>%
              select(player, final_rank),
            by = "player") %>%
  group_by(Tag) %>%
  mutate(remove = sum(is.na(odds))  > 0,
         return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         max_ev = max(EV),
         low_score = min(final_rank),
         num_tied = sum(final_rank == low_score)) %>%
  filter(max_ev > 0,
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
  ))

table(tmatches_bodog$result)
sum(tmatches_bodog$net, na.rm = T)
sum(tmatches_bodog$net, na.rm = T) / (sum(!is.na(tmatches_bodog$net)) * 100)

tmatches_365 <- tourney_package$value_calcs %>%
  filter(category == "TourneyMatches",
         site == "365") %>%
  left_join(current_results %>%
              select(player, final_rank),
            by = "player") %>%
  group_by(Tag) %>%
  mutate(remove = sum(is.na(odds))  > 0,
         return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         max_ev = max(EV),
         low_score = min(final_rank, na.rm = T),
         num_tied = sum(final_rank == low_score, na.rm = T)) %>%
  filter(max_ev > 0,
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

table(tmatches_365$result)
sum(tmatches_365$net, na.rm = T)
sum(tmatches_365$net, na.rm = T) / (sum(!is.na(tmatches_365$net)) * 100)

