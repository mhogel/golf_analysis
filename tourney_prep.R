library(dplyr)


##### load data ----

tourney_db <- rbind(
  readRDS(here::here("leaderboard_scrape_cumulative.rData")),
  readRDS(here::here("leaderboard_scrape_curyr.rData"))
) %>%
  mutate(earnings = gsub("(\\$|,)", "", earnings),
         across(.fns = ~ ifelse(. == "--", NA, .)),
         across(.cols = -c(pos, player, to_par), .fns = as.numeric))

tourneys_list <- rbind(
  readRDS(here::here("tournament_list_curyr.rData")),
  readRDS(here::here("tournament_list.rData"))
) %>%
  mutate(across(.cols = c(tournament_ids, tournament_year), as.numeric))

##### set parameters ----

st_dt <- as.Date("2021-01-21")
tourney_name <- "The American Express"
ignore_list <- c(401220113, 401056524, 401077168, 401251634)
course <- tourneys_list$tournament_sites[tourneys_list$tournament_names == tourney_name & tourneys_list$tournament_year == 2021]

##### get data ----

key_tourneys <- tourneys_list %>%
  filter(!tournament_ids %in% ignore_list,
         start_date < st_dt) %>%
  arrange(desc(start_date)) %>%
  left_join(select(., start_date) %>%
              distinct() %>%
              mutate(order = seq(1:n())),
            by = "start_date") %>%
  filter(order <= 5 | tournament_names %in% tourney_name) %>%
  group_by(tournament_year) %>%
  mutate(wgt_factor = rank(order),
         wgt_factor = ifelse(tournament_year == 2021, abs(order-6), abs(wgt_factor-6)),
         wgt_factor = ifelse(tournament_year < 2021 & tournament_sites != course, max(1, wgt_factor - 2), wgt_factor)) %>%
  select(-order) %>%
  ungroup()

player_data <- tourney_db %>%
  filter(tournament_id %in% key_tourneys$tournament_ids) %>%
  left_join(key_tourneys %>%
              select(tournament_id = tournament_ids, wgt_factor),
            by = "tournament_id") %>%
  select(player, starts_with("r"), wgt_factor)
