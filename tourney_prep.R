
library(dplyr)
library(readxl)
library(tidyr)
library(lubridate)

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

field <- read_excel(here::here("Farmers_2021", "field.xlsx"), col_names = F) %>%
  setNames("name_raw")

##### set parameters ----

base_folder <- "Farmers_2021"
st_dt <- as.Date("2021-01-28")
tourney_name <- "Farmers Insurance Open"
course <- "Torrey Pines North"
cut_line <- 65
ignore_list <- c(401220113, 401056524, 401077168, 401251634)

# manually dampen the deviation - not the best method
Sd_Skew <- 0.6
course_weight <- 2
lookback_wks <- 6

##### get data ----

key_tourneys <- tourneys_list %>%
  filter(!tournament_ids %in% ignore_list,
         start_date < st_dt) %>%
  arrange(desc(start_date)) %>%
  left_join(select(., start_date) %>%
              distinct() %>%
              mutate(order = seq(1:n())),
            by = "start_date") %>%
  filter(order <= lookback_wks | tournament_names %in% tourney_name) %>%
  select(-order)

player_prep <- tourney_db %>%
  filter(tournament_id %in% key_tourneys$tournament_ids) %>%
  left_join(key_tourneys %>%
              select(tournament_id = tournament_ids, tournament_names, tournament_year),
            by = "tournament_id") %>%
  mutate(tournament_names = ifelse(tournament_names == tourney_name, "Current", tournament_names),
         player = tolower(player)) %>%
  select(player, tournament_year, tournament_names, starts_with("r")) %>%
  pivot_longer(cols = starts_with("r"),
               names_to = "Round",
               values_to = "Score") %>%
  filter(!is.na(Score),
         Score > 58) %>%
  distinct()

Tourney_History <- player_prep %>%
  filter(tournament_names == "Current")

player_data <- player_prep %>%
  group_by(tournament_names, tournament_year, Round) %>%
  mutate(Score = Norm_Fun(Score))

tourney_field <- clean_field(field)
sum(!tourney_field$check)

##### collect relevant tournament data ----
(Tourney_Avg <- mean(Tourney_History$Score))
(Tourney_Sd <- sd(Tourney_History$Score))

hist(Tourney_History$Score)

Tourney_Weights <- key_tourneys %>%
  select(tournament_year, tournament_names, tournament_sites, start_date) %>%
  mutate(tournament_names = ifelse(tournament_names == tourney_name, "Current", tournament_names),
         value = ifelse(tournament_year < 2021, rank(tournament_year) + 1, NA),
         course = ifelse(tournament_sites == course, course_weight, 0))

# set weights of lead up tournaments
Tourney_Weights %>%
  filter(tournament_year == 2021) %>%
  select(tournament_names, tournament_sites, start_date)

Tourney_Weights$value[Tourney_Weights$tournament_year == 2021] <- c(6,5,4,NA,3,2,3)
Tourney_Weights$full_weight <- Tourney_Weights$value + Tourney_Weights$course

##### establish player sampling parameters ----

Golfer_Scores <- player_data %>%
  filter(player %in% tourney_field$player) %>%
  left_join(Tourney_Weights, by = c("tournament_names", "tournament_year")) %>%
  mutate(Weighted_Score = Score * full_weight) %>%
  group_by(player) %>%
  mutate(Weighted_Avg = sum(Weighted_Score) / sum(full_weight),
         Weighted_sd_Rd = (Score - Weighted_Avg)^2 * full_weight,
         Total_Weighted_sd = sqrt(sum(Weighted_sd_Rd)/sum(full_weight) * ((n()-1)/n()))* Sd_Skew) %>%
  select(player, Weighted_Avg, Total_Weighted_sd) %>%
  distinct()

tourney_min <- min(Tourney_History$Score)
tourney_max <- max(Tourney_History$Score)

Standardized_Scores <- data.frame(Score = seq(from = tourney_min, to = tourney_max)) %>%
  mutate(z.score = (Score - Tourney_Avg)/Tourney_Sd)

Tourney_Preds <- as.data.frame(tourney_field) %>%
  select(player) %>%
  left_join(Golfer_Scores, by = "player")

temp_db <- matrix(nrow = nrow(Tourney_Preds), ncol = nrow(Standardized_Scores),
                  dimnames = list(Tourney_Preds$player, Standardized_Scores$Score))

Score_Matrix <- as.data.frame(temp_db) %>%
  mutate(player = rownames(temp_db)) %>%
  pivot_longer(cols = -player, names_to = "Score", values_to = "temp") %>%
  mutate(Score = as.numeric(Score)) %>%
  arrange(player) %>%
  left_join(Standardized_Scores, by = "Score") %>%
  left_join(Tourney_Preds, by = "player") %>%
  mutate(Total_Weighted_sd = 2*Total_Weighted_sd,
         Score.Prob = pnorm(z.score, Weighted_Avg, Total_Weighted_sd)) %>%
  select(-temp)

player_avg_pred <- Tourney_Preds %>%
  mutate(avg_score = sapply(Weighted_Avg, function(x) max(Standardized_Scores$Score[Standardized_Scores$z.score <= x]))) %>%
  select(-c(Total_Weighted_sd, Weighted_Avg))
  
##### Model tournament ----

Repeats <- 10000
Rounds <- c("Rd1", "Rd2", "Rd3", "Rd4")

Ongoing_Tally <- data.frame()

base_db <- Tourney_Preds %>%
  filter(!is.na(Weighted_Avg)) %>%
  select(player)

start <- Sys.time()

board <- do.call("rbind", replicate(Repeats, base_db %>% select(player), simplify = F)) %>%
  group_by(player) %>%
  mutate(Run = rank(player, ties.method = "first"))

for(i in 1:length(Rounds)) {
  
  results <- board %>%
    mutate(prob = runif(n()),
           Score = mapply(find_score, player, prob),
           prob.2 = runif(n()),
           Score.2 = ifelse(Score <= 64, mapply(find_score, player, prob.2), NA),
           Score.2 = ifelse(Score.2 > 69, 69, Score.2),
           Score = ifelse(is.na(Score.2), Score, Score.2)) %>%
    select(player, Run, Score)
  
  results[results == "-Inf"] <- 61
  
  board[Rounds[i]] <- results$Score
  
}

end <- Sys.time()
(total <- end - start)

saveRDS(board, here::here("Farmers_2021","Tourney_Results.rData"))

Ongoing_Tally <- board %>%
  group_by(Run) %>%
  mutate(r2_rank = rank(Rd1 + Rd2, ties.method = "min"),
         Full.Score = Rd1 + Rd2 + Rd3 + Rd4,
         Cut = r2_rank <= cut_line,
         Cut.Rank = rank(r2_rank, ties.method = "min"),
         Full.Score = ifelse(!Cut, NA, Full.Score),
         Temp.Rank = rank(Full.Score, ties.method = "min"),
         Final.Rank = ifelse(Temp.Rank == 1, rank(Temp.Rank, ties.method = "random"), ifelse(Cut, Temp.Rank, Cut.Rank)),
         player = tolower(player)) %>%
  select(player,Final.Rank, Run, Cut)

Final.Results <- Ongoing_Tally %>%
  group_by(player) %>%
  summarise(Outright = sum(Final.Rank == 1, na.rm = T)/Repeats,
            Top5 = sum(Final.Rank <= 5, na.rm = T)/Repeats,
            Top10 = sum(Final.Rank <= 10, na.rm = T)/Repeats,
            Top20 = sum(Final.Rank <= 20, na.rm = T)/Repeats,
            MissCut = mean(!Cut),
            MakeCut = mean(Cut)) %>%
  pivot_longer(cols = -player, names_to = "category", values_to = "Predict") %>%
  arrange(category, desc(Predict))

# pull rounds

Rounds_Long <- rbind(board[,c(1,3)] %>% setNames(c("player", "Score")),
                     board[,c(1,4)] %>% setNames(c("player", "Score"))) %>%
  group_by(player) %>%
  mutate(Run = rank(player, ties.method = "first"),
         player = tolower(player))

rbind(Rounds_Long %>% ungroup() %>% select(Score) %>% mutate(Method = "Predict"),
                      Tourney_History %>% select(Score) %>% mutate(Method = "Actual")) %>%
  group_by(Method, Score) %>%
  count() %>%
  group_by(Method) %>%
  mutate(Prop = n / sum(n)) %>%
  ggplot(aes(x = Score, y = Prop, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()

##### Assess value ----

# set up odds sheet
odds_setup <- odds_df %>%
  left_join(odds_df %>%
              filter(site == "bodog", category == "R13Balls") %>%
              select(player, category, rock_tag = Tag),
            by = c("category", "player")) %>%
  mutate(Tag = ifelse(category == "R13Balls", rock_tag, Tag),
         temp_tag = ifelse(is.na(Tag), NA, paste0(site, "-", Tag)),
         temp_tag = ifelse(grepl("3Balls", category), gsub("^.+-", "", temp_tag), temp_tag),
         player = sapply(player, Clean_Players),
         Return = ifelse(odds > 0, odds, 100^2/abs(odds))) %>%
    select(-rock_tag)

# shrink for categories where we want only the best price
price_shrink <- odds_setup %>%
  filter(grepl("(Cut|Outright|3Balls)", category)) %>%
  group_by(category, player) %>%
  mutate(keep = rank(Prob, ties.method = "first")) %>%
  filter(keep == 1) %>%
  select(-keep)

odds_setup <- rbind(
  odds_setup %>%
    filter(!category %in% price_shrink$category),
  price_shrink
)

max_run <- max(Ongoing_Tally$Run)
value_df <- data.frame()

# 1 > tourney outcomes which pay ties
tmp_db <- odds_setup %>%
  filter(category %in% c("Outright", "MakeCut", "MissCut") |
           (grepl("Top", category) & site == "bodog")) %>%
  left_join(Final.Results,
            by  = c("category", "player")) %>%
  mutate(Edge = Predict - Prob,
         EV = (Predict * Return) - 100 * (1-Predict)) %>%
  select(category, site, Tag, player, odds, Prob, Predict, Edge, EV)

value_df <- rbind(
  value_df,
  tmp_db
)

# 2 > tourney outcomes with dead heat rules

tmp_db <- odds_setup %>%
  filter(grepl("Top", category),
         site == "365")

cats_tmp <- unique(tmp_db$category)

for(i in 1:length(cats_tmp)) {
  
  thresh <- as.numeric(gsub("Top","", cats_tmp[i]))
  
  value_df <- rbind(
    value_df,
    
    Ongoing_Tally %>%
      filter(Final.Rank <= thresh) %>%
      group_by(Run) %>%
      mutate(
        last_rank = max(Final.Rank),
        num.tied = sum(Final.Rank == last_rank)) %>%
      left_join(tmp_db %>%
                  filter(category == cats_tmp[i]),
                by = "player") %>%
      filter(!is.na(odds)) %>%
      mutate(check = Final.Rank < last_rank,
             paid = case_when(
               check ~ Return,
               !check ~ ((1/num.tied) * Return) - ((1- 1/num.tied) * 100)
             )) %>%
      group_by(player, odds, Return, Prob) %>%
      summarise(pos_EV = sum(paid),
                cashes = n()) %>%
      mutate(neg_EV = (max_run - cashes) * 100,
             EV = (pos_EV - neg_EV)/max_run,
             Predict = (EV+100)/(Return + 100),
             Edge = Predict - Prob,
             category = cats_tmp[i],
             site = "365",
             Tag = NA) %>%
      ungroup() %>%
      select(category, site, Tag, player, odds, Prob, Predict, Edge, EV)
  )
}

# 3 > 6 shooters

tmp_db <- odds_setup %>%
  filter(category == "SixShooter")

tags_tmp <- unique(tmp_db$temp_tag)

for(i in 1:length(unique(tags_tmp))) {
  
  value_df <- rbind(
    value_df,
    Ongoing_Tally %>%
      filter(player %in% tmp_db$player[tmp_db$temp_tag == tags_tmp[i]]) %>%
      group_by(Run) %>%
      mutate(
        top_rank = min(Final.Rank),
        num.tied = sum(Final.Rank == top_rank)) %>%
      left_join(tmp_db %>%
                  filter(temp_tag == tags_tmp[i]),
                by = "player") %>%
      filter(!is.na(odds)) %>%
      mutate(check = Final.Rank == top_rank,
             paid = case_when(
               check & num.tied == 1 ~ Return,
               check & num.tied > 1 ~ ((1/num.tied) * Return) - ((1- 1/num.tied) * 100),
               T ~ -100
             )) %>%
      group_by(category, site, Tag, player, odds, Return, Prob) %>%
      summarise(EV = sum(paid)/max_run,
                .groups = "drop") %>%
      mutate(Predict = (EV+100)/(Return + 100),
             Edge = Predict - Prob) %>%
      select(category, site, Tag, player, odds, Prob, Predict, Edge, EV)
    
  )
}

# 4 > Tourney matchups with pushes

tmp_db <- odds_setup %>%
  filter(category == "TourneyMatches",
         site == "bodog")

tags_tmp <- unique(tmp_db$temp_tag)

for(i in 1:length(unique(tags_tmp))) {
  
  value_df <- rbind(
    value_df,
    Ongoing_Tally %>%
      filter(player %in% tmp_db$player[tmp_db$temp_tag == tags_tmp[i]]) %>%
      group_by(Run) %>%
      mutate(Tie.Check = sum(Final.Rank == min(Final.Rank)) != 2) %>%
      filter(Tie.Check) %>%
      mutate(Winner = Final.Rank == min(Final.Rank)) %>%
      group_by(player) %>%
      summarise(Predict = mean(Winner),
                .groups = "drop") %>%
      left_join(tmp_db %>%
                  filter(temp_tag == tags_tmp[i]),
                by = "player") %>%
      mutate(Edge = Predict - Prob,
             EV = (Predict * Return) - 100 * (1-Predict)) %>%
      select(category, site, Tag, player, odds, Prob, Predict, Edge, EV)
  )
}

# 5 > Tourney matchups where ties lose

tmp_db <- odds_setup %>%
  filter(category == "TourneyMatches",
         site == "365")

tags_tmp <- unique(tmp_db$temp_tag)

for(i in 1:length(tags_tmp)) {
  
  db <- Ongoing_Tally %>%
    filter(player %in% tmp_db$player[tmp_db$temp_tag == tags_tmp[i]]) %>%
    group_by(Run) %>%
    mutate(Tie = sum(Final.Rank == min(Final.Rank)) == 2,
           Winner = ifelse(Tie, NA, Final.Rank == min(Final.Rank))) %>%
    group_by(player) %>%
    summarise(Predict = sum(Winner, na.rm = T),
              Ties = sum(is.na(Winner)),
              .groups = "drop") %>%
    rbind(
      slice(., 1) %>%
        mutate(player = "tie",
               Predict = Ties)
    ) %>%
    select(-Ties) %>%
    mutate(Predict = Predict / sum(Predict)) %>%
    left_join(tmp_db %>%
                filter(temp_tag == tags_tmp[i]),
              by = "player") %>%
    mutate(Edge = Predict - Prob,
           Return = ifelse(odds > 0, odds, 100^2/abs(odds)),
           EV = Return * Predict - 100 * (1-Predict)) %>%
    select(category, site, Tag, player, odds, Prob, Predict, Edge, EV)
    
    
  value_df <- rbind(
    value_df,
    db
  )
}

# 6 > 3 balls

tmp_db <- odds_setup %>%
  filter(category == "R13Balls",
         !is.na(Tag))

tags_tmp <- unique(tmp_db$temp_tag)

for(i in 1:length(tags_tmp)) {
  
  value_df <- rbind(
    value_df,
    
    Rounds_Long %>%
      filter(player %in% tmp_db$player[tmp_db$temp_tag == tags_tmp[i]]) %>%
      group_by(Run) %>%
      mutate(
        top_score = min(Score),
        num.tied = sum(Score == top_score)) %>%
      left_join(tmp_db %>%
                  filter(temp_tag == tags_tmp[i]),
                by = "player") %>%
      filter(!is.na(odds)) %>%
      mutate(check = Score == top_score,
             paid = case_when(
               check & num.tied == 1 ~ Return,
               check & num.tied > 1 ~ ((1/num.tied) * Return) - ((1- 1/num.tied) * 100),
               T ~ -100
             )) %>%
      group_by(category, site, Tag, player, odds, Return, Prob) %>%
      summarise(EV = sum(paid)/max_run,
                .groups = "drop") %>%
      mutate(Predict = (EV+100)/(Return + 100),
             Edge = Predict - Prob) %>%
      select(category, site, Tag, player, odds, Prob, Predict, Edge, EV)
  )
}

# 7 > Round matches

tmp_db <- odds_setup %>%
  filter(category == "R1Matches")

tags_tmp <- unique(tmp_db$temp_tag)

for(i in 1:length(tags_tmp)) {
  
  value_df <- rbind(
    value_df,
    
  Rounds_Long %>%
    filter(player %in% tmp_db$player[tmp_db$temp_tag == tags_tmp[i]]) %>%
    group_by(Run) %>%
    mutate(Tie.Check = sum(Score == min(Score)) != 2) %>%
    filter(Tie.Check) %>%
    mutate(Winner = Score == min(Score)) %>%
    group_by(player) %>%
    summarise(Predict = mean(Winner),
              .groups = "drop") %>%
    left_join(tmp_db %>%
                filter(temp_tag == tags_tmp[i]),
              by = "player") %>%
    mutate(Edge = Predict - Prob,
           EV = (Predict * Return) - 100 * (1-Predict)) %>%
    select(category, site, Tag, player, odds, Prob, Predict, Edge, EV)
  )
}

##### output tournament package ----

output_list <- list(
  tournament = tourney_name,
  year = year(st_dt),
  course = course,
  player_predictions = Rounds_Long %>%
    group_by(player) %>%
    summarise(avg_score = mean(Score),
              sd = sd(Score),
              .groups = "drop"),
  value_calcs = value_df
)

saveRDS(output_list, here::here(base_folder, paste0("Tourney package_", base_folder, ".rData")))
