library(fitdistrplus)
library(dplyr)
library(readxl)
library(tidyr)
library(lubridate)
library(ggplot2)

##### load data ----

base_folder <- "ArnoldPalmer_2021"

source(here::here("predict_script_functions.R"))

# refresh current year data if required
update_tourneys()


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
  mutate(across(.cols = c(tournament_ids, tournament_year), as.numeric),
         tournament_names = case_when(
           tournament_names == "Genesis Open" ~ "The Genesis Invitational",
           grepl("Mexico", tournament_names) ~"WGC-Workday Championship",
           grepl("Arnold Palmer", tournament_names) ~ "Arnold Palmer Invitational Pres. By Mastercard",
           T ~ tournament_names))

field <- read_excel(here::here(base_folder, "field.xlsx"), col_names = F) %>%
  setNames("name_raw")


##### set parameters ----

st_dt <- as.Date("2021-03-04")
tourney_name <- "Arnold Palmer Invitational Pres. By Mastercard"
course <- "Bay Hill Club &amp; Lodge"
cut_line <- 65
ignore_list <- c(401220113, 401056524, 401077168, 401251634)

# manually dampen the deviation - not the best method
Sd_Skew <- 0.6
course_weight <- 2
lookback_wks <- 5

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
  select(player, tournament_year, tournament_names, pos, starts_with("r")) %>%
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
         value = ifelse(tournament_year < 2021, rank(tournament_year),NA),
         course = ifelse(tournament_year < 2021 & tournament_sites != course, -1 * course_weight, 0))

# set weights of lead up tournaments
Tourney_Weights %>%
  filter(tournament_year == 2021) %>%
  select(tournament_names, tournament_sites, start_date)

Tourney_Weights$value[Tourney_Weights$tournament_year == 2021] <- c(2,5:1)
Tourney_Weights$full_weight <- Tourney_Weights$value + Tourney_Weights$course
Tourney_Weights$full_weight[Tourney_Weights$full_weight < 1] <- 1

##### establish player sampling parameters ----

# generate player data to feed into beta sampling

player_samples <- data.frame()

for(i in 1:nrow(Tourney_Weights)) {
  
  df <- player_data %>%
    ungroup() %>%
    filter(tournament_year == Tourney_Weights[i,"tournament_year"],
           tournament_names == Tourney_Weights[i, "tournament_names"]) %>%
    select(player, Score)
  
  player_samples <- rbind(player_samples,
                          do.call("rbind" ,replicate(Tourney_Weights[i, "full_weight"], df, simplify = F)))
}

player_samples <- player_samples %>%
  filter(player %in% tourney_field$player) %>%
  mutate(Score_std = (abs(Score - max(Score))) / (max(Score) - min(Score)),
         Score_std = case_when(
           Score_std == 1 ~ max(Score_std[Score_std < 1]),
           Score_std == 0 ~ min(Score_std[Score_std > 0]),
           T ~ Score_std
         ))

beta_key <- tibble(player_samples) %>%
  select(player) %>%
  distinct() %>%
  mutate(beta = I(list(NA)))

for(i in 1:nrow(beta_key)) {
  
  db <- player_samples %>%
    filter(player == tmp$player[i]) %>%
    pull(Score_std)
  
  beta_key$beta[i] <- list(fitdist(db, "beta")$estimate)
  
}

historical_scores <- Tourney_History %>%
  mutate(score_std = abs(Score - max(Score)) / (max(Score) - min(Score))) %>%
  select(Score, score_std) %>%
  distinct() %>%
  arrange(Score)

tourney_beta <- fitdist(tourney_std$score_std, "beta")$estimate

tourney_prob <- Tourney_History %>%
  arrange(Score) %>%
  mutate(rk = 1:n(),
         prob = 1-(rk / max(rk)))

Standardized_Scores <- data.frame(Score = seq(from = tourney_min, to = tourney_max)) %>%
  mutate(z.score = (Score - Tourney_Avg)/Tourney_Sd,
         score_std = abs(z.score - max(z.score)) / (max(z.score) - min(z.score)))


##### Model tournament ----
Repeats <- 10000
beta_board <- data.frame()
for(i in 1:nrow(beta_key)) {
  
  get_score <- rbeta(Repeats * 4, beta_key$beta[[i]][1], beta_key$beta[[i]][2])
#  get_p <- qbeta(get_score, tourney_beta[[1]], tourney_beta[[2]])
#  get_p[get_p > max(tourney_prob$prob)] <- max(tourney_prob$prob)
#  final_score <- sapply(get_score, function(x) min(historical_scores$Score[historical_scores$score_std <= x]))
#  final_score <- sapply(get_p, function(x) max(tourney_prob$Score[tourney_prob$prob >= x]))
#  get_score[get_score > max(tourney_prob$prob)] <- max(tourney_prob$prob)
#  final_score <- sapply(get_score, function(x) max(tourney_prob$Score[tourney_prob$prob >= x]))
  
  beta_board <- rbind(beta_board,
                      as.data.frame(matrix(get_score, ncol = 4)) %>% 
                        setNames(c(paste0("Rd", 1:4))) %>% 
                        mutate(player = beta_key$player[i],
                               Run = seq(from = 1, to = n())))
  
}

beta_board <- select(beta_board, player, Run, matches("Rd"))

trnslt <- function(x) {
  
  h <- rank(-x)
  h <- 1 - h / max(h)
  sapply(h, function(x) max(tourney_prob$Score[tourney_prob$prob > x]) )
  
}

for(i in 3:6){
  beta_board[,i] <- trnslt(beta_board[,i])
}

beta_board[,3:6][beta_board[,3:6] == "-Inf"] <- tourney_min

Ongoing_Tally <- beta_board %>%
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
  summarise(Outright = sum(Final.Rank == 1, na.rm = T)/max(Ongoing_Tally$Run),
            Top5 = sum(Final.Rank <= 5, na.rm = T)/max(Ongoing_Tally$Run),
            Top10 = sum(Final.Rank <= 10, na.rm = T)/max(Ongoing_Tally$Run),
            Top20 = sum(Final.Rank <= 20, na.rm = T)/max(Ongoing_Tally$Run),
            MissCut = mean(!Cut),
            MakeCut = mean(Cut),
            .groups = "drop") %>%
  pivot_longer(cols = -player, names_to = "category", values_to = "Predict") %>%
  arrange(category, desc(Predict))

# pull rounds

Rounds_Long <- rbind(beta_board[,c(1,3)] %>% setNames(c("player", "Score")),
                     beta_board[,c(1,4)] %>% setNames(c("player", "Score"))) %>%
  group_by(player) %>%
  mutate(Run = rank(player, ties.method = "first"),
         player = tolower(player))

rbind(Rounds_Long_test %>% ungroup() %>% select(Score = adjusted_score) %>% mutate(Method = "Predict"),
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
              filter(site == "365", category == "R13Balls") %>%
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

get_max <- function(cat, st) {
  
  if(cat == "R13Balls"){
    
    value_df %>% 
      filter(category == cat) %>%
      group_by(Tag) %>%
      mutate(max_edge = max(Edge)) %>%
      pull(max_edge)
  } else if(cat %in% c("R1Matches", "TourneyMatches")) {
    
    value_df %>% 
      filter(category == cat) %>%
      group_by(site, Tag) %>%
      mutate(max_edge = max(Edge)) %>%
      pull(max_edge)
    
  } else {
    value_df %>%
      filter(category == cat) %>%
      pull(Edge)
  }
}


grouping_key <- data.frame(
  group = c("FRL", "FRL", "Finals/Cut", "Finals/Cut", "TourneyMatches",  "TourneyMatches", "RoundMatches", "3balls"),
  site = c(rep(c("bodog", "365"),3 ), "both", "both"),
  outcome = c("r", "r", rep("t",4), rep("r", 2))
)

#for(i in 1:nrow(grouping_key)) {
  
  build_df(grouping_key[i,])
  
}

#build_df <- function(g_row) {
  
  if(g_row$outcome == "r") {
    tmp_db <- left_join(odds_setup, )
  }
  
  tmp_db <- odds_setup %>%
    filter(category %in% c("Outright", "MakeCut", "MissCut") |
             (grepl("Top", category) & site == "bodog")) %>%
    left_join(Final.Results,
              by  = c("category", "player")) %>%
    mutate(Edge = Predict - Prob,
           EV = (Predict * Return) - 100 * (1-Predict)) %>%
    select(category, site, Tag, player, odds, Prob, Predict, Edge, EV)
  
  
}


# 1 > tourney outcomes which pay ties
tmp_db <- odds_setup %>%
  filter(category %in% c("Outright", "MakeCut", "MissCut") |
           (grepl("Top", category) & site == "bodog")) %>%
  left_join(Final.Results,
            by  = c("category", "player")) %>%
  mutate(Edge = Predict - Prob,
         EV = (Predict * Return) - 100 * (1-Predict)) %>%
  select(category, site, Tag, player, odds, Prob, Predict, Edge, EV) %>%
  mutate(max_edge = Edge)

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
                cashes = n(),
                .groups = "drop") %>%
      mutate(neg_EV = (max_run - cashes) * 100,
             EV = (pos_EV - neg_EV)/max_run,
             Predict = (EV+100)/(Return + 100),
             Edge = Predict - Prob,
             category = cats_tmp[i],
             site = "365",
             Tag = NA) %>%
      ungroup() %>%
      select(category, site, Tag, player, odds, Prob, Predict, Edge, EV) %>%
      mutate(max_edge = Edge)
  )
}

# 3 > 6 shooters

tmp_db <- odds_setup %>%
  filter(category == "SixShooter")

tags_tmp <- unique(tmp_db$temp_tag)

for(i in 1:length(unique(tags_tmp))) {
  
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
      select(category, site, Tag, player, odds, Prob, Predict, Edge, EV) %>%
      mutate(max_edge = max(Edge, na.rm = T))
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
      select(category, site, Tag, player, odds, Prob, Predict, Edge, EV) %>%
      mutate(max_edge = max(Edge, na.rm = T))
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
    select(category, site, Tag, player, odds, Prob, Predict, Edge, EV) %>%
    mutate(max_edge = max(Edge, na.rm = T))
    
    
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
      select(category, site, Tag, player, odds, Prob, Predict, Edge, EV) %>%
      mutate(max_edge = max(Edge, na.rm = T))
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
    select(category, site, Tag, player, odds, Prob, Predict, Edge, EV) %>%
    mutate(max_edge = max(Edge, na.rm = T))
  )
}

# 8 > FRL outcomes which pay ties
tmp_db <- odds_setup %>%
  filter(grepl("FRL", category), site == "bodog") %>%
  left_join(  Rounds_Long %>%
                group_by(Run) %>% 
                mutate(min_score = min(Score)) %>%
                group_by(player) %>%
                summarise(Predict = mean(Score == min_score),
                          .groups = "drop"),
            by  = "player") %>%
  mutate(Edge = Predict - Prob,
         EV = (Predict * Return) - 100 * (1-Predict)) %>%
  select(category, site, Tag, player, odds, Prob, Predict, Edge, EV) %>%
  mutate(max_edge = Edge)

value_df <- rbind(
  value_df,
  tmp_db
)

# 9 > FRL outcomes with dead heat rules

tmp_db <- odds_setup %>%
  filter(grepl("FRL", category),
         site == "365")

value_df <- rbind(
  value_df,
  
  Rounds_Long %>%
    group_by(Run) %>% 
    mutate(min_score = min(Score),
           num.tied = sum(Score == min_score)) %>%
    left_join(tmp_db,
              by = "player") %>%
    filter(!is.na(odds)) %>%
    mutate(check = Score == min_score,
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
    select(category, site, Tag, player, odds, Prob, Predict, Edge, EV)  %>%
    mutate(max_edge = Edge)
)

##### analyze options ----

val_check <- value_df %>%
  filter(
    (category %in% c("R1Matches", "TourneyMatches") & max_edge >= 0.06) |
      (category %in% c("R13Balls") & max_edge >= 0.09) |
      max_edge > 0
  )

Player_Report("R1Matches", 13, site = "bodog" , var = "round")

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

saveRDS(output_list, here::here(base_folder, paste0("Tourney package_", base_folder, ".rds")))
file.remove(here::here(base_folder, "Tourney_Results.rds"))