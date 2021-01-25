library(plyr)
library(tidyverse)
library(readxl)
library(zoo)
library(openxlsx)

rm(list = ls())
#setwd("C:/Users/Matthew Hogel/My Documents/Gambling/Golf/Masters 2019")
setwd("M:/My Documents/MyScripts/Golf/Canadian")
#setwd("C:/Users/mhogel/Desktop")

path <- "./Tourney_Prep.xlsx"
num.sheets <- length(excel_sheets(path = path))
Tourneys <- c(excel_sheets(path = path)[-1])
Tourney_Years <- seq(from = 2018, to = 2009)

##### Set functions ----

Player_Spread <- function(player) {
  
  temp <- filter(Rounds_Long, Player %in% player)
  
  ggplot(temp, aes(x = Score, fill = Player)) +
    geom_density(alpha = 0.25) +
    theme_bw()
  
}

Player_Results <- function(player) {
  
  temp <- filter(Ongoing_Tally, Player %in% player)
  
  ggplot(temp, aes(x = Final.Rank, fill = Player)) + 
    geom_density(stat = "count", alpha = 0.25) +
    theme_bw()
}

Player_Report <- function(db, tag, var = "round") {
  
  players <- db$Player[db$Tag == tag]
  
  report <- Full_History %>%
    mutate(Player = tolower(Player)) %>%
    filter(Player %in% players) %>%
    arrange(Player)
  
  if(var == "round") {
    p <- Player_Spread(players)
  } else {
    p <- Player_Results(players)
  }
  
  list(View(report), p)
}

Clean_Players <- function(data) {
  
  temp <- data
  temp <- tolower(gsub("(^.+)[[:punct:]][[:digit:]]+$", "\\1", temp))
  temp <- gsub("kyoung hoon", "kyounghoon", temp)
  #  temp <-  gsub("varner", "varner iii", temp)
  temp <-  gsub("[[:punct:]]", "", temp)
  temp <-  gsub("byeonghun", "byeong hun", temp)
  temp <-  gsub("hao tong", "haotong", temp)
  temp <-  gsub("cabrerabello", "cabrera bello", temp)
  temp <-  gsub("rafael", "rafa", temp)
  #  temp <-  gsub("russel", "russell", temp)
  temp <-  gsub("schaufflele", "schauffele", temp)
  temp <-  gsub("^(.)[[:space:]](.) ", "\\1\\2 ", temp)
  
  return(temp)
  
  
}

##### Import Field ----
Field <- read_excel(path, sheet = "Field", col_names = F) %>%
  mutate(Player = gsub("(^.+)[[:punct:]] (.+$)", "\\2 \\1", `...1`),
         Player = gsub("[[:punct:]]", "", Player),
         Player = tolower(iconv(Player,from="UTF-8",to="ASCII//TRANSLIT")))

Full_History <- data.frame()
Tourney_History <- data.frame()

Norm_Fun <- function(col) {
  (col - mean(col, na.rm = T))/sd(col, na.rm = T)
}

for(i in 1:length(Tourneys)) {
  temp <- read_excel(path, sheet = i + 1, col_names = T) %>%
    select(PLAYER, ROUNDS:`...6`) %>%
    setNames(c("Player", "Rd1", "Rd2", "Rd3", "Rd4")) %>%
    filter(!is.na(Player)) %>%
    mutate(Year = ifelse(as.numeric(Tourneys[i]) %in% Tourney_Years, as.numeric(Tourneys[i]), 2019),
           Tournament = ifelse(as.numeric(Tourneys[i]) %in% Tourney_Years, "Current", Tourneys[i]))
  
  if(Tourneys[i] %in% Tourneys[1:10]){
    Tourney_History <- rbind(Tourney_History, temp)
  }
  
  temp[2:5] <- apply(temp[2:5], 2, Norm_Fun)
  
  Full_History <- rbind(Full_History, temp)
} 

Tourney_Stats <- Tourney_History %>%
  gather(Round, Score, Rd1:Rd4) %>%
  filter(!is.na(Score))

(Tourney_Avg <- mean(Tourney_Stats$Score))
(Tourney_Sd <- sd(Tourney_Stats$Score))

ggplot(Tourney_Stats, aes(x = Score)) +
  geom_histogram(binwidth = 1) +
  theme_bw()

# Calculate weighted z-score per golfer

Tourney_Key <- data.frame(Tournament = unique(Full_History$Tournament[Full_History$Tournament != "Current"]), stringsAsFactors = F) %>%
  mutate(Value = c(10,8,9,6,5,4,7,4,1,2))

Tourney_Weights <- select(Full_History, Year, Tournament) %>%
  distinct() %>%
  left_join(Tourney_Key, by = "Tournament") %>%
  mutate(Value = ifelse(Tournament == "Current", Year - 2010, Value),
         Course = ifelse(Tournament == "Current" & Year == 2012, 2, 0),
         Full_Weight = Value + Course,
         Full_Weight = ifelse(Tournament == "Current" & Year == 2019, 15, Full_Weight)) %>%
  select(Year, Tournament, Full_Weight) %>%
  filter(Year > 2010)

# manually dampen the deviation - not the best method
Sd_Skew <- 0.6

Golfer_Scores <- Full_History %>%
  mutate(Player = gsub("[[:punct:]]", "", Player),
         Player = tolower(iconv(Player,from="UTF-8",to="ASCII//TRANSLIT"))) %>%
  filter(Player %in% Field$Player) %>%
  left_join(Tourney_Weights, by = c("Tournament", "Year")) %>%
  filter(!is.na(Full_Weight)) %>%
  gather(Round, Score, Rd1:Rd4) %>%
  filter(!is.na(Score)) %>%
  group_by(Player, Tournament, Year) %>%
  mutate(Weighted_Score = Score * Full_Weight) %>%
  group_by(Player) %>%
  mutate(Weighted_Avg = sum(Weighted_Score) / sum(Full_Weight),
         Weighted_sd_Rd = (Score - Weighted_Avg)^2 * Full_Weight,
         Total_Weighted_sd = sqrt(sum(Weighted_sd_Rd)/sum(Full_Weight) * ((n()-1)/n()))* Sd_Skew) %>%
  select(Player, Weighted_Avg, Total_Weighted_sd) %>%
  distinct()

Standardized_Scores <- data.frame(Score = seq(from = 62, to = 85)) %>%
  mutate(z.score = (Score - Tourney_Avg)/Tourney_Sd)

Tourney_Preds <- as.data.frame(Field) %>%
  select(Player) %>%
  left_join(Golfer_Scores, by = "Player")

temp_db <- matrix(nrow = nrow(Tourney_Preds), ncol = nrow(Standardized_Scores),
                  dimnames = list(Tourney_Preds$Player, Standardized_Scores$Score))

Score_Matrix <- as.data.frame(temp_db) %>%
  mutate(Player = rownames(temp_db)) %>%
  gather("Score", "temp", 1:ncol(temp_db)) %>%
  mutate(Score = as.numeric(Score)) %>%
  arrange(Player) %>%
  left_join(Standardized_Scores, by = "Score") %>%
  left_join(Tourney_Preds, by = "Player") %>%
  mutate(Total_Weighted_sd = 2*Total_Weighted_sd,
         Score.Prob = pnorm(z.score, Weighted_Avg, Total_Weighted_sd)) %>%
  select(-temp)

find_score <- function(player, score) {
  max(Score_Matrix$Score[Score_Matrix$Player == player & Score_Matrix$Score.Prob < score])
}


Prob_Calc <- function(odds) {
  return(
    ifelse(odds > 0, 100/(odds + 100), abs(odds)/(abs(odds) + 100))
  )
}

Clean_Odds <- function(db) {
  return(
    db %>%
      setNames("temp") %>%
      mutate(temp = gsub("EVEN", "+100", temp),
             Player = tolower(gsub("(^.+)[[:punct:]][[:digit:]]+$", "\\1", temp)),
             Player = gsub("rafael", "rafa", Player),
             Player = gsub("j b", "jb", Player),
             Odds = as.numeric(gsub("^.+([[:punct:]][[:digit:]]+$)", "\\1", temp)),
             Prob = Prob_Calc(Odds))
    
  )
}


##### Model start ----

Repeats <- 10000
Rounds <- c("Rd1", "Rd2", "Rd3", "Rd4")

Ongoing_Tally <- data.frame()

base_db <- Tourney_Preds %>%
  filter(!is.na(Weighted_Avg)) %>%
  select(Player)

start <- Sys.time()

board <- do.call("rbind", replicate(Repeats, base_db %>% select(Player), simplify = F)) %>%
  group_by(Player) %>%
  mutate(Run = rank(Player, ties.method = "first"))

for(i in 1:length(Rounds)) {
  
  results <- board %>%
    mutate(prob = runif(n()),
           Score = mapply(find_score, Player, prob),
           prob.2 = runif(n()),
           Score.2 = ifelse(Score <= 64, mapply(find_score, Player, prob.2), NA),
           Score.2 = ifelse(Score.2 > 69, 69, Score.2),
           Score = ifelse(is.na(Score.2), Score, Score.2)) %>%
    select(Player, Run, Score)
  
  results[results == "-Inf"] <- 61
  
  board[Rounds[i]] <- results$Score
  
}

end <- Sys.time()
(total <- end - start)

saveRDS(board, "Tourney_Results.rds")

Ongoing_Tally <- board %>%
  group_by(Run) %>%
  mutate(Cut.Line = rank(Rd1 + Rd2, ties.method = "min"),
         Full.Score = Rd1 + Rd2 + Rd3 + Rd4,
         Cut = Cut.Line <= 70,
         Cut.Rank = rank(Cut.Line, ties.method = "min"),
         Full.Score = ifelse(!Cut, NA, Full.Score),
         Temp.Rank = rank(Full.Score, ties.method = "min"),
         Final.Rank = ifelse(Temp.Rank == 1, rank(Temp.Rank, ties.method = "random"), ifelse(Cut, Temp.Rank, Cut.Rank))) %>%
  select(Player,Final.Rank, Run, Cut)

Final.Results <- Ongoing_Tally %>%
  group_by(Player) %>%
  summarise(Top1 = sum(Final.Rank == 1, na.rm = T)/Repeats,
            Top5 = sum(Final.Rank <= 5, na.rm = T)/Repeats,
            Top10 = sum(Final.Rank <= 10, na.rm = T)/Repeats,
            Top20 = sum(Final.Rank <= 20, na.rm = T)/Repeats,
            Miss.Cut = mean(!Cut)) %>%
  arrange(desc(Top1))

# pull rounds

Rounds_Long <- rbind(board[,c(1,3)] %>% setNames(c("Player", "Score")),
                     board[,c(1,4)] %>% setNames(c("Player", "Score"))) %>%
  group_by(Player) %>%
  mutate(Run = rank(Player, ties.method = "first"))

Data_Compare <- rbind(Rounds_Long %>% ungroup() %>% select(Score) %>% mutate(Method = "Predict"),
                      Tourney_Stats %>% select(Score) %>% mutate(Method = "Actual")) %>%
  group_by(Method, Score) %>%
  count() %>%
  group_by(Method) %>%
  mutate(Prop = n / sum(n))

ggplot(Data_Compare, aes(x = Score, y = Prop, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()

##### Read in Odds ----

path <- "./Odds.xlsx"
num.sheets <- length(excel_sheets(path = path))
sheets <- excel_sheets(path = path)

## Final Results ---

Top_Sheets <- which(grepl("Top", sheets))
Top_Finish <- data.frame()

for(s in 1:length(Top_Sheets)){
  
  name <- sheets[Top_Sheets[s]]
  temp <- read_excel(path, sheet = name, col_names = T) %>%
    setNames("Col") %>%
    mutate(Spot = name)
  
  Top_Finish <- rbind(Top_Finish, temp)
  #assign(paste0(sheets[s],".R4_raw"), read_excel(path, sheet = sheets[s], col_names = T))
  
}

Top_Probs <- Clean_Odds(Top_Finish[,1]) %>%
  cbind(Top_Finish[,2]) %>%
  mutate(Player = gsub("kyoung hoon", "kyounghoon", Player),
         Player = gsub("varner", "varner iii", Player),
         Player = gsub("[[:punct:]]", "", Player),
         Player = gsub("byeonghun", "byeong hun", Player),
         Player = gsub("hao tong", "haotong", Player),
         Player = gsub("cabrerabello", "cabrera bello", Player),
         Player = gsub("c t p", "ct p", Player),
         Player = gsub("schaufflele", "schauffele", Player),
         #         Player = gsub("alexander", "alex", Player),
         Player = gsub("^(.)[[:space:]](.) ", "\\1\\2 ", Player))  %>%
  left_join(Final.Results %>%
              gather(Spot, Predict, Top1:Top20), by = c("Player", "Spot")) %>%
  filter(!is.na(Odds)) %>%
  mutate(Edge = Predict - Prob,
         Return = ifelse(Odds > 0, Odds, 100^2/abs(Odds)),
         EV = (Predict * Return) - 100 * (1-Predict)) %>%
  select(Spot, Player, Odds, Prob, Predict, Edge, EV)

## First Round Leader

FRL_Sheets <- which(grepl("FRL", sheets))

FRL_Raw <- read_excel(path = path, sheet = FRL_Sheets, col_names = T)

FRL_clean <- FRL_Raw %>%
  setNames("Col") %>%
  filter(!grepl("Round", Col)) %>%
  mutate(Col = gsub("EVEN", "+100", Col),
         Player = tolower(gsub("(^.+)[[:punct:]][[:digit:]]+$", "\\1", Col)),
         Player = gsub("kyoung hoon", "kyounghoon", Player),
         #         Player = gsub("varner", "varner iii", Player),
         Player = gsub("[[:punct:]]", "", Player),
         Player = gsub(" hoon", "", Player),
         Player = gsub("rafael", "rafa", Player),
         Player = gsub("byeonghun", "byeong hun", Player),
         Player = gsub("fredrik", "freddie", Player),
         Player = gsub("meenwhee", "whee", Player),
         Player = gsub("c t p", "ct p", Player),
         Player = gsub("schaufflele", "schauffele", Player),
         #         Player = gsub("alexander", "alex", Player),
         Player = gsub("^(.)[[:space:]](.) ", "\\1\\2 ", Player),
         Odds = as.numeric(gsub("^.+([[:punct:]][[:digit:]]+$)", "\\1", Col)),
         Prob = Prob_Calc(Odds),
         Return = ifelse(Odds > 0, Odds, 100^2/abs(Odds)))

FRL_Odds <- Rounds_Long %>%
  group_by(Run) %>%
  mutate(Win.Score = min(Score),
         Leader = Score == Win.Score) %>%
  group_by(Player) %>%
  summarise(Predict = mean(Leader))

FRL_Results <- left_join(FRL_clean, FRL_Odds, by = "Player") %>%
  filter(!is.na(Odds)) %>%
  mutate(Edge = Predict - Prob,
         Return = ifelse(Odds > 0, Odds, 100^2/abs(Odds)),
         EV = (Predict * Return) - 100 * (1-Predict)) %>%
  select(Player, Odds, Prob, Predict, Edge, EV)


## Matchups 

Matchups_Sheets <- which(grepl("Matchups", sheets))[1]

Matchups_Raw <- read_excel(path = path, sheet = Matchups_Sheets, col_names = T)

Matchups_clean <- Matchups_Raw %>%
  setNames("Col") %>%
  mutate(Tag = ifelse(grepl("Tournament", Col), 1, NA),
         Tag = rank(Tag, ties.method = "first", na.last = "keep")) %>%
  mutate(Tag = na.locf(Tag)) %>%
  filter(!grepl("RBC", Col),
         !grepl("ournament", Col),
         !grepl("Winner", Col),
         !grepl("Both players must", Col),
         !grepl("/", Col)) %>%
  mutate(Col = gsub("EVEN", "+100", Col),
         Player = sapply(Col, Clean_Players),
         Odds = as.numeric(gsub("^.+([[:punct:]][[:digit:]]+$)", "\\1", Col)),
         Prob = Prob_Calc(Odds),
         Return = ifelse(Odds > 0, Odds, 100^2/abs(Odds)))

matchups.db <- data.frame()

for(Tags in 1:max(Matchups_clean$Tag)) {
  
  Balls_Run <- Ongoing_Tally %>%
    filter(Player %in% Matchups_clean$Player[Matchups_clean$Tag == Tags]) %>%
    group_by(Run) %>%
    mutate(Tie.Check = sum(Final.Rank == min(Final.Rank)) != 2) %>%
    filter(Tie.Check) %>%
    mutate(Winner = Final.Rank == min(Final.Rank)) %>%
    group_by(Player) %>%
    summarise(Predict = mean(Winner)) %>%
    mutate(Tag = Tags)
  
  matchups.db <- rbind(matchups.db, Balls_Run)
  
  
}

Matchups_Results <- left_join(Matchups_clean, matchups.db, by = c("Player", "Tag")) %>%
  group_by(Tag) %>%
  mutate(Missing = sum(is.na(Predict))) %>%
  filter(Missing == 0) %>%
  select(-Missing) %>%
  mutate(Edge = Predict - Prob,
         Return = ifelse(Odds > 0, Odds, 100^2/abs(Odds)),
         EV = Return * Predict - 100 * (1-Predict)) %>%
  select(Tag, Player, Odds, Prob, Predict, Edge, EV) %>%
  group_by(Tag) %>%
  mutate(Top.Edge = max(EV)) %>%
  arrange(desc(Top.Edge), Tag)

write.csv(Matchups_Results, "Matchup Edges.csv", row.names = F)

## Groups

Groups_Sheets <- which(grepl("Groups", sheets))

Groups_Raw <- read_excel(path = path, sheet = Groups_Sheets, col_names = T)

Groups_clean <- Groups_Raw %>%
  setNames("Col") %>%
  mutate(Tag = ifelse(grepl("Top", Col), 1, NA),
         Tag = rank(Tag, ties.method = "first", na.last = "keep")) %>%
  mutate(Tag = na.locf(Tag)) %>%
  filter(!grepl("Top", Col),
         !grepl("RBC", Col),
         !grepl("All players listed", Col),
         !grepl("/", Col)) %>%
  mutate(Col = gsub("EVEN", "+100", Col),
         Player = sapply(Col, Clean_Players),
         Odds = as.numeric(gsub("^.+([[:punct:]][[:digit:]]+$)", "\\1", Col)),
         Prob = Prob_Calc(Odds),
         Return = ifelse(Odds > 0, Odds, 100^2/abs(Odds)))

groups.db <- data.frame()

for(Tags in 1:max(Groups_clean$Tag)) {
  
  Groups_Run <- Ongoing_Tally %>%
    filter(Player %in% Groups_clean$Player[Groups_clean$Tag == Tags]) %>%
    group_by(Run) %>%
    mutate(Tie.Check = sum(Final.Rank == min(Final.Rank))) %>%
    mutate(Rank = rank(Final.Rank, ties.method = "min")) %>%
    group_by(Player, Tie.Check) %>%
    summarise(Wins = sum(Rank == 1)) %>%
    group_by(Player) %>%
    mutate(Misses = Repeats - sum(Wins),
           Tag = Tags) %>%
    ungroup()
  
  groups.db <- rbind(groups.db, Groups_Run)
  
  
}

Groups_Results <- left_join(Groups_clean, groups.db, by = c("Player", "Tag")) %>%
  group_by(Tag) %>%
  mutate(EV = (Return * Wins / Tie.Check) - (100 * ((Tie.Check - 1) / Tie.Check))) %>%
  group_by(Tag, Player, Odds, Prob, Return) %>%
  summarise(EV = (sum(EV) - 100 * max(Misses))/Repeats)


## Round 1 H2H

H2H_Sheets <- which(grepl("R1H2H", sheets))
H2H_Raw <- read_excel(path = path, sheet = H2H_Sheets, col_names = T)

H2H_clean <- H2H_Raw %>%
  setNames("Col") %>%
  mutate(Tag = ifelse(grepl("Round", Col), 1, NA)) %>%
  mutate(Tag = rank(Tag, ties.method = "first", na.last = "keep")) %>%
  mutate(Tag = na.locf(Tag)) %>%
  filter(!grepl("Round", Col),
         !grepl("Winner", Col),
         !grepl("RBC", Col),
         !grepl("Both players must", Col),
         !grepl("/", Col)) %>%
  mutate(Col = gsub("EVEN", "+100", Col),
         Player = tolower(gsub("(^.+)[[:punct:]][[:digit:]]+$", "\\1", Col)),
         Player = gsub("kyoung hoon", "kyounghoon", Player),
         Player = gsub("varner", "varner iii", Player),
         Player = gsub("[[:punct:]]", "", Player),
         Player = gsub("byeonghun", "byeong hun", Player),
         Player = gsub("hao tong", "haotong", Player),
         Player = gsub("cabrerabello", "cabrera bello", Player),
         #Player = gsub("russel", "russell", Player),
         #         Player = gsub("alexander", "alex", Player),
         Odds = as.numeric(gsub("^.+([[:punct:]][[:digit:]]+$)", "\\1", Col)),
         Prob = Prob_Calc(Odds),
         Return = ifelse(Odds > 0, Odds, 100^2/abs(Odds)))

H2H.db <- data.frame()

for(Tags in 1:max(H2H_clean$Tag)) {
  
  H2H_Run <- Rounds_Long %>%
    filter(Player %in% H2H_clean$Player[H2H_clean$Tag == Tags]) %>%
    group_by(Run) %>%
    mutate(Tie.Check = sum(Score == min(Score)) != 2) %>%
    filter(Tie.Check) %>%
    mutate(Winner = Score == min(Score)) %>%
    group_by(Player) %>%
    summarise(Predict = mean(Winner)) %>%
    mutate(Tag = Tags)
  
  H2H.db <- rbind(H2H.db, H2H_Run)
  
  
}

H2H_Results <- left_join(H2H_clean, H2H.db, by = c("Player", "Tag")) %>%
  group_by(Tag) %>%
  mutate(EV = Return * Predict - 100 * (1-Predict),
         TopEV = max(EV)) %>%
  arrange(desc(TopEV)) %>%
  select(Tag, Player, Odds, Return, Predict, EV)

write.csv(H2H_Results, "Rd1 Head to Head.csv", row.names = F)

# Rd 1 3 balls

Threeball_Sheets <- which(grepl("R13Balls", sheets))
Threeball_Raw <- read_excel(path = path, sheet = Threeball_Sheets, col_names = T)

ThreeBall_clean <- Threeball_Raw %>%
  setNames("Col") %>%
  mutate(Tag = ifelse(grepl("Winner", Col), 1, NA)) %>%
  mutate(Tag = rank(Tag, ties.method = "first", na.last = "keep")) %>%
  mutate(Tag = na.locf(Tag)) %>%
  filter(!grepl("ROUND", Col),
         !grepl("Winner", Col),
         !grepl("Both players must", Col),
         !grepl("/", Col)) %>%
  mutate(Col = gsub("EVEN", "+100", Col),
         Player = sapply(Col, Clean_Players),
         Odds = as.numeric(gsub("^.+([[:punct:]][[:digit:]]+$)", "\\1", Col)),
         Prob = Prob_Calc(Odds),
         Return = ifelse(Odds > 0, Odds, 100^2/abs(Odds)))

ThreeBall.db <- data.frame()


for(Tags in 1:max(ThreeBall_clean$Tag)) {
  
  ThreeBalls_Run <- Rounds_Long %>%
    filter(Player %in% ThreeBall_clean$Player[ThreeBall_clean$Tag == Tags]) %>%
    group_by(Run) %>%
    mutate(Tie.Check = sum(Score == min(Score))) %>%
    mutate(Rank = rank(Score, ties.method = "min")) %>%
    group_by(Player, Tie.Check) %>%
    summarise(Wins = sum(Rank == 1)) %>%
    group_by(Player) %>%
    mutate(Misses = max(Rounds_Long$Run) - sum(Wins),
           Tag = Tags) %>%
    ungroup()
  
  ThreeBall.db <- rbind(ThreeBall.db, ThreeBalls_Run)
  
  
}

ThreeBall_Results <- left_join(ThreeBall_clean, ThreeBall.db, by = c("Player", "Tag")) %>%
  group_by(Tag) %>%
  mutate(EV = (Return * Wins / Tie.Check) - (100 * ((Tie.Check - 1) / Tie.Check))) %>%
  group_by(Tag, Player, Odds, Prob, Return) %>%
  summarise(EV = (sum(EV) - 100 * max(Misses))/max(Rounds_Long$Run)) %>%
  mutate(Predict = (100+EV)/(100+Return)) %>%
  select(Tag, Player, Odds, Return, Prob, Predict, EV)

write.csv(ThreeBall_Results, "ThreeBalls.csv", row.names = F)

## Write out predictions

wb <- createWorkbook()

addWorksheet(wb, "Futures")
writeDataTable(wb, sheet = "Futures", x = Top_Probs)

addWorksheet(wb, "Group Matchups")
writeDataTable(wb, sheet = "Group Matchups", x = Groups_Results)

addWorksheet(wb, "Tourney Matchups")
writeDataTable(wb, sheet = "Tourney Matchups", x = Matchups_Results)

addWorksheet(wb, "Rd1 H2H")
writeDataTable(wb, sheet = "Rd1 H2H", x = H2H_Results)

addWorksheet(wb, "Rd1 3balls")
writeDataTable(wb, sheet = "Rd1 3balls", x = ThreeBall_Results)

saveWorkbook(wb, "./PreTourneyPreds_Canadian.xlsx", overwrite = T)


