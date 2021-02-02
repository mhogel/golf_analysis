
##### set functions -----

# used in prep; cleans the names of the imported tournament field to match with historic results
clean_field <- function(db) {
  return(db %>%
           mutate(player = gsub("(^.+), (.+$)", "\\2 \\1", name_raw),
                  player = gsub(",", "", player),
                  player = gsub("Byeong Hun", "Byeong-Hun", player),
                  player = gsub("Mark Anderson", "Mark D. Anderson", player),
                  player = gsub("Fabián Gómez", "Fabian Gomez", player),
                  player = gsub("Nelson Ledesma", "Nelson Lauta Ledesma", player),
                  player = gsub("Seung-Yul Noh", "Seung-yul Noh", player),
                  player = gsub("Alex Noren", "Alexander Noren", player),
                  player = tolower(player),
                  check = player %in% player_data$player)
  )
}

# used in modeling; given a random number and a player, the function returns the predicted round score
find_score <- function(player, score) {
  max(Score_Matrix$Score[Score_Matrix$player == player & Score_Matrix$Score.Prob < score])
}

# used in cleaning odds reports; returns the break even probability for a given odds
Prob_Calc <- function(odds) {
  return(
    ifelse(odds > 0, 100/(odds + 100), abs(odds)/(abs(odds) + 100))
  )
}

# used in cleaning odds reports; splits apart names and odds from imported Excel data (may not be needed if web scraping)
Clean_Odds <- function(db) {
  return(
    db %>%
      setNames("temp") %>%
      mutate(temp = gsub("EVEN", "+100", temp),
             Player = tolower(gsub("(^.+)[[:punct:]][[:digit:]]+$", "\\1", temp)),
             Player = gsub("byeong hun an","byeong-hun an", Player),
             Player = gsub("alex ", "alexander ", Player),
             Player = gsub("mark anderson", "mark d. anderson", Player),
             Player = gsub("kyoung hoon", "kyoung-hoon", Player),
             Player = gsub("sebastion", "sebastian", Player),
             Player = gsub("nelson ledesma", "nelson lauta ledesma", Player),
             Player = gsub("ted potter jr", "ted potter jr.", Player),
             Player = gsub("rafa campos", "rafael campos", Player),
             Player = gsub("j. b. holmes","j.b. holmes", Player),
             Player = gsub("jt poston", "j.t. poston", Player),
             Player = gsub("j. j. spaun", "j.j. spaun", Player),
             Player = gsub("kj choi", "k.j. choi", Player),
             Player = gsub("rafael", "rafa", Player),
             Player = gsub("j b", "jb", Player),
             Odds = as.numeric(gsub("^.+([[:punct:]][[:digit:]]+$)", "\\1", temp)),
             Prob = Prob_Calc(Odds))
    
  )
}

# used in preparing historic score data; converts round scores into a standardized format
Norm_Fun <- function(col) {
  (col - mean(col, na.rm = T))/sd(col, na.rm = T)
}

# used in cleaning odds reports; converts names to be able to align to other datasets
Clean_Players <- function(data) {
  
  temp <- data
  #  temp <-  gsub("varner", "varner iii", temp)
  temp <-  gsub("[[:punct:]]", "", temp)
  temp <- gsub("kyoung.*hoon", "kyoung-hoon", temp)
  temp <-  gsub("byeonghun", "byeong hun", temp)
  temp <- gsub(".+holmes$", "j.b. holmes", temp)
  temp <- gsub("mark anderson", "mark d. anderson", temp)
  temp <- gsub(".+choi$", "k.j. choi", temp)
  temp <- gsub(".+spaun$", "j.j. spaun", temp)
  temp <- gsub("nelson ledesma", "nelson lauta ledesma", temp)
  temp <- gsub("ted potter jr", "ted potter jr.", temp)
  temp <- gsub("seung.*yul noh", "seung-yul noh", temp)
  temp <- gsub("sebastion", "sebastian", temp)
  
  temp <-  gsub("hao tong", "haotong", temp)
  temp <-  gsub("alex ", "alexander ", temp)
  temp <-  gsub("^.+poston", "j.t. poston", temp)
  temp <-  gsub("byeong hun an","byeong-hun an", temp)
  temp <-  gsub("cabrerabello", "cabrera bello", temp)
  temp <-  gsub("rafael", "rafa", temp)
  #  temp <-  gsub("russel", "russell", temp)
  temp <-  gsub("schaufflele", "schauffele", temp)
  temp <-  gsub("^(.)[[:space:]](.) ", "\\1\\2 ", temp)
  
  return(temp)
  
  
}

# used in analyzing outputs; compares the round scoring projections for a group of players
Player_Spread <- function(player) {
  
  temp <- filter(Rounds_Long, tolower(Player) %in% player)
  
  ggplot(temp, aes(x = Score, fill = Player)) +
    geom_density(alpha = 0.25) +
    theme_bw()
  
}

# used in analyzing outputs; compares the tournament result projections for a group of players
Player_Results <- function(p) {
  
  temp <- filter(Ongoing_Tally, tolower(player) %in% p)
  
  ggplot(temp, aes(x = Final.Rank, fill = player)) + 
    geom_density(stat = "count", alpha = 0.25) +
    theme_bw()
}

# used in analyzing outputs; returns a report that consists of historic scoring results and either the round or tournament projection graphic
Player_Report <- function(db, tag, var = "round") {
  #browser()
  players <- tolower(db$Player[db$Tag == tag])
  players <- players[players != "Tie"]
  
  report <- player_prep %>%
    mutate(player = tolower(player)) %>%
    filter(player %in% players) %>%
    arrange(player) %>%
    pivot_wider(names_from = "Round", values_from = "Score")
  
  if(var == "round") {
    p <- Player_Spread(players)
  } else {
    p <- Player_Results(players)
  }
  
  list(View(report), p)
}
