
##### set functions -----

# refresh current year tournament results

update_tourneys <- function() {
  
  library(rvest)
  library(tidyverse)
  library(lubridate)
  library(jsonlite)
  library(janitor)
  
  ##### get tournament schedule and IDs ----
  grab_id <- "[[:digit:]]+"
  grab_tournament_name <- "(?<=>).*(?=</option)" 
  grab_date <- "(?<=>).*(?=</span)"
  
  tournament_ids <- integer() #empty vector to pull tournament ids for season
  tournament_names <- character() ##empty vector to pull tournament names for season
  tournament_dates <- character()
  tournament_year <- character()
  tournament_sites <- character()
  
  url <- paste0('https://www.espn.com/golf/leaderboard/_/tournamentId/401242997/')
  webpage <- read_html(url)
  
  tournament_id_data <- webpage %>% 
    html_nodes('optgroup') %>% 
    html_nodes('option')
  
  #loop id_data and grab tournament ids and names for individual seasons
  for(i in 1:length(tournament_id_data)){
    to_string <- toString(tournament_id_data[i])
    #tournament ids
    tournament_id <- str_extract(to_string, grab_id)
    tournament_ids <- c(tournament_ids, tournament_id)
    
    #tournament names
    tournament_name <- str_extract(to_string, grab_tournament_name)
    tournament_names <- c(tournament_names, tournament_name)
    
    #tournament dates
    url <- paste0('https://www.espn.com/golf/leaderboard/_/tournamentId/', tournament_id)
    webpage <- read_html(url)
    
    tournament_date <- webpage %>% 
      html_nodes('span.Leaderboard__Event__Date.n7') %>% 
      toString() %>% 
      str_extract(grab_date)
    tournament_dates <- c(tournament_dates, tournament_date)
    
    tournament_site <- webpage %>%
      html_nodes('div.Leaderboard__Course__Location.n8') %>%
      toString() %>%
      str_extract("(?<=>).*(?=<!-- --> -)")
    tournament_sites <- c(tournament_sites, tournament_site)
  }
  
  tournaments <- data.frame(tournament_ids, tournament_year = 2021, tournament_sites, tournament_names, tournament_dates, stringsAsFactors = F) %>%
    mutate(start_date = gsub(" - .+,", ",", tournament_dates),
           start_date = gsub("Oct ", "October ", start_date),
           start_date = as.Date(start_date, format = "%B %d, %Y"))
  
  saveRDS(tournaments, here::here("tournament_list_curyr.rData"))
  
  ##### pull leaderboard data for each tournament ----
  
  #establish empty dataframe
  scores <- tibble(pos = numeric(),
                   player = character(),
                   to_par = numeric(),
                   r1 = numeric(),
                   r2 = numeric(),
                   r3 = numeric(),
                   r4 = numeric(),
                   tot = numeric(),
                   earnings = numeric(),
                   fedex_pts = numeric())
  
  ignore_list <- c(401251634)
  
  tournament_ids <- tournaments %>%
    filter(!tournament_ids %in% ignore_list,
           start_date <= (Sys.Date() - 4)) %>%
    pull(tournament_ids)
  
  for( t in 1:length(tournament_ids)){
    
    url <- paste0('https://www.espn.com/golf/leaderboard/_/tournamentId/', tournament_ids[t])
    webpage <- read_html(url)
    
    id_data <- html_nodes(webpage, 'a.AnchorLink.leaderboard_player_name') #links with player ids
    
    grab_id <- "[[:digit:]]+" #reg expression to pull id from player links
    ids <- integer() #empty vector to put player ids
    
    #loop id_data and grab only player ids
    for(i in 1:length(id_data)){
      to_string <- toString(id_data[i])
      id <- str_extract(to_string, grab_id)
      ids <- c(ids, id)
    }
    
    node_list <- webpage %>% 
      html_nodes("table")
    
    if(length(node_list) == 1){
      leaderboard <- node_list %>% 
        html_table(fill = TRUE) %>% 
        .[[1]] %>% 
        clean_names() %>% 
        mutate(player_id = ids)
    } else if(length(node_list) == 2) {
      leaderboard <- node_list %>% 
        .[2] %>% 
        html_table(fill = TRUE) %>% 
        .[[1]] %>% 
        clean_names() %>% 
        mutate(player_id = ids)
    } else {
      leaderboard <- tibble(pos = numeric(),
                            player = character(),
                            to_par = numeric(),
                            r1 = numeric(),
                            r2 = numeric(),
                            r3 = numeric(),
                            r4 = numeric(),
                            tot = numeric(),
                            earnings = numeric(),
                            fedex_pts = numeric(),
                            player_id = character())
    }
    
    leaderboard <- leaderboard %>% 
      mutate(tournament_id = tournament_ids[t])
    
    if(ncol(leaderboard) < 12) {
      leaderboard$r4 <- NA
    }
    
    scores <- rbind(scores, leaderboard)
  }
  
  saveRDS(scores, here::here("leaderboard_scrape_curyr.rData"))
  
  
  
}

# used in preparing historic score data; converts round scores into a standardized format
Norm_Fun <- function(col) {
  (col - mean(col, na.rm = T))/sd(col, na.rm = T)
}

# used in prep; cleans the names of the imported tournament field to match with historic results
clean_field <- function(db) {
  return(db %>%
           mutate(player = gsub("(^.+), (.+$)", "\\2 \\1", name_raw),
                  player = gsub(",", "", player),
                  player = gsub("Tyler Strafaci", "ty strafaci (a)", player),
                  player = gsub("Byeong Hun", "Byeong-Hun", player),
                  player = gsub("Mark Anderson", "Mark D. Anderson", player),
                  player = gsub("Fabián Gómez", "Fabian Gomez", player),
                  player = gsub("Nelson Ledesma", "Nelson Lauta Ledesma", player),
                  player = gsub("Seung-Yul Noh", "Seung-yul Noh", player),
                  player = gsub("Alex Noren", "Alexander Noren", player),
                  player = gsub("Sebastián Muñoz", "Sebastian Munoz", player),
                  player = gsub("Sangmoon", "Sang-Moon", player),
                  player = gsub("Rafa ", "Rafael ", player),
                  player = tolower(player),
                  player = gsub("sebasti.n mu.oz", "sebastian munoz", player),
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
  temp <- gsub("c[[:space:]]*t[[:space:]]*pan$", "c.t. pan", temp)
  temp <- gsub("^brooke", "brooks", temp)
  temp <- gsub("ohair$", "o'hair", temp)
  
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

# used in analyzing outputs; compares the round scoring projections for a group of players
Player_Spread <- function(p) {
  
  temp <- filter(Rounds_Long, player  %in% p)
  
  ggplot(temp, aes(x = Score, fill = player)) +
    geom_density(alpha = 0.25) +
    theme_bw()
  
}

# used in analyzing outputs; compares the tournament result projections for a group of players
Player_Results <- function(p) {
  
  temp <- filter(Ongoing_Tally, player %in% p)
  
  ggplot(temp, aes(x = Final.Rank, fill = player)) + 
    geom_density(stat = "count", alpha = 0.25) +
    theme_bw()
}

# used in analyzing outputs; returns a report that consists of historic scoring results and either the round or tournament projection graphic
Player_Report <- function(cat,tag,  var = "round", site = NA) {
  #browser()
  if(!is.na(site)) {s <- site} else {s <- c("365", "bodog")}
  
  p <- value_df$player[value_df$Tag == tag & value_df$category == cat & value_df$site %in% s]
  p <- p[p != "tie"]
  
  report <- player_prep %>%
    filter(player %in% p) %>%
    arrange(player) %>%
    pivot_wider(names_from = "Round", values_from = "Score")
  
  if(var == "round") {
    g <- Player_Spread(p)
  } else {
    g <- Player_Results(p)
  }
  
  list(View(report), g)
}
