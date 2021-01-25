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
