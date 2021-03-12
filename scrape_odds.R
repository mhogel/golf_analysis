library(rvest)
library(dplyr)
library(purrr)

odds_files <- list.files(here::here(base_folder, "odds_scrape"))

odds_package <- list()

html_key <- list(
  FRL = list(
    nm = ".gl-ParticipantBorderless_Name",
    od = ".gl-ParticipantBorderless_Odds",
    gs = 1,
    cn = 0,
    ties = F
  ),
  MakeCut = list(
    nm = ".srb-ParticipantLabel_Name",
    od = ".gl-ParticipantOddsOnly_Odds",
    gs = 1,
    cn = 2,
    ties = F
  ),
  Outright = list(
    nm = ".gl-ParticipantBorderless_Name",
    od = ".gl-ParticipantBorderless_Odds",
    gs = 1,
    cn = 0,
    ties = F
  ),
  R13Balls = list(
    nm = ".src-ParticipantFixtureDetailsExtraLine_Team",
    od = ".src-ParticipantOddsOnly80_Odds",
    gs = 3,
    cn = NA,
    ties = F
  ),
  R1Matches = list(
    nm = ".rcl-ParticipantFixtureDetails_Team",
    od = ".sgl-ParticipantOddsOnly80_Odds",
    gs = 2,
    cn = NA,
    ties = F
  ),
  R1SixShooter = list(
    nm = ".gl-ParticipantBorderless_Name",
    od = ".gl-ParticipantBorderless_Odds",
    gs = 6,
    cn = 1,
    ties = F
  ),
  TopFinishes = list(
    nm = ".srb-ParticipantLabelSmallBorder_Name",
    od = ".gl-ParticipantOddsOnly_Odds",
    gs = 1,
    cn = 3,
    ties = F
  ),
  TourneyMatches = list(
    nm = ".rcl-ParticipantFixtureDetails_Team",
    od = ".sgl-ParticipantOddsOnly80_Odds",
    gs = 2,
    cn = 1,
    ties = T
  ),
  FRL = list(
    nm = ".gl-ParticipantBorderless_Name",
    od = ".gl-ParticipantBorderless_Odds",
    gs = 1,
    cn = 0,
    ties = F
  )
)

# grab 365 odds
files_365 <- odds_files[which(grepl("365", odds_files))]
files_365 <- files_365[!grepl("Groups", files_365)]

for(i in 1:length(files_365)) {
  url <- files_365[i]
  file_name <- gsub("^.+_365_(.+).html", "\\1", url)
  webpage <- read_html(here::here(base_folder, "odds_scrape", url))
  
  tmp <- html_key[[file_name]]
  
  nms <- webpage %>%
    html_nodes(tmp$nm) %>%
    html_text()
  
  odds <- webpage %>%
    html_nodes(tmp$od) %>%
    html_text()
  
  db <- data.frame(
    player = nms,
    site = "365",
    stringsAsFactors = F
  )
  
  if(file_name == "MakeCut") {
    odds_package[["MakeCut"]][["365"]] <- 
      db %>%
      mutate(odds = odds[1:(length(odds)/tmp$cn)])
    
    odds_package[["MissCut"]][["365"]] <-
      db %>%
      mutate(odds = odds[(length(odds)/tmp$cn + 1):length(odds)])
  }
  
  if(file_name == "TopFinishes") {
    odds_package[["Top5"]][["365"]] <-
      db %>%
      mutate(odds = odds[1:(length(odds)/tmp$cn)])
    
    odds_package[["Top10"]][["365"]] <-
      db %>%
      mutate(odds = odds[(length(odds)/tmp$cn + 1):(2/3 * length(odds))])
    
    odds_package[["Top20"]][["365"]] <-
      db %>%
      mutate(odds = odds[(2/3 * length(odds) + 1): length(odds)])
  }
  
  if(file_name %in% c("Outright", "FRL")) {
    odds_package[[file_name]][["365"]] <-
      db %>%
      mutate(odds = odds)
  }
  
  if(file_name == "R1SixShooter") {
    odds_package[["SixShooter"]][["365"]] <-
      db %>%
      mutate(Tag = rep(1:(length(nms)/tmp$gs), each = tmp$gs),
             odds = odds)
  }
  
  if(file_name %in% c("R13Balls", "R1Matches", "TourneyMatches")) {
    
    db <- db %>%
      mutate(Tag = rep(1:(length(nms)/tmp$gs), each = tmp$gs)) %>%
      group_by(Tag) %>%
      mutate(tag_order = rank(Tag, ties.method = "first"))
    
    if(tmp$ties) {
      db <- rbind(db, data.frame(
        player = "Tie",
        Tag = 1:(length(nms)/tmp$gs),
        site = "365",
        tag_order = 1.5
      ))
    }
    
    db <- db %>%
      arrange(tag_order, Tag) %>%
      ungroup() %>%
      mutate(odds = odds) %>%
      arrange(Tag) %>%
      select(-tag_order)
    
    odds_package[[file_name]][["365"]] <- db
  }
  
}

##### bodog scrape ----

files_bodog <- odds_files[which(grepl("bodog", odds_files))]
main_names <- c("FRL","Outright","Top5", "Top10", "Top20", "MakeCut", "MissCut", "TourneyMatches", "SixShooter", "R13Balls", "R1Matches")

html_key_bd <- list(
  Outright = list(
    f_nm = "main",
    txt = "Winner",
    gs = 1
  ),
  Top5 = list(
    f_nm = "main",
    txt = "Top 5",
    gs = 1
  ),
  Top10 = list(
    f_nm = "main",
    txt = "Top 10",
    gs = 1
  ),
  Top20 = list(
    f_nm = "main",
    txt = "Top 20",
    gs = 1
  ),
  MakeCut = list(
    f_nm = "main",
    txt = "Make",
    gs = 1
  ),
  MissCut = list(
    f_nm = "main",
    txt = "Miss",
    gs = 1
  ),
  R1Matches = list(
    f_nm = "Matchups",
    txt = "1st Round Match Up",
    gs = 2
  ),
  SixShooter = list(
    f_nm = "main",
    txt = "Six[[:space:]]*.hooter",
    gs = 6
  ),
  R13Balls = list(
    f_nm = "3Balls",
    txt = "3 Balls",
    gs = 3
  ),
  TourneyMatches = list(
    f_nm = "Matchups",
    txt = "Tournament Match Up",
    gs = 2
  ),
  FRL = list(
    f_nm = "main",
    txt = "(1st|First) Round Leader",
    1
  )
)

for(i in 1:length(main_names)) {

# include if not all bodog odds have been posted  
  if(i %in% c(6,7)) {next}
  
  cur_nm <- main_names[i]
  tmp <- html_key_bd[[cur_nm]]
  
  url <- files_bodog[which(grepl(tmp$f_nm, files_bodog))]
  file_name <- gsub("^.+_bodog_(.+).html", "\\1", url)
  
  webpage <- read_html(here::here(base_folder, "odds_scrape", url))
  
  pull <- webpage %>%
    html_nodes("sp-coupon")
  
  if(file_name == "main") {
    tgt <- min(which(map_lgl(pull, ~ grepl(tmp$txt, .x))))
    base <- pull[[tgt]]
    
    if(cur_nm %in% c("TourneyMatches", "SixShooter", "R13Balls")) {
      
      get_nms <- base %>% html_nodes(".market-name") %>% html_text()
      
      if(cur_nm == "SixShooter"){
        nd_num <- which(grepl("Six", get_nms))
      } else {nd_num <- which(grepl(".", get_nms))}
      
      nms <- base %>% html_nodes(".markets-container") %>% .[nd_num] %>%  html_nodes(".outcomes") %>% html_text()
      odds <- base %>% html_nodes(".markets-container") %>% .[nd_num] %>%  html_nodes(".bet-price") %>% html_text() %>% gsub("^\r\n[[:space:]]*(.+)\r\n.+$", "\\1", .)
    } else {
      temp <- base %>% html_nodes(".markets-container")
      nd_num <- which(map_lgl(temp, ~ grepl(tmp$txt, .x)))
      
      if(cur_nm == "R1Matches") {
      } else if(grepl("(Top[[:space:]]*5|Top[[:space:]]*10)", cur_nm)) {
        nd_num <- nd_num[2]} else {
          nd_num <- nd_num[1]}
      
      nms <- base %>% html_nodes(".markets-container") %>% .[nd_num] %>% html_nodes(".outcomes") %>% html_text()
      odds <- base %>% html_nodes(".markets-container") %>% .[nd_num] %>% html_nodes(".bet-price") %>% html_text() %>% gsub("^\r\n[[:space:]]*(.+)\r\n.+$", "\\1", .)
      
    }
  } else if(cur_nm %in% c("TourneyMatches", "R1Matches")) {
    
    temp <- pull %>% html_nodes(".markets-container")
    nd_num <- which(map_lgl(temp, ~ grepl(tmp$txt, .x)))
    
    nms <- temp %>% .[nd_num] %>% html_nodes(".outcomes") %>% html_text()
    odds <- temp %>% .[nd_num] %>% html_nodes(".bet-price") %>% html_text() %>% gsub("^\r\n[[:space:]]*(.+)\r\n.+$", "\\1", .)

  } else {
    
    nms <- pull %>% html_nodes(".markets-container") %>%  html_nodes(".outcomes") %>% html_text()
    odds <- pull %>% html_nodes(".markets-container") %>%  html_nodes(".bet-price") %>% html_text() %>% gsub("^\r\n[[:space:]]*(.+)\r\n.+$", "\\1", .)
    
  }
  
  grp_size <- tmp$gs
  
  db <- data.frame(
    player = nms,
    odds = odds,
    stringsAsFactors = F
  ) 
  
  if(cur_nm %in% c("TourneyMatches", "SixShooter", "R13Balls", "R1Matches")){
    
    db <- mutate(db, Tag = rep(1:(length(nms)/tmp$gs), each = tmp$gs))
  }
  
  odds_package[[cur_nm]][["bodog"]] <- db
}


odds_df <- odds_package %>%
  map_dfr(.f = ~ .x %>% map_dfr(rbind,
                                .id = "site"),
  .id = "category") %>%
  mutate(odds = gsub("EVEN", "+100", odds),
         odds = as.numeric(odds),
         Prob = Prob_Calc(odds),
         Return = ifelse(odds > 0, odds, 100^2/abs(odds)),
         player = tolower(player))








