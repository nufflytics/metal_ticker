library(htmltools)
library(tidyverse)
library(stringr)
library(nufflytics)

## functions -----
make_ticker <- function(match_data) {
  
  div(class = "news",
      tags$ul(
        tags$li(
          make_game_summary(match_data[[1]])
        ),
        tags$li(
          make_game_summary(match_data[[2]])
        ),
        tags$li(
          make_game_summary(match_data[[3]])
        ),
        tags$li(
          make_game_summary(match_data[[4]])
        )
      )
  )
  
}

make_game_summary <- function(md) {
  
  match_table <- data.frame(
    home = team_summary(md$teams[[1]], is_home = T),
    separators = c(
      img(src="img/BigIconVS.png", height = 10) %>% as.character(),
      img(src="img/REBBL_s.png", height = 25, class="separator") %>% as.character(),
      img(src="img/Casualty.png", height=20, class="separator") %>% as.character()
    ),
    away = team_summary(md$teams[[2]], is_home = F)
  )
  
  
  HTML(
    knitr::kable(
      match_table,
      format="html", escape = F, col.names = NULL, align = "rcl"
    )
  )
  
}

team_summary <- function(team_data, is_home) {
  race = id_to_race(team_data$idraces) %>% str_replace_all(" ","")
  
  if(is_home) {
    name = paste(team_data$teamname, img(src=glue::glue("img/race/{race}.png"), height = 20, class = "separator") %>% as.character)
  } else {
    name = paste(img(src=glue::glue("img/race/{race}.png"), height = 20, class = "separator") %>% as.character, team_data$teamname)
  }
  
  TDs = team_data$inflictedtouchdowns
  
  CAS = team_data$sustainedcasualties
  
  if(team_data$sustaineddead > 0 ) {
    if(is_home) {
      CAS <- paste0("(",team_data$sustaineddead," ",img(src="img/Dead.png", height = 20, class = "separator") %>% as.character,") ",CAS)
    } else {
      CAS <- paste0(CAS," (",img(src="img/Dead.png", height = 20, class = "separator") %>% as.character," ",team_data$sustaineddead,")")
    }
  }
  
  c(name,TDs,CAS)
}

#load data -----

load("data/key.rda")

old_data <- readRDS("data/matches.rds")

new_data <- api_matches(key, league = "Cabalvision Official League", competition = "Champion Ladder XI", limit = 4)

new_uuids <- new_data$matches %>% map_chr("uuid")

#replace data if newer matches available -----

if(all(new_uuids != old_data$uuids)) {
  
  new_ticker <- make_ticker(new_data$matches)
  
  to_save <- list(uuids = new_uuids, ticker = new_ticker)
  
  saveRDS(to_save, file = "data/matches.rds")
}

