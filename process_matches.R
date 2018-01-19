library(htmltools)
library(tidyverse)
library(stringr)
library(nufflytics)

## functions -----
make_ticker <- function(match_data) {
  
  if(is.null(match_data)) return(NULL)
  
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
  
  match_summary <- api_match(key, md$uuid)
  
  match_table <- data.frame(
    home = team_summary(md$teams[[1]], match_summary$match$teams[[1]]$roster, is_home = T),
    separators = c(
      img(src="img/BigIconVS.png", height = 10) %>% as.character(),
      img(src="img/GoldBall.png", height = 20, class="ball") %>% as.character(),
      img(src="img/Casualty.png", height=20, class="separator") %>% as.character(),
      "&nbsp;"
    ),
    away = team_summary(md$teams[[2]], match_summary$match$teams[[2]]$roster, is_home = F)
  )
  
  
  HTML(
    knitr::kable(
      match_table,
      format="html", escape = F, col.names = NULL, align = "rcl"
    )
  )
  
}

team_summary <- function(team_data, team_roster, is_home) {
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
      CAS <- paste0("(",team_data$sustaineddead,img(src="img/Dead.png", height = 20, class = "separator") %>% as.character,")&nbsp;&nbsp;&nbsp;",CAS)
    } else {
      CAS <- paste0(CAS,"&nbsp;&nbsp;&nbsp;(",img(src="img/Dead.png", height = 20, class = "separator") %>% as.character,team_data$sustaineddead,")")
    }
  }
  
  INJ <- team_roster %>% 
    compact("casualties_state") %>% 
    keep(~any(as.numeric(.$casualties_sustained_id)>9)) %>% 
    map(~glue::glue_data(., "{star_player_name(name)}: {map_chr(casualties_sustained_id[casualties_sustained_id>9], ~id_to_casualty(.) %>% glue::collapse('/'))}")) %>% 
    glue::collapse(", ")
  
  if(length(INJ)==0) INJ <- ""
  
  c(name,TDs,CAS, INJ)
}

#load data -----

load("data/key.rda")

old_data <- readRDS("data/matches.rds")

new_data <- api_matches(key, league = "REBBL Cripple Cup", limit = 4)

new_uuids <- new_data$matches %>% map_chr("uuid")

#replace data if newer matches available -----

if(!all(new_uuids == old_data$uuids) | length(new_uuids) == 0) {
  
  new_ticker <- make_ticker(new_data$matches)
  
  if(length(new_uuids) == 0) new_uuids <- c("","","","")
  
  to_save <- list(uuids = new_uuids, ticker = new_ticker)
  
  saveRDS(to_save, file = "data/matches.rds")
}

