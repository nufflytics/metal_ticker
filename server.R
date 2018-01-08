library(shiny)
library(tidyverse)
library(magrittr)

shinyServer(function(input, output, session) {
  
  read_data <- function(file) {
    read_lines(file) %>% as.list()
  }
  
vals = reactiveFileReader(20000, session, "data/info.tsv", read_data)
  
  output$ticker = renderUI({
      div(class = "news",
        tags$ul(
          tags$li(
            HTML(knitr::kable(data.frame(
              c(paste("TeamA",vals()[[1]]), sample(1:4,1), sample(1:4,1)), 
              c(img(src="img/BigIconVS.png", height = 10) %>% as.character(),img(src="img/REBBL_s.png", height = 25, class="separator") %>% as.character(),img(src="img/Casualty.png", height=20, class="separator") %>% as.character()), 
              c(paste("TeamB",vals()[[1]]),sample(1:4,1), sample(1:4,1))
              ), format="html", escape = F, col.names = NULL, align = "rcl"))
            ),
          tags$li(
            HTML(knitr::kable(data.frame(
              c(paste("TeamA",vals()[[2]]), sample(1:4,1), sample(1:4,1)), 
              c(img(src="img/BigIconVS.png", height = 10) %>% as.character(),img(src="img/REBBL_s.png", height = 25, class="separator") %>% as.character(),img(src="img/Casualty.png", height=20, class="separator") %>% as.character()), 
              c(paste("TeamB",vals()[[2]]),sample(1:4,1), sample(1:4,1))
            ), format="html", escape = F, col.names = NULL, align = "rcl"))
          ),
          tags$li(
            HTML(knitr::kable(data.frame(
              c(paste("TeamA",vals()[[3]]), sample(1:4,1), sample(1:4,1)), 
              c(img(src="img/BigIconVS.png", height = 10) %>% as.character(),img(src="img/REBBL_s.png", height = 25, class="separator") %>% as.character(),img(src="img/Casualty.png", height=20, class="separator") %>% as.character()), 
              c(paste("TeamB",vals()[[3]]),sample(1:4,1), sample(1:4,1))
            ), format="html", escape = F, col.names = NULL, align = "rcl"))
          ),
          tags$li(
            HTML(knitr::kable(data.frame(
              c(paste("TeamA",vals()[[4]]), sample(1:4,1), sample(1:4,1)), 
              c(img(src="img/BigIconVS.png", height = 10) %>% as.character(),img(src="img/REBBL_s.png", height = 25, class="separator") %>% as.character(),img(src="img/Casualty.png", height=20, class="separator") %>% as.character()), 
              c(paste("TeamA",vals()[[4]]),sample(1:4,1), sample(1:4,1))
            ), format="html", escape = F, col.names = NULL, align = "rcl"))
          )
        )
      )
  })
})
