library(shiny)
library(tidyverse)
library(magrittr)

shinyServer(function(input, output, session) {
  
  data = reactiveFileReader(20000, session, "data/matches.rds", readRDS)
  
  output$ticker = renderUI({
      data()$ticker
  })
})
