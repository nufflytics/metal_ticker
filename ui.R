library(shiny)

shinyUI(tagList(
  suppressDependencies("bootstrap"),
  shiny::includeCSS("www/ticker.css"),
  shiny::uiOutput("ticker")
))
