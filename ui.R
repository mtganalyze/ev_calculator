
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com





library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(magrittr)

shinyUI(fluidPage(

  # Application title
  titlePanel("MTG-Tournament EV Calculator"),
  mainPanel(
    inputPanel(
      selectInput("tourn", label = h4("Event type:"), choices = c("Grand Prix" = 1, "Pro Tour" = 2)),
      numericInput("fieldwp", label = h4("Average Win rate (in %) against the field:"), value = 50),
      numericInput("n_players", label = h4("Number of players:"), value = 1500),
      selectInput("sdens", label = h4("Skill density:"), choices = c("dense" = 1, "normal" = 2, "diverse" = 3), selected = 2)
    ),
    h3("Current Score:"),
    inputPanel(
      numericInput("wins", label = h4("Wins (incl. byes):"), value = 0),
      numericInput("losses", label = h4("Losses:"), value = 0),
      numericInput("draws", label = h4("Draws:"), value = 0)
    ),
    h2("Your Current Expected Winnings:"),
    tableOutput("ev"),
    tags$head(tags$style("#ev th {background-color: #cce6ff; 
                                  font-size: 18px;
                                  padding: 15px}
                          #ev td {background-color: #e6f2ff; 
                                  font-size: 20px;
                                  padding: 15px}", 
                         media="screen", type="text/css")),
    h2("Next Round at stake:"),
    tableOutput("delta_ev"),
    tags$head(tags$style("#delta_ev th {background-color: #ffffb3; 
                                        font-size: 18px;
                                        padding: 15px}
                         #delta_ev td {background-color: #ffffe6; 
                                       font-size: 20px;
                                       padding: 15px}", 
                         media="screen", type="text/css")),
    h3("Your Virtual MTGELOPROJECT rating:"),
    textOutput("virtrating"),
    tags$head(tags$style("#virtrating{color: navy;
                                      font-size: 20px;
                                      font-style: bold;
                                     }")),
    h3("Your WPT:"),
    plotlyOutput("wpt"),
    h2("Links and documentation:"),
    a("MTGELOPROJECT", href= "http://www.mtgeloproject.net/", target = "_blank" ),
    p(""),
    a("MTGANALYZE BLOG: on EV-CALCULATOR", href= "https://mtganalyze.github.io/post/extract_value/" , target = "_blank"),
    p(""),
    a("MTGANALYZE BLOG: on WPTs", href= "https://mtganalyze.github.io/post/skills_and_scores/" , target = "_blank")
  )
))
