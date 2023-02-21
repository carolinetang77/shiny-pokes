library(shiny)
library(tidyverse)
library(ggplot2)

mgr_mods <- list.files(
  path = 'modules/', 
  full.names = TRUE
)

sapply(mgr_mods, FUN = source)

# load data
pokedata <- read.csv('data/pokemon.csv')

# ui function
ui <- fluidPage(
  # title of app
  headerPanel(title = 'Pokemon Visualizer'),
  
  # tabs
  tabsetPanel(id = 'pokemon_tabs',
    tabPanel(
      title = 'Pokemon Summary',
      value = 'pokemon_summary',
      uiOutput('summary')
    )
  )
  
)

# server function
server <- function(input, output, session) {
  output$summary <- renderUI({
    summary_ui('summary')
  })
  summary_server('summary', pokedata)
}

shinyApp(ui, server)