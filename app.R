library(shiny)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(cowplot)

mgr_mods <- list.files(
  path = 'modules/', 
  full.names = TRUE
)

sapply(mgr_mods, FUN = source)

# pokeAPI (v2) url
poke_url <- 'https://pokeapi.co/api/v2/'

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
  summary_server('summary', poke_url)
}

shinyApp(ui, server)