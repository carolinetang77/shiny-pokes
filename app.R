library(shiny)
library(tidyverse)
library(ggplot2)

# load data
pokedata <- read.csv('data/pokemon.csv')

# ui function
ui <- fluidPage(
  # title of app
  headerPanel(title = 'Pokemon Visualizer'),
  
  # sidebar layout
  sidebarLayout(
    # sidebar (filters)
    sidebarPanel(
      selectizeInput(
        inputId = 'pokename',
        label = 'Pokemon Name',
        choices = NULL,
        multiple = FALSE
      )
    ),
    # plots
    mainPanel(
      plotOutput(outputId = 'statplot')
    )
  )
  
)

# server function
server <- function(input, output, session) {
  
  # server-side update of selectize
  updateSelectizeInput(
    session,
    inputId = 'pokename',
    choices = pokedata$NAME,
    selected = pokedata$NAME[1],
    server = TRUE)
  
  #plot output
  output$statplot <- renderPlot({
    pokedata |> 
      filter(NAME == input$pokename) |> 
      select(HP, ATK, DEF, SP_ATK, SP_DEF, SPD) |> 
      pivot_longer(
        cols = c(HP, ATK, DEF, SP_ATK, SP_DEF, SPD), 
        names_to = 'Stat', 
        values_to = 'Value'
      ) |> 
      ggplot(aes(x = Stat, y = Value)) +
        geom_bar(stat = 'identity') +
        labs(title = paste(input$pokename, 'stats'))
  })
}

shinyApp(ui, server)