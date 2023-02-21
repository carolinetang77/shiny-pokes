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
    options = list(maxOptions = 2000),
    server = TRUE)
  
  #plot output
  output$statplot <- renderPlot({
    # filter for selected pokemon
    pokedata |> 
      filter(NAME == input$pokename) |> 
      select(HP, ATK, DEF, SP_ATK, SP_DEF, SPD) |> 
      pivot_longer(
        cols = c(HP, ATK, DEF, SP_ATK, SP_DEF, SPD), 
        names_to = 'Stat', 
        values_to = 'Value'
      ) |> 
      # plot
      ggplot(aes(y = Stat, x = Value, fill = Value)) +
        geom_bar(stat = 'identity') +
        labs(title = paste(input$pokename, 'stats')) +
        scale_fill_viridis_c(
          limits = c(1, 255), 
          values = scales::rescale(c(1, 60, 100, 255))
        )
  })
}

shinyApp(ui, server)