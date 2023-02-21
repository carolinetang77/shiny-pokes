
summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # sidebar layout
    sidebarLayout(
      # sidebar (filters)
      sidebarPanel(
        selectizeInput(
          inputId = ns('pokename'),
          label = 'Pokemon Name',
          choices = NULL,
          multiple = FALSE
        )
      ),
      # plots
      mainPanel(
        fluidRow(
          column(
            4,
            tagAppendAttributes(
              textOutput(outputId = ns('summary_info')),
              style="white-space:pre-wrap;"
            )
          ),
          column(
            8,
            plotOutput(outputId = ns('statplot'))
          )
        )
      )
    )
  )
}

summary_server <- function(id, pokedata) {
  moduleServer(id, function(input, output, session) {
    # server-side update of selectize
    updateSelectizeInput(
      session,
      inputId = 'pokename',
      choices = pokedata$NAME,
      selected = pokedata$NAME[1],
      options = list(maxOptions = 2000),
      server = TRUE)
    
    onepoke <- reactive(
      pokedata |> 
        filter(NAME == input$pokename)
    )
    
    # text output
    output$summary_info <- renderText({
      paste0(
        'Name: ', onepoke()$NAME, ' (National Dex #', onepoke()$NUMBER, ')', '\n',
        'Generation: ', onepoke()$GENERATION, '\n',
        'Type: ', onepoke()$TYPE1, '/', onepoke()$TYPE2, '\n',
        'Ability: ', onepoke()$ABILITY1, '/', onepoke()$ABILITY2, '\n',
        'Hidden Ability: ', onepoke()$ABILITY.HIDDEN, '\n',
        'Height (m): ', onepoke()$HEIGHT, '\n',
        'Weight (kg): ', onepoke()$WEIGHT, '\n',
        'Colour: ', onepoke()$COLOR, '\n\n'
      )
    })
    # plot output
    output$statplot <- renderPlot({
      # filter for selected pokemon
       onepoke() |> 
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
  })
}

