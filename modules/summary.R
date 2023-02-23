
summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # sidebar layout
    sidebarLayout(
      # sidebar (filters)
      sidebarPanel(
        selectizeInput(
          inputId = ns('pokename'),
          label = 'Pokemon Species',
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

summary_server <- function(id, poke_url) {
  moduleServer(id, function(input, output, session) {
    # get all pokemon species
    all_pokemon <- GET(paste0(poke_url, 'pokemon-species?limit=10000&offset=0'))
    pokemon_names <- lapply(
      content(all_pokemon, 'parsed')$results, 
      FUN = function(x) x$name
    ) |> 
      unlist()
    # server-side update of selectize
    updateSelectizeInput(
      session,
      inputId = 'pokename',
      choices = pokemon_names,
      selected = pokemon_names[1],
      options = list(maxOptions = 2000),
      server = TRUE)
    
    # get one pokemon (all forms)
    onepoke <- reactive({
      r <- GET(paste0(poke_url, 'pokemon-species/', input$pokename))
      one_species <- content(r)$varieties
      varieties <- lapply(
        one_species,
        function(x) {
          r <- GET(x$pokemon$url)
          form <- content(r, 'parsed')[c(
            'name', 
            'types',
            'abilities',
            'height', 
            'weight', 
            'stats'
          )]
          
          # format stats to df
          form$stats <- lapply(
            form$stats, 
            function(x) data.frame(name = x$stat$name, stat = x$base_stat)
          ) |> 
            bind_rows()
          
          # format abilities to df
          form$abilities <- lapply(
            form$abilities, 
            function(x) data.frame(name = x$ability$name, hidden = x$is_hidden)
          ) |> 
            bind_rows()
          
          # format types to df
          form$types <- lapply(
            form$types,
            function(x) data.frame(name = x$type$name)
          ) |> 
            bind_rows()
          
          return(form)
        }
      )
    })
    
    # text output
    output$summary_info <- renderText({
      summary_text <- ''
      for (form in onepoke()) {
        reg_abilities <- form$abilities[form$abilities$hidden == FALSE,]
        hidden_abilities <- form$abilities[form$abilities$hidden == TRUE,]
        summary_text <- paste0(summary_text,
          'Name: ', form$name, '\n',
          'Type: ', form$types$name[1], '/', form$types$name[2], '\n',
          'Ability: ', reg_abilities$name[1], '/', reg_abilities$name[2], '\n',
          'Hidden Ability: ', hidden_abilities$name[1], '\n',
          'Height (m): ', form$height/10, '\n',
          'Weight (kg): ', form$weight/10, '\n\n'
        )
      }
      summary_text
    }) # end render text
    
    # plot output
    output$statplot <- renderPlot({
      req(input$pokename)
      
      stat_names <- c(
        speed = 'SPD',
        `special-defense` = 'SPDEF',
        `special-attack` = 'SPATK',
        hp = 'HP',
        defense = 'DEF',
        attack = 'ATK'
      )
      
      plot_list <- lapply(
        onepoke(),
        function(x) {
          ggplot(x$stats, aes(y = name, x = stat, fill = stat)) +
            geom_bar(stat = 'identity') +
            labs(
              x = 'Stat Value',
              y = NULL,
              title = paste(x$name, 'base stats')
            ) +
            scale_y_discrete(
              labels = stat_names
            ) +
            scale_x_continuous(
              limits = c(0, 255)
            ) +
            scale_fill_viridis_c(
              limits = c(1, 255), 
              values = scales::rescale(c(1, 60, 100, 255))
            )
        }
      )
      plot_grid(
        plotlist = plot_list, 
        ncol = 1, 
        nrow = length(plot_list),
        align = 'hv'
      )
    }) # end renderplot
  }) # end module function
}

