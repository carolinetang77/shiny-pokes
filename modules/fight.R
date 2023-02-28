fight_ui <- function(id) {
  ns <- NS(id)
  # probably also a sidebar
  tagList(
    sidebarLayout(
      # sidebar
      sidebarPanel(
        selectizeInput(
          inputId = ns('games'),
          label = "Game/Generation:",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        selectizeInput(
          inputId = ns('opponent'),
          label = 'Opposing Pokemon:',
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        uiOutput(ns('opp_form')),
        selectizeInput(
          inputId = ns('mine'),
          label = 'Your Pokemon:',
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        uiOutput(ns('my_form')),
        actionButton(
          inputId = ns('done'),
          label = 'Can I fight it?'
        )
      ),
      # main panel
      mainPanel(
        textOutput(ns('fightable'))
      )
    )
  )
}

fight_server <- function(id, poke_url) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # update game selection
    observe({
      if (is.null(input$games)) {
        # get the games
        vr <- GET(paste0(poke_url, 'version?limit=100'))
        vr_contents <- content(vr, as = 'parsed')$results
        version_list <- lapply(
          vr_contents,
          function(x) {
            ifelse(x$name %in% c('colosseum', 'xd'), NA, x$name)
          }
        ) |> unlist()
        version_list <- version_list[!is.na(version_list)]
        names(version_list) <- str_to_title(version_list)
        
        # update dropdown
        updateSelectizeInput(
          session,
          inputId = 'games',
          choices = version_list,
          selected = version_list[1],
          server = TRUE
        )
      }
    })
    
    # get generation number
    gen <- reactive({
      req(input$games)
  
      # version group
      vr <- GET(paste0(poke_url, 'version/', input$games))
      vg_url <- content(vr, as = 'parsed')$version_group$url
      
      # generation
      vg_r <- GET(vg_url)
      generation <- content(vg_r, as = 'parsed')$generation
      gen <- as.numeric(
        as.roman(
          gsub('generation-(\\w+)', '\\1', generation$name)
        )
      )
      return(gen)
    })
    
    # get version pokemon
    gen_pokemon <- reactive({
      req(input$games)
      
      # generation urls
      gen_url_list <- sapply(1:gen(), function(x) paste0(poke_url, 'generation/', x, '/'))
      
      # get pokemon names
      pokemon <- sapply(gen_url_list, function(x) {
        r <- GET(x)
        pokemon <- content(r, as = 'parsed')$pokemon_species
        pokemon_names <- lapply(pokemon, function(y) {
          y$name
        })
      }) |> unlist()
      
      names(pokemon) <- str_to_title(pokemon)
      return(pokemon)
      
    }) |> bindCache(input$games)
    
    # update opponent pokemon list based on game
    observe({
      updateSelectizeInput(
        session,
        inputId = 'opponent',
        choices = gen_pokemon(),
        selected = gen_pokemon()[1],
        server = TRUE
      )
    }) |> bindEvent(input$games)
    
    # update own pokemon list based on game
    observe({
      updateSelectizeInput(
        session,
        inputId = 'mine',
        choices = gen_pokemon(),
        selected = gen_pokemon()[1],
        server = TRUE
      )
    }) |> bindEvent(input$games)
    
    # update pokemon form (opponent)
    observe({
      output$opp_form <- renderUI({
        req(input$opponent)
        
        r <- GET(paste0(poke_url, 'pokemon-species/', input$opponent))
        one_species <- content(r)$varieties
        variety_names <- lapply(one_species, function(x) {
          x$pokemon$name
        }) |> unlist()
        
        names(variety_names) <- str_to_title(variety_names)
        
        selectInput(
          inputId = ns('opp_form'),
          label = "Opponent's Form",
          choices = variety_names,
          selected = variety_names[1]
        )
      })
    }) |> 
      bindEvent(input$opponent)
    
    # update pokemon form (own)
    observe({
      output$my_form <- renderUI({
        req(input$mine)
        
        r <- GET(paste0(poke_url, 'pokemon-species/', input$mine))
        one_species <- content(r)$varieties
        variety_names <- lapply(one_species, function(x) {
          x$pokemon$name
        }) |> unlist()
        
        names(variety_names) <- str_to_title(variety_names)
        
        selectInput(
          inputId = ns('my_form'),
          label = "Your Pokemon's Form",
          choices = variety_names,
          selected = variety_names[1]
        )
      })
    }) |> bindEvent(input$mine)
    
    # calculate stuff on button click
    observe({
      output$fightable <- renderText({
        paste('Fighting', str_to_title(input$opp_form), 'with', str_to_title(input$my_form))
      })
    }) |> bindEvent(input$done)
    
  }) # end moduleServer
}