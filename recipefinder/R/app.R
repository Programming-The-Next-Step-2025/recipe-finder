library(shiny)

# Get path to the CSV inside the package
#recipe_file <- system.file("extdata", "Recipes.csv", package = "recipefinder")

parse_total_time <- function(time_str) {
  time_str <- tolower(time_str)
  hours <- as.numeric(ifelse(grepl("hr", time_str), sub("^(\\d+).*", "\\1", time_str), 0))
  minutes <- as.numeric(ifelse(grepl("min", time_str), sub(".*?(\\d+)\\s*min.*", "\\1", time_str), 0))
  hours[is.na(hours)] <- 0
  minutes[is.na(minutes)] <- 0
  hours * 60 + minutes
}

ui <- fluidPage(
  titlePanel("🍽️ Recipe Finder"),

  tags$head(
    tags$style(HTML("
      .list-group-item {
        white-space: normal;
        word-wrap: break-word;
      }
    "))
  ),

  sidebarLayout(
    sidebarPanel(
      textInput("ingredients", "Enter ingredients (comma-separated):", placeholder = "e.g. chicken, flour, pepper"),
      selectInput("time_filter", "Maximum Total Time:",
                  choices = c("No filter" = Inf,
                              "Under 30 minutes" = 30,
                              "Under 1 hour" = 60,
                              "Under 3 hours" = 180),
                  selected = Inf),
      actionButton("search", "Find Recipes"),
      conditionalPanel(
        condition = "output.showBackButton == true",
        actionButton("back", "⬅️ Back to Recipe List")
      )
    ),

    mainPanel(
      uiOutput("resultsUI")
    )
  )
)

server <- function(input, output, session) {
  recipes <- reactiveVal(NULL)
  selected_recipe <- reactiveVal(NULL)

  observeEvent(input$search, {
    req(nzchar(input$ingredients))
    ing <- unlist(strsplit(input$ingredients, ",\\s*"))
    matches <- find_recipes(ing, recipe_file, as.numeric(input$time_filter))
    recipes(matches)
    selected_recipe(NULL)
  })

  observeEvent(input$back, {
    selected_recipe(NULL)
  })

  output$showBackButton <- reactive({
    !is.null(selected_recipe())
  })
  outputOptions(output, "showBackButton", suspendWhenHidden = FALSE)

  output$resultsUI <- renderUI({
    if (is.null(recipes())) return(NULL)

    if (is.null(selected_recipe())) {
      tagList(
        h4("Matching Recipes:"),
        div(class = "list-group",
            lapply(seq_along(recipes()), function(i) {
              title <- sub("🍽️ (.*?)\\n.*", "\\1", recipes()[[i]])
              actionLink(
                inputId = paste0("recipe_", i),
                label = title,
                class = "list-group-item list-group-item-action"
              )
            })
        )
      )
    } else {
      uiOutput("recipeDetails")
    }
  })

  observe({
    lapply(seq_along(recipes()), function(i) {
      observeEvent(input[[paste0("recipe_", i)]], {
        selected_recipe(recipes()[[i]])
      })
    })
  })

  output$recipeDetails <- renderUI({
    recipe <- selected_recipe()
    if (is.null(recipe)) return(NULL)

    title <- sub("🍽️ (.*?)\\n.*", "\\1", recipe)
    total_time <- sub(".*⏱️ Total time: (.*?)\\n\\n.*", "\\1", recipe)
    ingredients_section <- sub(".*🧂 Ingredients:\\n- (.*?)\\n\\n📖 Directions:.*", "\\1", recipe)
    directions <- sub(".*📖 Directions:\\n", "", recipe)
    ingredients <- unlist(strsplit(ingredients_section, "\\n- "))

    HTML(paste0(
      "<h4>🍽️ ", title, "</h4>",
      "<p><strong>⏱️ Total time:</strong> ", total_time, "</p>",
      "<p><strong>🧂 Ingredients:</strong> ", paste(ingredients, collapse = ", "), "</p>",
      "<p><strong>📖 Directions:</strong><br>", directions, "</p>"
    ))
  })
}

startApp = function() {
  shinyApp(ui = ui, server = server)
}
