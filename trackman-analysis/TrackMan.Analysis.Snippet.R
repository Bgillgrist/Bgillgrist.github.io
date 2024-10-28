library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Baseball TrackMan Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Choose CSV Files", multiple = TRUE),
      selectInput("analysis_type", "Select Analysis Type", 
                  choices = c("ANOVA", "Player Comparison", "Stat Leaders")),
      conditionalPanel(
        condition = "input.analysis_type == 'ANOVA'",
        selectInput("role", "Select Role", choices = c("Batter", "Pitcher")),
        uiOutput("player"),
        selectInput("stat", "Select Statistic", choices = NULL),
        actionButton("analyze", "Analyze")
      )
    ),
    mainPanel(
      conditionalPanel(condition = "input.analysis_type == 'ANOVA'", 
                       verbatimTextOutput("summary"), 
                       plotOutput("boxplot"))
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$files)
    combined_data <- do.call(rbind, lapply(input$files$datapath, read.csv))
    combined_data
  })
  
  output$player <- renderUI({
    req(data())
    role_col <- if (input$role == "Batter") "Batter" else "Pitcher"
    selectInput("player", "Choose Player", choices = unique(data()[[role_col]]))
  })
  
  observeEvent(input$analyze, {
    req(input$player, input$stat)
    filtered_data <- data() %>% 
      filter((input$role == "Batter" & Batter == input$player) |
               (input$role == "Pitcher" & Pitcher == input$player))
    
    output$summary <- renderPrint({
      # Simple mock-up of ANOVA summary
      "ANOVA results will display here"
    })
    
    output$boxplot <- renderPlot({
      ggplot(filtered_data, aes_string(x = "role", y = input$stat)) +
        geom_boxplot() +
        theme_minimal()
    })
  })
}

shinyApp(ui = ui, server = server)