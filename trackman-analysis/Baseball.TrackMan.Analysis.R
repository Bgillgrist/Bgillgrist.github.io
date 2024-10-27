library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(viridis)

# Increase upload size limit
options(shiny.maxRequestSize = 30 * 1024^2)  # Set to 30 MB

# Define UI for application
ui <- fluidPage(
  titlePanel("Baseball TrackMan Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Choose CSV Files", multiple = TRUE, accept = ".csv"),
      selectInput("analysis_type", "Select Analysis Type", choices = c("ANOVA", "Player Comparison","Stat Leaders", "Pitcher Report", "Hitter Report")),
      conditionalPanel(
        condition = "input.analysis_type == 'ANOVA'",
        selectInput("role", "Select Role", choices = c("Batter", "Pitcher")),
        uiOutput("player"),
        uiOutput("xcol"),
        uiOutput("ycol"),
        actionButton("analyze", "Analyze")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Player Comparison'",
        selectInput("role_compare", "Select Role", choices = c("Batter", "Pitcher")),
        uiOutput("players_compare"),
        uiOutput("stat_compare"),
        uiOutput("situation_column"),
        uiOutput("situation_value"),
        actionButton("compare", "Compare")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Stat Leaders'",
        selectInput("role_stat_leader", "Select Role", choices = c("Batter", "Pitcher", "Catcher")),
        uiOutput("stat_leader_column"),
        conditionalPanel(
          condition = "input.role_stat_leader == 'Pitcher'",
          uiOutput("pitch_type")  # For TaggedPitchType selection
        ),
        actionButton("show_leaders", "Show Leaders")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Pitcher Report'",
        uiOutput("pitcher"),
        actionButton("generate_report", "Generate Report")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Hitter Report'",
        uiOutput("hitter"),
        actionButton("generate_hitter_report", "Generate Report")
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.analysis_type == 'ANOVA'",
        verbatimTextOutput("summary"),
        plotOutput("boxplot")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Player Comparison'",
        plotOutput("compare_plot")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Stat Leaders'",
        print("*Minimum 30 Observations*"),
        tableOutput("leaderboard")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Pitcher Report'",
        print("*Overall stats my be inaccurate due to missing games and incorrect tagging*"),
        tableOutput("pitcher_summary"),
        tableOutput("pitcher_pitch_type_stats"),
        uiOutput("pitch_heatmaps")  # Dynamic UI output for heatmaps
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Hitter Report'",
        print("*Overall stats my be inaccurate due to missing games and incorrect tagging*"),
        tableOutput("hitter_summary"),
        tableOutput("hitter_additional_stats"),
        plotOutput("spray_chart")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to read and combine CSV files
  data <- reactive({
    req(input$files)
    files <- input$files$datapath
    combined_data <- do.call(rbind, lapply(files, read_csv))
    
    # Consolidate pitch types
    combined_data$TaggedPitchType <- recode(combined_data$TaggedPitchType,
                                            "Cutter" = "Fastball",
                                            "Sinker" = "Fastball",
                                            "FourSeamFastball" = "Fastball",
                                            "TwoSeamFastball" = "Fastball")
    combined_data
  })
  
  # Dynamically generate UI for player selection based on role
  output$player <- renderUI({
    req(data())
    if (input$role == "Batter") {
      players <- unique(data()$Batter)
    } else {
      players <- unique(data()$Pitcher)
    }
    selectInput("player", "Choose Player", choices = players)
  })
  
  # Dynamically generate UI for column selection
  output$xcol <- renderUI({
    req(data())
    selectInput("xcol", "Choose Statistic", names(data()))
  })
  
  output$ycol <- renderUI({
    req(data())
    selectInput("ycol", "Choose Category", choices = names(data()))
  })
  
  # Perform ANOVA analysis when the button is clicked
  observeEvent(input$analyze, {
    req(input$player, input$xcol, input$ycol)
    
    # Filter data based on role and selected player
    filtered_data <- data() %>% 
      filter((input$role == "Batter" & Batter == input$player) | 
               (input$role == "Pitcher" & Pitcher == input$player))
    
    x <- filtered_data[[input$xcol]]
    
    # Treat y as a factor (categorical)
    y <- as.factor(filtered_data[[input$ycol]])
    
    output$summary <- renderPrint({
      summary(aov(x ~ y))
    })
    output$boxplot <- renderPlot({
      ggplot(filtered_data, aes_string(x = input$ycol, y = input$xcol)) +
        geom_boxplot() +
        theme_minimal() +
        labs(x = input$ycol, y = input$xcol)  # Add labels for clarity
    })
  })
  
  # Dynamically generate UI for player selection for comparison
  output$players_compare <- renderUI({
    req(data(), input$role_compare)
    if (input$role_compare == "Batter") {
      players <- unique(data()$Batter)
    } else {
      players <- unique(data()$Pitcher)
    }
    selectInput("players_compare", "Choose Players", choices = players, multiple = TRUE)
  })
  
  # Dynamically generate UI for stat selection for comparison
  output$stat_compare <- renderUI({
    req(data())
    selectInput("stat_compare", "Choose Statistic", names(data()))
  })
  
  # Dynamically generate UI for situation column selection
  output$situation_column <- renderUI({
    req(data())
    selectInput("situation_column", "Choose Situation Column", names(data()))
  })
  
  # Dynamically generate UI for situation value selection
  output$situation_value <- renderUI({
    req(data(), input$situation_column)
    values <- unique(data()[[input$situation_column]])
    selectInput("situation_value", "Choose Situation Value", choices = values)
  })
  
  # Perform side-by-side comparison when the button is clicked
  observeEvent(input$compare, {
    req(input$players_compare, input$stat_compare)
    
    # Filter data based on role and selected players
    filtered_data <- data() %>%
      filter((input$role_compare == "Batter" & Batter %in% input$players_compare) |
               (input$role_compare == "Pitcher" & Pitcher %in% input$players_compare))
    
    # Optionally filter by situation
    if (!is.null(input$situation_column) && !is.null(input$situation_value)) {
      filtered_data <- filtered_data %>%
        filter(!!sym(input$situation_column) == input$situation_value)
    }
    
    stat <- filtered_data[[input$stat_compare]]
    
    output$compare_plot <- renderPlot({
      ggplot(filtered_data, aes_string(x = ifelse(input$role_compare == "Batter", "Batter", "Pitcher"), y = input$stat_compare)) +
        geom_boxplot() +
        theme_minimal() +
        labs(x = "Player", y = input$stat_compare)  # Add labels for clarity
    })
  })
  
  # Define hitter and pitcher stats
  hitter_stats <- c("ExitSpeed", "HitSpinRate", "PositionAt110X", "PositionAt110Y", "PositionAt110Z", "Distance", "LastTrackedDistance", "HangTime", "MaxHeight", "ContactPositionX", "ContactPositionY", "ContactPositionZ", "HitSpinAxis", "HitTrajectoryXc0", "HitTrajectoryXc1", "HitTrajectoryXc2", "HitTrajectoryXc4", "HitTrajectoryXc5", "HitTrajectoryXc6", "HitTrajectoryXc7", "HitTrajectoryXc8", "HitTrajectoryYc0", "HitTrajectoryYc1", "HitTrajectoryYc3", "HitTrajectoryYc4", "HitTrajectoryYc5", "HitTrajectoryYc6", "HitTrajectoryYc7", "HitTrajectoryYc8", "HitTrajectoryZc0", "HitTrajectoryZc1", "HitTrajectoryZc2", "HitTrajectoryZc3", "HitTrajectoryZc4", "HitTrajectoryZc5", "HitTrajectoryZc6", "HitTrajectoryZc7", "HitTrajectoryZc8")  # Replace with actual hitter stats
  pitcher_stats <- c("RelSpeed", "VertRelAngle", "HorzRelAngle", "SpinRate", "SpinAxis", "RelHeight", "RelSide", "Extension", "VertBreak", "InducedVertBreak", "HorzBreak", "ZoneSpeed", "VertApprAngle", "HorzApprAngle", "ZoneTime", "pfxx", "pfxz", "x0", "y0", "z0", "vx0", "vy0", "vz0", "ax0", "ay0", "az0", "EffectiveVelo", "SpeedDrop", "PitchTrajectoryXc0", "PitchTrajectoryXc1", "PitchTrajectoryXc2", "PitchTrajectoryYc0", "PitchTrajectoryYc1", "PitchTrajectoryYc2", "PitchTrajectoryZc0", "PitchTrajectoryZc1", "PitchTrajectoryZc2")  # Replace with actual pitcher stats
  catcher_stats <- c("ThrowSpeed", "PopTime", "ExchangeTime", "TimeToBase", "CatchPositionX", "CatchPositionY", "CatchPositionZ", "ThrowPositionX", "ThrowPositionY", "ThrowPositionZ", "BasePositionX", "BasePositionY", "BasePositionZ", "ThrowTrajectoryXc0", "ThrowTrajectoryXc1", "ThrowTrajectoryXc2", "ThrowTrajectoryYc0", "ThrowTrajectoryYc1", "ThrowTrajectoryYc2", "ThrowTrajectoryZc0", "ThrowTrajectoryZc1", "ThrowTrajectoryZc2")  # Replace with actual catcher stats
  
  # Dynamically generate UI for stat selection based on role
  output$stat_leader_column <- renderUI({
    req(input$role_stat_leader)
    if (input$role_stat_leader == "Batter") {
      selectInput("stat_leader_column", "Choose Statistic", choices = hitter_stats)
    } else if (input$role_stat_leader == "Pitcher") {
      selectInput("stat_leader_column", "Choose Statistic", choices = pitcher_stats)
    } else {
      selectInput("stat_leader_column", "Choose Statistic", choices = catcher_stats)
    }
  })
  
  # Dynamically generate UI for TaggedPitchType selection if role is Pitcher
  output$pitch_type <- renderUI({
    req(input$role_stat_leader)
    if (input$role_stat_leader == "Pitcher") {
      selectInput("pitch_type", "Choose Tagged Pitch Type", choices = unique(data()$TaggedPitchType))
    } else {
      return(NULL)
    }
  })
  
  # Observe and execute when the "Show Leaders" button is clicked
  observeEvent(input$show_leaders, {
    req(input$stat_leader_column, input$role_stat_leader)
    
    # Filter data based on role and TaggedPitchType if applicable
    filtered_data <- if (input$role_stat_leader == "Batter") {
      data() %>%
        select(Batter, all_of(input$stat_leader_column)) %>%
        group_by(Batter) %>%
        summarise(
          mean_stat = mean(!!sym(input$stat_leader_column), na.rm = TRUE),
          observations = n()
        ) %>%
        filter(observations >= 30) %>%
        arrange(desc(mean_stat))
    } else if (input$role_stat_leader == "Pitcher") {
      req(input$pitch_type)
      data() %>%
        filter(TaggedPitchType == input$pitch_type) %>%
        select(Pitcher, all_of(input$stat_leader_column)) %>%
        group_by(Pitcher) %>%
        summarise(
          mean_stat = mean(!!sym(input$stat_leader_column), na.rm = TRUE),
          observations = n()
        ) %>%
        filter(observations >= 30) %>%
        arrange(desc(mean_stat))
    } else {
      data() %>%
        select(Catcher, all_of(input$stat_leader_column)) %>%
        group_by(Catcher) %>%
        summarise(
          mean_stat = mean(!!sym(input$stat_leader_column), na.rm = TRUE),
          observations = n()
        ) %>%
        filter(observations >= 30) %>%
        arrange(desc(mean_stat))
    }
    
    # Add rank column
    filtered_data <- filtered_data %>%
      mutate(Rank = row_number()) %>%
      select(Rank, everything())
    
    # Render leaderboard table
    output$leaderboard <- renderTable({
      filtered_data
    })
  })
  
  # Dynamically generate UI for pitcher selection
  output$pitcher <- renderUI({
    req(data())
    pitchers <- unique(data()$Pitcher)
    selectInput("pitcher", "Choose Pitcher", choices = pitchers)
  })
  
  # Generate pitcher report when the button is clicked
  observeEvent(input$generate_report, {
    req(input$pitcher)
    
    # Filter data for the selected pitcher
    pitcher_data <- data() %>% filter(Pitcher == input$pitcher)
    
    # Calculate summary statistics
    summary_stats <- pitcher_data %>%
      summarise(
        GamesPlayed = n_distinct(Date),
        InningsPitched = sum(OutsOnPlay) / 3,
        Strikeouts = sum(KorBB == "Strikeout"),
        Walks = sum(KorBB == "Walk"),
        Hits = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"))
      )
    
    # Calculate average statistics by pitch type and order by specified order
    pitch_type_stats <- pitcher_data %>%
      group_by(TaggedPitchType) %>%
      summarise(
        Count = n(),
        RelSpeed = mean(RelSpeed, na.rm = TRUE),
        SpinRate = mean(SpinRate, na.rm = TRUE),
        RelHeight = mean(RelHeight, na.rm = TRUE),
        InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
        HorzBreak = mean(HorzBreak, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      filter(Count > 15 & TaggedPitchType %in% c("Fastball", "ChangeUp", "Slider", "Curveball")) %>%
      arrange(match(TaggedPitchType, c("Fastball", "ChangeUp", "Slider", "Curveball")))
    
    output$pitcher_summary <- renderTable(summary_stats)
    output$pitcher_pitch_type_stats <- renderTable(pitch_type_stats)
    
    # Dynamically generate heatmaps for each pitch type
    pitch_types <- unique(pitch_type_stats$TaggedPitchType)
    num_pitch_types <- length(pitch_types)
    num_rows <- ceiling(num_pitch_types / 2)
    
    output$pitch_heatmaps <- renderUI({
      plot_output_list <- lapply(1:num_pitch_types, function(i) {
        plotOutput(outputId = paste0("pitch_heatmap_", i), height = "400px", width = "400px")
      })
      fluidRow(
        lapply(1:num_rows, function(row) {
          fluidRow(
            lapply(1:2, function(col) {
              index <- (row - 1) * 2 + col
              if (index <= num_pitch_types) {
                column(6, plot_output_list[[index]])
              }
            })
          )
        })
      )
    })
    
    lapply(1:num_pitch_types, function(i) {
      output[[paste0("pitch_heatmap_", i)]] <- renderPlot({
        pitch_type <- pitch_types[i]
        ggplot(pitcher_data %>% filter(TaggedPitchType == pitch_type), aes(x = PlateLocSide, y = PlateLocHeight)) +
          geom_density2d_filled(aes(fill = ..level..), contour_var = "density") +
          scale_fill_viridis_d(option = "C") +  # Use a discrete color scale
          geom_rect(aes(xmin = -0.708333, xmax = 0.708333, ymin = 1.55, ymax = 3.78), color = "black", fill = NA, linetype = "solid", size = 1.2) +
          coord_fixed(ratio = 1) +  # Set a 1:1 aspect ratio
          coord_cartesian(xlim = c(-2, 2), ylim = c(0, 5)) +  # Ensure the strike zone is centered
          theme_minimal() +
          labs(title = paste(pitch_type, "Heatmap"), x = "Plate Location Side", y = "Plate Location Height") +
          theme(
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_text(hjust = 0.5),
            axis.title.y = element_text(hjust = 0.5)
          )
      })
    })
  })
  
  # Dynamically generate UI for hitter selection
  output$hitter <- renderUI({
    req(data())
    hitters <- unique(data()$Batter)
    selectInput("hitter", "Choose Hitter", choices = hitters)
  })
  
  # Generate hitter report when the button is clicked
  observeEvent(input$generate_hitter_report, {
    req(input$hitter)
    
    # Filter data for the selected hitter
    hitter_data <- data() %>% filter(Batter == input$hitter)
    
    # Calculate overall stats
    overall_stats <- hitter_data %>%
      summarise(
        Games = n_distinct(Date),
        PA = hitter_data %>% filter(PitchofPA == 1) %>% summarise(count = n()),
        AB = PA - (sum(KorBB == "Walk")+sum(PlayResult == "HitByPitch")+sum(PlayResult == "Sacrifice")),
        H = sum(PlayResult == "Single")+sum(PlayResult == "Double")+sum(PlayResult == "Triple")+sum(PlayResult == "HomeRun"),
        AVG = round(H/AB,3),
        OBP = (H+sum(KorBB == "Walk")+sum(PlayResult == "HitByPitch"))/(AB+sum(KorBB == "Walk")+sum(PlayResult == "HitByPitch")+sum(PlayResult == "Sacrifice")),
        SLG = (((sum(PlayResult == "Single"))+(sum(PlayResult == "Double")*2)+(sum(PlayResult == "Triple")*3)+(sum(PlayResult == "HomeRun")*4))/AB)
      )
    
    # Calculate Unique Stats
    additional_stats <- hitter_data %>%
      summarise(
        AVG_Exit_Velo = mean(ExitSpeed, na.rm = TRUE),
        Chase_per = (sum((PlateLocSide > 0.70833333 | PlateLocSide < -0.70833333 | PlateLocHeight > 3.78 | PlateLocHeight < 1.55) & PitchCall %in% c("StrikeSwinging", "FoulBallNotFieldable", "InPlay"), na.rm = TRUE) / 
                       sum(PlateLocSide > 0.70833333 | PlateLocSide < -0.70833333 | PlateLocHeight > 3.78 | PlateLocHeight < 1.55, na.rm = TRUE)) * 100,
        Whiff_per = (sum(PitchCall == "StrikeSwinging")/(sum(PitchCall == "StrikeSwinging")+sum(PitchCall == "InPlay")+sum(PitchCall == "FoulBallNotFieldable")+sum(PitchCall == "FoulBallFieldable")))*100,
        K_per = (sum(KorBB == "Strikeout")/hitter_data %>% filter(PitchofPA == 1) %>% summarise(count = n()))*100,
        BB_per = (sum(KorBB == "Walk")/hitter_data %>% filter(PitchofPA == 1) %>% summarise(count = n()))*100,
      ) %>%
      rename(
        `Average Exit Velocity` = AVG_Exit_Velo,
        `Chase %` = Chase_per,
        `Whiff %` = Whiff_per,
        `K %` = K_per,
        `BB %` = BB_per
      )
    
    output$hitter_summary <- renderTable(overall_stats)
    output$hitter_additional_stats <- renderTable(additional_stats)
    
    # Render spray chart
    output$spray_chart <- renderPlot({
      req(input$hitter)
      
      # Filter data for the selected hitter
      hitter_data <- data() %>% filter(Batter == input$hitter)
      
      # Base plot for the field
      base_plot <- ggplot() +
        # Drawing the 45 degree lines from home plate
        geom_segment(aes(x = 0, y = 0, xend = 32.5 * cos(pi/4), yend = 32.5 * sin(pi/4)), color = "black") +
        geom_segment(aes(x = 0, y = 0, xend = 32.5 * cos(3*pi/4), yend = 32.5 * sin(3*pi/4)), color = "black") +
        # Drawing the base lines
        geom_segment(aes(x = 0, y = 0, xend = 9 * cos(pi/4), yend = 9 * sin(pi/4)), color = "black") +  # Home to 1st base
        geom_segment(aes(x = 9 * cos(pi/4), y = 9 * sin(pi/4), xend = 9 * cos(pi/4) - 9 * sin(pi/4), yend = 9 * sin(pi/4) + 9 * cos(pi/4)), color = "black") +  # 1st to 2nd base
        geom_segment(aes(x = 0, y = 0, xend = -9 * cos(pi/4), yend = 9 * sin(pi/4)), color = "black") +  # Home to 3rd base
        geom_segment(aes(x = -9 * cos(pi/4), y = 9 * sin(pi/4), xend = 9 * cos(pi/4) - 9 * sin(pi/4), yend = 9 * sin(pi/4) + 9 * cos(pi/4)), color = "black") +  # 3rd to 2nd base
        geom_segment(aes(x = 0, y = 0, xend = -9 * cos(pi/4), yend = 9 * sin(pi/4)), color = "black") +  # Home to 3rd base
        geom_segment(aes(x = 9 * cos(pi/4), y = 9 * sin(pi/4), xend = 0, yend = 9 * cos(pi/4) - 9 * sin(pi/4)), color = "black") +  # 1st to 2nd base
        # Drawing the outfield arc
        geom_path(aes(x = 32.5 * cos(seq(pi/4, 3*pi/4, length.out = 100)), y = 32.5 * sin(seq(pi/4, 3*pi/4, length.out = 100))), color = "black") +
        # Plot adjustments
        coord_fixed(ratio = 1) +
        theme_void() +
        ggtitle("Spray Chart") +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.margin = margin(20, 20, 20, 20)
        )
      
      # Add lines for each hit based on Bearing and Distance
      hit_lines <- hitter_data %>%
        mutate(
          Bearing = -Bearing + 90,
          xend = (Distance/10) * cos(Bearing * pi / 180),
          yend = (Distance/10) * sin(Bearing * pi / 180)
        )
      
      spray_plot <- base_plot +
        geom_segment(data = hit_lines, aes(x = 0, y = 0, xend = xend, yend = yend), color = "blue", alpha = 0.5)
      
      spray_plot
    }, height = 400, width = 400)
  })
}

# Run the application
shinyApp(ui = ui, server = server)