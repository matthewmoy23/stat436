library(tidyverse)
library(ggplot2)
library(dplyr)
library(shiny)

stats <- read_csv("https://uwmadison.box.com/shared/static/pxdoapavrtp15im106tph95alsogic9x.csv")

stats <- stats %>%
  rename(player_name = `last_name, first_name`)

# Descriptions for each statistic
stat_descriptions <- list(
  "batting_avg" = "Batting Average (AVG): The ratio of a player's hits to their total at-bats. A good AVG is typically above .300, while a poor AVG is below .250.",
  "slg_percent" = "Slugging Percentage (SLG): Measures the power of a hitter by calculating total bases per at-bat. A good SLG is .450 or higher.",
  "on_base_percent" = "On-Base Percentage (OBP): The percentage of plate appearances where a player reaches base. A good OBP is .360 or higher.",
  "on_base_plus_slg" = "On-Base Plus Slugging (OPS): Combines OBP and SLG to give a fuller measure of a hitter's ability. A great OPS is above .900.",
  "barrel_batted_rate" = "Barrel %: The percentage of balls hit with an ideal combination of exit velocity and launch angle for home runs or extra-base hits.",
  "hard_hit_percent" = "Hard Hit %: The percentage of batted balls hit at 95 mph or higher. A high percentage indicates strong contact.",
  "k_percent" = "Strikeout Rate (K%): The percentage of plate appearances that result in strikeouts. A low K% is better, typically below 20%.",
  "bb_percent" = "Walk Rate (BB%): The percentage of plate appearances that result in walks. A higher BB% indicates good plate discipline, typically above 10%."
)

ui <- fluidPage(
  titlePanel("2020-2024 Baseball Stat Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      # Select players for comparison (multiple players can be chosen)
      selectizeInput("players", "Select Players:", choices = unique(stats$player_name), multiple = TRUE, options = list(placeholder = 'Type to search...')),
      # Select first and second stats for comparison
      selectInput("stat1", "Select First Stat:", 
                  choices = c("Batting Average (AVG)" = "batting_avg", 
                              "Slugging Percentage (SLG)" = "slg_percent", 
                              "On-Base Percentage (OBP)" = "on_base_percent",
                              "On-Base Plus Slugging (OPS)" = "on_base_plus_slg",
                              "Barrel %" = "barrel_batted_rate", 
                              "Hard Hit %" = "hard_hit_percent",
                              "K%" = "k_percent", 
                              "BB%" = "bb_percent")),
      textOutput("stat1_description"),  # Dynamic description for stat1
      selectInput("stat2", "Select Second Stat:", 
                  choices = c("Batting Average (AVG)" = "batting_avg", 
                              "Slugging Percentage (SLG)" = "slg_percent", 
                              "On-Base Percentage (OBP)" = "on_base_percent",
                              "On-Base Plus Slugging (OPS)" = "on_base_plus_slg",
                              "Barrel %" = "barrel_batted_rate", 
                              "Hard Hit %" = "hard_hit_percent",
                              "K%" = "k_percent", 
                              "BB%" = "bb_percent")),
      textOutput("stat2_description"),  # Dynamic description for stat2
      # Slider to select year range
      sliderInput("yearRange", "Select Year Range:", 
                  min = min(stats$year), max = max(stats$year), 
                  value = c(min(stats$year), max(stats$year)))
    ),
    
    mainPanel(
      plotOutput("comparisonPlot")
    )
  )
)

server <- function(input, output) {
  
  # Function to convert percentages or decimal stats into consistent format
  format_stats <- function(data) {
    data %>%
      mutate(batting_avg = batting_avg * 100,
             on_base_percent = on_base_percent * 100,
             on_base_plus_slg = on_base_plus_slg * 100,
             slg_percent = slg_percent * 100) 
  }
  
  # Reactive description for stat1
  output$stat1_description <- renderText({
    stat_descriptions[[input$stat1]]
  })
  
  # Reactive description for stat2
  output$stat2_description <- renderText({
    stat_descriptions[[input$stat2]]
  })
  
  # Reactive subset of data based on player selection and year range
  filteredData <- reactive({
    data <- stats %>%
      filter(player_name %in% input$players, year >= input$yearRange[1], year <= input$yearRange[2]) %>%
      format_stats() %>%
      pivot_longer(cols = c(input$stat1, input$stat2), names_to = "stat_type", values_to = "value")
    
    return(data)
  })
  
  # Dynamic plot for comparing selected stats for the selected players across selected years
  output$comparisonPlot <- renderPlot({
    data <- filteredData() %>%
      filter(!is.na(value)) 
    
    ggplot(data, aes(x = year, y = value, color = interaction(player_name, stat_type), group = interaction(player_name, stat_type))) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      labs(title = paste("Comparison of", input$stat1, "and", input$stat2, "for Selected Players"),
           x = "Year", y = "Value (%)", color = "Player and Stat") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
