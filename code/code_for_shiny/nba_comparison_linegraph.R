library(shiny)
library(ggplot2)
library(dplyr)


nba <- read.csv('nba.csv')

# Define UI for application that draws a barchart
ui <- fluidPage(
  
  # Application title
  titlePanel("Comparing NBA Salaries"),
  
  # Sidebar with drop down menu and plot customizing buttons
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "team", 
                  label = "Team", 
                  choices = unique(nba['team']), 
                  selected = 'ATL'),
      checkboxInput(inputId = "min", 
                    label = "Show Minimum", 
                    value = TRUE),
      checkboxInput(inputId = "max", 
                    label = "Show Maximum", 
                    value = TRUE),
    ),
    
    # Show plot
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
  
  output$linePlot <- renderPlot({
    
    team = nba[nba['team'] == input$team, ]
    
    
    min_by_season = nba %>% group_by(season) %>% summarise('min'=min(adjusted_salaries))
    max_by_season = nba %>% group_by(season) %>% summarise('max'=max(adjusted_salaries))
    
    p <- ggplot() + geom_line(data=team, aes(x=season, y=adjusted_salaries / 1000000), size=1.2) + 
      theme_bw() + 
      ylab('Total Salary (millions)') +
      scale_x_continuous(breaks=2010:2019) 
    
    if (input$min) {
      p <- p + geom_line(data=min_by_season, aes(x=season, y=min / 1000000), color='red', size=1.2) 
    }
    
    if (input$max) {
      p <- p + geom_line(data=max_by_season, aes(x=season, y=max / 1000000), color='blue', size=1.2)
    }
    
    
    if (input$team == 'ATL') {
      team_name = 'Atlanta'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.25
      team_y = 91
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'BOS') {
      team_name = 'Boston'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 98
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'BRK') {
        team_name = 'Brooklyn'
        max_x = 2017.75
        max_y = 125
        team_x = 2018.25
        team_y = 95
        min_x = 2018.5
        min_y = 72
    }
    else if (input$team == 'CHI') {
      team_name = 'Chicago'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.25
      team_y = 91
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'CHO') {
      team_name = 'Charlotte'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 98
      min_x = 2018.5
      min_y = 72
    } 
    else if (input$team == 'CLE') {
      team_name = 'Cleveland'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 105
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'DAL') {
      team_name = 'Dallas'
      max_x = 2017.75
      max_y = 125
      team_x = 2018
      team_y = 85
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'DEN') {
      team_name = 'Denver'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.25
      team_y = 91
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'DET') {
      team_name = 'Detroit'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 100
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'GSW') {
      team_name = 'Golden State'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 115
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'HOU') {
      team_name = 'Houston'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 100
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'IND') {
      team_name = 'Indiana'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 95
      min_x = 2018.5
      min_y = 72

    }
    else if (input$team == 'LAC') {
      team_name = 'L.A Clippers'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 97
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'LAL') {
      team_name = 'L.A Lakers'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 95
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'MEM') {
      team_name = 'Memphis'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.25
      team_y = 91
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'MIA') {
      team_name = 'Miami'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 115
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'MIL') {
      team_name = 'Milwaukee'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 97
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'MIN') {
      team_name = 'Minnesota'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 90
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'NOP') {
      team_name = 'New Orleans'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 95
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'NYK') {
      team_name = 'New York'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 91
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'OKC') {
      team_name = 'Oklahoma'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 115
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'ORL') {
      team_name = 'Orlando'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 96
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'PHI') {
      team_name = 'Philadelphia'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 100
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'PHO') {
      team_name = 'Phoenix'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.25
      team_y = 91
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'POR') {
      team_name = 'Portland'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.25
      team_y = 91
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'SAC') {
      team_name = 'Sacramento'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.25
      team_y = 91
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'SAS') {
      team_name = 'San Antonio'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 95
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'TOR') {
      team_name = 'Toronto'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 100
      min_x = 2018.5
      min_y = 72
    }
    else if (input$team == 'UTA') {
      team_name = 'Utah'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 91
      min_x = 2018.5
      min_y = 72
    }
    else {
      team_name = 'Washington'
      max_x = 2017.75
      max_y = 125
      team_x = 2018.5
      team_y = 100
      min_x = 2018.5
      min_y = 72
    }
    
    #add last couple of details to plot
    p + geom_text(aes(x = team_x, y = team_y, label = team_name), size=4) +
      geom_text(aes(x = min_x, y = min_y, label = "Season Min Salary"), size=4) + 
      geom_text(aes(x = max_x, y = max_y, label = "Season Max Salary"), size=4) + 
      theme_bw() + 
      ylab('Total Salary (millions)') + 
      labs(title=paste0('Comparing ', team_name, " Team Salary"), 
           subtitle="Comparisons are done for seasons 2010-1019 with teams having the lowest/highest salaries for that season") + 
        theme(plot.title = element_text(hjust = 0.5, size=13, face='bold'), 
              plot.subtitle=element_text(size=8, hjust=0.5, face='italic'), 
              panel.border=element_blank(),  
              axis.text=element_text(size=8), 
              axis.title=element_text(size=9))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
