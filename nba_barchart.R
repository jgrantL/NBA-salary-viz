library(shiny)
library(ggplot2)
library(dplyr)


nba <- read.csv('nba.csv')

# Define UI for application that draws a barchart
ui <- fluidPage(
  
  # Application title
  titlePanel("Ranking NBA Salaries"),
  
  # Sidebar with drop down menu and plot customizing buttons
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "season", 
                  label = "season", 
                  choices = unique(nba['season']), 
                  selected = '2010'),
      radioButtons(inputId = "order", 
                   label = 'Select Ordering',
                   choices = list('Increasing' = 1,
                                  'Decreasing' = 2),
                   selected = 2),
      sliderInput(inputId='baseline',
                  label='Change Baseline',
                  min=0,
                  max=50,
                  value=50),
      checkboxInput(inputId = "avg", 
                   label = 'Show Average',
                   value=FALSE),
      checkboxInput(inputId = "median", 
                   label = 'Show Median',
                   value=FALSE),
      checkboxInput(inputId = "quarters", 
                   label = 'Show Quarters',
                   value=FALSE)
      ),
    
    # Show plot
    mainPanel(
      plotOutput("barchart")
    )
  )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
  
  output$barchart <- renderPlot({
    
    this_season = nba[nba['season'] == input$season, ]
    #need stats for adjusted salaries
    stats = summary(nba[nba['season'] == input$season, ][['adjusted_salaries']]) / 1000000
    mean_salary <- stats[4]
    median_salary <- stats[3]
    first_quartile <- stats[2]
    third_quartile <- stats[5]
    
    if (input$order == 2) {
        #build barchart based on season input in decreasing order
        p <- ggplot(data = this_season, 
                    aes(x=team, y=sort(adjusted_salaries / 1000000, 
                                       decreasing=FALSE))) + geom_bar(stat='identity') +      
          coord_flip(ylim = c(input$baseline, max(this_season[['adjusted_salaries']] / 1000000))) + ylab('Team Salaries (millions)') + 
          labs(title='NBA Teams Ranked by Salary', subtitle='Salaries for 2010 season') + 
          theme_bw() + 
          theme(plot.title = element_text(hjust = 0.5, size=13, face='bold'), 
                             plot.subtitle=element_text(size=8, hjust=0.5, face='italic'), 
                             panel.border=element_blank(),  axis.text=element_text(size=8),
                             axis.title=element_text(size=9)) 
        
        if (input$avg) {
          p <- p + geom_hline(yintercept = mean_salary, color='blue', size=1) 
        }
        
        if (input$median) {
          p <- p + geom_hline(yintercept= median_salary, color='red', size=1)
        }
        
        if (input$quarters) {
          p <- p + geom_hline(yintercept = first_quartile, linetype='dashed', size=1) + 
             geom_hline(yintercept = third_quartile, linetype='dashed', size=1)
        }
        p
        
    }
    else {
        #build barchart based on season input in increasing order
        p <- ggplot(data = nba[nba['season'] == input$season, ], 
                    aes(x=team, y=sort(adjusted_salaries / 1000000, 
                                       decreasing=TRUE))) + geom_bar(stat='identity') +      
          coord_flip(ylim = c(input$baseline, max(this_season[['adjusted_salaries']] / 1000000))) + ylab('Team Salaries (millions)') + 
          labs(title='NBA Teams Ranked by Salary', subtitle='Salaries for 2010 season') + 
          theme_bw() + 
          theme(plot.title = element_text(hjust = 0.5, size=13, face='bold'), 
                plot.subtitle=element_text(size=8, hjust=0.5, face='italic'), 
                panel.border=element_blank(),  axis.text=element_text(size=8),
                axis.title=element_text(size=9)) 
        if (input$avg == 1) {
          p <- p + geom_hline(yintercept = mean_salary, color='blue') 
        }
        
        if (input$median == 1) {
          p <- p + geom_hline(yintercept= median_salary, color='red')
        }
        
        if (input$quarters == 1) {
          p <- p + geom_hline(yintercept = first_quartile, linetype='dashed') + 
            geom_hline(yintercept = third_quartile, linetype='dashed')
        }
        p
        
      }
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
