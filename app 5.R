#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

df = read.csv("df combined.csv")

# Define UI for application that draws a histogram
ui <- navbarPage("Baseball's Greatest Hitting Season",
                 tabPanel("Home",
                          # Application title
                          titlePanel("A Statistical Analysis of the Greatest Baseball Hitting Season Ever"),
                          
                          p("By: Oscar Witt, Nathan Uran, and Koji Asuncion"),
                          
                          # Header
                          h2("What are we trying to prove?"),
                          
                          # Paragraph
                          p("The purpose of this project was to determine what the greatest baseball hitting season ever was, and who it was by.
      While many are familiar with the home run, baseball's iconic hit over the wall, the reality of the sport is
      that there are many ways to be a good hitter. Some hitters specialize in getting on base as much as possible,
      which gives their team as many chances as possible to drive them in, some make their careers of hitting
      as many home runs as possible, and some sit in the middle. We decided to accept both angles of what matters to hitting, in both an advanced
                            and more basic sense."),
                          
                          # Image
                          img("", src = "https://static01.nyt.com/images/2021/12/31/multimedia/30Novitzky/30Novitzky-articleLarge-v2.jpg?quality=75&auto=webp&disable=upscale"),
                          
                          p("Barry Bonds: one of the greatest hitters ever, but tainted by potential steroid use in the baseball community."),
                          
                          # Header
                          h2("What is the statistical approach for accomplishing this?"),
                          
                          # Paragraph
                          p("By taking notable baseball seasons from throughout baseball history we are able to sort through statistics as they matter to us.
      In this instance, we decided that we wanted to take two statistical approaches to appease both sections of baseball fans. On one end, we have the
                            trendier baseball fan, who will often prefer to use the advanced sabermetrics to form their opinions. For this, we have used
                            WAAO (Offensive Wins Above Average) and OPS+ (On Base Plus Slugging normalized based on league average per year). On the other side
                            of the aisle are the baseball fans who prefer the stats that have been recorded since the game began, home runs and batting average.
                            We respect that each group will want to use their own stats to create their own opinions, and thus have done analysis for each, which
                            can be found in the respective tabs. For more information on calculation on how OPS+ is calculated, the first link below explains
                            in detail (please note that the lack of widespread acceptance is talked about has changed, as the article is from 2010). For more
                            information on why we used WAAO (referenced as WAA in the article), especially instead of WAR, reference the second link."),
                          
                          # URL
                          a("Explanation on OPS+ Calculation", href = "https://library.fangraphs.com/offense/ops/"),
                          p(""),
                          
                          a("Explanation on why we used WAAO", href = "https://sabr.org/journal/article/waa-vs-war-which-is-the-better-measure-for-overall-performance-in-mlb-wins-above-average-or-wins-above-replacement/"),
                          
                 ),
                 tabPanel("Frequency of High Caliber Seasons",
                          # Application title
                          titlePanel("Frequency of High Caliber Seasons"),
                          
                          p("This page is an introduction to our dataset, which we call the High Caliber Seasons data set. On this page
                            viewers can see six examples of famous hitters that appear most often in our dataset, which has 93 different seasons.
                            While this page doesn't prove necessarily which season is the best, it does show the consistency of certain players
                            in the dataset, which is impressive within itself."),
                          
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(
                                inputId = "hit",
                                label = "How many times did these famous hitters appear in the dataset?",
                                choices = list("Barry Bonds" = 1, "Babe Ruth" = 2, "Ty Cobb"=3, "Albert Pujols"=4, "Dan Musial"=5, "Rodgers Hornsby"=6),
                              )
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              textOutput(outputId="hitScore"),
                              tags$head(tags$style("#hitScore{font-size:25px}")),
                            ),
                            
                          ),
                 ),
                 tabPanel("Advanced Sabermetrics",
                          titlePanel("Advanced Sabermetrics"),
                          p("On this page we have the scatterplot for our sabermetrics. The further to the top right of the graph, the better
                            the season according to our research, as the player will have a better WAAO and a better OPS+. According to this graph,
                          we would likely conclude that Barry Bonds had the best ever season in 2001. However, if we wanted to factor in Barry Bonds
                            potential steroid use, that would instead fall to Babe Ruth in either 1920 or 1921, depending on preference. It is
                            interesting to note that these seasons are extremely highly regarded in baseball history already, not necessarily
                            proving our point, but suggesting that our data is more likely sound."),
                          selectInput("player", "Select Player", choices = c("All", unique(df$PLAYER))),  # Dropdown for player selection
                          plotlyOutput("scatterplot"), 
                 ),
                 tabPanel("Basic Stats",
                 selectInput("player", "Select Player", choices = c("All", unique(df$PLAYER))), 
                 plotlyOutput("scatterplot2"),),
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$hitScore <- renderText({
    if(input$hit == 1){
      return("7 times")
    } else if(input$hit ==2){
      return("10 times")
    }else if(input$hit ==3){
      return("6 times")
    }else if(input$hit==4){
      return("3 times")
    }else if(input$hit==5){
      return("3 times")
    }else{
      return("7 times")
    }
  })
  output$scatterplot <- renderPlotly({
    filtered_df <- df  # Initialize with the full dataset
    
    # Check if "All" or a specific player is selected
    if (input$player != "All") {
      filtered_df <- df[df$PLAYER == input$player, ]  # Filter dataframe based on selected player
    }
    
    # Create scatter plot using plotly
    p <- plot_ly(data = filtered_df, x = ~WAAO, y = ~OPS._2, text = ~PLAYER, color = ~factor(YEAR),
                 type = 'scatter', mode = 'markers')
    
    p <- add_text(p, text = ~PLAYER, showlegend = FALSE, textposition = "top right")
    
    p <- layout(p, title = "WAAO vs OPS._2", xaxis = list(title = "WAAO"), yaxis = list(title = "OPS._2"),
                showlegend = TRUE)
    
    p  # Return the plotly plot
  })
  output$scatterplot2 <- renderPlotly({
    filtered_df <- df  # Initialize with the full dataset
    
    # Check if "All" or a specific player is selected
    if (input$player != "All") {
      filtered_df <- df[df$PLAYER == input$player, ]  # Filter dataframe based on selected player
    }
    
    # Create scatter plot using plotly
    p <- plot_ly(data = filtered_df, x = ~HR, y = ~BA, text = ~PLAYER, color = ~factor(YEAR),
                 type = 'scatter', mode = 'markers')
    
    p <- add_text(p, text = ~PLAYER, showlegend = FALSE, textposition = "top right")
    
    p <- layout(p, title = "Home Runs vs Batting Average", xaxis = list(title = "Home Runs"), yaxis = list(title = "Batting Average"),
                showlegend = TRUE)
    
    p  # Return the plotly plot
  })
}

shinyApp(ui = ui, server = server)
