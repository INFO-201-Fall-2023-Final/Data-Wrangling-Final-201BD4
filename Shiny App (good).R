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
      as many home runs as possible, and some sit in the middle. "),
                          
                          # Image
                          img("", src = "https://static01.nyt.com/images/2021/12/31/multimedia/30Novitzky/30Novitzky-articleLarge-v2.jpg?quality=75&auto=webp&disable=upscale"),
                          
                          p("Barry Bonds: one of the greatest hitters ever, but tainted by potential steroid use in the baseball community."),
                          
                          p("For example, Barry Bonds (above) is the all time leader in home runs, and considered by many as the greatest hitter ever."),
                          
                          # Header
                          h2("What is the statistical approach for accomplishing this?"),
                          
                          # Paragraph
                          p("By taking notable baseball seasons from throughout baseball history we are able to sort through statistics as they matter to us.
      In this instance, we decided that we wanted to take two statistical approaches to appease both sections of baseball fans. On one end, we have the
                            trendier baseball fan, who will often prefer to use the advanced sabermetrics to form their opinions. For this, we have used
                            WAAO (Offensive Wins Above Average) and OPS+ (On Base Plus Slugging normalized based on league average per year). On the other side
                            of the aisle are the baseball fans who prefer the stats that have been recorded since the game began, home runs and batting average.
                            We respect that each group will want to use their own stats to create their own opinions, and thus have done analysis for each."),
                          
                          # URL
                          a("Link to the Hitting Vault Article", href = "https://thehittingvault.com/3-basic-mlb-hitting-stats-that-define-a-great-hitter/"),
                          
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
                              textOutput(outputId="hitScore")
                          ),
                  ),
                 ),
                 tabPanel("Advanced Sabermetrics",
                          plotOutput("scatterplot"),
                          ),
                 tabPanel("No War"),
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
  output$scatterplot <- renderPlot({
    ggplot(df, aes(x = WAAO, y = OPS._2, label = player_szn)) +
      geom_point(size = 3, color = "blue") +
      geom_text(size = 3, vjust = -0.5) +  # Adjust label position
      labs(title = "WAAO vs OPS+", x = "WAAO", y = "OPS+") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
