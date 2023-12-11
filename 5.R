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
ui <- fluidPage(
  plotOutput("scatterplot"),
  # Application title
  titlePanel("Frequency of High Caliber Seasons"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "hit",
        label = "how many times did these famous hitters appear in the dataset?",
        choices = list("Barry Bonds" = 1, "Babe Ruth" = 2, "Ty Cobb"=3, "Alber Pujols"=4, "Dan Musial"=5, "Rodgers Hornsby"=6),
      ),
    ),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "decades",
          label = "How many seasons in our dataset occurred per decade?",
          choices = list("1890" = 1, "1900" = 2, "1910"=3, "1920"=4, "1930"=5, "1940"=6, "1950"=7, "1960"=8, "1970"=9, "1980"=10, "1990"=11, "2000"=12, "2010"=13),
        ),
      ),
    ),
      
    # Show a plot of the generated distribution
    mainPanel(
      textOutput(outputId="hitScore"),
      tags$head(tags$style("#hitScore{font-size:10px}",
      ))
    )
    mainPanel(
      textOutput(outputId="decadesScore"),
      tags$head(tags$style("#decadesScore{font-size:10px}",
      ))
  )
    )
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
  output$decadesScore <- renderText({
    if(input$decades == 1){
      return("1 time")
    } else if(input$decades ==2){
      return("5 times")
    }else if(input$decades ==3){
      return("12 times")
    }else if(input$decades==4){
      return("19 times")
    }else if(input$decades==5){
      return("11 times")
    }else if(input$decades==6){
      return("9 times")
    }else if(input$decades ==7){
      return("7 times")
    }else if(input$decades ==8){
      return("5 times")
    }else if(input$decades==9){
      return("3 times")
    }else if(input$decades==10){
      return("1 time")
    }else if(input$decades==11){
      return("6 times")
    }else if(input$decades ==12){
        return("9 times")
    }else if(input$decades==13){
        return("4 times")
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

shinyApp(ui = ui, server = server)
