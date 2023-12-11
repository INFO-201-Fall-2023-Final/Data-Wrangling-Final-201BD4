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

# Define UI for application that draws a histogram
ui <- fluidPage(
  plotOutput("barplot"),
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
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput(outputId="hitScore"),
      tags$head(tags$style("#hitScore{font-size:100px}",
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
  output$barplot <- renderPlot({
    ggplot(df, aes(x = PLAYER)) +
      geom_bar(aes(y = WAAO), stat = "identity", fill = "blue", alpha = 0.5) +
      geom_bar(aes(y = OPS._2), stat = "identity", fill = "red", alpha = 0.5) +
      labs(title = "WAAO and OPS._2 Comparison", x = "Teams", y = "Values") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)