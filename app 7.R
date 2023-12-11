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
  selectInput("player", "Select Player", choices = c("All", unique(df$PLAYER))),  # Dropdown for player selection
  plotlyOutput("scatterplot"), 
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
}

shinyApp(ui = ui, server = server)
