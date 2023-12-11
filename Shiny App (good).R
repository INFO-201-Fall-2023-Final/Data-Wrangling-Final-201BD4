#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("Baseball's Greatest Hitting Season",
                 tabPanel("Home",
                          # Application title
                          titlePanel("A Statistical Analysis of the Greatest Baseball Hitting Season Ever"),
                          # Header
                          h2("What are we trying to prove?"),
                          
                          # Paragraph
                          p("The purpose of this project was to determine what the greatest baseball hitting season ever was, and who it was by.
      While many are familiar with the home run, baseball's iconic hit over the wall, the reality of the sport is
      that there are many ways to be a good hitter. Some hitters specialize in getting on base as much as possible,
      which gives their team as many chances as possible to drive them in, some make their careers of hitting
      as many home runs as possible, and some sit in the middle. "),
                          
                          # Image
                          img("description", src = "path"),
                          
                          # Header
                          h2("What is the statistical approach for accomplishing this?"),
                          
                          # Paragraph
                          p("By taking notable baseball seasons from throughout baseball history we are able to sort through statistics as they matter to us.
      "),
                 ),
                 tabPanel("With WAR"),
                 tabPanel("Oldheads")
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
