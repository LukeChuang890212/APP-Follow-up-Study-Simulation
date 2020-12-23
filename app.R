#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

setwd("C:/Users/莊明儒/Desktop/Epidemiology/Follow-up.Simulation")
source("Simulation.r")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simulation for Estimation in Follow-up Study"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("p.mr",
                        "Population Parameter:",
                        min = 0.0,
                        max = 0.1,
                        value = 0.03),
            sliderInput("ltfur",
                        "Proportion of Loss of Follow-up:",
                        min = 0.0,
                        max = 1.0,
                        value = 0.0),
            sliderInput("per.m.in.ltfus",
                        "Level of Association:",
                        min = 0.0,
                        max = 0.1,
                        value = 0.00)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        sim(input$p.mr,input$ltfur,input$per.m.in.ltfus)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
