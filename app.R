#Packaged that are needed to run the projects/web application
library(shiny)
library(ggplot2)
library(plotly)
library(DataExplorer)
library(dplyr)
#setwd("~/Desktop/Jay/Harrisburg University/Semester 5/Software Validation and Testing/StrokeAnalysis/All Code and Report/Code/")
data <- read.csv("Stroke-Dataset.csv")
cols <- c(2,5,7, 8,9,12,14)
data[cols] <- lapply(data[cols], as.factor)
data <- data[-c(1)] ## as.factor() could also be used


#The code for the application
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # App title ----
    headerPanel("Stroke Prediction"),
    
    mainPanel("Dashboard",
              fluidRow(
                  splitLayout(cellWidths = c("70%", "50%"),plotlyOutput("plot"), plotOutput("plotgraph2"))
              ),
              
              
              selectInput("y_varb", label="Y-axis variable",choices=names(data)[c(13)]),
              
              #2 Select any variable in dataset as x-variable
              selectInput("x_varb", label="X-axis variable", choices=names(data)),
              # Main panel for displaying outputs ----
              
              #3. Reset plot1 output after each selection
              #plotOutput("plot", dblclick = "plot_reset"),
              #plotOutput("plotgraph2")
    )  
)
# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
    
    remaining <- reactive({
        names(data)[-match(input$y_varb,names(data))]
    })
    
    observeEvent(remaining(),{
        choices <- remaining()
        updateSelectInput(session = getDefaultReactiveDomain(),inputId = "x_varb", choices = choices)
    })
    
    output$plot <- renderPlotly({
        
        ggplotly(ggplot(data=data,aes_string(x = input$x_varb, fill = input$y_varb)) +
                     geom_density(alpha = 0.5) +
                     labs(title = "Density Plot",
                          subtitle = "by Stroke Status",
                          y = input$y_varb, 
                          x = input$x_varb,
                          fill = "Stroke Status\n0=No 1=Yes") +
                     theme(legend.position = "bottom"))
        
    })
    
    output$plotgraph2 <- renderPlot({
        data%>%plot_correlation()
    })
}


shinyApp(ui, server)