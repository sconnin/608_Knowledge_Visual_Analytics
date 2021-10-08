library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)
library(shinythemes)

#Construct data sets

data <- read.csv("cdc_hw3.csv")

# Construct Shiny App

ui <- fluidPage(theme = shinytheme("cerulean"),
    
    #shinythemes::themeSelector(),
    
        titlePanel(h2('Data 608 Homework 3')),
   
    
        fluidRow(
            column(5,
            "Author: Sean Connin", offset= 0.5)
        ),
        
        fluidRow(
            column(5, 
            "Date: 10/6/21", offset = 0.5)
        ),

    
        fluidRow(
            column(6,
                   wellPanel(
                   selectInput(inputId = "death", 
                   label = "Select Cause", 
                   unique(data$Cause), 
                   selected ='Cancer'),
                   plotlyOutput("plot1", height=800)
                   )
                   
            ),
            column(6,
                   wellPanel(
                   selectInput(inputId = "state", 
                   label = 'Select State', 
                   unique(data$State), selected ='AK'),
                   selectInput(inputId = "killer", 
                   label='Select Cause', 
                   unique(data$Cause), selected ='Cancer'),
                   plotlyOutput("plot2")
                   ),
                   
                   p("Note: State crude mortality rates are compared relative to the national average by division with the latter.")
            )

        )
)


server <- function(input, output) {
    
    output$plot1 <-renderPlotly ({
        
        df<-data%>%
                filter(Year == "2010")%>%
                filter(Cause == input$death)
        
        
         plot_ly(data=df, 
                 x = ~Crude.Rate/10, 
                 y = ~State,
                 type = "scatter")%>%
        layout(
            title ="Crude Mortality Rates in 2010 by State",
            xaxis=list(dtick=2,
            orientation = 'h',
            title='Rate Per 1000'),
            yaxis=list(title = "State",
            categoryorder='total ascending')
        ) 
            
    })
    
    output$plot2 <-renderPlotly ({
        
        df2<-data%>%
            filter(State == input$state, Cause == input$killer)
        
        plot_ly(data=df2, 
                x = ~Year, 
                y = ~Comp_Rate, 
                type = "scatter", mode = "lines", color = "red")%>%
            layout(
                title ="Mortality Rates Relative to National Average",
                xaxis=list(dtick=2,
                          title = "Year",
                           orientation = 'h'),
                yaxis=list(title = "Multiple Of National Average")
            ) 
        
    })
    
}
    

shinyApp(ui, server)