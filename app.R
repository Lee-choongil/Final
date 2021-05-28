# Load packages ----
library(shiny)
library(shinythemes)
library(maps)
library(mapproj)
library(tidyverse)
library(lubridate)
library(gridExtra)

# Load data ----
project_data <- readRDS("data/pd.rds")
nation_data <- readRDS("data/nationmurder.rds")
cli_sui <- readRDS("data/clisui.rds")
kospi1 <- readRDS("data/kospi.rds")

# Source helper functions -----
source("helpers.R")


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Analysis of Climate with Impulse"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      selectInput("crimeSummer", h3("Select crime kind"), 
            choices = list("kill" = "kill", "violence" = "violence",
                                 "rape" = "rape","threat_fear"="threat_fear","drunk_drive"="drunk_drive"), selected = 1)
    ),
    mainPanel(
      h3("Crime with Disport Index"),
      textOutput("crimeSummer"),
      plotOutput("plotSummer")
    )
  ),
  hr(),
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
            selectInput("crimeYS", h3("Select crime kind"), 
                  choices = list("kill" = "kill", "violence" = "violence",
                                 "rape" = "rape","threat_fear"="threat_fear","drunk_drive"="drunk_drive"), selected = 1),
      numericInput("num", 
                   h3("Select Month (7~9)"),max = 9, min = 7, value = 8)
    ),
    mainPanel(
      h3("Monthly Average Crime with Disport Index"),
      textOutput("crimeMonth"),
      plotOutput("plotMonth")
    )
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(
      selectInput("crimeWinter", h3("Select crime kind"), 
                  choices = list("kill" = "kill", "violence" = "violence",
                                 "rape" = "rape","threat_fear"="threat_fear","drunk_drive"="drunk_drive"), selected = 1)
    ),
    mainPanel(
      h3("Crime in Winter"),
      textOutput("crimeWinter"),
      plotOutput("plotWinter")
    )
  ),
  hr(),
  h3("Trading Value of KOSPI"),
  textOutput("kospi"),
  plotOutput("kospibar")
  ,
  hr(),
  h3("Monthly Average Suicide"),
  textOutput("suicide"),
  plotOutput("plotsuicide")
  ,
  hr(),
  sidebarLayout(
    sidebarPanel(
      selectInput("enrollment", h3("Select High School Enrollment Degree"),
                  choices = list("high(over 60%)"="high", "middle(between 25% and 60%)"= "middle",
                                 "low(under 25%)"= "low"))
    ),
    mainPanel(
      h3("Murder Rate by Country"),
      textOutput("country"),
      plotOutput("plotMurder")
    )
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(
      sliderInput("range", h3("Sliders"),
                  min = 0, max = 100, value = c(25,60))
    ),
    mainPanel(
      h3("Murder Rate by Country"),
      textOutput("range"),
      plotOutput("MurderRange")
    )
  ),
  hr()
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$crimeSummer <- renderText({
    paste("You have selected", input$crimeSummer)
  })
  output$plotSummer <- renderPlot({
    crime_M_S(project_data,input$crimeSummer)
  })
  output$crimeMonth <- renderText({
    paste("You have selected", input$crimeYS, "(month :",input$num,")")
  })
  output$plotMonth <- renderPlot({
    crime_Y_S(project_data,input$crimeYS,input$num)
  })
  output$crimeWinter <- renderText({
    paste("You have selected", input$crimeWinter)
  })
  output$plotWinter <- renderPlot({
    crime_M_W(project_data,input$crimeWinter)
  })
  output$kospi <- renderText({
    paste("Kospi")
  })
  output$kospibar <- renderPlot({
    ggplot(kospi1, aes(x = factor(date), y= meanTV)) +
      geom_col(fill = "lightblue", color = "darkblue")+
      ggtitle("KOSPI Trade")+
      xlab("month") +
      ylab("trade")
  })
  output$suicide <- renderText({
    paste("suicide")
  })
  output$plotsuicide <- renderPlot({
    ggplot(data = cli_sui,aes(x = meanT, y = meanS)) +
      geom_point()+
      geom_text(aes(label = date), vjust = -0.5, size = 3)
  })
  output$country <- renderText({
    paste("You have selected", input$enrollment)
  })
  output$plotMurder <- renderPlot({
    murderplot(nation_data,input$enrollment,"cornflowerblue")
  })
  output$range <- renderText({
    paste("From", input$range[1],"% to", input$range[2],"%")
  })
  output$MurderRange <- renderPlot({
    murderenrollplot(nation_data,input$range[1],input$range[2])
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)