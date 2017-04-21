#For plotting

library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)

ui <- pageWithSidebar(
  headerPanel("Multi y-axes Plot Generator"),
  sidebarPanel(
    fileInput('file', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    tags$hr(),
    
    checkboxInput('header', 'Header', TRUE),
    fluidRow(
      selectInput("xaxis", "X-Axis", ""),
      
      checkboxGroupInput("variable", "Y-Axis:", "")
    ),
                 
    uiOutput("choose_columns"),
    hr(),
    actionButton("action", "Generate plot data")
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
    tabPanel("Instruction", htmlOutput("inst")),            
    tabPanel("Data", tableOutput('content')),
    tabPanel("Plot", plotOutput("plot")),
    tabPanel("Selected", textOutput("text"))
    )
  )
)
