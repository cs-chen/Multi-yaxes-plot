#For plotting
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
library(dplyr)


# Define server logic required
shinyServer(function(input, output, session) { 
  
  
  dsnames <- c()
  data_set <- reactive({
    req(input$file)
    inFile <- input$file
    data_set <- read.csv(inFile$datapath, header = input$header, stringsAsFactors = F)
    
  })
  
  output$inst <- renderUI({
    str1 <- h2("Instruction for preparing your data before uploading:")
    str2 <- h4("1. Make sure you have the right column names in your data")
    str3 <- h4("2. Make sure you have your time column (e.g. elapsed time in fermentation), rather than just 2016/11/30 5pm.")
    str4 <- h4("3. No meaningless empty row/column in the data")
    str5 <- h4("4. The file format should be .csv")
    str6 <- h5("The data file you uploaded should look like this:")
    
    HTML(paste(str1, str2, str3, str4, str5, str6, '<img src = "example.png"/ width = "800" height = "480">',sep = "<br/>"))
    
  })
  
  output$content <- renderTable({
     
    data_set() 
    
  })
  observe({
    withProgress(message = "Uploading file", min = 0, max = 100, {
      for(i in 1:100) {
        incProgress(i, detail = "Uploading parameters")
        
      }
    req(input$file)
    dsnames <- names(data_set())
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    updateSelectInput(session, "xaxis", label = "X-Axis", choices = cb_options, selected = "")
    updateCheckboxGroupInput(session, "variable", label = "Y-Axis", choices = cb_options, selected = "")
    
  })
    
  })
  
  
  
  # Check boxes
  output$choose_columns <- renderUI({
   
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set with the appropriate name
    
    colnames <- names(content)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns", "Choose columns", 
                       choices  = colnames,
                       selected = colnames)
  
  })
  
  
  dat <- reactive({ 
    
    rdata <- data_set()
    
    if (!is.null(rdata)){
    yv <- input$variable
    
    if(!is.null(yv)){
      if(sum(yv %in% names(rdata)) > 0) {
    rdata <- rdata[c(match(yv, names(rdata)))]
      }}}
    rdata
  }) 
  
  yv <- reactive({
    if (!is.null(data_set)){
      yv <- input$variable
    }
    return(yv)
    })
  
  xv <- reactive({
    rdata <- data_set()
    if (!is.null(data_set)){
      x <- input$xaxis
      xx <- rdata[c(match(x, names(rdata)))]
      xv <- pretty(c(min(na.omit(xx[1])), max(na.omit(xx[1]))))
      return(xv)
    }
    
  })
  
  xlab <- reactive({
    rdata <- data_set()
    if (!is.null(data_set)){
      x <- input$xaxis
      xx <- rdata[c(match(x, names(rdata)))]
      return(xx)
    }
  })
  
  color <- reactive({
    if (!is.null(data_set)){
      yv <- input$variable
    }
    rainbow(length(yv))
  })
  
  
  observeEvent(input$action, {
    output$plot <- renderPlot({ 
    
    isolate({
      
      # define margins
      b <- 2+3.5*(length(yv())-1)+2
      par(mar = c(5, b, 4, 4) + 0.1)
      
      #1st plot:pH
      #don't put axes ticks yet: xaxt&yaxt
      plot(x=xlab()[[1]], y=dat()[[1]], type = 'l', col = color()[[1]], lty = 4, pch = 1, cex = 1, xlab = input$xaxis, 
           ylab = '', xaxt = 'n', yaxt = 'n', lwd = 2)
      
      #label the 1st y axis
      axis(2, col = color()[[1]], col.lab = color()[[1]])
      mtext(yv()[[1]], side = 2, line = 2, col = color()[[1]])
      
      if(length(yv()) > 1) {
      for(i in 2:length(yv())){
        
      #draw another plot over the 1st plot
      par(new = T)
      
      #2nd plot, don't put y label yet
      plot(x=xlab()[[1]], y=dat()[[i]], type = 'l', col = color()[[i]], lty = 4, pch = 1, cex = 1, xaxt = 'n', yaxt = 'n', ylab = '', xlab = '')
      
      axis(side = 2, col = color()[[i]], line = 3.5*(i-1))
      mtext(side = 2, line = 2+3.5*(i-1), yv()[[i]], col = color()[[i]])
      }}
      
      #x axis
      axis(1, at = xv())
      
    
      } )})
  
    })
  observeEvent(input$action, {
    output$text <- renderText({
      isolate({
        
        xv()
        
        
      })
    })
  })
  
   
  })
 