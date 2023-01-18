#
# This is a Shiny web application. 
#
#This app lets user upload an RGB picture and select a threshold to mask pixels.
#It returns a count of masked and unmasked pixels.
#
#This was for a project to estimate shade. 
#Take a picture of the sky under obstruction (trees)
#this app will threshold the brightest pixels so you can estimate
#relative amount of sky pixels vs obstructions.
#

library(shiny)
library(raster)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Count pixels above threshold"),
    p("If this app is hosted at shinyapps.io the image size is limited to 1MB"),
    # Sidebar with data upload widget and date range slider 
    sidebarLayout(
        #data upload widget
        sidebarPanel(
        #select file
            fileInput("image1", "Choose an RGB image file",
                      multiple = FALSE,
                      ),

        #ask for thrshold    
           p("Enter threshold value for mask. For an RGB image the range is 0 to 255. 160 is the default threshold."),
            
            numericInput("threshold", "Threshold:", value = 160, min = 0, max = 255, step = 1),
            
            checkboxInput("invert", "invert mask", value=FALSE)),
            #actionButton("RunAnl","Count pixels above threshold")),
   
        mainPanel(
           strong("Image"),
           plotOutput("distPlot1"),
           
           strong("Mask"),
           p("This is the mask. Pixels above the threshold are black"),
           plotOutput("distPlot2"),
           # Show table of results
           DT::dataTableOutput("contents")
           )
        )
    )

# Define server logic
server <- function(input, output, session) {
options(shiny.maxRequestSize=100*1024^2) 
#initialize table for data  

  values <- reactiveValues()
  
  values$df <- data.frame("name" = character(),
                   "threshold" = numeric(), 
                   "pix above" = numeric(),
                   "total pix" = numeric(),
                   "pct above"= numeric(),
                   stringsAsFactors = FALSE)

#load image and make raster
  observeEvent(input$image1, {
    inFile <- input$image1
    print(inFile$name)
    if (is.null(inFile))
      return()
    values$fname<-inFile$name
    values$myraster <- brick(inFile$datapath)
  })

#graph image 
  output$distPlot1 <- renderPlot({
    req(length(values$myraster)>0)
    plotRGB(values$myraster)
  })

#count pixels
  observe({
      req(length(values$myraster)>0)
      values$skypix<-sum(getValues(values$myraster[[1]]>input$threshold))
      #print(values$skypix)
      values$totalpix<-ncell(values$myraster[[1]])
      #print(values$totalpix)
      values$pctsky<-round((values$skypix/values$totalpix)*100,1)
      #print(values$pctsky)
      })
  
  observeEvent(values$pctsky,{
               values$df[nrow(values$df)+1,]<-cbind(values$fname,input$threshold,values$skypix,values$totalpix,values$pctsky)
               })
#plot mask
  output$distPlot2 <- renderPlot({
    #added an option to invert mask
    req(length(values$myraster)>0)
    if (input$invert==TRUE){
      plot(values$myraster[[3]],breaks=c(0,input$threshold,255),col=c("white","black"))
    }else if (input$invert==FALSE){
      plot(values$myraster[[3]],breaks=c(0,input$threshold,255),col=c("black","white"))
    }
  })

#output data table
  output$contents <-DT::renderDataTable({
    req(length(values$myraster)>0)
    values$df
  })

}

# Run the application 
shinyApp(ui = ui, server = server)




