#
# This is a Shiny web application. 
#
#This app lets user upload an RGB picture and select a threshold to mask pixels.
#It returns a count of masked and unmasked pixels.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##This should detect and install missing packages before loading them - hopefully!

# list.of.packages <- c("shiny","ggplot2","shinyFiles","rgdal","raster","DT")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# lapply(list.of.packages,function(x){library(x,character.only=TRUE)})

library(shiny)
library(ggplot2)
library(shinyFiles)
library(rgdal)
library(raster)
library(DT)

#source("helpers.R")


# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Count pixels above threshold"),

    # Sidebar with data upload widget and date range slider 
    sidebarLayout(
        #data uplead widget
        sidebarPanel(
        #select file
            fileInput("image1", "Choose an RGB image file",
                      multiple = FALSE,
                      accept = c("image/jpg",
                                 "image/tif","image/png")),

         #anylise image   
           "Enter threshold value for mask. For RGB range is 0 to 255. 160 is the default threshold.",
            
            numericInput("threshold", "Threshold:", value = 160, min = 0, max = 255, step = 1),
            
            checkboxInput("invert", "invert mask", value=FALSE)),
            #actionButton("RunAnl","Count pixels above threshold")),
   
        mainPanel(
           strong("Image"),
          
           plotOutput("distPlot1"),
           
           strong("Mask"),
           #"This is the mask. Pixels above the threshold are black",

           plotOutput("distPlot2"),

           # Show table of results
           DT::dataTableOutput("contents")
           )

))

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
      #print(inFile$name)
      if (is.null(inFile))
        return()
      path<-file.path(paste0(getwd(),'/images/'))
      dir.create(path)
      file.copy(inFile$datapath, paste0(path,"/",inFile$name))
      #read file into reactive value
      values$image<-paste0(path,"/",inFile$name)
      values$path<-path
      values$fname<-inFile$name
      values$myraster<-brick(paste0(path,"/",inFile$name))
    })

  
#graph image 
  output$distPlot1 <- renderPlot({
    
    req(length(values$myraster)>0)
    
    plotRGB(values$myraster)

  })

  observe({
      req(length(values$myraster)>0)
      values$skypix<-sum(getValues(values$myraster[[3]]>input$threshold))
      #print(values$skypix)
      values$totalpix<-ncell(values$myraster[[3]])
      #print(values$totalpix)
      values$pctsky<-(values$skypix/values$totalpix)*100
      #print(values$pctsky)

      })
  
  observeEvent(values$pctsky,{
               values$df[nrow(values$df)+1,]<-cbind(values$fname,input$threshold,values$skypix,values$totalpix,values$pctsky)
               })

output$distPlot2 <- renderPlot({
  
  req(length(values$myraster)>0)
  if (input$invert==TRUE){
    
    plot(values$myraster[[3]],breaks=c(0,input$threshold,255),col=c("white","black"))
    
  }else if (input$invert==FALSE){
    
    plot(values$myraster[[3]],breaks=c(0,input$threshold,255),col=c("black","white"))
  }
})


  output$contents <-DT::renderDataTable({

    req(length(values$myraster)>0)

    values$df

  })

}

# Run the application 
shinyApp(ui = ui, server = server)




