
################################################################################
## Initialize Environment ######################################################
################################################################################


library(shiny)



ui <- fluidPage(
  
  
  fluidPage(
    sliderInput("bins",
                "Stunde:",
                min = 1,
                max = 24,
                value = 1,
                animate = animationOptions(loop = T,interval = 500),
                step = 1),
    plotOutput("distPlot",width = "50%")
    
  )
  
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderImage({
    filename <- normalizePath(gg_p[[input$bins]])
    
    list(src = filename,
         alt = "This is alternate text")
    
  },deleteFile = F)
}

# Run the application 
shinyApp(ui = ui, server = server)

