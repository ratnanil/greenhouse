

if (interactive()) {
  options(device.ask.default = FALSE)
  
  fils <- list.files("Plots/Try3/",pattern = ".png")
  
  
  ui <- fluidPage(
    # sliderInput("n", "Number of observations", 2, 1000, 500),
    shinyWidgets::sliderTextInput("n", "sdf", fils),
    plotOutput("plot3")
  )
  
  server <- function(input, output, session) {
    


    # Send a pre-rendered image, and don't delete the image after sending it
    # NOTE: For this example to work, it would require files in a subdirectory
    # named images/
    output$plot3 <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path('./Plots/Try3/',
                                          input$n))
      
      # Return a list containing the filename
      list(src = filename)
    }, deleteFile = FALSE)
  }
  
  shinyApp(ui, server)
}