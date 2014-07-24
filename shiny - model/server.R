############################################
# renders saved images 
# images saved in ./images
# images named "image1.jpg"..."image4.jpg"
############################################

shinyServer(function(input, output, session) {
 
  # Send pre-rendered images, don't delete images after sending
  
  ###########
  # INITIAL #
  ###########
  output$initial <- renderImage({

    # When input$simulation is 3, filename is ./images/3/initial.jpg
    filename <- file.path('./images',paste(input$simulation,"/initial.jpg", sep=""))
    
    # Return a list containing the filename and alt text
    list(src = filename, contentType = "image/jpg",alt = paste("image", input$n))
    
  }, deleteFile = FALSE)
  
  ##########
  # MIDDLE #
  ##########
  output$middle <- renderImage({
    
    # When input$simulation is 3, filename is ./images/3/middle.jpg
    filename <- file.path('./images',paste(input$simulation,"/middle.jpg", sep=""))
    
    # Return a list containing the filename and alt text
    list(src = filename, contentType = "image/jpg",alt = paste("image", input$n))
    
  }, deleteFile = FALSE)
  
  #########
  # FINAL #
  #########
  output$final <- renderImage({
    
    # When input$simulation is 3, filename is ./images/3/final.jpg
    filename <- file.path('./images',paste(input$simulation,"/final.jpg", sep=""))
    
    # Return a list containing the filename and alt text
    list(src = filename, contentType = "image/jpg",alt = paste("image", input$n))
    
  }, deleteFile = FALSE)
  
})