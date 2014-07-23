############################################
# renders saved images 
# images saved in ./images
# images named "image1.jpg"..."image4.jpg"
############################################

shinyServer(function(input, output, session) {
 
  # Send a pre-rendered image, and don't delete the image after sending it
  output$preImage <- renderImage({
    
    # When input$simulation is 3, filename is ./images/image3.jpeg
    filename <- file.path('./images',paste("image",input$simulation,".jpg", sep=""))
                
    # Return a list containing the filename and alt text
    list(src = filename, contentType = "image/jpg",alt = paste("image", input$n))
    
  }, deleteFile = FALSE)
})