############################################
# renders saved images 
# images saved in ./images
# images named "image1.jpg"..."image4.jpg"
############################################

shinyUI(pageWithSidebar(
  
  headerPanel("renderImage example"),
  
  sidebarPanel(
    selectInput("simulation", 
                "Simulation:",
                choices=list("A"=1,"B"=2,"C"=3,"D"=4)
    )
  ),
  
  mainPanel(
    # Use imageOutput to place the image on the page
    imageOutput("preImage")
  )
)
)
  
