############################################
# renders saved images 
# images saved in ./images
# images named "image1.jpg"..."image4.jpg"
############################################
library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("renderImage example"),
  
  sidebarPanel(
    selectInput("simulation", 
                "Simulation:",
                choices=list("A"=1,"B"=2)
    )
  ),
  
  mainPanel(
    # Use imageOutput to place the image on the page
    imageOutput("initial",height="100%"),
    hr(),
    imageOutput("middle",height="100%"),
    hr(),
    imageOutput("final",height="100%")
    #imageOutput("preImage"),
    #imageOutput("preImage"),
    #imageOutput("preImage")
  )
)
)
  
