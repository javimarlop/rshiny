library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Dynamic Systems"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
    radioButtons("dynasys", "Dynamic System:",
                 list(
                      "Lorenz - 3D" = "lorenz3d",
                      "Lorenz - 2D" = "lorenz2d",
                      "Rossler - 3D" = "rossler3d",
                      "Rossler - 2D" = "rossler2d",
                      "Pleiade" = "pleiade",
                      "Pleiade All" = "pleiadeall",
                      "Pleiade 4" = "pleiade4",
                      "Pleiade 4 All" = "pleiade4all",
                      "Mandelbrot" = "mandel",
                      "Julia" = "julia",
                      "Catalytic - 3D" = "catalitic3d",
                      "Catalytic - 2D" = "catalitic2d"
                      )
              ),
    
    br(),
    
    submitButton(text="Run")
  ), #sidebar...  
  
    
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
                   plotOutput("plot", ,height=600,width=800)
              
                        ,h4("Description")
                        ,verbatimTextOutput("descrip")  
               
                        ,br()
                        ,h4("Credits:")
                        ,h6("Solving Differential Equations in R (Springer 2013)")
                        ,h6("http://users.utu.fi/attenka/")
                        ,h6("Complex dynamics of a catalytic network having faulty replication into error species - Physica D63,21-40 (1993)")
              #,imageOutput("myImage")
           ) #mainPanel
  
))  #shinyUI