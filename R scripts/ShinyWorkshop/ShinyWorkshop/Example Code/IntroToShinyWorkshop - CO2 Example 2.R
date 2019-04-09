#############################################
# Title:    "Intro to Shiny" Workshop       #
# Subtitle: 2017 UNC Biology Symposium      #
# Author:   Chris Payne                     #
# Date:     May 3, 2017                     #
#############################################

# CO2 Example #2:

# Set-up:
  #install.packages("shiny", repos="http://cran.rstudio.com/")
  library(shiny)
  dat <- data.frame(ID = as.numeric((CO2$Plant)),CO2) 
  #rename CO2 data set as `dat` while simultaneously creating a numeric ID# for each unique plant:

# Code: 
  
  ui <- fluidPage(  ## creates display that auto adjusts to user's device dimensions
    # Main title
    titlePanel(
      ## paragraph of size-20 text: 
      p("Shiny Workshop Example - CO2 Uptake in Plants", style = "font-size: 20px")), 
    # Establish Sidebar layout:
    sidebarLayout(
      # create sidebar (which we'll fill with input controls)
      sidebarPanel(width = 3,
                   div(style = "color:green; padding:0px 0px 150px 0px;", #padding: top, right, bottom, left
                       checkboxGroupInput(inputId = "type", label = "Plant Type", choices = levels(dat$Type), 
                                          selected = levels(dat$Type))
                   ) ## everything held within this div() tag helper function will take on the styles
                   ##   assigned to it (Unless overridden internally) -- see next code example
      ),
      # Spot for the plot
      mainPanel(width = 9,
                plotOutput(outputId = "scatter.plot")
      )
    ))
  
  server <- function(input, output) {
    
    # renderPlot indicates that the function is "reactive"... 
    # ...it will automatically re-execute when input changes
    output$scatter.plot <- renderPlot({
      
      # render the plot:    
      ##Create a stable plot size using all data before adding [changing] subsets using points().
      plot(uptake ~ conc, data = dat, type = "n")  ##type = "n" causes no points to be drawn.
      points(uptake ~ conc, data = dat[dat$Type  %in% c(input$type),])
      title(main = "Plant Trends")
    })
  }
  
  shinyApp(ui = ui, server = server)