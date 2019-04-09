#############################################
# Title:    "Intro to Shiny" Workshop       #
# Subtitle: 2017 UNC Biology Symposium      #
# Author:   Chris Payne                     #
# Date:     May 3, 2017                     #
#############################################

# CO2 Example #1:

# Set-up:
  #install.packages("shiny", repos="http://cran.rstudio.com/")
  library(shiny)
  dat <- data.frame(ID = as.numeric((CO2$Plant)),CO2) 
  #rename CO2 data set as `dat` while simultaneously creating a numeric ID# for each unique plant:

# Code: 
  
  ui <- fluidPage(  ## creates display that auto adjusts to user's device dimensions
    # Main title
    titlePanel("Shiny Workshop Example - CO2 Uptake in Plants"),
    # Establish sidebar layout:
    sidebarLayout(
      # Create sidebar (which we'll fill with input controls)
      sidebarPanel(
        # Create a check box input control allowing multiple items to be checked
        checkboxGroupInput(inputId = "type", label = "Plant Type", choices = levels(dat$Type), 
                           selected = levels(dat$Type))  
        ## Note: levels() shows all unique "names" of a class = factor object
      ),
      # Spot for the plot
      mainPanel(
        plotOutput(outputId = "scatter.plot")  
        ## Note: "scatter.plot" output object is created in server.R
      )
    )
    )
  
  server <- function(input, output) {
    
    # renderPlot indicates that the function is "reactive" 
    # It will automatically re-execute when input changes
    output$scatter.plot <- renderPlot({
      
      # Render the plot:    
      ##Create a stable plot size using all data before adding [changing] subsets using points().
      plot(uptake ~ conc, data = dat, type = "n")  ## type = "n" causes no points to be drawn.
      points(uptake ~ conc, data = dat[dat$Type  %in% c(input$type),])
      title(main = "Plant Trends")
    })
  }
  
  shinyApp(ui = ui, server = server)