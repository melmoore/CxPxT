#############################################
# Title:    "Intro to Shiny" Workshop       #
# Subtitle: 2017 UNC Biology Symposium      #
# Author:   Chris Payne                     #
# Date:     May 3, 2017                     #
#############################################

# CO2 Example #3:

# Set-up:
  #install.packages("shiny", repos="http://cran.rstudio.com/")
  library(shiny)
  dat <- data.frame(ID = as.numeric((CO2$Plant)),CO2) 
  #rename CO2 data set as `dat` while simultaneously creating a numeric ID# for each unique plant:

# Code: 
  
  ui <- fluidPage(
    
    # Title Panel: ####
    titlePanel(
      p("Shiny Workshop Example - CO2 Uptake in Plants", style = "font-size: 20px")
    ),
    
    
    sidebarPanel(width = 3,
                 div(style = "color:green; padding:0px 0px 150px 0px;", 
                     ## padding is top, right, bottom, left
                     p("Data Controls", style = "font-size: 15px; color:blue;"),           
                     
                     # Check box to give us the option whether to show all plants... 
                     # ...or just the plant selected in the slider input below
                     checkboxInput(inputId = "all.plants", label = "Show All Plants", 
                                   value = T),
                     
                     # Slider bar allows us to select which plant ID to plot.
                     # This slider only works if the above checkboxInput is unchecked... 
                     # ...(see server.R code).
                     sliderInput(inputId = "plant", label = "Plant ID", min = min(dat$ID), 
                                 max = max(dat$ID), value = 1),  ## sets starting value at 1
                     
                     # This group check box let's us narrow which plant Types are plotted.
                     # checkboxGroupInput differs from checkboxInput by allowing more than one...
                     # ...option to be checked.
                     checkboxGroupInput(inputId = "type", label = "Plant Type", 
                                        choices = levels(dat$Type), 
                                        selected = levels(dat$Type)), 
                     ## sets starting checks to all levels 
                     
                     # Radio buttons work like checkboxes, but result is in strings/#'s...
                     # ...instead of boolean T/F. 
                     # They allow multiple options like checkboxGroupInputs...
                     # ...but only one selection. 
                     radioButtons(inputId = "treat", label = "Treatment", 
                                  choices = c(levels(dat$Treatment), "Both"), 
                                  selected = "Both"),
                     
                     fluidRow(
                       div(style = "color:red;",
                           ## since both columns below are assigned a width of 6 (with 0 offset), 
                           ## they'll fill the tabpanel width equally.
                           column(width = 6, offset = 0,   
                                  selectInput(inputId = "colorQ", label = "Quebec Color", 
                                              choices = palette(), selected = "black")
                           ),
                           column(width = 6, offset = 0,
                                  selectInput(inputId = "colorM", label = "Miss. Color", 
                                              choices = palette(), selected = "black") 
                           )
                       )
                       
                     )
                 ) ## Everything held within this div tag helper function takes on the styles...
                 ##  ...assigned in div() (Unless overridden internally) 
                 ## see the div() function assigned in fluidRow above. 
    ),
    
    # Spot for the plot  
    mainPanel(width = 9,
              plotOutput(outputId = "scatter.plot",click = "plot.click")
    )
  )
  
  
  server <- function(input, output) {
    
    # We need to Set-up various input values:
    
    ## Allows us to narrow data by one plant or to include all plants. 
    ## This is necessary b/c checkboxInput simply generates T/F, and we must translate that...
    ## ...to plant ID #'s. 
    plants.to.plot <- reactive({ 
      if(input$all.plants == T) {
        sort(unique(dat$ID))
      } else { input$plant
      }
    })
    
    ## Allows us to narrow data by one treatment level or to include both. 
    ## This is necessary because "both" is not actually a Treatment type, ... 
    ## ...and therefore needs to be assigned something meaningful (i.e., relevant to the data)
    treats.to.plot <- reactive({ 
      if(input$treat == "Both") {
        levels(dat$Treatment)
      } else {input$treat
      }
    }) 
    
    ## provides vector of color options (corresponding to plant$Type) to apply to...
    ## ...coloring points in plot
    colors <- reactive({
      Colors <- rep(NA,length(dat2()$Type))
      Colors[which(dat2()$Type == "Quebec")] <- input$colorQ
      Colors[which(dat2()$Type == "Mississippi")] <- input$colorM
      Colors
    })   
    
    # Create modified data set based on our numerous input selections:
    ## This will keep our plotting code neater...
    
    dat2 <- reactive({  dat[dat$ID %in% c(plants.to.plot()) &
                              dat$Type  %in% c(input$type) &
                              dat$Treatment %in% c(treats.to.plot()), ] 
    })
    
    
    # Create plot to be rendered in mainpanel. Notice we now use dat2 as the data source. 
    
    output$scatter.plot <- renderPlot({
      plot(uptake ~ conc, data = dat, type = "n")
      points(uptake ~ conc, data = dat2(),col = colors())
      title(main = paste0("Plant(s): ", 
                          paste(levels(dat$Plant)[plants.to.plot()],collapse =", ")))
      ## Combines "Plant(s)" w/ the plant names of whichever plants are included by the inputs. 
    })
  }
  
  # Note that we can save a shinyApp as an object that, when called, runs the app automatically 
  app <- shinyApp(ui = ui, server = server) 
  app