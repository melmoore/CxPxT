#############################################
# Title:    "Intro to Shiny" Workshop       #
# Subtitle: 2017 UNC Biology Symposium      #
# Author:   Chris Payne                     #
# Date:     May 3, 2017                     #
#############################################

# CO2 Example #5 (Answer):

# Set-up:
  #install.packages("shiny", repos="http://cran.rstudio.com/")
  library(shiny)
  dat <- data.frame(ID = as.numeric((CO2$Plant)),CO2) 
  #rename CO2 data set as `dat` while simultaneously creating a numeric ID# for each unique plant:

# Code: 
  
  ui <- fluidPage(
    
    # Title Panel: 
    titlePanel(
      p("Shiny Workshop Example - CO2 Uptake in Plants", style = "font-size: 20px")
    ),
    
    ## we must move the tabsetPanel (and subsequent tabPanel) functions before the sidebarPanel... 
    ## ...in order to have changing sidePanels.
    ## (If we wanted different titles, we place titlePanel inside tabsetPanel, too.)
    tabsetPanel( 
      tabPanel(title = "Scatter Plot",
               
               sidebarPanel(width = 3,
                            div(style = "color:green; padding:0px 0px 150px 0px;", 
                                ## padding is top, right, bottom, left
                                p("Data Controls", style = "font-size: 15px; color:blue;"),           
                                
                                # Check box to give us the option whether to show all plants... 
                                # ...or just the plant selected in the slider input below
                                checkboxInput(inputId = "all.plants",label = "Show All Plants",value = T),
                                
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
                                radioButtons(inputId = "treat", label = "Treatment", selected = "Both",
                                             choices = c(levels(dat$Treatment), "Both")),
                                
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
                                ),
                                
                                ## We'll insert printed R output generated from clicking the plot
                                ## Note: this text is created using nearPoints() in server.R
                                verbatimTextOutput(outputId = "plot.info")
                                
                            )
               ),
               
               mainPanel(width = 9,
                         ## scatter plot
                         plotOutput(outputId = "scatter.plot",click = "plot.click"), 
                         ## data table
                         dataTableOutput(outputId = "data")  
               )
               
      ),
      
      ## Create 2nd tab:
      tabPanel(title = "Box Plot",
               
               ## Add subtitle using paragraph HTML tag helper function:
               p("Box Plot Viewer",style = "font-size:18px;"),
               
               ## Set up new sidebarPanel
               sidebarPanel(width = 3,
                            
                            ## Provide sidebarPanel title:
                            p("Box Plot Controls", style = "color:purple"),
                            
                            ## Add input selector to choose which x variable is plotted:
                            radioButtons(inputId = "box.plot.chooser", label = "X Variable", 
                                         choices = names(CO2)[names(CO2) != "uptake"], 
                                         selected = "Type"),
                            
                            ## Add slider that controls at which y value a "moveable"...
                            ## ...horizontal line is located:
                            sliderInput(inputId = "box.slider", label = "Comparison Aid", 
                                        min = 0, max = max(CO2$uptake), value = 0)
                            
               ),
               ## Set up new mainPanel for this 2nd tab:
               mainPanel(width = 9,
                         plotOutput(outputId = "boxplot")
               )
      )
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
    
    # Create data.frame showing data of only the plants (i.e. the rows) selected to be plotted
    output$data <- renderDataTable({ dat2()},
                                   options = list(lengthMenu = list(c(5, 10, -1), 
                                                                    list("5", "10", "All")), 
                                                  pageLength = 5, ordering = T))
    ## Options allow for modification of the data table presented.
    ## This is a rare case in which UI formatting is assigned...
    ## ...in server.R code
    ## lengthMenu creates drop down menu giving you choice...
    ## ...in # of rows to show.
    ## (-1 = show all rows)
    ## pageLength assigns how many rows are shown when table...
    ## ...is 1st rendered
    ## ordering allows you to click on columns to change...
    ## ...ordering of the data.
    
    # Create data to be generated when the scatter plot is clicked 
    
    ## Note: input$plot.click is generated by clicking on the scatter plot. 
    ## This plot-click input object provides x & y coordinates of where you click 
    
    ## We could use the nearPoints function to do this, but I don't like that it has a...
    ## ..."messy" NULL output
    
    # output$plot.info <- renderPrint({
    #   nearPoints(dat2()[,2:6], input$plot.click, xvar = "conc", yvar = "uptake",
    #   threshold=3)
    # })
    
    ## Instead, I'll make my own function that provides a custom string of text when...
    ## ...nothing is clicked. 
    
    # Update to make plot clicker info look better:
    nearPoints2 <- function(coordinfo,blank.text = "Click on Plot Points for Details", 
                            cols, ...) {
      if (is.null(coordinfo)) {
        cat(blank.text,"\n")   #"Click on Plot Points for Details"
      } else { 
        points.info.table <- nearPoints(coordinfo = coordinfo,...)
        print(points.info.table[,cols], row.names = F)
      }
    }
    
    # Create table containing columns 2:6 of dat2 for the point clicked and assign to...
    # ...renderPrint output:
    output$plot.info <- renderPrint({
      nearPoints2(coordinfo = input$plot.click, cols = c(2:6), df = dat2(), xvar = "conc", 
                  yvar = "uptake",threshold=3)
    })  ## Note: nearPoints (default or our custom nearPoints2) actually reports...
    ## ...all points (i.e., rows in your data.frame) that are within the threshold...              ## ...value distance from your click. 
    ## So you might want to shrink `threshold` if your clicks result in too many data.
    
    ###############
    
    ##Here is the added boxplot code. 
    
    output$boxplot <- renderPlot({
      
      ## I want the boxplot function to plot different x variables...
      ## ...which are input as character strings.
      ## Character strings need to be slightly modified to be included in a plotting function.
      
      boxplot(CO2[,"uptake"] ~ CO2[,input$box.plot.chooser], 
              xlab = input$box.plot.chooser, ylab = "uptake")
      lines(x = c(0:15), y = rep(input$box.slider,16), lty = 6)  
      ## lines function changes y based on input$box.slider
    })
  }
  
  shinyApp(ui = ui, server = server)