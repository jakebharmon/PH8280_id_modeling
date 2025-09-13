#------------------------------------------------------------------------------#
#                                                                              #
#     Nowcast-It: A interactive interface for calculating reporting delay      #
#                                                                              #
#------------------------------------------------------------------------------#
 

#------------------------------------------------------------------------------#
#                             Needed Packages                                  #
#------------------------------------------------------------------------------#
# About: This section loads needed packages to be able to successfully use the #
# R Shiny App.                                                                 #
#------------------------------------------------------------------------------#
pacman::p_load(shiny, shinydashboard, shinyWidgets, ggplot2, DT,
               lubridate, shinyalert, chron, tidyverse)


#------------------------------------------------------------------------------#
# Loading needed functions -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section loads the functions needed for the various options the   #
# toolbox provides for modeling. Each function complete different tasks, and   #
# are called based on user selections.                                         #
#------------------------------------------------------------------------------#
source("matrixfill.R")
source("psum.R")
source("shift.R")
source("shiftvec.R")
source("strip.R")
source("ddelay.R")
source("performanceMetrics.R")

#------------------------------------------------------------------------------#
# Creating the dashboard interface ---------------------------------------------
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This section creates the user interface for the reporting delay dashboard.   #
# It provides the user-friendly features along with the structure of the       #
# dashboard itself.                                                            #
#------------------------------------------------------------------------------#
ui <- dashboardPage(
  
  # Changing the theme
  skin = "purple", 
  
  ########################################
  # Creating the header of the dashboard #
  ########################################
  dashboardHeader(title = HTML("<i>Nowcast-It</i>")),
  
  ################################
  # Creating the sidebar options #
  ################################
  dashboardSidebar(
    
    # Loading the data file 
    fileInput("datafile", "Choose a Data File", accept = ".txt"),
    
    # Date Type
    pickerInput("dateType", "Temporal Resolution: ", choices = c("Daily", "Weekly", "Monthly")), 
    
    # Start Date
    uiOutput("startDate"),
    
    # Selecting "M"
    uiOutput("mInput")
 
  ),
  
  ######################################
  # Creating the body of the dashboard #
  ######################################
  dashboardBody(
    
#------------------------------------------------------------------------------#
# Row One: Creating the delay plot and showing the performance metric ----------
#------------------------------------------------------------------------------#
# About: This section creates the `tabBox` that contains the delay adjusted    #
# curve and the performance metrics evaluating the adjusted curve.             #
#------------------------------------------------------------------------------#

    fluidRow(
      
      # Width of row
      width = 12, 
      
      ###########################
      # Creating the tabbed box #
      ###########################
      tabBox(
        
        # Width of box
        width = 12, 
        
#------------------------------------------------------------------------------#
# Row One A: Creating the delay plot -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the delay plot and the settings for customizing, #
# and downloading the plot to the user's computer.                             #
#------------------------------------------------------------------------------#

      ###################################
      # Creating a box to hold the plot #
      ###################################
      tabPanel(
        
        # Title of tab 
        title = "Delay Plot",
        
        # Width of box
        width = 12, 
        
        ##########################
        # Showing the delay plot #
        ##########################
        plotOutput("delayPlot"),
        
        #########################################
        # Creating the style for bottom buttons #
        #########################################
        div(style = "display:flex; vertical-aline: top",
            
            # Download the delay plot 
            div(style = "margin-right: 10px", actionButton("delayAdjust", "Download Figure", icon = icon("download"))),
            
            # Editing the figure 
            div(style = "margin-right: 10px", actionButton("editFigureDelay", "Figure Options")), 
            
        ), # End of buttons 
        
      ), # End of tab for plot 

#------------------------------------------------------------------------------#
# Row One B: Calculating the Performance Metrics -------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the tab which will contain the performance       #
# metrics comparing the delay-adjusted incidence to "truth" data.              #
#------------------------------------------------------------------------------#

    ######################################
    # Creating a box to hold the metrics #
    ######################################
    tabPanel(
      
      # Title for panel
      title = "Metrics",
      
      # Width of panel
      width = 12, 
      
      ##########################
      # Row to hold everything #
      ##########################
      fluidRow(
        
        # Width of row 
        width = 12, 
        
        #################################
        # Column containing the buttons #
        #################################
        column(
          
          # Column width 
          width = 4, 
          
          # Button to load data 
          uiOutput("truthData"),
          
          # Button to download metrics data 
          downloadButton("downloadMetricsData", "Download Metrics", style = "width: 100%;"),
  
        ), # End of column 
        
        #####################################
        # Column to render the metrics data #
        #####################################
        column(
          
          # Width of column 
          width = 8, 
          
          # Rendering the data 
          DTOutput("metricsOutput")
          
        ) # End of column 
      
      ) # End of `fluidRow`

    ) # End of tab for metrics 

  ) # End of 'tabBox'
      
), # End of row for 'tabBox'

#------------------------------------------------------------------------------#
# Row Two: Creating the delay-adjusted data set --------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the delay-adjusted data set and the option to    #
# download the data to the user's personal computer.                           #
#------------------------------------------------------------------------------#

    fluidRow(
      
      # Width of row
      width = 12,
      
      ####################################
      # Creating a box to hold the table #
      ####################################
      box(
        
        # Width of box
        width = 12,
        
        ####################################
        # Creating the column to save data #
        ####################################
        column(
          
          # Width of column
          width = 12, 
        
          # Showing the data
          style = "overflow-x: auto;", DTOutput("incidenceTable"), 
        
          # Overall style
          div(
            
            style = "display: flex; align-items: center; justify-content: space-between;",
            
            # Creating the style for the pickerInput with label on the left
            div(
              
              # Style for buttons 
              style = "display: flex; align-items: center; margin-left: 10px;",
              
              # Label
              tags$label("Data to Show:", style = "margin-right: 10px;"),
              
              # Picker Input
              pickerInput("dataToShow", NULL, 
                          choices = c("Reporting delay adjustment.",
                                      "One-step ahead prediction of data.",
                                      "Estimated reverse-hazard with CI limits.",
                                      "Right Truncated Probability #1",
                                      "Right Truncated Probability #2", 
                                      "Matrix for probabilities."),
                          selected = "Reporting delay adjustment.")
            ),
            
            # Creating the style for the download button
            div(style = "margin-right: 10px;",
                downloadButton("download_incidenceTable", "Download Table")
            )
            
          ) # End of 'div'
          
        ) # End of column

      ) # End of box 
      
    ) # End of Row 2

  ) # End of "dashboard body"

) # End of UI side of dashboard 


#------------------------------------------------------------------------------#
# Creating the server side of the dashboard ------------------------------------
#------------------------------------------------------------------------------#
# About: Below includes the code for the server side of EpiCurve. It is the    #
# backbone behind the UI above. Features include calculating the reporting     #
# delay, plotting the reporting delay incident data, showing the results, and  #
# plotting the distribution of reporting delays.                               #
#------------------------------------------------------------------------------#
server <- function(input, output) {
  

#------------------------------------------------------------------------------#
# Creating the start date input ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the start date input based upon the user's date  #
# selection.                                                                   #
#------------------------------------------------------------------------------#
  
  output$startDate <- renderUI({
    
    ###############################
    # Requiring a date input type #
    ###############################
    req(input$dateType)
    
    ##################################################
    # If daily or weekly data - Providing a Calendar #
    ##################################################
    if(input$dateType %in% c("Daily", "Weekly")){
      
      airDatepickerInput(
        "start",
        "Start Date:",
        multiple = F
        
      )
     
    #####################################################
    # If working with Monthly data - Proving the Months #
    #####################################################
    }else if(input$dateType %in% c("Monthly")){
      
      airDatepickerInput("start",
                         label = "Start Date: ",
                         view = "months", # Editing what the popup calendar shows when it opens
                         minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                         dateFormat = "yyyy-MM" # Formatting the date 
      )

    #########################################################
    # If working with yearly data - Providing numeric input #
    #########################################################
    }else{
      
      airDatepickerInput("start",
                         label = "Start Date: ",
                         view = "year", # editing what the popup calendar shows when it opens
                         minView = "year", #making it not possible to go down to a "days" view and pick the wrong date
                         dateFormat = "yyyy" # Formatting the date 
      )
      
    }
      
  })

#------------------------------------------------------------------------------#
# Creating some built in warnings ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates some built-in warnings based upon the user's     #
# actions. For example, it will warn users if they insert data in the metrics  #
# page prior to loading the main data.                                         #
#------------------------------------------------------------------------------#
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    ####################################
    # Checking for the main input data #
    ####################################
    if(is.null(input$datafile) & !is.null(truthData$dataFinal)){
      
      # Return the alert
      shinyalert("Please upload the main data file prior to loading the truth data.", type = "warning")
      
      # Clearing the truth data
      truthData$dataFinal <- NULL
      
      # Re-rendering the input options
      output$truthData <- renderUI({
        
        return(fileInput("truthFile", "Choose a Data File", accept = c(".txt", ".csv"), width = "100%"))
        
      })
      
    }
    
    ###############################
    # Checking for the start date #
    ###############################
    if(is.null(input$start) & !is.null(truthData$dataFinal)){
      
      # Return the alert
      shinyalert("Please select a start date prior to loading the truth data.", type = "warning")
      
      # Clearing the truth data
      truthData$dataFinal <- NULL
      
      # Re-rendering the input options
      output$truthData <- renderUI({
        
        return(fileInput("truthFile", "Choose a Data File", accept = c(".txt", ".csv"), width = "100%"))
        
      })
      
    }
    
    
  })
  
#------------------------------------------------------------------------------#
# Calculating the reporting delay ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calls the functions included within the working          #
# directory to calculating the delay-adjusted incident curve.                  #
#------------------------------------------------------------------------------#
  
  ##################################################
  # Creating the reactive value to save the matrix #
  ##################################################
  matOutput <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring the input data 
    req(input$datafile)
    
    ###########################################
    # Calling the file path the user selected #
    ###########################################
    dataFilePath <- input$datafile$datapath
    
    #############################
    # Reading in the data table #
    #############################
    Timeline <- read.table(dataFilePath)

    ##############################################################################################
    #  A 2-way crosstabulation of counts are generated as mat0, with structural zero's filled.   #
    #  This matrix should be an upper-triangle square matrix: week of onset vs week of reporting.#
    ##############################################################################################
    mat0 <- matrixfill(Timeline)

    ##########################################################################################################################################
    #  The following matrix will be used in ddelay function. It is also an upper-triangle square matrix: week of onset vs week of reporting. #
    ##########################################################################################################################################
    mat <- as.matrix(shift(mat0))
    
    # Adding column numbers
    dimnames(mat)[[2]] <- 0:(ncol(mat)-1)
    
    # Saving it to the reactive value 
    matOutput$matrix <- mat
    
  })
    
  ##########################
  # Creating the "M" input #
  ##########################
  output$mInput <- renderUI({
    
    numericInput("selectM", "Window 'm':", value = ncol(matOutput$matrix), min = 2)

  })
  
  ######################################
  # Reactive value for reporting delay #
  ######################################
  outDDelay <- reactiveValues()
  
  ########################################
  # Observing changes in reactive values #
  ########################################
  observe({
    
    # Requiring "mat"
    req(matOutput$matrix)
    
    # Requiring an input for "M" to run
    req(input$selectM)
    
    # Calculating reporting delay 
    allOutput <- ddelay(matOutput$matrix,input$selectM)
    
    # Saving the results in a reactive value 
    outDDelay$results <- allOutput
    
  })
    
#------------------------------------------------------------------------------#
# Creating the sequence of dates -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the sequence of dates to be used in plotting and #
# to be exported as part of the data files. The user must specify the          #
# temporal resolution and the start date. The code then determines the length  #
# of the date, and the corresponding dates.                                    #
#------------------------------------------------------------------------------#
  
  ##################################################
  # Creating the reactive value to store the dates #
  ##################################################
  datesSequence <- reactiveValues()
  
  ##################################
  # Creating the sequence of dates #
  ##################################
  observe({
    
    ##################
    # Required Files #
    ##################
    
    # Input data
    req(input$datafile)
    
    # Start date
    req(input$start)
    
    ########################
    # Specified Start Date #
    ########################
    startDate <- input$start
    
    #######################
    # Specified Date Type #
    #######################
    temporalType <- input$dateType
    
    ######################################
    # Determining the length of the data #
    ######################################
    
    # Reading in the data
    Timeline <- outDDelay$results$incidence
    
    # Determining the number of rows
    timelineRows <- nrow(Timeline)
    
    ##########################################
    # Creating the sequence of dates - Daily #
    ##########################################
    if(temporalType %in% c("Daily")){
      
      seqDates <- seq(startDate, by = "day", length.out = timelineRows)
      
    ###########################################
    # Creating the sequence of dates - Weekly #
    ###########################################
    }else if(temporalType %in% c("Weekly")){
      
      seqDates <- seq(startDate, by = "week", length.out = timelineRows)
     
    ############################################
    # Creating the sequence of dates - Monthly #
    ############################################
    }else if(temporalType == "Monthly"){
      
      seqDates <- seq(startDate, by = "month", length.out = timelineRows)
      
    ###########################################
    # Creating the sequence of dates - Yearly #
    ###########################################
    }else if(temporalType == "Yearly"){
      
      seqDates <- as.numeric(seq(year(startDate), length.out = timelineRows))
      
    }
    
    # Saving the sequence of dates in reactive value
    datesSequence$dates <- seqDates

  })
  
#------------------------------------------------------------------------------#
# Clearing out information when data is changed  -------------------------------
#------------------------------------------------------------------------------#
# About: This section clears out the date sequence and related date options if #
# the data is changed, as well as the truth data and produced metrics.         #
#------------------------------------------------------------------------------#
  
  ################################################
  # Observing changes in the data reactive value #
  ################################################
  observeEvent(input$datafile, {
    
    # Clearing out the date sequence
    datesSequence$dates <- NULL
    
    # Clearing the start value
    updateAirDateInput(session = getDefaultReactiveDomain(), 
                       "start", 
                      clear = T,
                      value = NULL)
    
    # Clearing the reactive value holding the 'truth' data
    truthData$dataFinal <- NULL
    
    # Clearing out the metrics data
    finalMetrics$metricsData <- NULL
    
  })
  
  
#------------------------------------------------------------------------------#  
# Creating the button for the delayed-adjusted figure --------------------------
#------------------------------------------------------------------------------#
# About: This section creates the reactivity for the button which allows for   #
# customization of the delay-adjusted time series curve.                       #
#------------------------------------------------------------------------------#
  
  ###################################################
  # Reactive value for color - Reporting Delay Line #
  ###################################################
  colorDelay <- reactiveValues(color = "red")

  ########################################
  # Reactive value for color - LB and UB #
  ########################################
  colorBound <- reactiveValues(color = "green")

  ############################################
  # Reactive value for color - Observed Line #
  ############################################
  colorObserved <- reactiveValues(color = "black")

  ##################################################
  # Reactive value for size - Observed data points #
  ##################################################
  sizeObserved <- reactiveValues(size = 2)

  ###################################################
  # Reactive value for line width - Reporting Delay #
  ###################################################
  widthAdjusted <- reactiveValues(width = 0.5)

  #############################################
  # Reactive value for line width - LB and UB #
  #############################################
  widthBounds <- reactiveValues(width = 0.5)

  ############################################
  # Reactive value for line width - Observed #
  ############################################
  widthObserved <- reactiveValues(width = 0.5)

  ######################################
  # Reactive value for labels - Y-Axis #
  ######################################
  yLabel <- reactiveValues(label = "Cases")

  ##############################################
  # Reactive value for axis text size - Y-Axis #
  ##############################################
  YAxisSize <- reactiveValues(size = 12)

  ##############################################
  # Reactive value for axis tick size - Y-Axis #
  ##############################################
  YAxisTextSize <- reactiveValues(size = 10)

  ############################################
  # Reactive values for axis breaks - X-Axis #
  ############################################
  XAxisBreaks <- reactiveValues(breaks = 2)

  ######################################
  # Reactive value for labels - X-Axis #
  ######################################
  xLabel <- reactiveValues(label = "")

  ##############################################
  # Reactive value for axis text size - X-Axis #
  ##############################################
  XAxisSize <- reactiveValues(size = 12)

  ##############################################
  # Reactive value for axis tick size - X-Axis #
  ##############################################
  XAxisTextSize <- reactiveValues(size = 10)

  ############################
  # Reactive value for title #
  ############################
  title <- reactiveValues(text = "Reporting Delay Adjustment")

  #################################
  # Reactive value for title size #
  #################################
  titleSize <- reactiveValues(size = 15)

  #####################################
  # Reactive value for showing legend #
  #####################################
  showLegend <- reactiveValues(show = T)

  #######################################
  # Reactive value for legend text size #
  #######################################
  legendText <- reactiveValues(size = 12)

  ##############################
  # Creating the button pop-up #
  ##############################
  observeEvent(input$editFigureDelay, {

    # Creating the pop-up
    showModal(modalDialog(

      # Title
      title = "Figure Options",

      tabsetPanel(

        # Y-Axis tab
        tabPanel("Y-Axis",
                 textInput("labelInputY", label = "Label:", value = yLabel$label),
                 numericInput("sizeInputAxisY", label = "Label Size:", value = YAxisSize$size),
                 numericInput("axisTickSizeY", label = "Y-Axis Tick Size:", value = YAxisTextSize$size)
        ),

        # X-Axis tab
        tabPanel("X-Axis",
                 textInput("labelInputX", label = "Label:", value = xLabel$label),
                 numericInput("sizeInputAxisX", label = "Label Size:", value = XAxisSize$size),
                 numericInput("breaksAxisX", label = "Number of Breaks:", value = XAxisBreaks$breaks),
                 numericInput("axisTickSizeX", label = "X-Axis Tick Size:", value = XAxisTextSize$size)
        ),

        # Title & Legend
        tabPanel("Title & Legend",
                 textInput("mainTitle", label = "Figure title:", value = title$text),
                 numericInput("titleSizeInput", label = "Title Size:", value = titleSize$size),
                 checkboxInput("showLegendInput", label = "Show the Legend:", value = showLegend$show),
                 numericInput("sizeInputLegend", label = "Legend Text Size:", value = legendText$size)
        ),
        
        # Colors tab
        tabPanel("Colors",
                 textInput("colorInputDelay", label = "Reporting Delay Line Color:", value = colorDelay$color),
                 textInput("colorInputBound", label = "Bound Line Color:", value = colorBound$color),
                 textInput("colorInputObserved", label = "Observed Data Line Color:", value = colorObserved$color)
        ),
        
        # Line tab
        tabPanel("Lines",
                 numericInput("sizeInputObserved", label = "Observed Data Point Size:", value = sizeObserved$size),
                 numericInput("widthInputObserved", label = "Observed Line Width:", value = widthObserved$width),
                 numericInput("widthInputAdjusted", label = "Adjusted Line Width:", value = widthAdjusted$width),
                 numericInput("widthInputBounds", label = "Bounds Line Width:", value = widthBounds$width)
        )
        
      ), # End of tabset

      easyClose = TRUE,

    )) # End of module

  }) # End of observe event


  #####################################################
  # Update the reactive value - Reporting Delay Color #
  #####################################################
  observeEvent(input$colorInputDelay, {

    # Updating the color
    colorDelay$color <- input$colorInputDelay

  })

  ######################################
  # Update the reactive value - Bounds #
  ######################################
  observeEvent(input$colorInputBound, {

    # Updating the color
    colorBound$color <- input$colorInputBound

  })

  #############################################
  # Update the reactive value - Observed Line #
  #############################################
  observeEvent(input$colorInputObserved, {

    # Updating the color
    colorObserved$color <- input$colorInputObserved

  })

  ###############################################
  # Update the reactive value - Observed Points #
  ###############################################
  observeEvent(input$sizeInputObserved, {

    # Updating the size
    sizeObserved$size <- input$sizeInputObserved

  })

  ###################################################
  # Update the reactive value - Width observed line #
  ###################################################
  observeEvent(input$widthInputObserved, {

    # Updating the width
    widthObserved$width <- input$widthInputObserved

  })

  ###################################################
  # Update the reactive value - Width Adjusted line #
  ###################################################
  observeEvent(input$widthInputAdjusted, {

    # Updating the width
    widthAdjusted$width <- input$widthInputAdjusted

  })

  #################################################
  # Update the reactive value - Width Bound lines #
  #################################################
  observeEvent(input$widthInputBounds, {

    # Updating the width
    widthBounds$width <- input$widthInputBounds

  })

  #######################################
  # Update the reactive value - Y-Label #
  #######################################
  observeEvent(input$labelInputY, {

    # Updating the label
    yLabel$label <- input$labelInputY

  })

  #################################################
  # Update the reactive value - Y-Label Text size #
  #################################################
  observeEvent(input$sizeInputAxisY, {

    # Updating the text size
    YAxisSize$size <- input$sizeInputAxisY

  })

  ###############################################
  # Updating the reactive value - Y-Axis Breaks #
  ###############################################
  observeEvent(input$breaksAxisY, {

    # Updating the number of Y-Axis breaks
    YAxisBreaks$breaks <- input$breaksAxisY

  })

  ####################################################
  # Updating the reactive value - Y-Axis Start Value #
  ####################################################
  observeEvent(input$startYAxis, {

    # Updating the start value for the Y-Axis
    YAxisStart$start <- input$startYAxis

  })

  ##################################################
  # Updating the reactive value - Y-Axis Tick Size #
  ##################################################
  observeEvent(input$axisTickSizeY, {

    # Updating the tick size
    YAxisTextSize$size <- input$axisTickSizeY

  })

  #######################################
  # Update the reactive value - X-Label #
  #######################################
  observeEvent(input$labelInputX, {

    # Updating the label
    xLabel$label <- input$labelInputX

  })

  #################################################
  # Update the reactive value - X-Label Text size #
  #################################################
  observeEvent(input$sizeInputAxisX, {

    # Updating the text size
    XAxisSize$size <- input$sizeInputAxisX

  })

  ###############################################
  # Updating the reactive value - X-Axis Breaks #
  ###############################################
  observeEvent(input$breaksAxisX, {

    # Updating the number of X-Axis breaks
    XAxisBreaks$breaks <- input$breaksAxisX

  })

  ##################################################
  # Updating the reactive value - X-Axis Tick Size #
  ##################################################
  observeEvent(input$axisTickSizeX, {

    # Updating the tick size
    XAxisTextSize$size <- input$axisTickSizeX

  })

  #######################################
  # Updating the reactive value - Title #
  #######################################
  observeEvent(input$mainTitle, {

    # Updating the main title
    title$text <- input$mainTitle

  })

  ############################################
  # Updating the reactive value - Title Size #
  ############################################
  observeEvent(input$titleSizeInput, {

    # Updating the main title size
    titleSize$size <- input$titleSizeInput

  })

  ####################################################
  # Updating the reactive value - Showing the legend #
  ####################################################
  observeEvent(input$showLegendInput, {

    # Updating if to show the legend or not
    showLegend$show <- input$showLegendInput

  })

  ##################################################
  # Updating the reactive value - Legend text size #
  ##################################################
  observeEvent(input$sizeInputLegend, {

    # Updating the legend text size
    legendText$size <- input$sizeInputLegend

  })



#------------------------------------------------------------------------------#
# Plotting the delay plot ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This is the server side to create the reporting delay plot shown at   #
# at the top of the dashboard. As the dashboard allows for customization, many #
# of the options here are macro coded and depend on the selections of the      #
# user.                                                                        #
#------------------------------------------------------------------------------#

  ###########################################
  # Empty reactive value to save the figure #
  ###########################################
  reportingDelayGraphSAVE <- reactiveValues()
  
  ######################################
  # Rendering the incidence delay plot #
  ######################################
  output$delayPlot <- renderPlot({
    
    # Requiring the incidence data
    req(outDDelay$results)
    
    # Requiring a start value
    req(input$start)

    #######################################################
    # Reading in the data from the corresponding function #
    #######################################################
    
    # Calling the incidence
    incidence <- outDDelay$results$incidence
    
    # Adding the column names to the incidence data frame
    dimnames(incidence)[[2]] <- c("as reported", "delay adjusted", "95% LCL", "95% UCL")

    # Changing data to a data frame
    dataForGraph <- data.frame(incidence) %>%
      dplyr::mutate(Dates = c(datesSequence$dates))

    #######################
    # Customization graph #
    #######################

    # Creating the legend
    if(showLegend$show){

      legendPosition <- c(0.02, 0.99)

    }else{

      legendPosition <- "none"

    }
    
    ########################################################
    # Determining how to show dates - Daily or Weekly Data #
    ########################################################
    if(input$dateType %in% c("Daily", "Weekly")){
      
      # Creating the x-axis scale 
      xBreaks <- scale_x_date(limits = c(min(dataForGraph$Dates), max(dataForGraph$Dates)), breaks = seq.Date(min(dataForGraph$Dates), max(dataForGraph$Dates), by = XAxisBreaks$breaks)) 
    
    ###########################################
    # Determining how to show dates - Monthly #
    ###########################################
    }else if(input$dateType == "Monthly"){
      
      # Determining the 'by' for date sequence
      if(XAxisBreaks$breaks > 1){breaksLabel = " months"}else{breaksLabel = " month"}
      
      # Creating the x-axis scale
      xBreaks <- scale_x_date(limits = c(min(dataForGraph$Dates), max(dataForGraph$Dates)), breaks = seq.Date(min(dataForGraph$Dates), max(dataForGraph$Dates), by = paste0(XAxisBreaks$breaks, breaksLabel)), date_labels = "%Y-%m") 
      
    ##########################################
    # Determining how to show dates - Yearly #
    ##########################################
    }else if(input$dateType == "Yearly"){
      
      # Creating the x-axis scale
      xBreaks <- scale_x_continuous(limits = c(min(dataForGraph$Dates), max(dataForGraph$Dates)), breaks = seq(min(dataForGraph$Dates), max(dataForGraph$Dates), by = XAxisBreaks$breaks))
      
    }
        
   
    ################################
    # Graphing the reporting delay #
    ################################
    reportingDelayGraph <- ggplot(data = dataForGraph) +
      geom_point(aes(x = Dates, y = delay.adjusted, color = "Delay adjusted epicurve by date of onset"), size = sizeObserved$size, shape = 1) +
      geom_point(aes(x = Dates, y = as.reported, color = "Reported epicurve by date of onset"), size = sizeObserved$size, shape = 1) +
      geom_line(aes(x = Dates, y = as.reported, color = "Reported epicurve by date of onset", linetype = "Reported epicurve by date of onset"), linewidth = widthObserved$width, show.legend = showLegend$show) +
      geom_line(aes(x = Dates, y = `X95..LCL`, linetype = "95% Lower CL", color = "95% Lower CL"), position = position_nudge(y = -0.06), linewidth = widthBounds$width, show.legend = showLegend$show) +
      geom_line(aes(x = Dates, y = `X95..UCL`, linetype = "95% Upper CL", color = "95% Upper CL"), linewidth = widthBounds$width, show.legend = showLegend$show) +
      geom_line(aes(x = Dates, y = delay.adjusted, color = "Delay adjusted epicurve by date of onset", linetype = "Delay adjusted epicurve by date of onset"), linewidth = widthAdjusted$width, show.legend = showLegend$show) +
      geom_line(aes(x = Dates, y = as.reported, color = "Reported epicurve by date of onset", linetype = "Reported epicurve by date of onset"), linewidth = widthObserved$width, show.legend = showLegend$show) +
      scale_color_manual(values = c("95% Upper CL" = colorBound$color, "95% Lower CL" = colorBound$color, "Delay adjusted epicurve by date of onset" = colorDelay$color, "Reported epicurve by date of onset" = colorObserved$color)) +
      scale_linetype_manual(values = c("95% Upper CL" = "solid", "95% Lower CL" = "dashed", "Delay adjusted epicurve by date of onset" = "solid", "Reported epicurve by date of onset" = "solid")) +
      xBreaks + 
      scale_y_continuous(limits = c(0, max(dataForGraph$X95..UCL)),
                         breaks = seq(0, max(dataForGraph$X95..UCL), by = max(dataForGraph$X95..UCL)/6),
                         labels = scales::number_format(accuracy = 1)) +
      labs(title = title$text,
           y = yLabel$label,
           x = xLabel$label,
           color = "",
           linetype = "",
           shape = "") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5, size = titleSize$size, face = "bold"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = XAxisTextSize$size),
            axis.title.y = element_text(size = YAxisSize$size),
            axis.text.y = element_text(size = YAxisTextSize$size),
            axis.title.x = element_text(size = XAxisSize$size),
            legend.position = legendPosition,
            legend.justification = c(0, 1),
            legend.key = element_rect(fill = "transparent"),
            legend.text = element_text(size = legendText$size))

    #######################################
    # Saving the plot to a reactive value #
    #######################################
    reportingDelayGraphSAVE$figure <- reportingDelayGraph

    # Returning the graph
    return(reportingDelayGraph)

  })
  

#------------------------------------------------------------------------------#
# Pop-up for the download button for reporting delay figures -------------------
#------------------------------------------------------------------------------#
# About: This section creates the figure pop-up options for the reporting      #
# delay adjusted figure button. In this pop-up, users have the option to       #
# format how the figure is saved, and the download button is created.          #
#------------------------------------------------------------------------------#

  #####################################################
  # Setting Figure Specifications - Pop up for figure #
  #####################################################
  observeEvent(input$delayAdjust, {

    ################################
    # Figure specification options #
    ################################
    isolate({

      showModal(modalDialog(

        title = "Figure Specifications",
        numericInput("dpi", "Figure DPI:", value = 900),
        numericInput("width", "Figure Width:", value = 15),
        numericInput('height', 'Figure Height:', value = 11),
        pickerInput("units", label = "Unit of Measurement:", choices = c("in", "cm", "mm", "px")),
        pickerInput("extFig", label = "Figure Type:", choices = c("png", "eps", "pdf", "tiff", "jpeg", "svg")),
        downloadButton("downloadDelayAdjusted", "Download Figure"),
        easyClose = TRUE))

    }) # End of isolate

  }) # End of observe-event

#------------------------------------------------------------------------------#
# Downloading the reporting delay figure ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section allows the user to download the reporting delay figure   #
# to their personal computer.                                                  #
#------------------------------------------------------------------------------#

  ##############################################
  # Creating the option to download the figure #
  ##############################################
  output$downloadDelayAdjusted <- downloadHandler(

    ####################################
    # Function to create the file-name #
    ####################################
    filename = function() {

      # Closing the figure specification
      removeModal()

      # File name
      paste("reporting-delay-adjustment-timeseries.", input$extFig, sep = "")

    },

    #############################
    # Function to save the file #
    #############################
    content = function(file) {

      figure <-  reportingDelayGraphSAVE$figure

      # Running with compression if using a '.tiff'
      if(input$extFig == 'tiff'){

        # Saving the file
        ggsave(file, plot = figure,
               dpi = input$dpi,
               width = input$width,
               height = input$height,
               units = input$units,
               compression = "lzw")

      # Running without compression if not using a '.tiff'
      }else{

        # Saving the file
        ggsave(file, plot = figure,
               dpi = input$dpi,
               width = input$width,
               height = input$height,
               units = input$units)
      }

    }) # End of saving the figure(s)

  
#------------------------------------------------------------------------------#
# Rendering the data table for incidence ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the incidence data table to the main UI for the  #
# users to be able to download and navigate through.                           #
#------------------------------------------------------------------------------#

  ###############################################
  # Empty reactive value to save the data later #
  ###############################################
  incidenceTableSAVE <- reactiveValues()

  ############################
  # Rendering the data table #
  ############################
  output$incidenceTable <- renderDT({

    # Requiring the incidence data
    req(outDDelay$results)
    
    # Requiring a start value
    req(input$start)
    
    ##################################
    # Producing the Incidence output #
    ##################################
    if(input$dataToShow == "Reporting delay adjustment."){
      
      #######################################################
      # Reading in the data from the corresponding function #
      #######################################################
      
      # Calling the incidence
      incidence <- outDDelay$results$incidence
      
      # Adding the column names to the incidence data frame
      dimnames(incidence)[[2]] <- c("as reported", "delay adjusted", "95% LCL", "95% UCL")
  
      # Changing data to a data frame
      dataForTable <- data.frame(incidence) %>%
        dplyr::mutate(Dates = datesSequence$dates) %>%
        dplyr::rename("As Reported" = as.reported,
                      "Delay Adjusted" = delay.adjusted,
                      "95% LCL" = X95..LCL,
                      "95% UCL" = X95..UCL) %>%
        dplyr::select(Dates, `As Reported`, `Delay Adjusted`, `95% LCL`, `95% UCL`)
  
      # Set row names to NULL
      row.names(dataForTable) <- NULL
    
    ########################################
    # Producing the one-step ahead results #
    ########################################
    }else if(input$dataToShow == "One-step ahead prediction of data."){
      
      # Calling the one-step prediction of data 
      prediction <- outDDelay$results$Onestepinc
      
      # Adding the column names to the data set
      dimnames(prediction)[[2]] <- c("as reported", "prediction", "95% LCL", "95% UCL")
 
      # Changing data to a data frame
      dataForTable <- data.frame(prediction) %>%
        dplyr::mutate(Dates = datesSequence$dates) %>%
        dplyr::rename("As Reported" = as.reported,
                      "Prediction" = prediction,
                      "95% LCL" = X95..LCL,
                      "95% UCL" = X95..UCL) %>%
        dplyr::select(Dates, `As Reported`, `Prediction`, `95% LCL`, `95% UCL`)
      
      # Set row names to NULL
      row.names(dataForTable) <- NULL
      
    ########################################
    # Producing the `Matrix to Show` table #
    ########################################
    }else if(input$dataToShow == "Matrix for probabilities."){
      
      # Changing data to a data frame
      dataForTable <- data.frame(outDDelay$results$vg) 

    #########################################################
    # Producing the Estimated reverse-hazard with CI limits #
    #########################################################
    }else if(input$dataToShow == "Estimated reverse-hazard with CI limits."){
      
      # Changing data to a data frame
      dataForTable <- data.frame(outDDelay$results$g)
      
    #######################################################
    # Producing the first right truncated probability set #
    #######################################################
    }else if(input$dataToShow == "Right Truncated Probability #1"){
      
      # Changing data to a data frame
      dataForTable <- data.frame(outDDelay$results$prob)
      
    ########################################################
    # Producing the second right truncated probability set #
    ########################################################
    }else if(input$dataToShow == "Right Truncated Probability #2"){
      
      # Changing data to a data frame
      dataForTable <- data.frame(outDDelay$results$prob1)
      
    }

    # Saving the data to the reactive value
    incidenceTableSAVE$data <- data.frame(dataForTable)

    # Returning the table
    return(dataForTable)

  })

#------------------------------------------------------------------------------#
# Downloading the data frame ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section allows the users to download the available data table to #
# their personal computer.                                                     #
#------------------------------------------------------------------------------#

  output$download_incidenceTable <- downloadHandler(

    ################################################################
    # Function to create the file-name to save the incidence table #
    ################################################################
    filename = function() {

      paste(input$dataToShow, ".csv")

    },

    #######################################
    # Function to save the file - as .csv #
    #######################################
    content = function(file) {

      # Saving the file
      write.csv(incidenceTableSAVE$data, file, row.names = FALSE)

    }

  ) # End of download button

#------------------------------------------------------------------------------#
# Creating the file input for the "truth" data ---------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the file input for the "truth" data.             #
#------------------------------------------------------------------------------#
  
  ##########################
  # Creating the UI output #
  ##########################
  output$truthData <- renderUI({
    
    return(fileInput("truthFile", "Choose a Data File", accept = c(".txt", ".csv"), width = "100%"))
    
  })
  
  ########################################################
  # Re-rendering the input file if an option has changed #
  ########################################################
  observeEvent(input$datafile, {
    
    # Re-rendering the option 
    output$truthData <- renderUI({
      
      return(fileInput("truthFile", "Choose a Data File", accept = c(".txt", ".csv"), width = "100%"))
      
    })
    
  })
  
#------------------------------------------------------------------------------#
# Reading in the 'truth' data --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads in the truth data, either as a '.csv' or '.txt'    #
# file. In a later step, the data is checked to ensure it has one columns,     #
# dates and counts.                                                            #
#------------------------------------------------------------------------------#
  
  #############################################
  # Creating reactive value to store the data #
  #############################################
  truthData <- reactiveValues()
  
  ###############################
  # Creating the reactive value #
  ###############################
  observe({
    
    ################################
    # Requiring information to run #
    ################################
    req(input$truthFile)
    
    ############################
    # Reading in the file name #
    ############################
    fileName <- input$truthFile
    
    # Pulling the extension 
    ext <- tools::file_ext(fileName$datapath)
    
    #########################################################
    # Checking if a appropriate file is returned - No Issue #
    #########################################################
    if(ext %in% c("csv", "txt")){
      
      # File to proceed with
      fileToRead <- fileName$datapath
      
    ######################################################
    # Checking if a appropriate file is returned - Issue #
    ######################################################
    }else{
      
      # Error to show
      shinyalert::shinyalert(paste0("Please upload a '.csv' or '.txt' file. You have uploaded
                             a ", ext), type = "error")
      
      # Clearing out the file to proceed with
      fileToRead <- NULL
      
    }
    
    ################################################
    # Reading in the data based on extension - TXT #
    ################################################
    if(ext == "txt" & !is.null(fileToRead)){
      
      # Reading in the .txt
      data <- read.table(fileToRead)
      
    ################################################
    # Reading in the data based on extension - CSV #
    ################################################
    }else if(ext == "csv" & !is.null(fileToRead)){
      
      # Reading in the .csv 
      data <- read.csv(fileToRead, header = F)
        
    ######################
    # Reading in nothing #
    ######################
    }else{
      
      # Reading in nothing
      data <- NULL
      
    }
    
    ############################################
    # Saving the data under the reactive value #
    ############################################
    truthData$dataFinal <- data
    
  })

#------------------------------------------------------------------------------#
# Calculating the performance metrics ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the performance metrics: (1) Mean Absolute    #
# Error (MAE), Mean Squared Error (MSE), and 95% Prediction Interval Coverage  #
# (95% PI Coverage), comparing the delay-adjusted incidence curve to the truth #
# data read in by the user.                                                    #
#------------------------------------------------------------------------------#
  
  ###############################################
  # Reactive value to store performance metrics #
  ###############################################
  finalMetrics <- reactiveValues()
  
  ############################################
  # Observing changes in the reactive values #
  ############################################
  observe({
    
    ################################################
    # Requiring certain information before running #
    ################################################
    
    # Incidence data
    req(outDDelay$results$incidence)
    
    # Dates
    req(datesSequence$dates)
    
    # Truth data
    req(truthData$dataFinal)
    
    # Requiring the start
    req(input$start)
    
    #################################
    # Preparing the incidence curve #
    #################################
    
    # Calling the incidence
    incidence <- outDDelay$results$incidence
    
    # Adding the column names to the incidence data frame
    dimnames(incidence)[[2]] <- c("as reported", "delay adjusted", "95% LCL", "95% UCL")
    
    # Changing data to a data frame
    dataForTable <- data.frame(incidence) %>%
      dplyr::mutate(Dates = datesSequence$dates) %>%
      dplyr::rename("As Reported" = as.reported,
                    "Delay Adjusted" = delay.adjusted,
                    "95% LCL" = X95..LCL,
                    "95% UCL" = X95..UCL) %>%
      dplyr::select(Dates, `As Reported`, `Delay Adjusted`, `95% LCL`, `95% UCL`)
    
    # Set row names to NULL
    row.names(dataForTable) <- NULL
    
    ############################################
    # Calling the performance metrics function #
    ############################################
    metrics <- performanceMetrics(delay = dataForTable, 
                                  truth = truthData$dataFinal)
    
    ##################
    # Error checking #
    ##################
    if(all(metrics == "ERROR1")){
      
      # Error to show
      shinyalert(text = "Please check the format of the truth data. It should have one 
                 column which corresponds to the 'truth' incidence.", type = "error")
      
      # Clearing the reactive value
      finalMetrics$metricsData <- NULL
      
    ########################
    # No error has occured #
    ########################
    }else{
      
      #########################################
      # Saving the data in the reactive value #
      #########################################
      finalMetrics$metricsData <- metrics 
      
    }
    
  })

#------------------------------------------------------------------------------#
# Rendering the metrics data ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the data table containing the metrics calculated #
# above. In a later step, the user will be able to download the metrics to     #
# their personal computer.                                                     #
#------------------------------------------------------------------------------#
  
  #################################
  # Creating the render statement #
  #################################
  output$metricsOutput <- renderDT({
    
    # Returning the metrics 
    if(!is.null(finalMetrics$metricsData)){
    
      return(datatable(finalMetrics$metricsData, rownames= FALSE, options = list(scrollX = T)))
    
    # Returning nothing (Keeps Box Extended)  
    }else{
      
      return(NULL)
      
    }
    
  })

#------------------------------------------------------------------------------#
# Downloading the data frame ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section allows the users to download the metrics data table to   #
# their personal computer.                                                     #
#------------------------------------------------------------------------------#
  
  output$downloadMetricsData <- downloadHandler(
    
    ################################################################
    # Function to create the file-name to save the incidence table #
    ################################################################
    filename = function() {
      
      paste("performance-metrics.csv")
      
    },
    
    #######################################
    # Function to save the file - as .csv #
    #######################################
    content = function(file) {
      
      # Saving the file
      write.csv(data.frame(finalMetrics$metricsData), file, row.names = FALSE)
      
    }
    
  ) # End of download button




}

# Run the application
shinyApp(ui = ui, server = server)

