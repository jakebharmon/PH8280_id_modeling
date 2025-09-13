#------------------------------------------------------------------------------#
#                                                                              #
#                      Calculating the Performance Metrics                     #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function calculates the Mean Absolute Error (MAE), Mean Squared         #
# Error (MSE), and 95% Prediction Interval Coverage (95% PI coverage comparing #
# the adjusted incidence curve to the "truth" data. Users can load the truth   #
# data on the "Metrics" tab, from which the metrics will automatically be      #
# calculated based upon the loaded delay curve.                                #
#------------------------------------------------------------------------------#
#                         Author: Amanda Bleichrodt                            #
#------------------------------------------------------------------------------#
performanceMetrics <- function(delay, truth){
  
  
#------------------------------------------------------------------------------#
# Renaming the inputs ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section saves the function inputs under new names to prevent     #
# over-writing at any step.                                                    #
#------------------------------------------------------------------------------#
  
  #######################
  # Delay adjusted data #
  #######################
  delayAdjusted <<- delay
  
  ########################
  # Truth incidence data #
  ########################
  truthData <<- truth 
  
#------------------------------------------------------------------------------#
# Checking the truth data ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the truth data for formatting and prepares the    #
# dates for merging with the delay adjusted data.                              #
#------------------------------------------------------------------------------#
  
  ####################################################
  # Checking for number of columns and no processing #
  ####################################################
  if(ncol(truthData) != 1){
    
    return("ERROR1")
    
  }
  
#------------------------------------------------------------------------------#
# Merging the delay-adjusted incidence curve and truth data --------------------
#------------------------------------------------------------------------------#
# About: This section merges the delay-adjusted incidence curve and truth      #
# data to calculate the MAE, MSE, and 95% PI coverage in a later step.         #
#------------------------------------------------------------------------------#
  
  #################################################
  # Creating the "id" variable for both data sets #
  #################################################
  
  # ID for adjusted curve
  delayAdjustedFINAL <- delayAdjusted %>%
    dplyr::mutate(ID = c(1:nrow(delayAdjusted)))
  
  # ID for truth data
  truthDataFINAL <- truthData %>%
    dplyr::mutate(ID = c(1:nrow(truthData)))
  
  ############################
  # Creating the merged data #
  ############################
  mergedData <- full_join(delayAdjustedFINAL, truthDataFINAL) %>%
    na.omit() # Removing rows with NA
  
#------------------------------------------------------------------------------#
# Calculating the performance metrics ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the MSE, MAE, and 95% PI coverage, comparing  #
# the truth data to the delay-adjusted incidence.                              #
#------------------------------------------------------------------------------#
  
  #############################
  # Creating the metrics data #
  #############################
  metricsData <- mergedData %>%
    dplyr::mutate(MAE = round(mean(abs(`Delay Adjusted` - V1)), 2), # MAE 
                  MSE = round(mean((`Delay Adjusted` - V1)^2), 2), # MSE 
                  in.coverage = ifelse(V1 <= `95% UCL` & V1 >= `95% LCL`, 1, 0), # In coverage indicator 
                  PI = round(mean(in.coverage)*100, 2)) %>% # Calculating the PI 
    dplyr::select(MAE, MSE, PI) %>% # Keeping only the metrics 
    dplyr::distinct(MAE, MSE, PI) # Removing repeat rows
  

#------------------------------------------------------------------------------#
# Returning the data -----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns the final data set to the main dashboard.        #
#------------------------------------------------------------------------------#
  
  return(metricsData)
  
}