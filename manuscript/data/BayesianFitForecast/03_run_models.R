options_files <- c("options_SIQR_flu.R")
setwd("/Users/jake/Documents/PH8280_id_modeling/manuscript/data/BayesianFitForecast")

for (opt in options_files) {
  message("=== Running with ", opt, " ===")
  source(opt)
  
  source("run_MCMC.R")
  source("run_analyzeResults.R")
  source("prior_sltn.R")
  
  message("✅ Completed ", opt, "\n")
}
