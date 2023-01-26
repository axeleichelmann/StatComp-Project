# Axel Eichelmann (s2030757, axeleichelmann)
# Place analysis code that may take too long to run every time the report.Rmd
# document is run.
# Run the code in this file with
#   source("analysis.R")
# in a fresh R session to ensure consistency of the results.

# Load function definitions
source("functions.R")


# A code=readlines() code chunk in the report appendix will include the code
# in the report, without running it.
#
# You can place long-running analysis code in this file,
# and save results using
#   saveRDS(object, file = "data/object.rds")
# When the results are needed in the report.Rmd file, use
#   object <- readRDS(file = "data/object.rds")
# Make sure to use different filenames for each object, such as the object
# name itself.
#
# Remember to rerun this code to save new results when you change the code.
#
# The .gitignore file has been setup so that it ignores .rds files in the data/
# folder, so that you don't accidentally make git handle large binary data files.


#calculate the p-values of mean rainfall for each station
p <- 100
p_vals <- c(0,0,0,0,0,0,0,0)
orig_t <- as.vector(test_stat()$'T-Value')
for (i in 1:p){
  p_test_stat <- as.vector(test_stat(randomise = TRUE)$'T-Value')
  diff <- p_test_stat-orig_t
  for (i in 1:8){
    if (diff[i]>0){
      p_vals[i] = p_vals[i]+1
    }
  }
}
p_vals_mean <- p_vals/p
saveRDS(p_vals_mean, file = "data/p_vals_mean.rds")


#calculate the p-values of rainfall probability for each station
p <- 10000
p_vals_prob <- c(0,0,0,0,0,0,0,0)
orig_t <- as.vector(prcp_prob()$'T-Value')
for (i in 1:p){
  p_test_stat <- as.vector(prcp_prob(randomise = TRUE)$'T-Value')
  diff <- p_test_stat-orig_t
  for (i in 1:8){
    if (diff[i]>0){
      p_vals[i] = p_vals[i]+1
    }
  }
}
p_vals_prob <- p_vals_prob/p
saveRDS(p_vals_prob, file = "data/p_vals_prob.rds")
