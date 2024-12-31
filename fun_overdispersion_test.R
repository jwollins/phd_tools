## OVERDISPERSION CALCULATOR FUNCTION ####

# J COLLINS - HARPER ADAMS UNVIERSITY 

# Load required packages
library(broom)
library(kableExtra)

# Function to test for overdispersion and calculate Fisher's Index of Dispersion
test_overdispersion <- function(data, response_column, explanatory_columns) {
  # Create the formula for the model
  formula <- as.formula(paste(response_column, "~", paste(explanatory_columns, collapse = " + ")))
  
  # Fit a Poisson regression model
  poisson_model <- glm(formula, data = data, family = poisson)
  
  # Calculate the overdispersion statistic
  residual_deviance <- deviance(poisson_model)
  residual_df <- df.residual(poisson_model)
  dispersion_stat <- residual_deviance / residual_df
  
  # Interpret overdispersion from the Poisson model
  is_overdispersed_poisson <- ifelse(
    dispersion_stat <= 1.5, "None",
    ifelse(dispersion_stat <= 2, "Low",
           ifelse(dispersion_stat <= 4, "Moderate", "High"))
  )
  
  # Calculate Fisher's Index of Dispersion
  mean_response <- mean(data[[response_column]], na.rm = TRUE)
  variance_response <- var(data[[response_column]], na.rm = TRUE)
  fisher_index_dispersion <- variance_response / mean_response
  
  # Determine overdispersion based on Fisher's Index
  is_overdispersed_fisher <- ifelse(
    fisher_index_dispersion <= 1.5, "None",
    ifelse(fisher_index_dispersion <= 2, "Low",
           ifelse(fisher_index_dispersion <= 4, "Moderate", "High"))
  )
  
  # Create a summary column that combines both overdispersion tests
  summary_overdispersion <- ifelse(
    is_overdispersed_poisson %in% c("Moderate", "High") | 
      is_overdispersed_fisher %in% c("Moderate", "High"),
    "Overdispersed", "Not Overdispersed"
  )
  
  # Return a result as a named list
  list(
    response_column = response_column,
    dispersion_stat = round(dispersion_stat, 2),
    is_overdispersed_poisson = is_overdispersed_poisson,
    fisher_index_dispersion = round(fisher_index_dispersion, 2),
    is_overdispersed_fisher = is_overdispersed_fisher,
    summary_overdispersion = summary_overdispersion
  )
}



## EXAMPLE USAGE ####

# # Specify column numbers to test for overdispersion
# response_column_indices <- c(6:10, 13,14,15,16,17,18)  # Replace with the indices of the columns to test
# response_columns <- colnames(dat)[response_column_indices]  # Get the column names
# 
# # Specify explanatory variables
# explanatory_columns <- c("year", "crop", "treatment")
# 
# # Initialize a results list
# results_list <- lapply(response_columns, function(response_column) {
#   test_overdispersion(dat, response_column, explanatory_columns)
# })
# 
# # Convert results into a dataframe
# results_df <- do.call(rbind, lapply(results_list, as.data.frame))
# 
# # Print results
# print(results_df)