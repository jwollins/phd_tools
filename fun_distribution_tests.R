## DISTRIBUTION CALCULATOR FUNCTION ####

# J COLLINS - HARPER ADAMS UNVIERSITY 

# Load required packages
library(broom)
library(kableExtra)
library(fitdistrplus)


## POISSON DISTRIBUTION FUNCTION ####

# # Specify the columns to check (replace with your actual columns)
# columns_to_check <- c(7:19)  # Example column indices (plants_m2, shoots_m2, pods_ears_m2, etc.)
# 
# # Run the Poisson test on selected columns
# poisson_results_multiple <- check_poisson_multiple(dat, columns_to_check)
# 
# # Print the results
# print(poisson_results_multiple)

# Poisson Check Function for multiple columns
check_poisson_multiple <- function(data, columns) {
  # Create an empty data frame to store results
  results <- data.frame(
    Column = character(),
    Poisson_Dispersion = numeric(),
    Poisson_Chi_Squared_P_Value = numeric(),
    Poisson_Fit = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each column and perform the Poisson test
  for (col in columns) {
    # Extract the column data
    column_data <- data[[col]]
    
    # Remove NA values
    column_data <- column_data[!is.na(column_data)]
    
    # Skip if the column has no data after removing NAs
    if (length(column_data) == 0) next
    
    # Fit the Poisson regression model
    poisson_model <- glm(column_data ~ 1, family = poisson(link = "log"))
    
    # Summarize the model and calculate dispersion statistic
    residual_deviance <- deviance(poisson_model)
    residual_df <- df.residual(poisson_model)
    dispersion_stat <- residual_deviance / residual_df
    
    # Perform a chi-squared test for goodness-of-fit
    observed_counts <- table(factor(column_data, levels = 0:max(column_data)))
    expected_counts <- dpois(0:max(column_data), lambda = mean(column_data)) * sum(observed_counts)
    chi_squared_stat <- sum((observed_counts - expected_counts)^2 / expected_counts)
    chi_squared_p_value <- pchisq(chi_squared_stat, df = length(observed_counts) - 1, lower.tail = FALSE)
    
    # Check the fit based on dispersion and chi-squared p-value
    poisson_fit <- ifelse(chi_squared_p_value > 0.05 & dispersion_stat <= 1.5, "Poisson", "Not Poisson")
    
    # Append the results to the results data frame
    results <- rbind(results, data.frame(
      Column = names(data)[col],
      Poisson_Dispersion = round(dispersion_stat, 2),
      Poisson_Chi_Squared_P_Value = round(chi_squared_p_value, 4),
      Poisson_Fit = poisson_fit
    ))
  }
  
  # Return the results data frame
  return(results)
}



## GUASSIAN DIST ####

# # Example usage:
# columns_to_check <- c(6:10)  # Specify your columns of interest
# distribution_results <- check_guassian(dat, columns_to_check)
# print(distribution_results)

check_guassian <- function(data, columns_to_check) {
  results <- data.frame(
    Column = character(0),
    Shapiro_P_Value = numeric(0),
    KS_P_Value = numeric(0),
    Distribution_Type = character(0),
    stringsAsFactors = FALSE
  )
  
  for (col in columns_to_check) {
    column_data <- data[[col]]
    
    # Shapiro-Wilk test for normality
    shapiro_test <- tryCatch({
      shapiro.test(column_data)$p.value
    }, error = function(e) NA)  # In case of an error, assign NA
    
    # Kolmogorov-Smirnov test for normality
    ks_test <- tryCatch({
      ks_result <- ks.test(column_data, "pnorm", mean(column_data), sd(column_data))
      ks_result$p.value
    }, error = function(e) NA)  # In case of an error, assign NA
    
    # Classifying the distribution based on the tests
    if (shapiro_test > 0.05) {
      distribution_type <- "Normal"
    } else {
      distribution_type <- "Non-Normal"
    }
    
    # Append results to the data frame
    results <- rbind(results, data.frame(
      Column = colnames(data)[col],
      Shapiro_P_Value = shapiro_test,
      KS_P_Value = ks_test,
      Shap_W_Distribution_Type = distribution_type
    ))
  }
  
  return(results)
}


## EXPONENTIAL DIST ####

# # Example usage
# columns_to_check <- c(6:10)  # Specify the columns you want to check
# exponential_results <- check_exponential_distribution(dat, columns_to_check)
# print(exponential_results)

# Function to test for Exponential Distribution with a summary column
check_exponential_distribution <- function(data, columns_to_check) {
  results <- data.frame(
    Column = character(0),
    Exponential_P_Value_KS = numeric(0),
    Exponential_P_Value_Shapiro = numeric(0),
    Distribution_Type = character(0),
    stringsAsFactors = FALSE
  )
  
  for (col in columns_to_check) {
    column_data <- data[[col]]
    
    # Remove NA values
    column_data <- column_data[!is.na(column_data)]
    
    if(length(column_data) < 2) next  # Skip columns with insufficient data
    
    # Test Exponential distribution using Kolmogorov-Smirnov test
    ks_test <- tryCatch({
      ks.test(column_data, "pexp", rate = 1 / mean(column_data))$p.value
    }, error = function(e) NA)
    
    # Test Exponential distribution using Shapiro-Wilk test on log-transformed data
    shapiro_test <- tryCatch({
      # Exponential data is log-normal, so test on log-transformed data
      shapiro.test(log(column_data))$p.value
    }, error = function(e) NA)
    
    # Determine if the distribution is Exponential based on tests
    is_exponential <- ifelse(ks_test > 0.05 & shapiro_test > 0.05, "Exponential", "Not Exponential")
    
    # Store results
    results <- rbind(results, data.frame(
      Column = colnames(data)[col],
      Exponential_P_Value_KS = ks_test,
      Exponential_P_Value_Shapiro = shapiro_test,
      Distribution_Type = is_exponential
    ))
  }
  
  return(results)
}


## GAMMA DIST ####

# Example usage
# columns_to_check <- c(6:10)  # Specify the columns you want to check
# gamma_results <- check_gamma_distribution(dat, columns_to_check)
# print(gamma_results)

# Function to test for Gamma Distribution with a summary column
check_gamma_distribution <- function(data, columns_to_check) {
  results <- data.frame(
    Column = character(0),
    Gamma_P_Value_KS = numeric(0),
    Gamma_P_Value_Shapiro = numeric(0),
    Distribution_Type = character(0),
    stringsAsFactors = FALSE
  )
  
  for (col in columns_to_check) {
    column_data <- data[[col]]
    
    # Remove NA values
    column_data <- column_data[!is.na(column_data)]
    
    if(length(column_data) < 2) next  # Skip columns with insufficient data
    
    # Test Gamma distribution using Kolmogorov-Smirnov test
    ks_test <- tryCatch({
      ks.test(column_data, "pgamma", shape = 2, rate = 1 / mean(column_data))$p.value
    }, error = function(e) NA)
    
    # Test Gamma distribution using Shapiro-Wilk test on log-transformed data
    shapiro_test <- tryCatch({
      # Gamma data is log-normal, so test on log-transformed data
      shapiro.test(log(column_data))$p.value
    }, error = function(e) NA)
    
    # Determine if the distribution is Gamma based on tests
    is_gamma <- ifelse(ks_test > 0.05 & shapiro_test > 0.05, "Gamma", "Not Gamma")
    
    # Store results
    results <- rbind(results, data.frame(
      Column = colnames(data)[col],
      Gamma_P_Value_KS = ks_test,
      Gamma_P_Value_Shapiro = shapiro_test,
      Distribution_Type = is_gamma
    ))
  }
  
  return(results)
}


## homoscedasticity ####

# Bartlett's Test Function for Homogeneity of Variance
# Function to test for Homogeneity of Variance using Bartlett's Test with a summary column
check_homogeneity_variance_bartlett <- function(data, columns_to_check, group_column) {
  results <- data.frame(
    Column = character(0),
    Test_Statistic = numeric(0),
    P_Value = numeric(0),
    Summary = character(0),
    stringsAsFactors = FALSE
  )
  
  for (col in columns_to_check) {
    column_data <- data[[col]]
    
    # Remove NA values
    column_data <- column_data[!is.na(column_data)]
    
    if(length(column_data) < 2) next  # Skip columns with insufficient data
    
    # Test Homogeneity of variance using Bartlett's test
    bartlett_test <- tryCatch({
      bartlett.test(column_data ~ data[[group_column]])$p.value
    }, error = function(e) NA)
    
    # Determine if the variances are homogeneous based on p-value
    is_homogeneous <- ifelse(bartlett_test > 0.05, "Homogeneous", "Not Homogeneous")
    
    # Store results
    results <- rbind(results, data.frame(
      Column = col,
      Test_Statistic = bartlett_test,
      P_Value = bartlett_test,
      Variance_Summary = is_homogeneous
    ))
  }
  
  return(results)
}






