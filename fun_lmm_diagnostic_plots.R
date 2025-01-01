## lm model diagnostics 

library(ggplot2)
library(ggpubr)
library(lme4)

# Function to generate and arrange diagnostic plots for lmer models
diagnostic_plots_lmm <- function(model) {
  
  # Residuals vs. Fitted Values Plot
  fitted_values <- fitted(model)
  residuals <- residuals(model)
  p1 <- ggplot(data = data.frame(fitted_values, residuals), aes(x = fitted_values, y = residuals)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, col = "red") +
    theme_minimal() +
    labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs Fitted Values")
  
  # Histogram of Residuals
  p2 <- ggplot(data = data.frame(residuals), aes(x = residuals)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(x = "Residuals", y = "Frequency", title = "Histogram of Residuals")
  
  # Q-Q Plot of Residuals
  p3 <- ggplot(data = data.frame(residuals), aes(sample = residuals)) +
    geom_qq() +
    geom_qq_line() +
    theme_minimal() +
    labs(title = "Q-Q Plot of Residuals")
  
  # Random Effects Plot (if applicable)
  random_effects <- ranef(model)
  random_effect_names <- names(random_effects)  # List of random effects group names
  if (length(random_effect_names) > 0) {
    # Let's assume first random effect is the grouping variable
    group_name <- random_effect_names[1]
    random_data <- data.frame(group = rownames(random_effects[[group_name]]), 
                              effect = random_effects[[group_name]][, 1])
    p4 <- ggplot(random_data, aes(x = group, y = effect)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Group", y = "Random Effect", title = paste("Random Effects by", group_name))
  } else {
    p4 <- ggplot() + 
      labs(title = "No Random Effects in Model")
  }
  
  # Cook's Distance Plot
  cooksd <- cooks.distance(model)
  p5 <- ggplot(data = data.frame(cooksd), aes(x = 1:length(cooksd), y = cooksd)) +
    geom_point() +
    geom_hline(yintercept = 4/(length(cooksd) - length(fixef(model)) - 1), color = "red") +
    theme_minimal() +
    labs(x = "Observation", y = "Cook's Distance", title = "Cook's Distance Plot")
  
  # Arrange the plots into a single figure
  ggarrange(p1, p2, p3, p4, p5,
            ncol = 3, nrow = 2, 
            align = "v",
            labels = c("A", "B", "C", "D", "E"))
}

# Example usage:
# diagnostic_plots_lmm(lmm_model)
