## glm model diagnostics 

library(ggplot2)
library(ggpubr)

# Function to generate and arrange diagnostic plots for GLM models
diagnostic_plots_glm <- function(model) {
  
  # Residuals vs. Fitted Values Plot
  fitted_values <- fitted(model)
  residuals <- residuals(model, type = "response")
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
  
  # Deviance Residuals vs. Fitted Plot (GLM-specific)
  deviance_residuals <- residuals(model, type = "deviance")
  p4 <- ggplot(data = data.frame(fitted_values, deviance_residuals), aes(x = fitted_values, y = deviance_residuals)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, col = "red") +
    theme_minimal() +
    labs(x = "Fitted Values", y = "Deviance Residuals", 
         title = "Deviance Residuals vs Fitted Values")
  
  # Cook's Distance Plot
  cooksd <- cooks.distance(model)
  p5 <- ggplot(data = data.frame(cooksd), aes(x = 1:length(cooksd), y = cooksd)) +
    geom_point() +
    geom_hline(yintercept = 4/(length(cooksd) - length(coef(model)) - 1), color = "red") +
    theme_minimal() +
    labs(x = "Observation", y = "Cook's Distance", title = "Cook's Distance Plot")
  
  # Arrange the plots into a single figure
  ggarrange(p1, p2, p3, p5, p4,
            ncol = 3, nrow = 2, 
            align = "v",
            labels = c("A", "B", "C", "D", "E"))
}

# Example usage:
# Assuming your model is stored in glm_model
# diagnostic_plots_glm(glm_model)
