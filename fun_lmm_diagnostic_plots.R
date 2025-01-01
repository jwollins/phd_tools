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
  
  # Random Effects Plot (for each random effect)
  random_effects <- ranef(model)
  
  # Plot for each random effect
  p4_block <- ggplot(data.frame(effect = random_effects$block[, 1], group = rownames(random_effects$block)), 
                     aes(x = group, y = effect)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(x = "Block", y = "Random Effect", title = "Random Effects by Block") +
    theme(axis.text.x = element_text(angle = 45, size = 7))
  
  p5_crop <- ggplot(data.frame(effect = random_effects$crop[, 1], group = rownames(random_effects$crop)), 
                    aes(x = group, y = effect)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(x = "Crop", y = "Random Effect", title = "Random Effects by Crop") +
    theme(axis.text.x = element_text(angle = 45, size = 7))
  
  p6_year <- ggplot(data.frame(effect = random_effects$year[, 1], group = rownames(random_effects$year)), 
                    aes(x = group, y = effect)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(x = "Year", y = "Random Effect", title = "Random Effects by Year") +
    theme(axis.text.x = element_text(angle = 45, size = 7))
  
  # Cook's Distance Plot
  cooksd <- cooks.distance(model)
  p7 <- ggplot(data = data.frame(cooksd), aes(x = 1:length(cooksd), y = cooksd)) +
    geom_point() +
    geom_hline(yintercept = 4/(length(cooksd) - length(fixef(model)) - 1), color = "red") +
    theme_minimal() +
    labs(x = "Observation", y = "Cook's Distance", title = "Cook's Distance Plot")
  
  # Arrange the plots into a single figure
  ggarrange(p1, p2, p3, p4_block, p5_crop, p6_year, p7,
            ncol = 3, nrow = 3, 
            align = "v",
            labels = c("A", "B", "C", "D", "E", "F", "G"))
}


# Example usage:
# diagnostic_plots_lmm(lmm_model)
