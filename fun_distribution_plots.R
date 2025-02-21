# Distribution plots ####


## Example usage 

# distribution_plots(data = dat, variable = dat$penetration_resistance, colour = dat$treatment)

distribution_plots <- function(data, variable, colour) {
  
  
  dist_plot <- ggplot(data = data, 
                      aes(x = variable, 
                          color = colour, 
                          fill = colour)) +
    geom_density(alpha = 0.1) +
    labs(title = "Distribution", x = "Value", y = "Density") +
    theme_minimal()
  
  
  # Plot with jittered histogram bars using position_dodge
  hist_plot <- ggplot(data = data, 
                      aes(x = variable, 
                          fill = colour)) +
    geom_histogram(position = position_dodge(width = 0.5), 
                   bins = 20, alpha = 0.6) +
    labs(title = "Histogram", x = "Value", y = "Count") +
    theme_minimal()
  
  
  # Create Q-Q plot
  qq_plot <- ggplot(data = data, 
                    aes(sample = variable, 
                        color = colour)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  

  
  ggarrange(dist_plot, hist_plot, qq_plot,
            ncol = 3, 
            common.legend = TRUE, 
            legend = "bottom", 
            labels = c("A", "B", "C"))
  
}
