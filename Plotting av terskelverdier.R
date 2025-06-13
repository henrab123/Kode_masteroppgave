# Load ggplot2
library(ggplot2)

# Parameters for the normal distributions
mean1 <- 0.128436129 + 0.262769116
mean2 <- 0.128436129
std <- 1
thresholds <- c(alpha_to_theta(estimated_parameters_V23$alpha[[17]][,1]))  # Two thresholds for shading

# Generate data for the distributions within the domain [-3, 3]
x <- seq(-3, 3, length = 1000)  # Restrict x to the domain from -3 to 3
data <- data.frame(
  x = rep(x, 2),
  y = c(dnorm(x, mean = mean1, sd = std), dnorm(x, mean = mean2, sd = std)),
  Fordelinger = factor(rep(c("Med sensoreffekter", "Uten sensoreffekter"), each = length(x)))
)

# Filter data for shading
shading_data  <- subset(data, x >= thresholds[24] & x <= thresholds[25] & Fordelinger == "Med sensoreffekter")
shading_data2 <- subset(data, x >= thresholds[24] & x <= thresholds[25] & Fordelinger == "Uten sensoreffekter")
shading_datab  <- subset(data, x >= thresholds[19] & x <= thresholds[20] & Fordelinger == "Med sensoreffekter")
shading_data2b <- subset(data, x >= thresholds[19] & x <= thresholds[20] & Fordelinger == "Uten sensoreffekter")

# Clean, high-tech styled plot with shaded area
ggplot(data, aes(x = x, y = y, color = Fordelinger)) +
  # Add shaded area for the first distribution between thresholds
  geom_ribbon(data = shading_data, aes(x = x, ymin = 0, ymax = y), 
              fill = "#111111", alpha = 0.3, inherit.aes = FALSE) +
  
  # Add shaded area for the second distribution between thresholds
  geom_ribbon(data = shading_data2, aes(x = x, ymin = 0, ymax = y), 
              fill = "#111111", alpha = 0.3, inherit.aes = FALSE) +
  
  # Add shaded area for the first distribution between thresholds (second set of thresholds)
  geom_ribbon(data = shading_datab, aes(x = x, ymin = 0, ymax = y), 
              fill = "#A11111", alpha = 0.3, inherit.aes = FALSE) +
  
  # Add shaded area for the second distribution between thresholds (second set of thresholds)
  geom_ribbon(data = shading_data2b, aes(x = x, ymin = 0, ymax = y), 
              fill = "#A11111", alpha = 0.3, inherit.aes = FALSE) +
  
  # Main curves with transparency
  geom_line(size = 1.1, alpha = 1.2) +
  
  # Add thresholds
  geom_vline(xintercept = thresholds, color = "black", linetype = "solid", size = 0.5) +
  
  # Titles and labels
  labs(x = expression(theta-"verdier"),
       y = "Sannsynlighetstetthet") +
  
  # Customize colors
  scale_color_manual(values = c("#0072B2", "#D55E00")) + # Clean blue and orange
  
  # Customize theme with larger text
  theme_minimal(base_size = 18) + # Increase base size for better readability
  theme(
    panel.background = element_blank(),                          # White background
    panel.grid.major = element_line(color = "gray90"),           # Light grid lines
    panel.grid.minor = element_blank(),                          # Remove minor grid lines
    plot.title = element_text(color = "#333333", size = 24, face = "bold", hjust = 0.5), # Larger, bold title
    axis.title = element_text(color = "#333333", size = 18),     # Larger axis titles
    axis.text = element_text(color = "#555555", size = 16),      # Larger axis text
    legend.position = "top",                                     # Legend on top
    legend.background = element_blank(),                         # Transparent legend background
    legend.text = element_text(color = "#333333", size = 16)     # Larger legend text
  ) +
  
  # Set x and y axis limits
  xlim(-3, 3)  # Restrict x-axis from -3 to 3
