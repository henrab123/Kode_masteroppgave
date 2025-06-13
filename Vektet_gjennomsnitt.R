library(ggplot2)

set.seed(123)

# 10 observasjoner, litt mer spredt
n <- 10
x <- runif(n, 2.5, 4.0)       # forventningsverdier litt bredere
sd <- runif(n, 0.3, 0.7)      # litt bredere spredning p?? standardavvik
w <- 1 / sd^2

mu_hat <- sum(w * x) / sum(w)

x_range <- seq(min(x) - 1, max(x) + 1, length.out = 1000)

curve_data <- data.frame()
total_density <- rep(0, length(x_range))

for (i in 1:n) {
  density <- dnorm(x_range, mean = x[i], sd = sd[i])
  total_density <- total_density + density
  temp_df <- data.frame(x = x_range, y = density, group = paste0("Obs ", i))
  curve_data <- rbind(curve_data, temp_df)
}

total_df <- data.frame(x = x_range, y = total_density, group = "Sum")
combined_data <- rbind(curve_data, total_df)

mu_df <- data.frame(mu = mu_hat)

ggplot(combined_data, aes(x = x, y = y, group = group)) +
  geom_line(aes(color = group, linetype = ifelse(group == "Sum", "sum", "obs")), size = 1) +
  geom_vline(data = mu_df, aes(xintercept = mu), linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = mu_hat, y = max(total_density) + 0.5, 
           label = paste0("MLE = ", round(mu_hat, 2)), hjust = -0.1, angle = 90) +
  labs(title = "MLE fra 10 observasjoner med litt mer spredning",
       subtitle = "Svarte linje = sum av likelihoods, stiplet linje = vektet gjennomsnitt (MLE)",
       x = "Parameterverdi (??)",
       y = "Tetthet (ikke normalisert)") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c(rep("grey70", n), "black")) +
  scale_linetype_manual(values = c("obs" = "solid", "sum" = "twodash"), guide = "none") +
  guides(color = "none")

