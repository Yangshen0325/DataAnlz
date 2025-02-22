## linear model and exponential model
## different models of diversity dependence affect rates

library(ggplot2)
library(reshape2)
library(patchwork)

# Define parameters
alpha <- 100
gamma_0 <- 1
K_values <- seq(1, 101, by=10)
N_values <- seq(0, 100, by=10)

# Initialize data frames to store results
results_gamma1 <- data.frame(K = numeric(), N = numeric(), Gamma = numeric())
results_gamma2 <- data.frame(K = numeric(), N = numeric(), Gamma = numeric())

# Calculate gamma values for each K and N
for (N in N_values) {
  for (K in K_values) {

    gamma1 <- ifelse(K - N > 0, gamma_0 * (1 - N / K), 0)
    gamma2 <- ifelse(K - N > 0, gamma_0 * exp(-alpha / (K - N)), 0)
    results_gamma1 <- rbind(results_gamma1, data.frame(K = K, N = N, Gamma = gamma1))
    results_gamma2 <- rbind(results_gamma2, data.frame(K = K, N = N, Gamma = gamma2))
  }
}

# Plot for the first gamma equation
plot_gamma1 <- ggplot(results_gamma1, aes(x = K, y = N, fill = Gamma)) +
  geom_tile() +
  coord_fixed() +
  scale_fill_gradient(name = expression(gamma), low = "blue", high = "red", limits = c(0, 1)) +
  geom_abline(slope = 1,
              intercept = 0,
              linetype = "dashed",
              linewidth = 0.8) +
  labs(tag = "(a)",
       x = "K",
       y = "N") +
  theme_bw() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"))

# Plot for the second gamma equation
plot_gamma2 <- ggplot(results_gamma2, aes(x = K, y = N, fill = Gamma)) +
  geom_tile() +
  coord_fixed() +
  scale_fill_gradient(name = expression(gamma), low = "blue", high = "red", limits = c(0, 1)) +
  geom_abline(slope = 1,
              intercept = 0,
              linetype = "dashed",
              linewidth = 0.8) +
  labs(tag = "(b)",
       x = "K",
       y = "N") +
  theme_bw() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"))

# Combine the plots and adjust layout
p <- plot_gamma1 + plot_gamma2 + plot_layout(ncol = 2, guides = "collect")

# Save the plot
ggsave("script/DDgamma.png", p,
       width = 6, height = 3, units = "in", dpi = 300)

ggsave("figures/DDgamma.pdf", p,
       width = 6, height = 3, units = "in", dpi = 300)

