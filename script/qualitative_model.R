# Fixed parameters
library(dplyr)
library(tidyr)
library(plotly)




# For immigration and cladogenesis ----------------------------------------


K_0 <- 50
alpha <- 100
K_1 <- 1

gamma_0 <- 1.0

get_rates_fun <- function(K_0, K_1, alpha, gamma_0, partners, present){
  denominator <- max(0, K_0 + K_1 * partners - present)
  if (denominator == 0) {
    return(0)
  }
  gamma_i <- gamma_0 * exp(-alpha / denominator)
  return(gamma_i)
}

partners <- seq(0, 100, 20)
present <- seq(0, 100, 20)

get_rate <- expand.grid(partners = partners, present = present) %>%
  rowwise() |>
  mutate(rates = get_rates_fun(K_0, K_1, alpha, gamma_0, partners, present)) |>
  ungroup()
# Convert rates into a matrix format
rates_matrix <- matrix(get_rate$rates, nrow = length(partners), ncol = length(present), byrow = TRUE)

# Horizantal surface
horizantal_surface <- matrix(gamma_0, nrow = length(partners), ncol = length(present), byrow = TRUE)
no_mutual <- get_rate %>% filter(partners == 0)

p <- plot_ly() |>
  add_surface(z = ~rates_matrix, x = ~partners, y = ~present) |>
  add_surface(z = ~horizantal_surface, x = ~partners, y = ~present, opacity = 0.8) |>
  add_trace(x = ~rep(0, length(no_mutual$present)),
            y = ~no_mutual$present,
            z = ~no_mutual$rates,
            type = "scatter3d", mode = "lines",
            line = list(color = "red", width = 15)) |>  # Red line
  layout(scene = list(
    xaxis = list(title = "Mutualistic Partners"),
    yaxis = list(title = "Present Species"),
    zaxis = list(title = "Rates (immigration/cladogenesis)"),
    camera = list(
      eye = list(x = 2.3, y = 1, z = 1)  # Adjust these to rotate the view
    )
  ))



# Extinction --------------------------------------------------------------

rm(list = ls())
library(tidyverse)

partners = seq(0, 100, 10)
mu_0 <-  1.0
mu_1 <-  c(0, 0.1, 0.05, 0.03, 0.01, 0.005)

grid <- expand.grid(partners = partners, mu_1 = mu_1)

mu_i <- grid |>
  rowwise() |>
  mutate(mu_i = mu_0 * exp(- mu_1 * partners)) |>
  ungroup()

# p <- ggplot(mu_i, aes(x = partners, y = mu_i, color = as.factor(mu_1)))+
#   geom_line(linewidth = 1.0) +
#   labs(x = "Mutualistic Partners",
#        y = "Rates (extinction)",
#        color = "Mutualism Effects") +
#   theme_minimal() +
#   theme(legend.position = "top") +
#   scale_color_manual(values = c("red", "black", "blue","yellow", "purple", "green"))

p <- ggplot(mu_i, aes(x = partners, y = mu_i, color = mu_1, group = as.factor(mu_1))) +
  geom_line(linewidth = 1.0) +
  labs(x = "Mutualistic Partners",
       y = "Rates (extinction)",
       color = "Mutualism Effects") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_gradientn(colors = c("lightblue", "blue", "darkblue"))

  p

ggsave("figures/extinction.pdf", p, width = 4, height = 4, dpi = 300)



# anagenesis --------------------------------------------------------------

rm(list = ls())

partners_diff = seq(0, 100, 10)
laa_0 <-  1.0
laa_1 <-  c(0, 0.005, 0.01, 0.015, 0.02)

grid <- expand.grid(partners_diff = partners_diff, laa_1 = laa_1)

laa_i <- grid |>
  rowwise() |>
  mutate(laa_i = laa_0 + laa_1 * partners_diff) |>
  ungroup()

p <- ggplot(laa_i, aes(x = partners_diff, y = laa_i, color = laa_1, group = as.factor(laa_1)))+
  geom_line(linewidth = 1.0) +
  labs(x = "Mutualistic Partners Difference",
       y = "Rates (anagenesis)",
       color = "Mutualism Effects") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_gradientn(colors = c("lightyellow", "orange", "darkorange"))

ggsave("figures/anagenesis.pdf", p, width = 4, height = 4, units = "in", dpi = 300)

# cospeciation ------------------------------------------------------------

rm(list = ls())

# Define parameters
lambda_0 <- 1.0
alpha <- 100
K_P0 <- 100
K_A0 <- 100

# Create grid for N_P and N_A from 0 to 100
present_P <-seq(0, 100)
present_A <-seq(0, 100)

grid <- expand.grid(present_P = present_P, present_A = present_A)

# Compute cospeciation rates based on the formula
grid <- grid  |>
  mutate(
    min_capacity = pmin(K_P0 - present_P, K_A0 - present_A),
    lambda_ij = ifelse(
      (K_P0 > present_P) & (K_A0 > present_A),
      lambda_0 * exp(-alpha / pmax(min_capacity, 1)),
      0  # Set to 0 when conditions are not met
    )
  )

# Heatmap + Contour plot
p <- ggplot(grid, aes(x = present_P, y = present_A, fill = lambda_ij)) +
  geom_raster() +  # Heatmap of cospeciation rates
  scale_fill_viridis_c(option = "plasma", name = "Cospeciation Rate") +
  labs(
    title = "Effect of Available Space on Cospeciation Rates",
    x = "Present Plant Species",
    y = "Present Animal Species"
  ) +
  theme_minimal()



ggsave("figures/cospeciation.pdf",  width = 6, height = 4, p, dpi = 300)













