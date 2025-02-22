
## Plot the species richness on the island across different values of mutualism-related parameters.
# It has nine scenarios.


# Necessary packages
library(tidyverse)
library(dplyr)


# 01 Deal with data. Read all paths containing data sets
all_paths <- c("script/mu1_01/l0_0l1_0/",
               "script/mu1_01/l0_0.1l1_0/",
               "script/mu1_01/l0_1l1_0/",
               "script/mu1_01/l0_0l1_0.01/",
               "script/mu1_01/l0_0.1l1_0.01/",
               "script/mu1_01/l0_1l1_0.01/",
               "script/mu1_01/l0_0l1_0.1/",
               "script/mu1_01/l0_0.1l1_0.1/",
               "script/mu1_01/l0_1l1_0.1/"


              )

all_data <- lapply(all_paths, function(path) {
  get_sr_all(path) |> mutate(path = path) # Add the path as a column for check
})

# 01 Deal with data. Combine all results into one data frame
final_data <- bind_rows(all_data)


# 02 Plot

  # Prepare the data
  plot_data <- final_data  |>
    group_by(path, effects)  |>
    summarise(
      avg_plant = mean(island_p, na.rm = TRUE),
      avg_animal = mean(island_a, na.rm = TRUE)
    ) |>
    pivot_longer(
      cols = c(avg_plant, avg_animal),
      names_to = "species",
      values_to = "value"
    )

  # Adjust species names for better labels
  plot_data$species <- recode(plot_data$species,
                              avg_plant = "Plant",
                              avg_animal = "Animal")
  plot_data$species <- factor(plot_data$species, levels = c("Plant", "Animal"))

  # Define labels for scenarios
  scenario_labels <- c(
    "script/mu1_01/l0_0l1_0/" = "lambda[0] == 0 ~ ',' ~ lambda[1]^a == 0",
    "script/mu1_01/l0_0.1l1_0/" = "lambda[0] == 0.1 ~ ',' ~ lambda[1]^a == 0",
    "script/mu1_01/l0_1l1_0/" = "lambda[0] == 1.0 ~ ',' ~ lambda[1]^a == 0",
    "script/mu1_01/l0_0l1_0.01/" = "lambda[0] == 0 ~ ',' ~ lambda[1]^a == 0.01",
    "script/mu1_01/l0_0.1l1_0.01/" = "lambda[0] == 0.1 ~ ',' ~ lambda[1]^a == 0.01",
    "script/mu1_01/l0_1l1_0.01/" = "lambda[0] == 1.0 ~ ',' ~ lambda[1]^a == 0.01",
    "script/mu1_01/l0_0l1_0.1/" = "lambda[0] == 0 ~ ',' ~ lambda[1]^a == 0.1",
    "script/mu1_01/l0_0.1l1_0.1/" = "lambda[0] == 0.1 ~ ',' ~ lambda[1]^a == 0.1",
    "script/mu1_01/l0_1l1_0.1/" = "lambda[0] == 1.0 ~ ',' ~ lambda[1]^a == 0.1"
  )

  # Convert `path` to factor with parsed labels
  plot_data$path <- factor(
    plot_data$path,
    levels = names(scenario_labels),
    labels = scenario_labels
  )

  # Plot with facet_wrap
 p  <- ggplot(plot_data, aes(x = effects, y = value, fill = species)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    scale_fill_manual(values = c("darkgreen", "orange")) +  # Custom colors for Plant and Animal
   scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, 25)) +
    labs(
      x = "Mutualism Effects",
      y = "Number of Species",
      fill = "Species",
      title = expression("When "~ mu[1]== 0.1)
    ) +
    facet_wrap(~ path, ncol = 3, labeller = labeller(path = label_parsed)) +
    theme_minimal(base_size = 12) +
    theme(
      aspect.ratio = 3/4,  # Aspect ratio of each subplot
      strip.text = element_text(size = 10, face = "bold"),  # Customize facet labels
      legend.position = "bottom",  # Shared legend at the bottom
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)  # Frames around subplots
    )

  ggsave("script/mu1_01/plot_sr_bar.png", p, width = 8, height = 8, units = "in",
         dpi = 300)






