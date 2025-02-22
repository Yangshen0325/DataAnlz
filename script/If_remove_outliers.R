
# To remove outliers

all_frequencies <- list()

for (i in 1:length(frequencies)) {
  sim_data <- frequencies[[i]]
  complete_data <- data.frame(event_id = 1:11, frequency = 0)
  complete_data$frequency[as.integer(names(sim_data))] <- as.numeric(sim_data)
  complete_data$replicate <- i
  all_frequencies[[i]] <- complete_data
}

# Combine all replicates into a single data frame
combined_frequencies <- bind_rows(all_frequencies) %>%
  mutate(event_type = factor(event_id, levels = 1:11, labels = event_labels))  # Add event labels

# Step 2: Identify outlier replicates based on frequency for each event type
outlier_replicates <- combined_frequencies %>%
  group_by(event_type) %>%
  mutate(
    Q1 = quantile(frequency, 0.25),
    Q3 = quantile(frequency, 0.75),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  ) %>%
  filter(frequency < lower_bound | frequency > upper_bound) %>%
  pull(replicate) %>%  # Extract replicate indices with outliers
  unique()  # Keep unique replicate indices only

# Step 3: Remove outlier replicates from both frequencies and island_replicates
filtered_frequencies <- frequencies[-outlier_replicates]
filtered_island_replicates <- island_replicates[-outlier_replicates]

