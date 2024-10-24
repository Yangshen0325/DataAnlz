


rm(list = ls())

the_path <- "script/mu1_0/laa1_0lambda0_0/"

# List of dataset file paths
data_sets <- c(paste0(the_path, "none.rds"),
               paste0(the_path, "low.rds"),
               paste0(the_path, "medium.rds"),
               paste0(the_path, "high.rds"))

# Apply the function to each dataset and combine the results
all_results <- lapply(data_sets, get_sr_info)

# Combine all results into a single data frame and add a column to identify the dataset
final_table <- bind_rows(all_results, .id = "dataset")

# Add a label column to identify the dataset
dataset_labels <- c("none", "low", "medium", "high")
final_table$dataset <- dataset_labels[as.numeric(final_table$dataset)]

final_table <- final_table |>
  mutate(island_total = island_p + island_a,
         island_endemic_total = island_endemic_p + island_endemic_a,
         island_nonendemic_total = island_nonendemic_p + island_nonendemic_a)

final_table$dataset <- factor(final_table$dataset, levels = c("none", "low", "medium", "high"))

ggplot(final_table, aes(x = dataset, y = island_total)) +
  geom_boxplot()



