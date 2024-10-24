
#### To visualize the events frequency across all simulations####

# Extract `evo_table`
# `evo_table[, 1]` : time series until the `total_time`
# `evo_table[, 2]` : event happening at the specific `time_value`





#' This function visualizes the events frequency across all simulations
#'
#' @param the_data Data being handled
#'
#' @return A ggplot
#' @export
#'

plot_event_freq <- function(the_data) {

  # Initialize a list to store the frequency of each number in the second column
  all_frequencies <- list()

  for (i in seq_len(length(the_data))) {

    # Extract the second column of the evo_table
    second_column <- the_data[[i]][["evo_table"]][[1]][, 2]

    # Calculate the frequency of each unique number
    freq_table <- table(second_column)

    # Store the frequency table in the list
    all_frequencies[[i]] <- as.data.frame(freq_table)
  }

  # Combine all frequency tables and calculate the average frequency
  combined_frequencies <- do.call(rbind, all_frequencies)

  # Aggregate the frequencies by the second_column values and compute the mean frequency
  average_frequencies <- aggregate(Freq ~ second_column, data = combined_frequencies, FUN = mean)

  average_frequencies$second_column <- factor(average_frequencies$second_column,
                                              levels = c(1:11))

  x_labels <- c("ip", "ep", "cp", "ap", "ia", "ea", "ca",
                "aa", "co", "ga", "lo")
  # Plot
  p <- ggplot(average_frequencies, aes(x = second_column, y = Freq)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Average Frequency of Numbers in the Second Column",
         x = "Event",
         y = "Average Frequency") +
    scale_x_discrete(labels = x_labels) +
    theme_minimal()

  return(p)

}
