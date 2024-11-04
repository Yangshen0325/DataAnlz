

# Plot function, plant and animal seperately
Stt_pa_plot <- function(infolist, title) {

  # Combine sublists into a single data frame and set factor levels for Type
  stt_data <- bind_rows(infolist, .id = "Type")
  stt_data$Type <- factor(stt_data$Type, levels = c("non_endemic_dat", "endemic_dat", "total_dat"))

  # Plot
  p <- ggplot(stt_data, aes(x = Time, y = Median, color = Type, fill = Type, linetype = Type)) +
    geom_line(aes(group = Type), linewidth = 1) +
    geom_ribbon(aes(ymin = Q0.25, ymax = Q0.75, group = Type), alpha = 0.2, color = NA) +
    scale_x_reverse() +
    scale_y_continuous(limit = c(0, 20)) +
    scale_color_manual(values = c("non_endemic_dat" = "blue",
                                  "endemic_dat" = "green",
                                  "total_dat" = "purple")) +
    scale_fill_manual(values = c("non_endemic_dat" = "blue",
                                 "endemic_dat" = "green",
                                 "total_dat" = "purple")) +
    scale_linetype_manual(values = c("non_endemic_dat" = "solid",
                                     "endemic_dat" = "solid",
                                     "total_dat" = "dashed")) +
    labs(x ="Time to present", y = "Species richness", title = title) +
    theme_minimal()

  return(p)
}

