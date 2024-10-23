

# combine all plots (16), total --------------------------------------------------
rm(list = ls())
library(patchwork)
library(tidyverse)
library(rlang)


# Function for plot
crt_stt_plot <- function(data, x_var, y_var, q25_var, q75_var,
                         color_var = NULL, fill_var = NULL,
                         show_legend= NULL) {

  p <- ggplot(data, aes(x = !!rlang::sym(x_var),
                        y = !!rlang::sym(y_var),
                        color = !!rlang::sym(color_var),
                        fill = !!rlang::sym(fill_var))) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = !!rlang::sym(q25_var), ymax = !!rlang::sym(q75_var)), alpha = 0.2, color = NA) +
    scale_x_reverse() +
    scale_y_continuous(limits = c(0, 20)) +
    theme_bw() +
    scale_color_manual(values = c("black", "#006837", "#253494", "red")) +
    scale_fill_manual(values = c("black", "#006837", "#253494", "red")) +
    labs(x = "Time to present",
         y = "No.species") +
    theme(axis.title.x = element_text(vjust = -2, size = 12, face="bold"), # x label's attributes
          axis.title.y = element_text(vjust = 2, size = 12, face="bold"),
          axis.text = element_text(size = 10, face="bold"), # axes tick labels
          axis.ticks.x = element_blank(),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12, face = "bold"),
          legend.key.size = unit(0.3, 'cm'))

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }

  return(p)

}

# Define the function to create a plot from a given path
create_plot <- function(path, show_legend) {

  list_all <- readRDS(file.path(path, "list_all.rds"))

  # Filter and process the data
  total <- list_all |>
    filter(Type == "total_dat")
  total$Group <- factor(total$Group, levels = c("None", "Low", "Medium", "High"))


  # endemic species
  # endemics <- list_all |>
  #   filter(Type == "endemic_dat")
  # endemics$Group <- factor(endemics$Group, levels = c("None", "Low", "Medium", "High"))

  # Create the plot
  p <- crt_stt_plot(data = total,
                    x_var = "Time",
                    y_var = "Median",
                    q25_var = "Q0.25",
                    q75_var = "Q0.75",
                    color_var = "Group",
                    fill_var = "Group",
                    show_legend = show_legend)

  return(p)
}

