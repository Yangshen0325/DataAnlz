

# Rates and richness dynamics across different scenarios ------------------

rm(list = ls())


M0 <- readRDS("script/mu1_0/M0.rds")

# Define the K1 values
K1_value <- c(0, 1, 5, 25)

# Initialize an empty list to store the results for each K1 value
results_list <- list()

set.seed(1234)

for (K1 in K1_value){

  mutualism_pars <- specmutual::create_mutual_pars(
    lac_pars = c(0.3, 0.3),
    mu_pars = c(0.2, 0.2, 0, 0),
    K_pars = c(50, 50, K1, K1),
    gam_pars = c(0.016, 0.016),
    laa_pars = c(0.1, 0.1, 0, 0),
    qgain = 0.001,
    qloss = 0.001,
    lambda0 = 0.01,
    M0 = M0,
    transprob = 1.0,
    alpha = 50)

  # Run the simulation for the current K1 value
  the_data <- rates_richness_dynamic(total_time = 10,
                                     replicates = 1,
                                     mutualism_pars = mutualism_pars)
  # Process the data for plotting
  # 11 kinds of rates at every time value
  rates_rep <- the_data$rates_reps[[1]]

  # Every time value
  timeval_reps <- the_data$timeval_reps[[1]]

  # Plant species richness
  status_p_reps <- the_data[["status_p_reps"]][[1]]

  # Animal species richness
  status_a_reps <- the_data[["status_a_reps"]][[1]]

  # The sum of each kind of rate across each time value
  rates_each_sum <- lapply(rates_rep, function(x) sapply(x, sum))
  rates_each_sum <- do.call(rbind, rates_each_sum[-length(rates_each_sum)])

  # Convert rates to a data frame and add timeval, plant, and animal richness
  data_sum_df <- as.data.frame(rates_each_sum)
  data_sum_df$timeval <- unlist(timeval_reps[-length(timeval_reps)])  # Add time values as a new column
  data_sum_df$richness_p <- unlist(status_p_reps)  # Add plant richness
  data_sum_df$richness_a <- unlist(status_a_reps)  # Add animal richness

  # Add the current K1 value to the data for identification
  data_sum_df$K1 <- K1

  # Store the processed data in the list
  results_list[[as.character(K1)]] <- data_sum_df

}

# Combine the results from all K1 values into a single data frame
combined_data <- bind_rows(results_list, .id = "K1")
# Convert K1 to a factor for better plotting
combined_data$K1 <- factor(combined_data$K1, levels = K1_value)


# Plot plant, as a whole. Plot animal, as a whole -------------------------------

# Function to plot

plot_rate_richns <- function(data, richness_col, immig_col, ext_col,
                             clado_col, ana_col, K1_col,
                             secd_ylabel,
                             title) {

  richness_max <- max(data[[richness_col]], na.rm = TRUE)  # Maximum richness value
  rate_max <- max(data[[immig_col]], data[[ext_col]], data[[clado_col]], data[[ana_col]], na.rm = TRUE)  # Maximum rate value

  scaling_factor <- richness_max / rate_max  # Calculate scaling factor to match both axes

  # Create the plot
  plot <- ggplot(data, aes(x = timeval)) +

    # Define the rate lines with consistent colors
    geom_line(aes_string(y = immig_col, color = "'Immigration'"), linewidth = 0.8) +
    geom_line(aes_string(y = ext_col, color = "'Extinction'"), linewidth = 0.8) +
    geom_line(aes_string(y = clado_col, color = "'Cladogenesis'"), linewidth = 0.8) +
    geom_line(aes_string(y = ana_col, color = "'Anagenesis'"), linewidth = 0.8) +

    # Plot richness on the secondary y-axis with independent scaling
    geom_line(aes(y = !!rlang::sym(richness_col) / scaling_factor),
              color = "purple",
              linetype = "dashed",
              linewidth = 1.5) +

    # Define custom color scheme
    scale_color_manual(values = c("Immigration" = "green",
                                  "Extinction" = "red",
                                  "Cladogenesis" = "orange",
                                  "Anagenesis" = "blue")) +

    # Set the range for the primary y-axis (rates) and secondary y-axis (richness)
    scale_y_continuous(
      name = "Rates",
      limits = c(0, max(rate_max) + 1),  # Limits for rates
      sec.axis = sec_axis(~ . * scaling_factor,
                          name = secd_ylabel,
                          labels = label_number())
    ) +

    # Facet by K1 values
    facet_wrap(as.formula(paste("~", K1_col))) +

    labs(title = title,
         x = "Simulation Time",
         color = "Rate Type") +

    # Adjust theme for better readability
    theme_bw() +
    theme(
      axis.title.y.right = element_text(color = "purple"),  # Right y-axis title color
      axis.text.y.right = element_text(color = "purple"),   # Right y-axis text color
      axis.title.y = element_text(size = 12, face = "bold"),  # Left y-axis title
      axis.text.y = element_text(size = 10),  # Left y-axis text
      legend.position = "right"
    )

  return(plot)
}

# For plant, rates across different K1 with the second y-axis richness


plot_K1_rate_richns_p <- plot_rate_richns(data = combined_data,
                                          richness_col = "richness_p",
                                          immig_col = "immig_p",
                                          ext_col = "ext_p",
                                          clado_col = "clado_p",
                                          ana_col = "ana_p",
                                          K1_col = "K1",
                                          secd_ylabel = "Plant Richness",
                                          title = "Plant, Rates & Richness Over Time across K1")

ggsave("~/Downloads/phd_yang/chapter2/when_I_Explore/P_K1_052080_SR_alpha50_gam016.png",
       plot = plot_K1_rate_richns_p, width = 10, height = 10, dpi = 300)

plot_K1_rate_richns_a <- plot_rate_richns(data = combined_data,
                                          richness_col = "richness_a",
                                          immig_col = "immig_a",
                                          ext_col = "ext_a",
                                          clado_col = "clado_a",
                                          ana_col = "ana_a",
                                          K1_col = "K1",
                                          secd_ylabel = "Animal Richness",
                                          title = "Animal, Rates & Richness Over Time across K1")

ggsave("~/Downloads/phd_yang/chapter2/when_I_Explore/A_K1_052080_SR_alpha50_gam016.png",
       plot = plot_K1_rate_richns_a, width = 10, height = 10, dpi = 300)


# Plot immigration rates as well as richness over time, plant & animal -------------------

#### Plot function
# brewer.pal(11, "PiYG")
# brewer.pal(11, "RdBu")

plot_fun <- function(data, y_value, colors, title, y_label) {

  ggplot(data, aes(x = timeval, y = {{y_value}}, color = K1)) +
    geom_line(linewidth = 1.5) +
    scale_y_continuous(limits = c(min(data[[deparse(substitute(y_value))]], na.rm = TRUE) - 0.05,
                                  max(data[[deparse(substitute(y_value))]], na.rm = TRUE) + 0.05)) +
    scale_color_manual(values = colors) +
    labs(title = title,
         x = "Simulation Time",
         y = y_label) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10, face = "bold"))
}

p_immig_dnmc <- plot_fun(data = combined_data,
                         y_value = immig_p,
                         colors = c("#B8E186", "#7FBC41", "#4D9221", "#276419"),
                         title = "P immig over time",
                         y_label = "Plant immigration rates")

p_richns_dnmc <- plot_fun(data = combined_data,
                          y_value = richness_p,
                          colors = c("#B8E186", "#7FBC41", "#4D9221", "#276419"),
                          title = "P richness over time",
                          y_label = "Plant Richness")

a_immig_dnmc <- plot_fun(data = combined_data,
                         y_value = immig_a,
                         colors = c("#92C5DE", "#4393C3", "#2166AC", "#053061"),
                         title = "A immig over time",
                         y_label = "Animal immigration rates")

a_richns_dnmc <- plot_fun(data = combined_data,
                          y_value = richness_a,
                          colors = c("#92C5DE", "#4393C3", "#2166AC", "#053061"),
                          title = "A richness over time",
                          y_label = "Animal Richness")

# Combine immigration rates with richness together
plot_immig <- p_immig_dnmc + p_richns_dnmc +
  a_immig_dnmc + a_richns_dnmc+ plot_layout(guide = "collect")

# Save the combined plot
ggsave("~/Downloads/phd_yang/chapter2/when_I_Explore/K1_052080_alpha50_gam016_immig.png",
       plot = plot_immig, width = 10, height = 10, dpi = 300)

# Plot extinction rates as well as richness over time, plant & animal --------------------
p_ext_dnmc <- plot_fun(data = combined_data,
                       y_value = ext_p,
                       colors = c("#B8E186", "#7FBC41", "#4D9221", "#276419"),
                       title = "P ext over time",
                       y_label = "Plant extinction rates")

a_ext_dnmc <- plot_fun(data = combined_data,
                       y_value = ext_a,
                       colors = c("#92C5DE", "#4393C3", "#2166AC", "#053061"),
                       title = "A ext over time",
                       y_label = "Animal extinction rates")

# Combine extinction rates with richness together
plot_ext <- p_ext_dnmc + p_richns_dnmc +
  a_ext_dnmc + a_richns_dnmc+ plot_layout(guide = "collect")

# Save the combined plot
ggsave("~/Downloads/phd_yang/chapter2/when_I_Explore/K1_052080_alpha50_gam016_ext.png",
       plot = plot_ext, width = 10, height = 10, dpi = 300)


# Plot cladogenesis rates as well as richness over time, plant & animal--------

p_clado_dnmc <- plot_fun(data = combined_data,
                         y_value = clado_p,
                         colors = c("#B8E186", "#7FBC41", "#4D9221", "#276419"),
                         title = "P clado over time",
                         y_label = "Plant cladogenesis rates")

a_clado_dnmc <- plot_fun(data = combined_data,
                         y_value = clado_a,
                         colors = c("#92C5DE", "#4393C3", "#2166AC", "#053061"),
                         title = "A clado over time",
                         y_label = "Animal cladogenesis rates")

# Combine cladogenesis rates with richness together
plot_clado <- p_clado_dnmc + p_richns_dnmc +
  a_clado_dnmc + a_richns_dnmc+ plot_layout(guide = "collect")

# Save the combined plot
ggsave("~/Downloads/phd_yang/chapter2/when_I_Explore/K1_052080_alpha50_gam016_clado.png",
       plot = plot_clado, width = 10, height = 10, dpi = 300)


# Plot anagenesis rates as well as richness over time, plant & animal----------

p_ana_dnmc <- plot_fun(data = combined_data,
                       y_value = ana_p,
                       colors = c("#B8E186", "#7FBC41", "#4D9221", "#276419"),
                       title = "P ana over time",
                       y_label = "Plant anagenesis rates")

a_ana_dnmc <- plot_fun(data = combined_data,
                       y_value = ana_a,
                       colors = c("#92C5DE", "#4393C3", "#2166AC", "#053061"),
                       title = "A ana over time",
                       y_label = "Animal anagenesis rates")

# Combine cladogenesis rates with richness together
plot_ana <- p_ana_dnmc + p_richns_dnmc +
  a_ana_dnmc + a_richns_dnmc+ plot_layout(guide = "collect")

# Save the combined plot
ggsave("~/Downloads/phd_yang/chapter2/when_I_Explore/K1_052080_alpha50_gam016_ana.png",
       plot = plot_ana, width = 10, height = 10, dpi = 300)










# Plot cospeciation rates over time -------------

cospec_dnmc <- plot_fun(data = combined_data,
                        y_value = cospec_rate,
                        colors = c("#EF6548", "#D7301F", "#B30000", "#7F0000"),
                        title = "cospec over time",
                        y_label = "Cospeciation rates")

ggsave("~/Downloads/phd_yang/chapter2/when_I_Explore/K1_01525_alpha50_cospec.png",
       plot = cospec_dnmc, width = 10, height = 10, dpi = 300)


# Plot gain rates, loss rates over time -----------------------------------
gain_dnmc <- plot_fun(data = combined_data,
                      y_value = gain_rate,
                      colors = c( "#807DBA", "#6A51A3", "#54278F", "#3F007D"),
                      title = "gain over time",
                      y_label = "Gain rates")

loss_dnmc <- plot_fun(data = combined_data,
                      y_value = loss_rate,
                      colors = c( "#737373", "#525252", "#252525", "#000000"),
                      title = "loss over time",
                      y_label = "Loss rates")

p_combi <- gain_dnmc + loss_dnmc + plot_layout(guides = "collect")


ggsave("~/Downloads/phd_yang/chapter2/when_I_Explore/K1_052080_alpha50_gam016_GL.png",
       plot = p_combi, width = 10, height = 10, dpi = 300)
