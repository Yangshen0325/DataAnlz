# List of all paths
path_1 <- "~/Downloads/phd_yang/chapter2/DataAnlz/script/"
path_2 <- "mu1_0/"

all_paths <- c(paste0(path_1, path_2, "laa1_0lambda0_0/"),
               paste0(path_1, path_2, "laa1_0lambda0_0.001/"),
               paste0(path_1, path_2, "laa1_0lambda0_0.005/"),
               paste0(path_1, path_2, "laa1_0lambda0_0.025/"),
               paste0(path_1, path_2, "laa1_0.002lambda0_0/"),
               paste0(path_1, path_2, "laa1_0.002lambda0_0.001/"),
               paste0(path_1, path_2, "laa1_0.002lambda0_0.005/"),
               paste0(path_1, path_2, "laa1_0.002lambda0_0.025/"),
               paste0(path_1, path_2, "laa1_0.004lambda0_0/"),
               paste0(path_1, path_2, "laa1_0.004lambda0_0.001/"),
               paste0(path_1, path_2, "laa1_0.004lambda0_0.005/"),
               paste0(path_1, path_2, "laa1_0.004lambda0_0.025/"),
               paste0(path_1, path_2, "laa1_0.008lambda0_0/"),
               paste0(path_1, path_2, "laa1_0.008lambda0_0.001/"),
               paste0(path_1, path_2, "laa1_0.008lambda0_0.005/"),
               paste0(path_1, path_2, "laa1_0.008lambda0_0.025/")
)


# Generate all plots
all_plots <- lapply(1:length(all_paths), function(i) {
  create_plot(all_paths[i], show_legend = FALSE)
})

# Customize axis labels and ticks
for (i in 1:length(all_plots)) {
  if (i %% 4 != 1) {
    all_plots[[i]] <- all_plots[[i]] + theme(axis.title.y = element_blank(),
                                             axis.text.y = element_blank(),
                                             axis.ticks.y = element_blank())
  }
  if (i <= 12) {
    all_plots[[i]] <- all_plots[[i]] + theme(axis.title.x = element_blank(),
                                             axis.text.x = element_blank(),
                                             axis.ticks.x = element_blank())
  }
}

# Arrange the plots in a 4x4 layout
title_expression <- bquote(
  bold(
    .(quote(mu[1])) ~ "= 0, " ~
      .(quote(lambda[0])) ~ "0, 0.001, 0.005, 0.01, " ~
      .(quote(lambda[1]^a)) ~ "0, 0.001, 0.005, 0.01"
  )
)
combined_plot <- wrap_plots(all_plots, ncol = 4, nrow = 4) +
  plot_annotation(
    title = title_expression,
    theme = theme(plot.title = element_text(hjust = 0.5,
                                            size = 12,
                                            face = "bold"))
  )
ggsave("~/Downloads/phd_yang/chapter2/when_I_Explore/16plot_mu1_0_K1_052080_alpha50_gam016.png",
       plot = combined_plot, width = 20, height = 20, dpi = 300)


