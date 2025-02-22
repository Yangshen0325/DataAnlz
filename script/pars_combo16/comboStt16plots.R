
## Plot species richness through time for 16 scenarios. Four initial macro-evolutionary
## rates: immigration, extinction, anagenesis, and cladogenesis. Each of them has two
## values: low and high. Each scenario takes into "none", "medium" and "high" mutualism effects

# `species_type` options: "all", "plant", "animal"
# `data_type` options: "non_endemic_dat", "endemic_dat", "total_dat"

species_type <- "all"
data_type <- "total_dat"
M0 <- readRDS("script/mu1_0/M0.rds")
total_time <- 10
sample_freq <- 50
y_limit <- 2000 # change it after checking the data


all_paths <- c("script/pars_combo16/low_cu_high_ra/",
               "script/pars_combo16/low_c_high_ura/",
               "script/pars_combo16/high_cra_low_u/",
               "script/pars_combo16/high_cura/",
               "script/pars_combo16/low_cua_high_r/",
               "script/pars_combo16/low_ca_high_ur/",
               "script/pars_combo16/high_cr_low_ua/",
               "script/pars_combo16/high_cur_low_a/",
               "script/pars_combo16/low_cur_high_a/",
               "script/pars_combo16/low_cr_high_ua/",
               "script/pars_combo16/high_ca_low_ur/",
               "script/pars_combo16/high_cua_low_r/",
               "script/pars_combo16/low_cura/",
               "script/pars_combo16/low_cra_high_u/",
               "script/pars_combo16/high_c_low_ura/",
               "script/pars_combo16/high_cu_low_ra/")

all_cases <- c("low_cu_high_ra",
               "low_c_high_ura",
               "high_cra_low_u",
               "high_cura",
               "low_cua_high_r",
               "low_ca_high_ur",
               "high_cr_low_ua",
               "high_cur_low_a",
               "low_cur_high_a",
               "low_cr_high_ua",
               "high_ca_low_ur",
               "high_cua_low_r",
               "low_cura",
               "low_cra_high_u",
               "high_c_low_ura",
               "high_cu_low_ra")

all_plots <- list()
for ( i in seq_along(all_paths)) {

  the_path <- all_paths[i]
  the_case <- all_cases[i]
  all_plots[[i]] <- generate_combined_plot(the_path = the_path,
                                           the_case = the_case,
                                           species_type = species_type,
                                           data_type = data_type,
                                           M0 = M0,
                                           y_limit = y_limit,
                                           total_time = total_time,
                                           sample_freq = sample_freq,
                                           show_legend = TRUE)

}


for (i in 1:16) {
  if (i %% 4 != 1) {
    all_plots[[i]] <- all_plots[[i]] + theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  if (i <= 12) {
    all_plots[[i]] <- all_plots[[i]] + theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }
}

library(patchwork)

p <- wrap_plots(all_plots, ncol = 4, nrow = 4) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("script/pars_combo16/combo16Stt.png", p,
       width = 10, height = 10, units = "in", dpi = 300)

ggsave("figures/stt_total.pdf", p,
       width = 10, height = 10, units = "in",dpi = 300)

# wrap_plots(all_plots, ncol = 4, nrow = 4) +
#   plot_annotation(
#     title = expression(bold(gamma[0]) ~ "," ~ bold(lambda[0]^a) ~ "," ~ bold(mu[0]) ~ "," ~ bold(lambda[0]^c)),
#     theme = theme(plot.title = element_text(hjust = 0.5,
#                                             size = 12,
#                                             face = "bold"))
#   )
#










