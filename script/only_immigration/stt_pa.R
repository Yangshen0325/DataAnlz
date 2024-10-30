

#### Run it locally, for checking ####



## Turn off everything except immigration
rm(list = ls())

# library necessary packages
library(tidyverse)
library(patchwork)

# load necessary packages
devtools::load_all("~/Downloads/phd_yang/pkgs/specmutual")

# read M0
M0 <- readRDS("script/mu1_0/M0.rds")

mutualism_pars <- create_mutual_pars(
  lac_pars = c(0, 0),
  mu_pars = c(0, 0, 0, 0),
  K_pars = c(50, 50, 0, 0),
  gam_pars = c(0.016, 0.016),
  laa_pars = c(0, 0, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = M0,
  transprob = 0,
  alpha = 100)

total_time <- 10
replicates <- 50
sample_freq <- 50

seed <- as.integer(Sys.time()) %% .Machine$integer.max
set.seed(seed)
cat("The seed is:", seed, "\n")

out <- peregrine_sim(total_time = total_time,
                    replicates = replicates,
                    mutualism_pars = mutualism_pars,
                    return_parts = "island_parts",
                    verbose = FALSE)

# saveRDS(out, "script/only_immigration/out.rds")

# Format the output
cladeinfo <- specmutual:::format_island_mutual_pa(island_replicates = out,
                                                  total_time = total_time,
                                                  sample_freq = sample_freq,
                                                  M0 = M0,
                                                  verbose = FALSE)

# Get `Median` `Q0.25` and `Q0.75` for plant and animal species, seprately
infolist_plant <- get_endemism_list(cladeinfo = cladeinfo ,
                              species_type = "plant",
                              sample_freq = sample_freq)

infolist_animal <- get_endemism_list(cladeinfo = cladeinfo ,
                                    species_type = "animal",
                                    sample_freq = 50)


# Plot function
Stt_pa_plot <- function(infolist, title) {

  # Combine sublists into a single data frame and set factor levels for Type
  stt_data <- bind_rows(infolist, .id = "Type")
  stt_data$Type <- factor(stt_data$Type, levels = c("non_endemic_dat", "endemic_dat", "total_dat"))

  # Plot
  p <- ggplot(stt_data, aes(x = Time, y = Median, color = Type, fill = Type, linetype = Type)) +
    geom_line(aes(group = Type), size = 1) +
    geom_ribbon(aes(ymin = Q0.25, ymax = Q0.75, group = Type), alpha = 0.2, color = NA) +
    scale_x_reverse() +
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


plant_stt_plot <- Stt_pa_plot(infolist_plant, title = "Plant STT")
animal_stt_plot <- Stt_pa_plot(infolist_animal, title = "Animal STT")

plant_stt_plot + animal_stt_plot + plot_layout(guides = "collect")


