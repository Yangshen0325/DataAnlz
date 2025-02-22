

## Data can be from:
# `island_parts`, and it includes `Mt`, `M_true_list`, `status_p`, `status_a`,
# `island_spec`, `island`, `evo_table`.
# Plot options in data from `island_parts` are:
# "stt_pa": plant and animal species richness through time, respectively
# "stt_all": plant + animal species richness through time
# "event_frequency": average frequency of events happened on the island across simulation replicates
# "links_over_time": average total links on the island over time across simulation replicates

# `additional parts`,  and it includes `rates_list`, `timeval_list` , `richness_p_list`,
# `richness_a_list`, `sum_partners_p`, `sum_partners_a`
# Plot options in data from `additional part` are:
# "rates_over_time"：average the sum of each type of rates over time across simulation replicates
# "partners_over_time"：average the sum of partners over time across simulation replicates

rm(list = ls())

# library necessary packages
library(tidyverse)
library(patchwork)

# load necessary packages
devtools::load_all("~/Downloads/phd_yang/pkgs/specmutual")

# read M0
M0 <- readRDS("script/mu1_0/M0.rds")

total_time <- 10
replicates <- 100
sample_freq <- 50

mutualism_pars <- create_mutual_pars(
  lac_pars = c(0.6, 0.6),
  mu_pars = c(0, 0, 0, 0),
  K_pars = c(50, 50, 100, 100),
  gam_pars = c(0.016, 0.016),
  laa_pars = c(1.0, 1.0, 0.01, 0.01),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.5,
  M0 = M0,
  transprob = 1,
  alpha = 100)

seed <- as.integer(Sys.time()) %% .Machine$integer.max
set.seed(seed)
cat("The seed is:", seed, "\n")
# The seed is: 1730744469

island_parts <- peregrine_sim(total_time = total_time,
                              replicates = replicates,
                              mutualism_pars = mutualism_pars,
                              return_parts = "island_parts",
                              verbose = FALSE)

set.seed(seed)

additional_parts <- peregrine_sim(total_time = total_time,
                                  replicates = replicates,
                                  mutualism_pars = mutualism_pars,
                                  return_parts = "additional_parts",
                                  verbose = FALSE)

pa_9 <- plot_data(data = "from_island_parts", plot_the = "stt_pa")
all_9 <- plot_data(data = "from_island_parts", plot_the = "stt_all")
evt_9 <- plot_data(data = "from_island_parts", plot_the = "event_frequency")
link_9 <- plot_data(data = "from_island_parts", plot_the = "links_over_time")

rates_7 <- plot_data(data = "from_additional_parts", plot_the = "rates_over_time")




pa_3 + pa_14 + pa_4
all_3 + all_14 + all_4

evt_3 + evt_14 + evt_4
link_3 + link_14 + link_4

link_3 + link_4
rates_1 + rates_2

link_4 + link_6

pa_4 + all_4



# plot_data(data = "from_additional_parts", plot_the = "partners_over_time")
# this part doesn't make really sense
# For example
# partners_list <- get_partners(
#   Mt = Mt,
#   status_p = status_p,
#   status_a = status_a
# )
#
# sum_partners_p[[length(sum_partners_p) + 1]] <- sum(partners_list$partners_p)
# sum_partners_a[[length(sum_partners_a) + 1]] <- sum(partners_list$partners_a)
# Suppose an animal immigrated to the island, partners for each animals would be 0,
# and the `sum_partners_a = 0`, there may be 5 plants interacts with this animal,
# For species in the mainland, each of the 5 species has 1 partner, sum_partners_p = 5.










