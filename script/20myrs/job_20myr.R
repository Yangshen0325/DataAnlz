
#### What if it's 50 myrs

# load necessary packages
library(specmutual)

# read M0
M0 <- readRDS("~/script/M0.rds")

total_time <- 20
replicates <- 100

mutualism_pars <- create_mutual_pars(
  lac_pars = c(0.3, 0.3),
  mu_pars = c(0.05, 0.05, 0.01, 0.01),
  K_pars = c(50, 50, 100, 100),
  gam_pars = c(0.008, 0.008),
  laa_pars = c(0.5, 0.5, 0.01, 0.01),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.5,
  M0 = M0,
  transprob = 1,
  alpha = 100)

island_parts <- specmutual::peregrine_sim(total_time = total_time,
                              replicates = replicates,
                              mutualism_pars = mutualism_pars,
                              return_parts = "island_parts",
                              verbose = 1)

saveRDS(island_parts, file = "~/script/ispand_parts.rds")

