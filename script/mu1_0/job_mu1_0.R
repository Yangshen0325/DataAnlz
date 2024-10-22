## case when mu1 = 0

library(specmutual)
library(DataAnlz)
# mu1 <- 0

# Base directory to save results
base_dir <- "~/DataAnlz/script/mu1_0"

### Create parameters combination， function
create_parms <- function (laa1, mu1, lambda0, K1) {

  mutualism_pars <- specmutual::create_mutual_pars(
    lac_pars = c(0.8, 0.8),
    mu_pars = c(0.2, 0.2, mu1, mu1),
    K_pars = c(50, 50, K1, K1),
    gam_pars = c(0.016, 0.016),
    laa_pars = c(1.0, 1.0, laa1, laa1),
    qgain = 0.001,
    qloss = 0.001,
    lambda0 = lambda0,
    M0 = M0,
    transprob = 1.0,
    alpha = 100)

  return(mutualism_pars)

}

# read M0
M0 <- readRDS("~/DataAnlz/script/mu1_0/M0.rds")

par_combo <- function(mu1,
                      K1_values,
                      K1_labels,
                      laa1_values,
                      lambda0_values) {

  ## set seed
  set_random_seed <- function() {
    seed <- as.integer(runif(1, min = 1, max = .Machine$integer.max))
    set.seed(seed)
    return(seed)
  }
  seed <- set_random_seed()
  cat("Random seed set to:", seed, "\n")

  for (laa1 in laa1_values) {
    for(lambda0 in lambda0_values) {
      # Create a directory for each combination of laa1 and lambda0
      combo_dir <- file.path(base_dir, paste0("laa1_", laa1, "lambda0_", lambda0))
      dir.create(combo_dir, recursive = TRUE, showWarnings = FALSE)

      for(i in seq_along(K1_values)) {
        # Get K1 value and corresponding label
        K1 <- K1_values[i]
        K1_label <- K1_labels[i]

        mutualism_pars <- create_parms(laa1, mu1, lambda0, K1)

        # Print the parateter that have been used
        cat(
          "mu1:", mu1,
          "laa1:", mutualism_pars$laa_pars[3],
          "lambda0:", mutualism_pars$lambda0,
          "K_pars:", mutualism_pars$K_pars[3],
          "K1_label:", K1_label,
          "\n"
        )

        # Simulation outputs
        out <- specmutual::peregrine_sim(total_time = 10,
                                         replicates = 50,
                                         mutualism_pars = mutualism_pars,
                                         verbose = FALSE)

        # Save the results to a file
        saveRDS(out, file = file.path(combo_dir, paste0(K1_label, ".rds")))

        # Add a line for clarity after printing all parameters
        cat("---- End of Parameter Set for Folder:", combo_dir, "----\n\n")
      }
    }
  }
}

# Define the parameters
mu1 <- 0
laa1_values <- c(0, 0.002, 0.004, 0.008)
lambda0_values <- c(0, 0.001, 0.005, 0.025)
K1_values <- c(0, 1, 5, 25)
K1_labels <- c("none", "low", "medium", "high")

outs <- par_combo(mu1 = mu1,
                  K1_values = K1_values,
                  K1_labels = K1_labels,
                  laa1_values = laa1_values,
                  lambda0_values = lambda0_values)
