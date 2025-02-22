# Scenario high_cra_low_u OOM, when mutualism is high
# so, resubmit the job
library(specmutual)
# cladogenesis
high_lac0 <- 0.6
low_lac0 <- 0.3

# extinction
high_mu0 <- 0.2
low_mu0 <- 0.05

# immigration
high_gam0 <- 0.08
low_gam0 <- 0.04

# anagenesis
high_laa0 <- 1.0
low_laa0 <- 0.5

K0 <- 50  # carrying capacity

qgain <-  0.001 # gain links rate
qloss <-  0.001 # lose links rate

transprob <-  1.0
alpha <-  100

# island age and replications
total_time <- 10
replicates <- 100

# set the path to save outputs
the_path <- "~/extinction_005_02"

# read M0
M0 <- readRDS("~/extinction_005_02/M0.rds")
# Define mutualism effects
mutualism_effects <- list(
  none = list(  mu1 = 0,      K1 = 0,   laa1 = 0, lambda0 = 0),
  medium = list(mu1 = 0.005,  K1 = 50,  laa1 = 0.005, lambda0 = 0.25),
  high = list(  mu1 = 0.01,   K1 = 100, laa1 = 0.01, lambda0 = 0.5)
)

# job script --------------------------------------------------------------

effect <- "high"

prefix <- "high_cra_low_u"
cat("Running the case: ", prefix, "\n")

## set seed
set_random_seed <- function() {
  seed <- as.integer(runif(1, min = 1, max = .Machine$integer.max))
  set.seed(seed)
  return(seed)
}
seed <- set_random_seed()
cat("Random seed set to:", seed, "\n")



if (prefix == "high_cra_low_u") {



    lac0 <- high_lac0
    mu0 <- low_mu0
    gam0 <- high_gam0
    laa0 <- high_laa0

    params <- specmutual::create_mutual_pars(
      lac_pars = c(lac0, lac0),
      mu_pars = c(mu0, mu0, mutualism_effects[[effect]]$mu1, mutualism_effects[[effect]]$mu1),
      K_pars = c(K0, K0, mutualism_effects[[effect]]$K1, mutualism_effects[[effect]]$K1),
      gam_pars = c(gam0, gam0),
      laa_pars = c(laa0, laa0, mutualism_effects[[effect]]$laa1, mutualism_effects[[effect]]$laa1),
      qgain = qgain,
      qloss = qloss,
      lambda0 = mutualism_effects[[effect]]$lambda0,
      M0 = M0,
      transprob = transprob,
      alpha = alpha
    )

    cat("Running mutualism effects case: ", prefix, "_", effect, "\n")
    cat("cladogenensis is: ", lac0, "\n",
        "extinction is: ", mu0, "\n",
        "immigration is: ", gam0, "\n",
        "anagenesis is: ", laa0, "\n",
        "effect on K1, mu1, laa1, lambda0: ", params$K_pars[3], params$mu_pars[3],
        params$laa_pars[3], params$lambda0, "\n"
    )

    sim_pars_combo <- specmutual::get_simulation_outputs(the_path = the_path,
                                                         params = params,
                                                         effect = effect,
                                                         prefix = prefix,
                                                         total_time = total_time,
                                                         replicates = replicates)


  }














