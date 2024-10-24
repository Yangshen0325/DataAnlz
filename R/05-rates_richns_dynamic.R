#### Rates and species richness over time, for checking
# I wrote a new simulation script to speed up by cutting unnecessary steps off.
# This simulation script is for saving rates only.
# Examples explaining returns. `rates_reps[[1]]` would be all rates changing across
# `timeval`, so `length(rates_reps[[1]])` is probably different from `length(rates_reps[[2]])`.
# `time_reps[[1]]` contain the `timeval` within the first simulation. It has the
# same length as `rates_reps[[1]]`. `status_p_reps` whether plant species presents
# on the island, similar for `status_a_reps`.


#' Rates and species richness over time, for checking
#'
#' @param total_time Island age, simulation time
#' @param replicates Replicates
#' @param mutualism_pars Mutualism parameters
#'
#' @return A list
#' @export
#'
rates_richness_dynamic <- function(total_time,
                                   replicates,
                                   mutualism_pars) {

  rates_reps <- list()
  timeval_reps <- list()
  status_p_reps <- list()
  status_a_reps <- list()

  for (rep in 1:replicates) {

    # Initialize varaibles
    timeval <- 0
    M0 <- mutualism_pars$M0
    Mt <- M0
    alpha <- mutualism_pars$alpha
    maxplantID <- nrow(M0)
    maxanimalID <- ncol(M0)
    status_p <- matrix(0, nrow = nrow(M0), ncol = 1)
    status_a <- matrix(0, nrow = ncol(M0), ncol = 1)

    island_spec <- c()
    stt_table <- matrix(ncol = 7)
    # species through time table, `~p` stands for plant species, `~a` animal species
    colnames(stt_table) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
    stt_table[1, ] <- c(total_time, 0, 0, 0, 0, 0, 0)

    # Extract parameters
    lac_pars <- mutualism_pars$lac_pars
    mu_pars <- mutualism_pars$mu_pars
    K_pars <- mutualism_pars$K_pars
    gam_pars <- mutualism_pars$gam_pars
    laa_pars <- mutualism_pars$laa_pars
    qgain <- mutualism_pars$qgain
    qloss <- mutualism_pars$qloss
    lambda0 <- mutualism_pars$lambda0
    transprob <- mutualism_pars$transprob

    rates_list <- list()
    timeval_list <- list()
    richness_p_list <- list()
    richness_a_list <- list()

    # Start Monte Carlo iterations
    while (timeval < total_time) {
      rates <- update_rates_mutual(
        M0 = M0,
        Mt = Mt,
        alpha = alpha,
        status_p = status_p,
        status_a = status_a,
        lac_pars = lac_pars,
        mu_pars = mu_pars,
        K_pars = K_pars,
        gam_pars = gam_pars,
        laa_pars = laa_pars,
        qgain = qgain,
        qloss = qloss,
        lambda0 = lambda0,
        transprob = transprob,
        island_spec = island_spec
      )

      # Save rates list
      rates_list[[length(rates_list) + 1 ]] <- rates

      # Determine next time step
      timeval_and_dt <- sample_time_mutual(rates = rates, timeval = timeval)
      timeval <- timeval_and_dt$timeval

      # Save time values
      timeval_list[[length(timeval_list) + 1]] <- timeval

      if (timeval <= total_time){
        # Select next event
        possible_event <- sample_event_mutual(rates = rates)

        # Update states based on the selected event
        updated_states <- update_states_mutual(
          M0 = M0,
          Mt = Mt,
          status_p = status_p,
          status_a = status_a,
          maxplantID = maxplantID,
          maxanimalID = maxanimalID,
          timeval = timeval,
          total_time = total_time,
          rates = rates,
          possible_event = possible_event,
          island_spec = island_spec,
          stt_table = stt_table,
          transprob = transprob
        )

        Mt <- updated_states$Mt
        status_p <- updated_states$status_p
        status_a <- updated_states$status_a
        maxplantID <- updated_states$maxplantID
        maxanimalID <- updated_states$maxanimalID
        island_spec <- updated_states$island_spec
        stt_table <- updated_states$stt_table

        # Save richness for plants and animals
        richness_p_list[[length(richness_p_list) + 1]] <- sum(status_p)
        richness_a_list[[length(richness_a_list) + 1]] <- sum(status_a)
      }
    }

    status_p_reps[[rep]] <-  richness_p_list
    status_a_reps[[rep]] <-  richness_a_list
    rates_reps[[rep]] <- rates_list
    timeval_reps[[rep]] <- timeval_list
  }
  return(list(rates_reps = rates_reps,
              timeval_reps = timeval_reps,
              status_p_reps = status_p_reps,
              status_a_reps = status_a_reps))
}

