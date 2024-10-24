
#### Get "species richness through time" (stt) data list ####

# All simulation outputs are not formatted into DAISIE format. It's a list with the
# length of the number of replicates. Each replicate represents a completed simulation,
# in which has 7 elements:
# `Mt`: the "false" matrix on the island, including extinct and non-shown species occupying
#       the original index of matrix in the mainland.
# `M_true_list`: the "true" matrix list on the island every 0.5 time steps until "total_time"
# `status_p`: a vector showing whether a plant species is on the island or not
# `status_a`: a vector showing whether a animal species is on the island or not
# `island_spec`: a table showing the evolutionary trajectory of species on the island
# `island`: a list of 3, including `stt_table`, `clades_info_plant`, and `clades_info_animal`
# `evo_table`: a list of 2, including a table with 2 columns (one for time step from the
#             the beginning until "total_time", the other indicating which event has happened),
#             the last row of `stt_table`


# Process data, use `format_island_mutual_all.R`------------------------------

# `format_island_mutual_all.R` is for formatting data for both plant and animal species,
# and we will choose the one with `sample_freq` for an organised data output for plotting.


# Get the list for all dataset --------------------------------------------



#' This function collects stt data (median, q25, q75) across different levels of mutualism and save the data
#'
#' @param data_files Data need to be handled
#' @param path Path storing data
#' @param total_time Island age (simulation time)
#' @param sample_freq For formatting
#' @param M0 Mainland matrix
#' @param verbose
#'
#' @return A list listing all median, q25, q75 infomattion
#' @export
#'
get_all_lists <- function(data_files, path, total_time, sample_freq, M0, verbose) {
  # Initialize an empty list to store results
  result_list <- list()

  # Define the corresponding group labels for each dataset
  group_labels <- c("None", "Low", "Medium", "High")

  # Loop through each file and process the dataset
  for (i in seq_along(data_files)) {
    # Construct full path to the dataset
    file_path <- file.path(path, data_files[i])

    # Load the dataset
    island_replicates <- readRDS(file_path)

    # Process the dataset using the `get_list` function
    infolist <- get_list(island_replicates, total_time, sample_freq, M0, verbose)

    # Convert infolist to a data frame if it's not already
    infolist_df <- bind_rows(infolist, .id = 'Type')
    infolist_df$Group <- group_labels[i]

    # Append the result to the list
    result_list[[i]] <- infolist_df
  }

  # Bind all data frames together by rows
  list_all <- do.call(rbind, result_list)

  saveRDS(list_all, file = paste0(path, "list_all.rds"))
}


# Get the median q25 q75 list of one dataset -----------------------------


#' This function gets the edian q25 q75 list of one data set, e.g. data only for none-mutualism case
#'
#' @param island_replicates "Raw" simulation output from `sim_core_mutualism`, not being fomatted yet
#' @param total_time island age (simulation time)
#' @param sample_freq For fomatting
#' @param M0 Mainland mutualistic matrix
#' @param verbose
#'
#' @return A list

get_list <- function(island_replicates,
                     total_time,
                     sample_freq,
                     M0,
                     verbose) {

  cladeinfo <- specmutual:::format_island_mutual_all(island_replicates = island_replicates,
                                                     total_time = total_time,
                                                     sample_freq = sample_freq,
                                                     M0 = M0,
                                                     verbose = FALSE)

  rep <- length(cladeinfo)

  # Initialize space for saving data
  non_endemic_arr <- array(dim = c(sample_freq + 1, 2, rep))
  endemic_arr <- array(dim = c(sample_freq + 1, 2, rep))
  total_arr <- array(dim = c(sample_freq + 1, 2, rep))

  for (x in 1:rep) {
    # All: plant and animal
    sum_nonendemic_all <- cladeinfo[[x]][[1]]$stt_all[, "nIp"] +
      cladeinfo[[x]][[1]]$stt_all[, "nIa"]

    sum_endemic_all <- cladeinfo[[x]][[1]]$stt_all[, "nAp"] +
      cladeinfo[[x]][[1]]$stt_all[, "nCp"] +
      cladeinfo[[x]][[1]]$stt_all[, "nAa"] +
      cladeinfo[[x]][[1]]$stt_all[, "nCa"]

    total_all <- cladeinfo[[x]][[1]]$stt_all[, "nIp"] +
      cladeinfo[[x]][[1]]$stt_all[, "nAp"] +
      cladeinfo[[x]][[1]]$stt_all[, "nCp"] +
      cladeinfo[[x]][[1]]$stt_all[, "nIa"] +
      cladeinfo[[x]][[1]]$stt_all[, "nAa"] +
      cladeinfo[[x]][[1]]$stt_all[, "nCa"]

    non_endemic_arr[, , x] <- cbind(cladeinfo[[x]][[1]]$stt_all[, 'Time'],
                                    sum_nonendemic_all)

    endemic_arr[, , x] <- cbind(cladeinfo[[x]][[1]]$stt_all[, 'Time'],
                                sum_endemic_all)

    total_arr[, , x] <- cbind(cladeinfo[[x]][[1]]$stt_all[, 'Time'],
                              total_all)
  }

  # Calculate the median, the 25 quantile and 75 quantile across the replicates for
  # non-endemic data
  non_endemic_average <- apply(non_endemic_arr, c(1, 2), stats::median)
  non_endemic_q0.25 <- apply(non_endemic_arr, c(1, 2), stats::quantile, 0.25)
  non_endemic_q0.75 <- apply(non_endemic_arr, c(1, 2), stats::quantile, 0.75)

  # Combine them together, remove the first column of `non_endemic_q0.25` and `non_endemic_q0.75`
  # coz that's "Time"
  non_endemic_dat <- cbind(non_endemic_average, non_endemic_q0.25[, -1], non_endemic_q0.75[, -1])
  colnames(non_endemic_dat) <- c("Time", "Median", "Q0.25", "Q0.75")
  non_endemic_dat <- as.data.frame(non_endemic_dat)

  # Calculate the median, the 25 quantile and 75 quantile across the replicates for
  # endemic data
  endemic_average <- apply(endemic_arr, c(1, 2), stats::median)
  endemic_q0.25 <- apply(endemic_arr, c(1, 2), stats::quantile, 0.25)
  endemic_q0.75 <- apply(endemic_arr, c(1, 2), stats::quantile, 0.75)
  endemic_dat <- cbind(endemic_average,endemic_q0.25[, -1], endemic_q0.75[, -1])
  colnames(endemic_dat) <- c("Time", "Median", "Q0.25", "Q0.75")
  endemic_dat <- as.data.frame(endemic_dat)

  # Calculate the median, the 25 quantile and 75 quantile across the replicates for
  # total data
  total_average <- apply(total_arr, c(1, 2), stats::median)
  total_q0.25 <- apply(total_arr, c(1, 2), stats::quantile, 0.25)
  total_q0.75 <- apply(total_arr, c(1, 2), stats::quantile, 0.75)
  total_dat <- cbind(total_average,total_q0.25[, -1], total_q0.75[, -1])
  colnames(total_dat) <- c("Time", "Median", "Q0.25", "Q0.75")
  total_dat <- as.data.frame(total_dat)

  infolist <- list(non_endemic_dat = non_endemic_dat,
                   endemic_dat = endemic_dat,
                   total_dat = total_dat)
  return(infolist)

}


