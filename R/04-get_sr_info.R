


#' This function summarises the number of plant, animal, and endeminsm on the island
#' @param dataset_name Data name
#'
#' @return A list
#' @export
#'
get_sr_info <- function(dataset_name) {
  # read data
  data <- readRDS(dataset_name)

  sum_info_list <- list()

  # Loop through the dataset
  for (i in 1:length(data)) {
    # Number of plant species on the island
    island_p <- sum(data[[i]][["status_p"]])

    # Number of animal species on the island
    island_a <- sum(data[[i]][["status_a"]])

    # Species status on the island
    island_sp_status <- data[[i]][["evo_table"]][[2]]

    # Endemic and non-endemic plants and animals
    island_endemic_p <- island_sp_status[2]
    island_nonendemic_p <- island_sp_status[3] +  island_sp_status[4]
    island_endemic_a <- island_sp_status[5]
    island_nonendemic_a <- island_sp_status[6] +  island_sp_status[7]

    # Store the summary information
    sum_info_list[[i]] <- data.frame(
      island_p = island_p,
      island_a = island_a,
      island_endemic_p = island_endemic_p,
      island_nonendemic_p = island_nonendemic_p,
      island_endemic_a = island_endemic_a,
      island_nonendemic_a = island_nonendemic_a
    )
  }

  # Combine all sum_info into a single data frame for this dataset
  sum_info_df <- bind_rows(sum_info_list)

  return(sum_info_df)

}
