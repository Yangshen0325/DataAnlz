

#### Get the proportion of disconnected species and community size on islands ####

# "Com_p" = num_p, # plant species in community
# "Com_a" = num_a, # animal species in community
# "disconnect_p" = disconnect_p, # disconnnected plant species
# "disconnect_a" = disconnect_a, # disconnnected animal species

# Function to get community size for 16 scenarios.
get_NLH_size <- function(the_path, the_case){

  # Data source
  file_names <- c(paste0(the_path, the_case, "_none.rds"),
                  paste0(the_path, the_case, "_low.rds"),
                  paste0(the_path, the_case, "_high.rds"))

  # Process none, low and high files and store the results
  comm_info <- lapply(file_names, getSize)

  names(comm_info) <- c("None", "Low", "High")
  all_comm_df <- bind_rows(comm_info, .id = "Type")

  all_comm_df$Type <- factor(all_comm_df$Type, levels = c("None", "Low", "High"))

  return(all_comm_df)

}



# Function to get community information: community size, disconnected species
# For one case, e.g., none mutualism effects

getSize <- function(dataset_name) {

  # Read data
  data <- readRDS(dataset_name)

  comm_list <- lapply(data, function(rep_outputs){

    Mt <- rep_outputs$Mt
    status_p <- rep_outputs$status_p
    status_a <- rep_outputs$status_a

    # the community on islands `true_Mt` (with species with 0 links)
    true_Mt <- Mt[status_p == 1, status_a == 1, drop = FALSE]

    # the number of plant and animal species disconnected in network
    if (nrow(true_Mt) == 0) {
      disconnect_p <- 0
    } else {
      disconnect_p <- length(which(rowSums(true_Mt) == 0))
    }

    if (ncol(true_Mt) == 0) {
      disconnect_a <- 0
    } else {
      disconnect_a <- length(which(colSums(true_Mt) == 0))
    }

    # the number of plant and animal species involved in community
    num_p <- sum(status_p)
    num_a <- sum(status_a)

    data.frame(

      "Com_p" = num_p, # plant species in community
      "Com_a" = num_a, # animal species in community
      "disconnect_p" = disconnect_p, # disconnected plant species
      "disconnect_a" = disconnect_a # disconnected animal species

    )
  })

  # Combine together
  comm_df <- bind_rows(comm_list)
  return(comm_df)
}


