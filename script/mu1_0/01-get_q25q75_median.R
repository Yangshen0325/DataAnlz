# Process data and save it ------------------------------------------------


M0 <- readRDS("~/Downloads/phd_yang/chapter2/formal_v1/codeNdata/mu1_0/M0.rds")

# List of all paths
path_1 <- "script/"
path_2 <- "mu1_0/"

all_paths <- c(paste0(path_1, path_2, "laa1_0lambda0_0/"),
               paste0(path_1, path_2, "laa1_0lambda0_0.001/"),
               paste0(path_1, path_2, "laa1_0lambda0_0.005/"),
               paste0(path_1, path_2, "laa1_0lambda0_0.025/"),
               paste0(path_1, path_2, "laa1_0.002lambda0_0/"),
               paste0(path_1, path_2, "laa1_0.002lambda0_0.001/"),
               paste0(path_1, path_2, "laa1_0.002lambda0_0.005/"),
               paste0(path_1, path_2, "laa1_0.002lambda0_0.025/"),
               paste0(path_1, path_2, "laa1_0.004lambda0_0/"),
               paste0(path_1, path_2, "laa1_0.004lambda0_0.001/"),
               paste0(path_1, path_2, "laa1_0.004lambda0_0.005/"),
               paste0(path_1, path_2, "laa1_0.004lambda0_0.025/"),
               paste0(path_1, path_2, "laa1_0.008lambda0_0/"),
               paste0(path_1, path_2, "laa1_0.008lambda0_0.001/"),
               paste0(path_1, path_2, "laa1_0.008lambda0_0.005/"),
               paste0(path_1, path_2, "laa1_0.008lambda0_0.025/")
)

data_files <- c("none.rds", "low.rds", "medium.rds", "high.rds")

for (i in seq_along(all_paths)) {

  the_path <- all_paths[i]

  combined_data <- get_all_lists(data_files = data_files,
                                 path = the_path,
                                 total_time = 10,
                                 sample_freq = 50,
                                 M0 = M0,
                                 verbose = 0)

}




