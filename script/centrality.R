
# Closeness centrality is meaningful only for connected graphs. In disconnected graphs, consider
# using the harmonic centrality with harmonic_centrality()
# harmonic centrality measures the 'average' distance of a node to the other nodes in the network
# https://symbio6.nl/en/blog/analysis/harmonic-centrality
#
# Eigenvector centrality is meaningful only for (strongly) connected graphs. Undirected graphs that
# are not connected should be decomposed into connected components, and the eigenvector centrality
# calculated for each separately. This function does not verify that the graph is connected. If it is not,
# in the undirected case the scores of all but one component will be zeros.


library(igraph)
# Centrality measures

# Just for one network
low_cura_none <- readRDS("~/Downloads/phd_yang/chapter2/DataAnlz/waste/pars_combo16_FEB2025/low_cura/low_cura_none.rds")

getDeg_Centra <- function(rep_outputs) {

  Mt <- rep_outputs$Mt
  status_p <- rep_outputs$status_p
  status_a <- rep_outputs$status_a

  true_Mt <- Mt[status_p == 1, status_a == 1, drop = FALSE]

  # Assign row names as "p1", "p2", "p3", ...
  rownames(true_Mt) <- paste0("p", seq_len(nrow(true_Mt)))

  # Assign column names as "a1", "a2", "a3", ...
  colnames(true_Mt) <- paste0("a", seq_len(ncol(true_Mt)))

  # Convert it to igraph object
  g <- graph_from_biadjacency_matrix(true_Mt)

    plant_deg_centra <- degree(g, v = V(g)$type == FALSE, mode = "all")
    animal_deg_centra <- degree(g, v = V(g)$type == TRUE, mode = "all")
    plant_between <- betweenness(g, v = V(g)$type == FALSE, directed = FALSE, normalized = TRUE)
    animal_between <- betweenness(g, v = V(g)$type == TRUE, directed = FALSE, normalized = TRUE)

  }





      # Convert it to igraph object
      g <- graph_from_biadjacency_matrix(true_Mt)

    } else {
      # Convert it to igraph object
      g <- graph_from_biadjacency_matrix(true_Mt)

      # Plant degree centrality
      plant_deg_centra <- degree(g, v = V(g)$type == FALSE, mode = "all")

      # Plant betweenness centrality
      plant_between <- betweenness(g, v = V(g)$type == FALSE, directed = FALSE, normalized = TRUE)

      # Plant Harmonic centrality
      plant_harmonic <- harmonic_centrality(g, vids = V(g)$type == FALSE, normalized = TRUE, mode = "all")
    }

    if (ncol(true_Mt) == 0) {
      animal_deg_centra <- NA
    } else {

    }
  } else {

  }

  return(list(deg_centra_p = plant_deg_centra,
              deg_centra_a = animal_deg_centra))
}

# Compute betweenness centrality


# Add to dataframe
degree_df$Betweenness <- betweenness_values

# View results
print(degree_df)
