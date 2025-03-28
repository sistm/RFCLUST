#' summary S3 method for rfclust objects
#'
#' Merge all matrices and some analysis
#'
#' @param object the results of the apply function on the RF function (tree)
#' @param ... more parameters if required
#' @return Provides the cumulative similarity matrix and some analysis
#' @import dplyr gplots ggplot2 GGally
#' @export


summary.rfclust <- function(object, ...){

  ntrees <- length(object)

  matrices_sim <- lapply(object,'[[',1)
  sum_sim <- Reduce('+', matrices_sim)

  matrices_dist <- lapply(object,'[[',2)
  sum_dist <- Reduce('+', matrices_dist)

  matrices_absent <- lapply(object,'[[',3)
  sum_absent <- Reduce('+', matrices_absent)

  if(object[[1]]$distance == "co-clustering"){

    pair_appearances <- sum_dist + sum_sim                                          #Somme des occurence des paires dans les forêts.
    similarity_matrix <- sum_sim / pair_appearances
    distance_matrix <- 1 - similarity_matrix

  }else if(object[[1]]$distance == "inertia"){
    sum_present <- ntrees - sum_absent
    distance_matrix <- sum_dist / sum_present
  }

  # Diagonale nulle
  diag(distance_matrix) <- 0

  # Si NA, mettre à 0
  distance_matrix[is.na(distance_matrix)] <- 0
  output_summary <- list("distance_matrix" = distance_matrix)
  class(output_summary) <- "rfclust.summary"
  return(output_summary)

}

