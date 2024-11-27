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

  matrices_asso <- lapply(object,'[[',1)
  sum_asso <- Reduce('+', matrices_asso)

  matrices_diss <- lapply(object,'[[',2)
  sum_diss <- Reduce('+', matrices_diss)

  pair_appearances <- sum_diss + sum_asso                                          #Somme des occurence des paires dans les forêts.

  similarity_matrix <- sum_asso / pair_appearances

  # Diag must be 1
  diag(similarity_matrix) <- 1

  output_summary <- list("similarity_matrix" = similarity_matrix)
  class(output_summary) <- "rfclust.summary"
  return(output_summary)

}

