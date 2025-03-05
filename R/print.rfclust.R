#' S3 print method for rfclust objects
#'
#' @param x a \code{rfclust}.
#' @param ... other arguments to be passed to and from other methods.
#' @export

print.rfclust <- function(x, ...){

  print(paste0("A clustering random forest with ", length(x), " trees"))

}

