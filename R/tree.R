

#' Computes a divclust tree
#'
#' Computes a divisive monothetic tree from the divclust package using either
#' co-clustering or inertia distances
#'
#' @param X data.frame of input data
#' @param mtry number of variables selected at each tree node
#' @param distance a character string, either "co-clustering" or "inertia"
#' @return dissimilarity matrix and oob matrix
#' @import dplyr pbapply divclust progress
#' @export


tree <- function(X, mtry = ncol(X), distance=c("co-clustering")){


  stopifnot(distance %in% c("co-clustering", "inertia"))

  rn <- rownames(X)

  #observation bootstrap
  index_boot <- sample(1:nrow(X), size = nrow(X), replace = TRUE)
  oob <- rn[-unique(index_boot)]
  X_ib <- X[index_boot, , drop=FALSE]
  #rownames(X_ib) <- make.unique(rn[index_boot])

  #Matrice d'absence
  absent <- matrix(0, nrow(X), nrow(X), dimnames = list(rn, rn))
  absent[oob, ] <-  1
  absent[, oob] <-  1

  if (distance == "inertia"){
    #Initialisation des matrices de stockages
    dist <- matrix(0, nrow(X), nrow(X), dimnames = list(rn, rn))

    #Création de l'abre avec la profondeur maximale
    tree_kmax <- length(rn[unique(index_boot)])
    nombre_clusters <- floor(tree_kmax/2)
    tree_opti <- divclust(X_ib, K = nombre_clusters, mtry)
    B_diff <- tree_opti$height
  
    #Extraction des différents clusters
    clus_indiv_unik <- sapply(tree_opti$clusters,
                              function(x){
                                unique(sapply(strsplit(x, ".", fixed = TRUE), "[", 1))
                              }
    )

    #Distance entre chaque cluster
    rnc <- names(tree_opti$description)
    dist_clusters <- matrix(0, length(tree_opti$clusters), length(tree_opti$clusters), dimnames = list(rnc, rnc))
    for (cluster_i in rnc){
      for (cluster_j in rnc){
        if (cluster_i == cluster_j) {
          dist_clusters[cluster_i, cluster_j] <-0  # Distance intra-cluster
        } else {
          list_inter <- list()
          list_inter <- tree_opti[["inertia"]][[cluster_i]][tree_opti[["inertia"]][[cluster_i]] %in% tree_opti[["inertia"]][[cluster_j]]]
          min_val <- min(list_inter)
          min_index <- which(B_diff == min_val)[1]
          dist_clusters[cluster_i, cluster_j] <- sum(B_diff[min_index:(nombre_clusters - 1)])/sum(B_diff[1:(nombre_clusters - 1)])
          #dist_clusters[cluster_i, cluster_j] <- sum(B_diff[min_index:(nombre_clusters - 1)])
        }
      }
    }

    # Création des indices pour remplir la matrice de distance
    indices <- expand.grid(cluster_i = rnc, cluster_j = rnc)

    # Remplissage de la matrice de distance
    for (k in seq_len(nrow(indices))) {
      i_set <- clus_indiv_unik[[indices$cluster_i[k]]]
      j_set <- clus_indiv_unik[[indices$cluster_j[k]]]
      dist[i_set, j_set] <- dist_clusters[indices$cluster_i[k], indices$cluster_j[k]]
    }

    sim <- 1 - absent - dist

  } else if(distance == "co-clustering"){
    #Initialisation des matrices de stockages
    sim <- matrix(0, nrow(X), nrow(X), dimnames = list(rn, rn))
    
    tree_max <- divclust(X_ib, K = NULL, mtry)
    
    #Extraction de kmax et de la variation d'inertie
    tree_kmax <- tree_max$kmax
    B_diff <- tree_max$height #car B(k+1) - B(k) = W(k) - W(k+1)

    #Calcul des proportions d'inertie totale expliquées par l'inertie inter-cluster
    ratios <- B_diff[1:(length(B_diff)-1)]/B_diff[2:length(B_diff)]
    indice_max <- which.max(ratios)

    nombre_clusters <- indice_max +2 #pour permettre calcul du bon ratio
    tree_opti <- cutreediv(tree_max, K = nombre_clusters)

    #We specify each unique individual for each of our clusters
    clus_indiv_unik <- sapply(tree_opti$clusters,
                              function(x){
                                unique(sapply(strsplit(x, ".", fixed = TRUE), "[", 1))
                              }
    )

    # Co-clustering occurences in each cluster
    for(k in 1:nombre_clusters){
      for(i in clus_indiv_unik[k]){
        for(j in clus_indiv_unik[k]){
          sim[i,j] <- 1

        }
      }

    }

    dist <-  1 - absent - sim
  }

  # Returns the list of 3 matrices
  out <- list("sim" = sim, "dist" = dist, "absent" = absent,
              "distance"=distance)
  return(out)
}
