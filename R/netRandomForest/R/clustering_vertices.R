## Run this after running "mase" and "ase"
clustering_vertices <- function(Adj_list, K) {
  n=ncol(Adj_list[[1]])
  mase.res <- mase(Adj_list, K, scaled.ASE = TRUE)
  kmeans.res <- kmeans(mase.res$V, centers = K, nstart = 200)
  clusters <- kmeans.res$cluster
  Z <- matrix(0, n, K)
  Z[cbind(1:n, clusters)] <- 1
  n_k <- colSums(Z)
  # print(n_k)
  
  
  # For connectivity, calculate average edges per cell
  nk_nk <- tcrossprod(n_k)
  B_matrices <- lapply(Adj_list, function(A) 
    matrix(crossprod(Z, crossprod(A, Z)) / nk_nk, K, K) )
  B_matrices_vectorized <- t(sapply(B_matrices, function(B) B[upper.tri(B, diag = TRUE)]))
  
  result <- list(X_clusters = B_matrices_vectorized,
                 cluster_memberships = clusters)
  return(result)
}