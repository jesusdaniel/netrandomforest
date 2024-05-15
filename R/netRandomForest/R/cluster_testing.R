cluster_testing <- function(adj_list_test,clustering_instructions,num_clusters){ # clustering_instructions = instructions on how to partition the test data
  n=ncol(adj_list_test[[1]])
  Z <- matrix(0, n, num_clusters)
  Z[cbind(1:n, clustering_instructions)] <- 1
  n_k <- colSums(Z)
  # print(n_k)
  
  
  # For connectivity, calculate average edges per cell
  nk_nk <- tcrossprod(n_k)
  B_matrices <- lapply(adj_list_test, function(A) 
    matrix(crossprod(Z, crossprod(A, Z)) / nk_nk, num_clusters, num_clusters) )
  B_matrices_vectorized <- t(sapply(B_matrices, function(B) B[upper.tri(B, diag = TRUE)]))
  
  return(B_matrices_vectorized)
}