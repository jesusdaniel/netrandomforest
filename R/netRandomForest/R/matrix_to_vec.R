matrix_to_vec <- function(A, type=c("undirected", "directed")) {
  type <- match.arg(type)
  if(type=="undirected") {
    beta <- A[upper.tri(A)]
  }else{if(type=="directed") {
    mat <- matrix(T, ncol = ncol(A), nrow = nrow(A))
    diag(mat) <- F
    beta <- as.vector(A[mat])
  }else{
    stop("The value of type should be one between \"directed\" and \"undirected\"")
  }}
  return(beta)
}