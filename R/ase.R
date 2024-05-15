ase <- function (A, d = NA, d.max = sqrt(ncol(A)), diag.augment = TRUE, 
                 elbow = 1) 
{
  if (diag.augment & sum(abs(Matrix::diag(A))) == 0) {
    deg = Matrix::colSums(A)
    n = ncol(A)
    diag(A) = deg/(n - 1)
  }
  if (is.na(d)) {
    eig <- rARPACK::eigs(as(A, "dgeMatrix"), d.max)
    vals <- sort(x = abs(eig$values), decreasing = TRUE)
    d = getElbows(vals, plot = F)[elbow]
    selected.eigs <- which(abs(eig$values) >= vals[d])
    V <- eig$vectors[, selected.eigs, drop = F]
    D <- diag(sqrt(abs(eig$values[selected.eigs])), nrow = d)
    X <- V %*% D
    return(X)
  }
  else {
    eig <- rARPACK::eigs(as(A, "dgeMatrix"), k = d)
    X <- eig$vectors %*% diag(sqrt(abs(eig$values)), nrow = d)
    return(X)
  }
}