## Needed this for clustering data
mase <- function (Adj_list, d = NA, d_vec = NA, scaled.ASE = TRUE, diag.augment = TRUE, 
                  elbow_graph = 1, elbow_mase = 2, show.scree.results = FALSE, 
                  par = FALSE, numpar = 12) 
{
  if (is.na(d_vec)) {
    d_vec = rep(d, length(Adj_list))
  }
  if (par) {
    cl <- parallel::makeCluster(numpar)
    parallel::clusterEvalQ(cl, source("R/loadAll.R"))
    parallel::clusterExport(cl = cl, varlist = list("ase", 
                                                    "eig_embedding", "getElbows", "Adj_list", "elbow_graph", 
                                                    "d_vec", "diag.augment"), envir = environment())
    if (scaled.ASE) {
      latpos.list <- parallel::parLapply(cl = cl, 1:length(Adj_list), 
                                         function(i) ase(Adj_list[[i]], d = d_vec[i], 
                                                         diag.augment = diag.augment, elbow = elbow_graph))
    }
    else {
      latpos.list <- parallel::parLapply(cl = cl, 1:length(Adj_list), 
                                         function(i) eig_embedding(Adj_list[[i]], d = d_vec[i], 
                                                                   diag.augment = diag.augment, elbow = elbow_graph))
    }
    parallel::stopCluster(cl)
  }
  else {
    if (scaled.ASE) {
      latpos.list <- lapply(1:length(Adj_list), function(i) ase(Adj_list[[i]], 
                                                                d = d_vec[i], diag.augment = diag.augment, elbow = elbow_graph))
    }
    else {
      latpos.list <- lapply(1:length(Adj_list), function(i) eig_embedding(Adj_list[[i]], 
                                                                          d = d_vec[i], diag.augment = diag.augment, elbow = elbow_graph))
    }
  }
  V_all <- Reduce(cbind, latpos.list)
  jointsvd <- svd(V_all)
  if (is.na(d)) {
    if (show.scree.results) {
      hist(sapply(latpos.list, ncol), main = "Estimated d for each graph")
    }
    d = getElbows(jointsvd$d, plot = show.scree.results)[elbow_mase]
  }
  V = jointsvd$u[, 1:d, drop = FALSE]
  return(list(V = V, sigma = jointsvd$d))
}