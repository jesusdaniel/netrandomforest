generate_data=function(p1,p2,n,m1,m2){
  ## Generate population of graphs
  ## Class 1:
  Adj_list_1 <- list()
  Adj_list_1 <- lapply(1:m1, function(i) get.adjacency(sample_gnp(n, p = p1, directed = FALSE, loops = FALSE)))
  
  ## Class 2:
  Adj_list_2 <- list()
  Adj_list_2 <- lapply(1:m2, function(i) get.adjacency(sample_gnp(n, p = p2, directed = FALSE, loops = FALSE)))
  
  ## Simulated data:
  Adj_list <- c(Adj_list_1, Adj_list_2)
  class_label <- c(rep(1, m1), rep(-1, m2))
  
  ## Convert to a 100 x 435 dataframe
  erdos_sims_df=c() # List of all sims
  
  for(i in 1:100){
    sims_matrix=Adj_list[[i]]
    A = matrix(0, n, n)
    
    ## Convert empty elements in sparse matrix with 0's
    for(j in 1:n){
      for (k in 1:n){
        if (sims_matrix[j,k] == 1){
          A[j,k]=sims_matrix[j,k] # Change the value of (j,k) element in matrix A to 1 if the element the corresponding position of the sims_matrix is 1 
        }
      }
    }
    
    ## Appending the sims list with vectors (converted from matrices)
    erdos_sims_df=rbind(erdos_sims_df,matrix_to_vec(A, type="undirected"))
  }
  
  erdos_sims_df=as.data.frame(erdos_sims_df)
  
  ## Adding columns for response variable and id
  erdos_sims_df$id=c(1:100)
  erdos_sims_df$response=class_label
  erdos_sims_df=erdos_sims_df[,c(436,437,1:435)] # Rearranging the data so that the id and response show up as the first and second columns of the data frame, respectively
  
  return(erdos_sims_df)
}