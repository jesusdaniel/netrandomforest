## Load required libraries and custom functions
library(netRandomForest)
library(randomForest)
library(rerf)
library(e1071)
library(Matrix)
library(MASS)
library(lattice)
library(igraphdata)
library(igraph)
library(sand)
library(devtools)
library(dplyr)
library(tidyr)
library(caret)
library(naniar)
library(ggplot2)
library(reshape2)
library(rARPACK)
library(varhandle)
library(ROCR)

source("../../Plots.R")
source("../../extra_functions.R")

## Create a custom function that generates and processes data from SBM
generate_data_SBM=function(q,n,m1,m2){
  ## Stochastic block model
  ## Generate a network with K communities and n nodes
  ## Define all variables
  K = 5
  Adj_list_1 <- list() # Adjacency lists for the 2 classes
  Adj_list_2 <- list()
  SBM_results <- list()
  
  ## Define the block model
  z = kronecker(1:5, rep(1, n/K))
  p1 = 0.5
  p2 = 0.1
  B = (p1 - p2) *diag(K) + p2 ## Default value of q is 0
  
  ## Class 1
  for(i in 1:m1){
    sbm <- sample_sbm(n, B, block.sizes = rep(n/K, K))
    Adj_list_1[[i]] <- get.adjacency(sbm)
  }
  
  SBM_results$adj_1=Adj_list_1[[1]] # Store adjacency list for Class 1
  
  ## Class 2
  ## First change one of the sub-blocks using the value of q for Class 2
  B[1,3] = q
  B[3, 1] = q
  
  for(j in 1:m2){
    sbm <- sample_sbm(n, B, block.sizes = rep(n/K, K))
    Adj_list_2[[j]] <- get.adjacency(sbm)
  }
  
  SBM_results$adj_2=Adj_list_2[[1]] # Store adjacency list for Class 2
  
  ## Simulated data:
  Adj_list <- c(Adj_list_1, Adj_list_2)
  class_label <- c(rep(1, m1), rep(-1, m2))
  
  ## Convert to a 100 x 4950 dataframe
  SBM_sims_df=c() # List of all sims
  SBM_sims_df=Reduce(rbind, lapply(Adj_list, matrix_to_vec))
  SBM_sims_df=as.data.frame(SBM_sims_df)
  
  ## Adding columns for response variable and id
  SBM_sims_df$id=c(1:100)
  SBM_sims_df$response=class_label
  SBM_sims_df=SBM_sims_df[,c(4951,4952,1:4950)] # Rearranging the data so that the id and response show up as the first and second columns of the data frame, respectively
  
  SBM_results$df=SBM_sims_df
  
  return(SBM_results)
}

## Generate a single data set
set.seed(45)
SBM_sims=generate_data_SBM(q=0.4,n=100,m1=50,m2=50)
SBM_sims_df=SBM_sims$df

## Perform data splitting 
testing_data_ind = createDataPartition(SBM_sims_df$response, p = .25, list = FALSE) %>% as.vector(.)
training_data_ind = (1:nrow(SBM_sims_df))[-testing_data_ind]

## Obtain variable importance measures at K=5 clusters and q=0.2...0.5 for NRF2 (B=10) and the standard RF (B=1000) models. Compare their classification performances based on how 'close' the importance variables match up with those of the 'true' matrix
num_clusters=5

## Function for scaling a feature to have a range of 0 to 1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
## Citation: https://stackoverflow.com/questions/5665599/range-standardization-0-to-1-in-r

## Define the 'true' matrix; that is, the matrix with perfect assignment of important variables
correct_mat=matrix(0,100,100)
correct_mat[c(0:20),c(40:60)]=1; correct_mat[c(40:60),c(0:20)]=1

for(q in 1:length(q_vals)){
  
  ## Randomly generate a dataset
  set.seed(q)
  SBM_sims=generate_data_SBM(q=q_vals[q],n=100,m1=50,m2=50)
  SBM_sims_df=SBM_sims$df
  
  training_data=SBM_sims_df[training_data_ind,]
  testing_data=SBM_sims_df[testing_data_ind,]
  
  NRF2_mod=nrf2_training(B=10,n=length(training_data_ind),X_training=training_data[,c(3:ncol(training_data))],Y_training=training_data$response,num_clusters=num_clusters,prop_X=.8,method="class")
  
  vec_VI_list=list() # Define a list that stores the variable importances of each tree in the NRF2 model
  VI_mat=matrix(0,100,100); V_mat=matrix(0,100,100); S_mat=matrix(0,100,100) # Initialize V, VI, and S matrices
  
  ## Create a dataframe that scores all the variable importances from each tree
  VI_df=data.frame(matrix(0,nrow=10,ncol=(num_clusters*(num_clusters-1))/2 + num_clusters))
  
  for(b in 1:length(NRF2_mod$rf)){
    ## Create a matrix Z that indicates which cluster each node/observation belongs to
    ## Obtain the clustering memberships of all training data based on bth tree
    Z_mat=matrix(0,100,num_clusters)
    included_vert=NRF2_mod$m[[b]]
    for(i in c(1:80)){
      clust_num=NRF2_mod$clusters[[b]][i]
      if(clust_num==1){
        Z_mat[included_vert[i],1]=1
      } else if (clust_num==2){
        Z_mat[included_vert[i],2]=1
      } else if (clust_num==3){
        Z_mat[included_vert[i],3]=1
      } else if (clust_num==4){
        Z_mat[included_vert[i],4]=1
      } else {
        Z_mat[included_vert[i],5]=1
      }
    }
    
    vec_VI=NRF2_mod$rf[[b]]$variable.importance
    df_importance = data.frame(matrix(0, ncol = 15))
    df_importance[, names(vec_VI)]=vec_VI
    vec_VI_list[[b]]=as.numeric(df_importance)
    
    ## Create the C matrix
    C_mat <- matrix(0, num_clusters, num_clusters)
    C_mat[upper.tri(C_mat, diag = TRUE)] = vec_VI_list[[b]]
    C_mat=C_mat+t(C_mat)
    diag(C_mat)=diag(C_mat)/2
    
    ## Do a cumulative elementwise sum of all entries of each matrix
    V_mat=V_mat+(Z_mat %*% C_mat %*% t(Z_mat))
    S_mat=S_mat+tcrossprod(rowSums(Z_mat))
  }
  
  ## Find the means VI's for all nodes in the training set
  for(i in 1:nrow(V_mat)){
    for(j in 1:ncol(V_mat)){
      if(S_mat[i,j]!=0){
        VI_mat[i,j]=V_mat[i,j]/S_mat[i,j]
      }
    }
  }
  
  ## Scale VI_mat
  VI_mat=range01(VI_mat)
  
  ## Obtain the same plot for standard RF algorithm
  set.seed(45)
  standard_rf_mod <- randomForest(x=training_data[,c(3:ncol(training_data))],
                                  y=factor(training_data[,2]),
                                  ntree=1000,
                                  importance=TRUE)
  scaled_rf_importance=range01(standard_rf_mod$importance[,4])
  
  ## Obtain the binary matrix (predicted matrix) by subsetting the original variable importance matrix (which is plotted) by variables selected from a given threshold, tau; the 'true' matrix will contain 0's everywhere with 1's within the (3,1) and (1,3) block regions
  
  ## Plot ROC curve for NRF2
  pred_NRF2 <- prediction(matrix_to_vec(VI_mat), matrix_to_vec(correct_mat))
  perf_NRF2 <- performance(pred_NRF2,"tpr","fpr")
  plot(perf_NRF2, col="red")
  
  ## Plot ROC curve for standard RF
  pred_RF <- prediction(scaled_rf_importance, matrix_to_vec(correct_mat))
  perf_RF <- performance(pred_RF,"tpr","fpr")
  plot(perf_RF,add=TRUE,col="blue")
  
  ## Add Legend
  legend("bottomright", c("NRF2", "RF"), lty=1, 
         col = c("red", "blue"), bty="n")
  
  title_name=paste("q =",q_vals[q])
  
  ## Add title 
  title(main = title_name)
}
