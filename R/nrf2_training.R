#' Net Random Forest Ver. 2 (NRF2) - Training
#'
#' Performs spectral clustering within each tree in the random forest on the training data.
#'
#' @param B Number of trees.
#' @param n Number of subjects in each bootstrap sample.
#' @param X_training Features of the training data.
#' @param Y_training Supervisor of the training data.
#' @param num_clusters Number of clusters for the spectral clustering algorithm.
#' @param prop_X Proportion of features to be randomly sampled from the full set of features.
#' @param method "anova" (regression) or "class" (classification).
#'
#' @return A trained net random forest model that contains the predicted response values,
#' clustering memberships as well as a list of trees and selected features used in each tree.
#'
#' See the vignette for examples.
#'
#' @export

nrf2_training=function(B,n,X_training,Y_training,num_clusters,prop_X=.8,method){
  require(rpart)
  suppressWarnings({
    tree_list=list() # Create a list to store decision trees
    sel_vert_list=list() # List of selected vertices
    cluster_list=list() # List of clusters
    class_df=as.data.frame(matrix(NA,nrow=nrow(X_training),ncol=B)) # Matrix of predicted classes for all trees for a given row
    value_df=as.data.frame(matrix(NA,nrow=nrow(X_training),ncol=B)) # Matrix of predicted values for all trees for a given row

    A=get_matrix(as.numeric(X_training[1,]))
    num_vert=ncol(A)

    ## Grow B number of trees
    for (b in 1:B){
      set.seed(b)

      ## Select m variables at random from the p variables; typical value of m is 0.8*n, where n = nrow/ncol(A)
      m=sample(x=1:num_vert,size=prop_X*num_vert,replace=FALSE) # Sampled vertices at random

      ## Create the adjacency list to be fed into the clustering function
      adj_list=list()
      for(i in 1:nrow(X_training)){
        A=get_matrix(as.numeric(X_training[i,]))
        A=A[m,m] # Select a subset of vertices from the adjacency matrix
        adj_list[[i]]=A
      }

      ## Clustering data into num_clusters
      clusters <- clustering_vertices(adj_list,num_clusters)
      clustering_result=clusters$X_clusters
      clustering_memberships=clusters$cluster_memberships
      Xnew1=ifelse(is.nan(clustering_result),0,clustering_result)
      cluster_df=data.frame(response=Y_training,Xnew1)

      ## Step a): Draw a bootstrap sample Z* of size n from the clustered training data
      indicies=sample(1:nrow(cluster_df),n,replace=TRUE)
      cluster_sub_df=cluster_df[indicies,] # Bootstrap sample

      ### Grow a random-forest tree T_b to the bootstrapped data
      ### Construct a classification tree
      ### Fit a decision tree
      decision_tree=rpart(
        response~.,
        data=cluster_sub_df,
        method=method
      )
      tree_list[[b]]=decision_tree # Adding a new tree to the current list of decision trees
      sel_vert_list[[b]]=m
      cluster_list[[b]]=clustering_memberships

      ## Now generate a list of predicted values for the training data
      if (method == "class"){
        for (i in 1:nrow(cluster_df)){ # For each tree in the random forest
          T_b=tree_list[[b]]
          class_b=predict(T_b,newdata=cluster_df[i,],type ="class") # Predict the class from each tree
          class_df[i,b]=unfactor(class_b)
        }
      } else if (method == "anova") {
        for (i in 1:nrow(cluster_df)){
          T_b=tree_list[[b]]
          value_b=predict(T_b,newdata=cluster_df[i,],type ="vector")[[1]] # Predict the value from each tree
          value_df[i,b]=value_b
        }
      }
    }

    pred_vec=c() # Vector of final predicted class for each row in the training data
    if (method == "class"){
      for (i in 1:nrow(cluster_df)){ # For each row in the bth clustered training data, predict its class
        class_i=as.numeric(Mode(na.omit(class_df[i,]))) # Obtaining the "majority vote" for the final predicted class of row i
        pred_vec[i]=class_i
      }
    } else if (method == "anova") {
      for (i in 1:nrow(cluster_df)){ # For each row in the bth clustered training data, predict its value
        value_i=mean(as.matrix(value_df)[i,]) # Obtaining the "average" for the final predicted value of row i
        pred_vec[i]=value_i
      }
    }

    result=list()
    result$rf=tree_list # Outputting the tree list with B decision trees (random forest)
    result$m=sel_vert_list
    result$clusters=cluster_list
    result$pred_vals=pred_vec
    return(result) # Return the vector of predicted values for all rows in the training data
  })
}
