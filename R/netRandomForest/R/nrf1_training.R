#' Net Random Forest Ver. 1 (NRF1) - Training
#'
#' Performs spectral clustering outside of all trees of the random forest on the training data.
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
#' @export

nrf1_training=function(B,n,X_training,Y_training,num_clusters,prop_X=.8,method){
  require(rpart)
  suppressWarnings({
    tree_list=list() # Create a list to store decision trees
    result=list()
    m_list=list()

    ## Create the adjacency list to be fed into the clustering function
    adj_list=list()
    for(i in 1:nrow(X_training)){
      A=get_matrix(as.numeric(X_training[i,]))
      adj_list[[i]]=A
    }

    ## Clustering data into num_clusters
    set.seed(45)
    clusters <- clustering_vertices(adj_list,num_clusters)
    clustering_result=clusters$X_clusters
    clustering_memberships=clusters$cluster_memberships
    Xnew1=ifelse(is.nan(clustering_result),0,clustering_result)
    cluster_df=data.frame(response=Y_training,Xnew1)

    ## Grow B number of trees
    for (b in 1:B){
      ## Step a): Draw a bootstrap sample Z* of size n from the clustered training data
      indicies=sample(1:nrow(cluster_df),n,replace=TRUE)
      cluster_sub_df=cluster_df[indicies,] # Bootstrap sample

      ## Select m variables at random from the p variables; typical value of m is 0.8*n, where n = nrow/ncol(A)
      m=sample(x=2:ncol(cluster_sub_df),size=prop_X*(ncol(cluster_sub_df)-1),replace=FALSE) # Sampled vertices at random
      m_list[[b]]=m # Stores the selected vertices to make the same selection for the testing data
      # I don't see how I can perform the subselection for the vertices when the clustering occurs outside of the trees

      ## Further subset cluster_sub_df
      cluster_sub_df=cluster_sub_df[,c(1,m),drop=FALSE]

      ### Grow a random-forest tree T_b to the bootstrapped data
      ### Construct a classification tree
      ### Fit a decision tree
      set.seed(b)
      decision_tree=rpart(
        response~.,
        data=cluster_sub_df,
        method=method
      )
      tree_list[[b]]=decision_tree # Adding a new tree to the current list of decision trees
    }

    result$m=m_list
    result$rf=tree_list # Outputting the tree list with B decision trees (random forest)

    ## Now generate a list of predicted values for the training data
    pred_vec=c() # Vector of final predicted class for each row in the training data
    if (method == "class"){
      class_vec=c() # Vector of predicted classes for all trees for a given row
      for (i in 1:nrow(cluster_df)){ # For each row in the clustered training data, predict its class
        for (b in 1:B){ # For each tree in the random forest
          T_b=tree_list[[b]]
          class_b=predict(T_b,newdata=cluster_df[i,],type = "class") # Predict the class from each tree
          class_vec[b]=unfactor(class_b)
        }
        class_i=as.numeric(names(which.max(table(class_vec)))) # Obtaining the "majority vote" for the final predicted class of row i
        pred_vec[i]=class_i
      }
    } else if (method == "anova") {
      value_vec=c() # Vector of predicted classes for all trees for a given row
      for (i in 1:nrow(cluster_df)){ # For each row in the clustered training data, predict its value
        for (b in 1:B){ # For each tree in the random forest
          T_b=tree_list[[b]]
          value_b=predict(T_b,newdata=cluster_df[i,],type = "vector")[[1]] # Predict the numeric value from each tree
          value_vec[b]=value_b
        }
        value_i=mean(value_vec) # Obtaining the average for the final predicted class of row i
        pred_vec[i]=value_i
      }
    }

    result$pred_vals=pred_vec
    result$clusters=clustering_memberships
    return(result) # Return the vector of predicted values for all rows in the training data
  })
}
