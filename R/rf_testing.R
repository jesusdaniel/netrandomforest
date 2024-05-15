## Random Forest With clustering on Testing Data
## Requires the following parameters:
### - X_testing = testing dataset to implement random forest on
### - rf_training = the random forest model fitted on the training data

rf_testing_pred=function(X_testing,rf_training,num_clusters){ # rf_training contains the trees ($rf), predicted values ($pred_vals), and clustering instructions ($clusters) from training data
  suppressWarnings({
    ## Select m variables at random from the p variables; typical value of m is 0.8*n, where n = nrow/ncol(A)
    # m=rf_training$m
    
    ## Create the adjacency list to be fed into the clustering function
    adj_list=list()
    for(i in 1:nrow(X_testing)){
      A=get_matrix(as.numeric(X_testing[i,c(3:ncol(X_testing))]))
      # A=A[m,m] # Select a subset of vertices from the adjacency matrix
      adj_list[[i]]=A
    }
    
    ## Clustering data based on the clustering instructions from the training data
    clusters <- cluster_testing(adj_list,rf_training$clusters,num_clusters)
    clustering_result=clusters
    Xnew1=ifelse(is.nan(clustering_result),0,clustering_result)
    cluster_df=data.frame(health_status=X_testing[,2],Xnew1)
    
    ## Reduce dataset using the selected predictors from the training function
    # pred_selected=rf_training$pairwisecor$selected.var.cols
    # clusterred_df=cluster_df[,c(1,pred_selected)]
    
    ## Now generate a list of predicted values for the testing data
    pred_vec=c() # Vector of final predicted class for each row in the testing data
    class_vec=c() # Vector of predicted classes for all trees for a given row
    for (i in 1:nrow(cluster_df)){ # For each row in the clustered testing data, predict its class
      for (b in 1:length(rf_training$rf)){ # For each tree in the random forest
        m=rf_training$m[[b]]
        # browser()
        cluster_df_tree=cluster_df[,c(1,m)] # This line of code gave me error
        T_b=rf_training$rf[[b]]
        class_b=predict(T_b,newdata=cluster_df_tree[i,],type ="class") # Predict the class from each tree (1 or -1)
        class_vec[b]=unfactor(class_b)
      }
      class_i=as.numeric(names(which.max(table(class_vec)))) # Obtaining the "majority vote" for the final predicted class of row i
      pred_vec[i]=class_i
    }
    return(pred_vec) # Return the vector of predicted values for all rows in the testing data
  })
}