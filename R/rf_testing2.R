## Random Forest With clustering on Testing Data
## Requires the following parameters:
### - X_testing = testing dataset to implement random forest on
### - rf_training = the random forest model fitted on the training data

rf_testing_pred2=function(X_testing,rf_training,num_clusters){ # rf_training contains the trees ($rf), predicted values ($pred_vals), selected vertices ($m), and clustering instructions ($clusters) from training data
  suppressWarnings({
    class_df=as.data.frame(matrix(NA,nrow=nrow(X_testing),ncol=length(rf_training$rf))) # Matrix of predicted classes for all trees for a given row
    
    ## Create B different clustered datasets and the predict the values for each one
    for (b in 1:length(rf_training$rf)){ # For each tree in the random forest
      set.seed(b)
      
      ## Select m variables at random from the vector of selected vertices
      m=rf_training$m[[b]]
      
      ## Create the adjacency list to be fed into the clustering function
      adj_list=list()
      for(i in 1:nrow(X_testing)){
        A=get_matrix(as.numeric(X_testing[i,c(3:ncol(X_testing))]))
        A=A[m,m] # Select a subset of vertices from the adjacency matrix
        adj_list[[i]]=A
      }
      
      ## Clustering data based on the clustering instructions from the training data
      clusters <- cluster_testing(adj_list,rf_training$clusters[[b]],num_clusters)
      clustering_result=clusters
      Xnew1=ifelse(is.nan(clustering_result),0,clustering_result)
      cluster_df=data.frame(health_status=X_testing[,2],Xnew1)
      
      ## Reduce dataset using the selected predictors from the training function
      # pred_selected=rf_training$pairwisecor[[b]]$selected.var.cols
      # clusterred_df=cluster_df[,c(1,pred_selected)]
      
      for (i in 1:nrow(X_testing)){ # For each row in the bth testing data, predict its class
        ## Prediction
        T_b=rf_training$rf[[b]]
        class_b=predict(T_b,newdata=cluster_df[i,],type ="class") # Predict the class from each tree (1 or -1)
        class_df[i,b]=unfactor(class_b)
      }
    }
    
    ## Now generate a list of predicted values for the testing data
    pred_vec=c() # Vector of final predicted class for each row in the testing data
    for (i in 1:nrow(cluster_df)){ # For each row in the bth testing data, predict its class
      class_i=as.numeric(Mode(na.omit(class_df[i,]))) # Obtaining the "majority vote" for the final predicted class of row i
      pred_vec[i]=class_i
    }
    return(pred_vec) # Return the vector of predicted values for all rows in the testing data
  })
}