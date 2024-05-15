## Random Forest With clustering on Training Data
## Requires the following parameters:
### - B = number of trees
### - n = number of subjects in each bootstrap sample
### - X_training = training dataset to implement random forest on
### - num_clusters = number of clusters (above 10 is ideal)
### - prop_vert = number of vertices in the original dataset

rf_training=function(B,n,X_training,num_clusters,prop_vert=.8){
  suppressWarnings({
    tree_list=list() # Create a list to store decision trees
    result=list()
    ## Create a list to store the pairwise correlations of all clustered dataframes
    # pairwisecor_list=list()
    m_list=list()
    
    ## Create the adjacency list to be fed into the clustering function
    adj_list=list()
    for(i in 1:nrow(X_training)){
      A=get_matrix(as.numeric(X_training[i,c(3:ncol(X_training))]))
      adj_list[[i]]=A
    }
    
    ## Clustering data into num_clusters
    set.seed(45)
    clusters <- clustering_vertices(adj_list,num_clusters)
    clustering_result=clusters$X_clusters
    clustering_memberships=clusters$cluster_memberships
    Xnew1=ifelse(is.nan(clustering_result),0,clustering_result)
    cluster_df=data.frame(health_status=X_training[,2],Xnew1)
    # result$clusterdf=cluster_df # Save the clustered dataframe
    
    ## Perform pairwise correlation analysis
    # pairwise_cor=corSelect(cluster_df, sp.cols = 1, var.cols=2:ncol(cluster_df), coeff = TRUE, 
    #                        cor.thresh = 0.5,
    #                        select = "cor", family = "auto",
    #                        use = "pairwise.complete.obs", method = "pearson")
    # result$pairwisecor=pairwise_cor # Save the pairwise correlation of that clustered dataframe
    # pred_selected=pairwise_cor$selected.var.cols # Variables that were not excluded
    
    ## Reduce the dataset using the selected variables
    # clusterred_df=cluster_df[,c(1,pred_selected)]
    
    ## Grow B number of trees
    for (b in 1:B){
      ## Step a): Draw a bootstrap sample Z* of size n from the clustered training data
      indicies=sample(1:nrow(cluster_df),n,replace=TRUE)
      cluster_sub_df=cluster_df[indicies,] # Bootstrap sample
      
      ## Select m variables at random from the p variables; typical value of m is 0.8*n, where n = nrow/ncol(A)
      m=sample(x=2:ncol(cluster_sub_df),size=prop_vert*(ncol(cluster_sub_df)-1),replace=FALSE) # Sampled vertices at random
      m_list[[b]]=m # Stores the selected vertices to make the same selection for the testing data
      # I don't see how I can perform the subselection for the vertices when the clustering occurs outside of the trees 
      
      ## Further subset cluster_sub_df 
      cluster_sub_df=cluster_sub_df[,c(1,m)]
      
      ### Grow a random-forest tree T_b to the bootstrapped data
      ### Construct a classification tree
      ### Fit a decision tree
      set.seed(b)
      decision_tree=rpart(
        health_status~.,
        data=cluster_sub_df,
        method="class"
      )
      tree_list[[b]]=decision_tree # Adding a new tree to the current list of decision trees
    }
    # browser()
    result$m=m_list 
    result$rf=tree_list # Outputting the tree list with B decision trees (random forest)
    
    ## Now generate a list of predicted values for the training data
    pred_vec=c() # Vector of final predicted class for each row in the training data
    class_vec=c() # Vector of predicted classes for all trees for a given row
    for (i in 1:nrow(cluster_df)){ # For each row in the clustered training data, predict its class
      for (b in 1:B){ # For each tree in the random forest
        T_b=tree_list[[b]]
        class_b=predict(T_b,newdata=cluster_df[i,],type ="class") # Predict the class from each tree (1 or -1); 
        # The line above is giving me error
        class_vec[b]=unfactor(class_b)
      }
      class_i=as.numeric(names(which.max(table(class_vec)))) # Obtaining the "majority vote" for the final predicted class of row i
      pred_vec[i]=class_i
    }
    result$pred_vals=pred_vec
    result$clusters=clustering_memberships
    return(result) # Return the vector of predicted values for all rows in the training data
  })
}