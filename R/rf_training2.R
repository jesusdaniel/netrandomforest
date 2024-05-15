## Random Forest With clustering within each tree generated from the training Data
## Requires the following parameters:
### - B = number of trees
### - n = number of subjects in each bootstrap sample
### - X_training = training dataset to implement random forest on
### - num_clusters = number of clusters (above 10 is ideal)
### - num_vert = number of vertices in the original dataset

rf_training2=function(B,n,X_training,num_clusters,prop_vert=.8){
  suppressWarnings({
    tree_list=list() # Create a list to store decision trees
    sel_vert_list=list() # List of selected vertices
    cluster_list=list() # List of clusters
    # clusterdf_list=list() # List of clustered datasets for each tree
    # pairwisecor_list=list() # List of pairwise correlations for each tree
    class_df=as.data.frame(matrix(NA,nrow=nrow(X_training),ncol=B)) # Matrix of predicted classes for all trees for a given row
    
    A=get_matrix(as.numeric(X_training[1,c(3:ncol(X_training))]))
    num_vert=ncol(A)
    
    ## Grow B number of trees
    for (b in 1:B){
      set.seed(b)
      
      ## Select m variables at random from the p variables; typical value of m is 0.8*n, where n = nrow/ncol(A)
      m=sample(x=1:num_vert,size=prop_vert*num_vert,replace=FALSE) # Sampled vertices at random
      
      ## Create the adjacency list to be fed into the clustering function
      adj_list=list()
      for(i in 1:nrow(X_training)){
        A=get_matrix(as.numeric(X_training[i,c(3:ncol(X_training))]))
        A=A[m,m] # Select a subset of vertices from the adjacency matrix
        adj_list[[i]]=A
      }
      
      ## Clustering data into num_clusters
      clusters <- clustering_vertices(adj_list,num_clusters)
      clustering_result=clusters$X_clusters
      clustering_memberships=clusters$cluster_memberships
      Xnew1=ifelse(is.nan(clustering_result),0,clustering_result)
      cluster_df=data.frame(health_status=X_training[,2],Xnew1)
      # clusterdf_list[[b]]=cluster_df
      
      ## Perform pairwise correlation analysis
      # pairwise_cor=corSelect(cluster_df, sp.cols = 1, var.cols=2:ncol(cluster_df), coeff = TRUE, 
      #                        cor.thresh = 0.5,
      #                        select = "cor", family = "auto",
      #                        use = "pairwise.complete.obs", method = "pearson")
      # pairwisecor_list[[b]]=pairwise_cor # Save the pairwise correlation of that clustered dataframe
      # pred_selected=pairwise_cor$selected.var.cols # Variables that were not excluded
      
      ## Reduce the dataset using the selected variables
      # clusterred_df=cluster_df[,c(1,pred_selected)]
      
      ## Step a): Draw a bootstrap sample Z* of size n from the clustered training data
      indicies=sample(1:nrow(cluster_df),n,replace=TRUE)
      cluster_sub_df=cluster_df[indicies,] # Bootstrap sample
      
      ### Grow a random-forest tree T_b to the bootstrapped data
      ### Construct a classification tree
      ### Fit a decision tree
      decision_tree=rpart(
        health_status~.,
        data=cluster_sub_df,
        method="class"
      )
      tree_list[[b]]=decision_tree # Adding a new tree to the current list of decision trees
      sel_vert_list[[b]]=m
      cluster_list[[b]]=clustering_memberships
      
      ## Now generate a list of predicted values for the training data
      for (i in 1:nrow(cluster_df)){ # For each tree in the random forest
        T_b=tree_list[[b]]
        class_b=predict(T_b,newdata=cluster_df[i,],type ="class") # Predict the class from each tree (1 or -1)
        class_df[i,b]=unfactor(class_b)
      }
    }
    result=list()
    result$rf=tree_list # Outputting the tree list with B decision trees (random forest)
    result$m=sel_vert_list
    result$clusters=cluster_list
    # result$clusterdf=clusterdf_list
    # result$pairwisecor=pairwisecor_list
    
    pred_vec=c() # Vector of final predicted class for each row in the training data
    for (i in 1:nrow(cluster_df)){ # For each row in the bth clustered training data, predict its class
      class_i=as.numeric(Mode(na.omit(class_df[i,]))) # Obtaining the "majority vote" for the final predicted class of row i
      pred_vec[i]=class_i
    }
    result$pred_vals=pred_vec
    return(result) # Return the vector of predicted values for all rows in the training data
  })
}