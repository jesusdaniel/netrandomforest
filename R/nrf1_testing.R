#' Net Random Forest Ver. 1 (NRF1) - Testing
#'
#' Performs spectral clustering outside of all trees of the random forest on the testing data.
#'
#' @param X_testing Features of the testing data.
#' @param Y_testing Supervisor of the testing data.
#' @param nrf1_mod A trained net random forest model.
#' @param num_clusters Number of clusters for the spectral clustering algorithm.
#' @param method "anova" (regression) or "class" (classification).
#'
#' @return A vector of predicted response values from the testing set.
#'
#' @export

nrf1_testing=function(X_testing,Y_testing,nrf1_mod,num_clusters,method){ # nrf1_mod contains the trees ($rf), predicted values ($pred_vals), and clustering instructions ($clusters) from training data
  require(rpart)
  suppressWarnings({
    ## Create the adjacency list to be fed into the clustering function
    adj_list=list()
    for(i in 1:nrow(X_testing)){
      A=get_matrix(as.numeric(X_testing[i,]))
      adj_list[[i]]=A
    }

    ## Clustering data based on the clustering instructions from the training data
    clusters <- cluster_testing(adj_list,nrf1_mod$clusters,num_clusters)
    clustering_result=clusters
    Xnew1=ifelse(is.nan(clustering_result),0,clustering_result)
    cluster_df=data.frame(response=Y_testing,Xnew1)

    ## Now generate a list of predicted values for the testing data
    pred_vec=c() # Vector of final predicted class for each row in the testing data
    if (method == "class"){
      class_vec=c() # Vector of predicted classes for all trees for a given row
      for (i in 1:nrow(cluster_df)){ # For each row in the clustered testing data, predict its class
        for (b in 1:length(nrf1_mod$rf)){ # For each tree in the random forest
          m=nrf1_mod$m[[b]]
          cluster_df_tree=cluster_df[,c(1,m),drop=FALSE] # This line of code gave me error
          T_b=nrf1_mod$rf[[b]]
          class_b=predict(T_b,newdata=cluster_df_tree[i,],type ="class") # Predict the class from each tree
          class_vec[b]=unfactor(class_b)
        }
        class_i=as.numeric(names(which.max(table(class_vec)))) # Obtaining the "majority vote" for the final predicted class of row i
        pred_vec[i]=class_i
      }
    } else if (method == "anova") {
      value_vec=c() # Vector of predicted reponse for all trees for a given row
      for (i in 1:nrow(cluster_df)){ # For each row in the clustered testing data, predict its class
        for (b in 1:length(nrf1_mod$rf)){ # For each tree in the random forest
          m=nrf1_mod$m[[b]]
          cluster_df_tree=cluster_df[,c(1,m),drop=FALSE]
          T_b=nrf1_mod$rf[[b]]
          value_b=predict(T_b,newdata=cluster_df_tree[i,],type ="vector")[[1]] # Predict the value from each tree
          value_vec[b]=value_b
        }
        value_i=mean(value_vec) # Obtaining the average for the final predicted class of row i
        pred_vec[i]=value_i
      }
    }
    return(pred_vec) # Return the vector of predicted values for all rows in the testing data
  })
}
