#' Net Random Forest Ver. 2 (NRF2) - Testing
#'
#' Performs spectral clustering within each tree in the random forest on the testing data.
#'
#' @param X_testing Features of the testing data.
#' @param Y_testing Supervisor of the testing data.
#' @param nrf2_mod A trained net random forest model.
#' @param num_clusters Number of clusters for the spectral clustering algorithm.
#' @param method "anova" (regression) or "class" (classification).
#'
#' @return A vector of predicted response values from the testing set.
#'
#' @export

nrf2_testing=function(X_testing,Y_testing,nrf2_mod,num_clusters,method){ # nrf2_mod contains the trees ($rf), predicted values ($pred_vals), selected vertices ($m), and clustering instructions ($clusters) from training data
  require(rpart)
  suppressWarnings({
    class_df=as.data.frame(matrix(NA,nrow=nrow(X_testing),ncol=length(nrf2_mod$rf))) # Matrix of predicted classes for all trees for a given row
    value_df=as.data.frame(matrix(NA,nrow=nrow(X_testing),ncol=length(nrf2_mod$rf))) # Matrix of predicted values for all trees for a given row

    ## Create B different clustered datasets and the predict the values for each one
    for (b in 1:length(nrf2_mod$rf)){ # For each tree in the random forest
      set.seed(b)

      ## Select m variables at random from the vector of selected vertices
      m=nrf2_mod$m[[b]]

      ## Create the adjacency list to be fed into the clustering function
      adj_list=list()
      for(i in 1:nrow(X_testing)){
        A=get_matrix(as.numeric(X_testing[i,]))
        A=A[m,m] # Select a subset of vertices from the adjacency matrix
        adj_list[[i]]=A
      }

      ## Clustering data based on the clustering instructions from the training data
      clusters <- cluster_testing(adj_list,nrf2_mod$clusters[[b]],num_clusters)
      clustering_result=clusters
      Xnew1=ifelse(is.nan(clustering_result),0,clustering_result)
      cluster_df=data.frame(response=Y_testing,Xnew1)

      if (method == "class"){
        for (i in 1:nrow(X_testing)){ # For each row in the bth testing data, predict its class
          ## Prediction
          T_b=nrf2_mod$rf[[b]]
          class_b=predict(T_b,newdata=cluster_df[i,],type ="class") # Predict the class from each tree (1 or -1)
          class_df[i,b]=unfactor(class_b)
        }
      } else if (method == "anova") {
        for (i in 1:nrow(X_testing)){ # For each row in the bth testing data, predict the value
          ## Prediction
          T_b=nrf2_mod$rf[[b]]
          value_b=predict(T_b,newdata=cluster_df[i,],type ="vector")[[1]] # Predict the class from each tree (1 or -1)
          value_df[i,b]=value_b
        }
      }
    }

    ## Now generate a list of predicted values for the testing data
    pred_vec=c() # Vector of final predicted response for each row in the testing data
    if (method == "class"){
      for (i in 1:nrow(cluster_df)){ # For each row in the bth testing data, predict its class
        class_i=as.numeric(Mode(na.omit(class_df[i,]))) # Obtaining the "majority vote" for the final predicted class of row i
        pred_vec[i]=class_i
      }
    } else if (method == "anova") {
      for (i in 1:nrow(cluster_df)){ # For each row in the bth testing data, predict its value
        value_i=mean(as.matrix(value_df)[i,]) # Obtaining the "average" for the final predicted value of row i
        pred_vec[i]=value_i
      }
    }

    return(pred_vec) # Return the vector of predicted values for all rows in the testing data
  })
}
