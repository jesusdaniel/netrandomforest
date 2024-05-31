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

## Create a matrix to store the results of the simulation to later be converted to a dataframe object
### col 1 = "Sim", col 2 = "q value" (0.2-0.5, inclusive), col 3 = "RF" (Standard RF model), col 4 = "NRF1" (RF model with clustering outside the trees), col 5 = "NRF2" (RF model with clustering within each tree), col 6 = "RerF" (Randomer Forest model), , col 7 = "SVM"

rf_methods_all=matrix(ncol=7,nrow=5*4) # 5 reps * 4 different q values

## Define constant variables outside of all loops
num_clusters=3
index=0 # Initializing row index for the rf_methods_all table

## Obtain the accuracies for a given p1 value, then populate the sims table
for(j in c(1:5)){ # 5 replications
  
  q_vals=c(.2,.3,.4,.5)
  
  for(i in 1:length(q_vals)){
    
    index=index+1
    
    rf_methods_all[index,1]=j # Store sim number
    
    q=q_vals[i]
    rf_methods_all[index,2]=q
    
    ## Randomly generate a dataset
    set.seed(j)
    SBM_sims=generate_data_SBM(q=q,n=100,m1=50,m2=50)
    SBM_sims_df=SBM_sims$df
    
    ## Method 1: Standard random forest
    training_data=SBM_sims_df[training_data_ind,]
    testing_data=SBM_sims_df[testing_data_ind,]
    
    ## Train classifier on training data 
    set.seed(45)
    rf_mod1 <- randomForest(x=training_data[,c(3:ncol(training_data))],
                            y=factor(training_data[,2]),
                            ntree=10,
                            importance=TRUE)
    
    ## Test classifier on testing data
    test=predict(rf_mod1,newdata=testing_data[,c(3:ncol(testing_data))],type='response')
    
    ## Store results
    rf_methods_all[index,3]=1-mean(test != testing_data$response) # Store classification accuracy of the standard RF method
    
    ## Method 2: Custom random forest method with cluster outside the trees
    rf_mod2=nrf1_training(B=10,n=length(training_data_ind),X_training=training_data[,c(3:ncol(training_data))],Y_training=training_data$response,num_clusters=num_clusters,prop_X=.8,method="class")
    test=nrf1_testing(X_testing=testing_data[,c(3:ncol(testing_data))],Y_testing=testing_data$response,nrf1_mod=rf_mod2,num_clusters=num_clusters,method="class")
    rf_methods_all[index,4]=1-mean(test != testing_data$response)
    
    ## Method 3: Custom random forest method with cluster within the trees
    rf_mod3=nrf2_training(B=10,n=length(training_data_ind),X_training=training_data[,c(3:ncol(training_data))],Y_training=training_data$response,num_clusters=num_clusters,prop_X=.8,method="class") 
    test=nrf2_testing(X_testing=testing_data[,c(3:ncol(testing_data))],Y_testing=testing_data$response,nrf2_mod=rf_mod3,num_clusters=num_clusters,method="class")
    rf_methods_all[index,5]=1-mean(test != testing_data$response)
    
    ## Method 4: RerF
    rf_mod4 <- RerF(X=training_data[,c(3:ncol(training_data))], 
                    Y=factor(training_data$response), 
                    trees=10,
                    num.cores = 1L,
                    seed = 45)
    test <- Predict(testing_data[,c(2:ncol(testing_data))], rf_mod4, num.cores = 1L, Xtrain = 
                      training_data[,c(3:ncol(training_data))])
    rf_methods_all[index,6] <- 1-mean(test != testing_data$response)
    
    ## Remove columns that are constant (zero variance) from the training data (keep track of those indices); name it training_data_zv and testing_data_zv
    ## Subset the original dataframe to only columns with non-constant variances
    ## Since the testing data has more columns with constant variance, we should also remove those same columns from the training data to maintain consistency of the features between the two dataframes
    zv_ind_testing=testing_data%>%preProcess(method = 'zv')
    zv_ind_training=training_data%>%preProcess(method = 'zv')
    zv_ind=c(zv_ind_testing$method[[2]],zv_ind_training$method[[2]])
    testing_data_zv=testing_data[, !(names(testing_data) %in% zv_ind)]
    training_data_zv=training_data[, !(names(training_data) %in% zv_ind)]
    
    ## Method 5: SVM
    suppressWarnings({
    rf_mod5 <- svm(
      x=training_data_zv[,c(3:ncol(training_data_zv))],
      y=training_data_zv$response 
    )
    })
    test=predict(rf_mod5,newdata=testing_data_zv[,c(3:ncol(testing_data_zv))],type='raw')
    rf_methods_all[index,7] <- 1-mean(test != testing_data_zv$response)
  }
}

## Convert to a dataframe object, add column names, and display the results
rf_methods_all=data.frame(rf_methods_all)
colnames(rf_methods_all)=c("Sim","q_value","RF","NRF1","NRF2","RerF","SVM")

## Convert the dataframe object from wide to long format to plot the results
rf_methods_allw=rf_methods_all
rf_methods_allw=rf_methods_allw %>%
  group_by(q_value) %>%
  summarise(RF.Mean = mean(RF),RF.SD = sd(RF), NRF1.Mean = mean(NRF1),NRF1.SD = 
              sd(NRF1), NRF2.Mean = mean(NRF2),NRF2.SD = sd(NRF2),
            RerF.Mean = mean(RerF),RerF.SD = sd(RerF),SVM.Mean = 
              mean(SVM),SVM.SD = sd(SVM))

rf_methods_alll=rf_methods_allw %>% 
  gather(v, value, RF.Mean:SVM.SD) %>% 
  separate(v, c("RF_Method", "col")) %>% 
  arrange(q_value) %>% 
  spread(col, value)

## Graphing the results
ggplot(rf_methods_alll,aes(x=q_value,y=Mean,colour=RF_Method,group=RF_Method)) +
  geom_line(size=1) +
  labs(title = "Random Forest Models vs. q Value\n", 
       x = "q Value", 
       y = "Mean Classification Accuracy", 
       color = "RF Method\n") +
  theme(plot.title = element_text(hjust = 0.5))
