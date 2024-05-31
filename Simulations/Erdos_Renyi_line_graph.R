## Loading libraries and custom functions
library(netRandomForest)
library(randomForest) 
library(rerf) 
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
## Note: I am unable to load these functions, even though the files are in the same directory as this R script

## Create a custom function that generates and processes data from Erdos Renyi model
generate_data=function(p1,p2,n,m1,m2){
  ## Generate population of graphs
  ## Class 1:
  Adj_list_1 <- list()
  Adj_list_1 <- lapply(1:m1, function(i) get.adjacency(sample_gnp(n, p = p1, directed = FALSE, loops = FALSE)))
  
  ## Class 2:
  Adj_list_2 <- list()
  Adj_list_2 <- lapply(1:m2, function(i) get.adjacency(sample_gnp(n, p = p2, directed = FALSE, loops = FALSE)))
  
  ## Simulated data:
  Adj_list <- c(Adj_list_1, Adj_list_2)
  class_label <- c(rep(1, m1), rep(-1, m2))
  
  ## Convert to a 100 x 435 dataframe
  erdos_sims_df=c() # List of all sims
  
  for(i in 1:100){
    sims_matrix=Adj_list[[i]]
    A = matrix(0, n, n)
    
    ## Convert empty elements in sparse matrix with 0's
    for(j in 1:n){
      for (k in 1:n){
        if (sims_matrix[j,k] == 1){
          A[j,k]=sims_matrix[j,k] # Change the value of (j,k) element in matrix A to 1 if the element the corresponding position of the sims_matrix is 1 
        }
      }
    }
    
    ## Appending the sims list with vectors (converted from matrices)
    erdos_sims_df=rbind(erdos_sims_df,matrix_to_vec(A, type="undirected"))
  }
  
  erdos_sims_df=as.data.frame(erdos_sims_df)
  
  ## Adding columns for response variable and id
  erdos_sims_df$id=c(1:100)
  erdos_sims_df$response=class_label
  erdos_sims_df=erdos_sims_df[,c(436,437,1:435)] # Rearranging the data so that the id and response show up as the first and second columns of the data frame, respectively
  
  return(erdos_sims_df)
}

## Generate a single dataset
set.seed(45)
erdos_sims_df=generate_data(p1=0.5,p2=0.9,n=30,m1=50,m2=50)

## Perform data splitting 
testing_data_ind = createDataPartition(erdos_sims_df$response, p = .25, list = FALSE) %>% as.vector(.)
training_data_ind = (1:nrow(erdos_sims_df))[-testing_data_ind]

## Multiple Simulations for each RF method across different |p1-p2| values
## Create a matrix to store the results of the simulation to later be converted to a dataframe object
### col 1 = "Sim", col 2 = "p1_p2_AbsDiff" (0-0.4, inclusive), col 3 = "RF" (Standard RF model), col 4 = "NRF1" (RF model with clustering outside the trees), col 5 = "NRF2" (RF model with clustering within each tree), col 6 = "RerF"
rf_methods_all=matrix(ncol=6,nrow=5*4)

## Define constant variables outside of all loops
num_clusters=3
p1=0.5
index=0 # Initializing row index for the rf_methods_all table

## Obtain the accuracies for a given p1 value, then populate the sims table
for(j in c(1:5)){ # 5 replications
  
  p_vals=c(.5,.6,.7,.8,.9)
  
  for(i in 1:length(p_vals)){
    
    index=index+1
    
    rf_methods_all[index,1]=j # Store sim number
    
    p2=p_vals[i]
    rf_methods_all[index,2]=abs(p1-p2)
    
    ## Randomly generate a dataset
    set.seed(j)
    erdos_sims_df=generate_data(p1=p1,p2=p2,n=30,m1=50,m2=50)
    
    ## Method 1: Standard random forest
    training_data=erdos_sims_df[training_data_ind,]
    testing_data=erdos_sims_df[testing_data_ind,]
    
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
  }
}

## Convert to a dataframe object, add column names, and display the results
rf_methods_all=data.frame(rf_methods_all)
colnames(rf_methods_all)=c("Sim","p1_p2_AbsDiff","RF","NRF1","NRF2","RerF")

## Convert from wide to long format
rf_methods_allw=rf_methods_all
rf_methods_allw=rf_methods_allw %>%
  group_by(p1_p2_AbsDiff) %>%
  summarise(RF.Mean = mean(RF),RF.SD = sd(RF), NRF1.Mean = mean(NRF1),NRF1.SD = 
              sd(NRF1), NRF2.Mean = mean(NRF2),NRF2.SD = sd(NRF2),
            RerF.Mean = mean(RerF),RerF.SD = sd(RerF))

rf_methods_alll=rf_methods_allw %>% 
  gather(v, value, RF.Mean:RerF.SD) %>% 
  separate(v, c("RF_Method", "col")) %>% 
  arrange(p1_p2_AbsDiff) %>% 
  spread(col, value)

## Graphing the results
ggplot(rf_methods_alll,aes(x=p1_p2_AbsDiff,y=Mean,colour=RF_Method,group=RF_Method)) +
  geom_line() +
  labs(title = "Random Forest Models vs. Absolute Difference in Edge Probability\n", 
       x = "|p1 - p2|", 
       y = "Mean Classification Accuracy", 
       color = "RF Method\n")


