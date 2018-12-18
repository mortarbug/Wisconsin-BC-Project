##
## Classfication of Benign and Malignant Tumors
## Patrick Vo
##

###### Setup #####
setwd('C:/Users/MBG/Google Drive/Academic Work/Wisconsin Breast Cancer')
set.seed(100)
# Read in the data 
wbc <- as.matrix(read.csv('wisc_bc_data.csv'))
wbc <- na.omit(wbc)
# Import
library(glmnet)
library(e1071)
library(caret)
library(randomForest)
#####################


# Remove the ID column and separate the response column from the rest of the matrix
#    After ID and diagnosis have been removed, convert the matrix to numerics
wbc <- wbc[,-1]
diagnosis <- as.factor(wbc[,1])
wbc<- wbc[,-1]
wbc <- apply(wbc, 2, FUN = as.numeric)





###### Additional Functions #########

# Write a function for 10-fold cross-validation. It will take in a training
#    dataset and split it into training and test datasets
cross_v <- function(training_dataset, k){
  numrow <- floor(nrow(training_dataset) / 10)
  
  # Split the data into training and validation sets
  training_data <- training_dataset[(training_dataset$k_indices < numrow * (k-1)) | 
                                      (training_dataset$k_indices >= numrow*k), ]
  training_data <- subset(training_data, select = -k_indices)
  validation_data <- training_dataset[((training_dataset$k_indices >= numrow * (k-1)) & 
                                          (training_dataset$k_indices < numrow*k)), ]
  validation_data <- subset(validation_data, select = -k_indices)
  
  # Put both dataframes into a list
  return(list(training_data, validation_data))
}

##############################3

cross_v <- function(training_dataset, k){
  # Create a list of indices to be selected into the validation set
  k_fold <- c((k*10):((k+1)*10))
  
  # The rows within k_fold inclusive are to be the validation set, the rows outside of
  #    k-fold are to be used to train the model
  training_data <- training_dataset[k_fold, ]
  validation_data <- training_dataset[-k_fold, ]
  
  return(list(training, validation))
}









##### Train/Test Split #####
# Train/Test Split the data
training_indices <- sample(nrow(wbc), 0.7*nrow(wbc), replace = FALSE)
train <- wbc[training_indices,]
test <- wbc[-training_indices,]
diagnosis_train <- diagnosis[training_indices]
diagnosis_test <- diagnosis[-training_indices]




# Scale the training and test data, because lasso-logit and svm require scaling and rf isn't affected
train_colMeans <- colMeans(train)
train_colSD <- apply(train,2,sd)
train <- scale(train, center = TRUE, scale = TRUE)




###########################

######### Validation set splitter from the Caret library #########
validation_splits <- createFolds(train, k = 10, list = TRUE, returnTrain = FALSE)







##### Regularized Logistic Regression #####

# Split the training dataset into predictor and response columns. Convert the 
#    dataframes to numberic matrices for the glmnet functions
predictors <- as.matrix(train[,3:32])
storage.mode(predictors) <- "numeric"
response <- as.matrix(train[,2])

# Use cross-validation via the cv.glmnet function to find an optimal value of the 
#   LASSO tuning parameter
lasso_fit <- cv.glmnet(x = predictors, y = response, family = "binomial")
minimum_lambda <- lasso_fit$lambda.min 

# Fit the logistic lasso onto the training set and calculate the balanced
#    accuracy on the validation set
numrow <- nrow(train) / 10
for (i in 1:1) {
  
  # Fit the binomial glmnet model and calculate the probability as the 
  k_fold <- c(((i-1) * numrow):(i * numrow))
  logistic_val_fit <- glmnet(x = predictors[-k_fold,], y = response[-k_fold, ],
                                family = 'binomial', lambda = minimum_lambda)
  logistic_val_predict <- predict(logistic_val_fit, newx = predictors[k_fold,], type = 'response')
  
  # Print out the classification matrix and the null deviance value
  table(logistic_val_predict, newx = response[k_fold, ])
}



##
########## Support Vector Machine with Tuned Kernel and Crossover  ####################
##
library('e1071')


# Declare the possible parameters that can be passed into a support vector machine
possible_kernels = c('linear', 'polynomial', 'radial basis', 'sigmoid')

# Declare a grid of parameters that we want to use to tune C 
possible_cost <- c(0.01, 0.1, 1, 10, 100)

# Declare a training schema for gridsearch with 10-fold cross-validation
control <- trainControl(method = "cv", number = 10)
grid <- expand.grid(kernel=possible_kernels, cost = possible_cost)

# Fit the SVMs




##
######### Random Forest Algorithm with tuned number of nodes at each split 
##

# Load the library
library(randomForest)

# Split the training data into a training and validation set.
training_indices <- sample(0.8*nrow(train), replace = FALSE)
training_data <- train[training_indices,]
training_y <- diagnosis_train[training_indices]
validation_data <- train[-training_indices, ]
validation_y <- diagnosis_train[-training_indices]

# Fit the random forest classifier with the number of variables sampled = sqrt(p)
#     then print the OOB confusion matrix and draw the variable importance plot 
rf_fit <- randomForest(x = training_data, y = training_y)
rf_fit$confusion
varImpPlot(rf_fit, sort = TRUE)

# Evaluate the classification ability on the validation data
rf_pred <- predict(rf_fit, validation_data, type = 'class')
table(rf_pred, validation_y)





