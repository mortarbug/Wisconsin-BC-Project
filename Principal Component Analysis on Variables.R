### Principal Components Analysis on Variables 
setwd('C:/Users/MBG/Google Drive/Academic Work/Wisconsin Breast Cancer')
library(ggplot2)

# Read in the dataset 
wbc <- as.matrix(read.csv('wisc_bc_data.csv'))


# Remove the ID column and separate the response column from the rest of the matrix
#    After ID and diagnosis have been removed, convert the matrix to numerics
wbc <- wbc[,-1]
diagnosis <- wbc[,1]
wbc<- wbc[,-1]
wbc <- apply(wbc, 2, FUN = as.numeric)

# Center and scale the matrix according to column. This should end with mu = 0, sd = 1
wbc <-  scale(wbc, center = TRUE, scale = TRUE)

# Find the eigendecomposition of the variance matrix and save the values and vectors
egn <- eigen(var(wbc))
combo_coefficients <- egn$vectors
variance_explained <- egn$values

# Print a SCREE plot of the variance explained
par(mfrow = c(1,1))
percent_variance_explained <- variance_explained/sum(variance_explained)
x <- (1:length(variance_explained))
plot(x, percent_variance_explained) 

# Find the first two principal components 
PCs <- wbc %*% combo_coefficients[,1:2]

# Combine the matrix of principal components with the vector of responses
PCs <- data.frame(cbind(PCs, diagnosis))
names(PCs) <- c('PC1', 'PC2', 'Diagnosis')


# Convert the first two principal components into numerics instead of factors
PCs$PC1 <- as.numeric(PCs$PC1)
PCs$PC2 <- as.numeric(PCs$PC2)

# Create a plot by color of the first two principal components 
ggplot(PCs, aes(x = PC1,y = PC2)) + 
  geom_point(aes(col = Diagnosis)) 


# Print out the first two principal components in order of their largest values 
#    (in terms of the absolute value of the component) 
print(egn$vectors[,1])
print(signif(egn$vectors[order(egn$vectors[,1]),1], digits = 1))

print(egn$vectors[,2])
print(signif(egn$vectors[order(egn$vectors[,2]),2], digits = 1))


# Print out collections of variables corresponding to values of the eigenvectors
#    that have high absolute values
wbc_names <- colnames(wbc)
wbc_names[order(egn$vectors[,1])][1:15]

wbc_names[order(egn$vectors[,1])][1:7]
wbc_names[order(egn$vectors[,1])][23:30]


