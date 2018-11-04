### Hierarchical Clustering of Variables

# Read in the dataset and convert into a matrix
wbc <- as.matrix(read.csv('wisc_bc_data.csv'))

# Group into benign and malignant
wbc_b <- wbc[wbc[,2] == "B",]
wbc_m <- wbc[wbc[,2]=="M",]

# Remove the first two columns of the dataset and center/scale it
wbc <- wbc[,3:ncol(wbc)]
wbc <- apply(wbc,2,as.numeric)
wbc <- scale(wbc, center = TRUE, scale = TRUE)

# Following this: http://research.stowers.org/mcm/efg/R/Visualization/cor-cluster/index.htm
# Get the correlation matrix and turn it into a dissimilarity matrix 
wbc_var_dist <- as.dist(cor(wbc))

# Call the hierarchical clustering algorithm and plot 
plot(hclust(wbc_var_dist))


# Define a function to perform the same operations on all the data
hc_processor <- function(dataset){
  dataset <- dataset[,3:ncol(dataset)]
  dataset <- apply(dataset,2,as.numeric)
  dataset <- scale(dataset, center = TRUE, scale = TRUE)
  dataset_var_dist <- as.dist(cor(dataset))
  plot(hclust(dataset_var_dist))
}

par(mfrow = c(1,3))
hc_processor(wbc_b)
hc_processor(wbc_m)
hc_processor(wbc)

