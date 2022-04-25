
normalize_standard <- function(dataset) {
  # Normalizes each column in the dataset to standard normalization
  # https://en.wikipedia.org/wiki/Standard_score
  normalized_data <- data.frame()
  for (column in colnames(dataset)) {
    data_mean <- mean(dataset[[column]])
    data_sd <- sd(dataset[column])
    normalized_data[[column]] <- ((dataset[[column]] - data_mean))/data_sd
  }
  return(normalized_data)
}

kmeans_iris <- function() {
  # Load data
  data(iris)
  
  # Get a copy of the iris dataset
  iris_data <- iris
  
  # Normalize data
  iris_normalized <- normalize_standard(iris_data)
  
  # Take species out
  species <- iris_data$Species
  iris_data$Species <- NULL
  
  # Calculate kmeans clustering
  # https://en.wikipedia.org/wiki/K-means_clustering
  clustering <- kmeans(iris_normalized, 3)
  
  # Calculate confusion matrix
  # https://en.wikipedia.org/wiki/Confusion_matrix#Confusion_matrices_with_more_than_two_categories
  table(species, clustering$clusters)
}

print(kmeans_iris())