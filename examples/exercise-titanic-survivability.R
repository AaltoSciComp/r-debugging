# Load titanic survivability data
data(Titanic)

titanic <- as.data.frame(Titanic)

check_survivability <- function(feature) {
  # Function for checking survivability per feature
  
  # Get the different feature levels
  levels <- levels(as.factor(titanic[[feature]]))
  
  # Create output data frame
  titanic_survivability <- data.frame(row.names=levels)
  
  # Calculate survivability per level
  for (level in levels) {
    level_data <- titanic[Titanic[[feature]] == level, ]
    n_survived <- sum(level_data[level_data$Survived == 'Yes', 'Freq'])
    n_died <- sum(level_data[level_data$Survived == 'No', 'Freq'])
    titanic_survivability[levels, 'survivability'] <- n_survived / (n_survived + n_died)
  }
  return(titanic_survivability)
}

# Features to iterate over
different_features <- c('Class', 'Sex', 'Age')

# Analyze survivability per feature
for (feature in different_features) {
  cat(paste('Feature to check:', feature, '\n'))
  print(check_survivability(feature))
}