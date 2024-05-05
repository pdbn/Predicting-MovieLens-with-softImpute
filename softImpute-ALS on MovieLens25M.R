#Load necessary libraries
packages <- c("data.table",
              "reshape2", 
              "softImpute")

packages_to_install <- packages[!packages %in% installed.packages()[,"Package"]]
if(length(packages_to_install)) install.packages(packages_to_install, repos = "https://cloud.r-project.org/")

lapply(packages, require, character.only = TRUE)

setwd()

ratings <- fread("ratings.csv")

set.seed(6290653)   #my student number

#Subsample 0.1% of data randomly
sample_size <- 0.001
n_rows_to_sample <- floor(nrow(ratings) * sample_size)
sample_indices <- sample(1:nrow(ratings), size = n_rows_to_sample)
ratings_sample <- ratings[sample_indices, ]

#Print structure, summary of sampled data
str(ratings_sample)
summary(ratings_sample)

# Create the matrix
ratings_matrix <- dcast(ratings_sample, userId ~ movieId, value.var = "rating")

ratings_matrix <- as.matrix(ratings_matrix[,-1])  #remove first column userId
print(dim(ratings_matrix))   #20046 user x 5765 movies

#Implement softImpute

