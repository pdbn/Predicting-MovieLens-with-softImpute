#Load necessary libraries
packages <- c("data.table",
              "reshape2", 
              "softImpute",
              "dplyr")

packages_to_install <- packages[!packages %in% installed.packages()[,"Package"]]

if(length(packages_to_install)) install.packages(packages_to_install, repos = "https://cloud.r-project.org/")

lapply(packages, require, character.only = TRUE)

#Set working directory
setwd()

#Read CSV file
ratings <- fread("ratings.csv")
head(ratings)
head(ratings$timestamp)

set.seed(6290653)   #student number

#####-----------------------------------------------------------------------#####
# Convert timestamp to Date

#Filter data for ratings on or after February 18, 2003 (half-star scale)
ratings[, datetime := as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")]
ratings[, date := as.Date(datetime)]
ratings <- ratings[date >= as.Date("2003-02-18")]

#Subsample 1% of data randomly 
sample_size <- 0.01
n_rows_to_sample <- floor(nrow(ratings) * sample_size)
sample_indices <- sample(1:nrow(ratings), size = n_rows_to_sample)
ratings_sample <- ratings[sample_indices, ]

#Print structure, summary of sampled data
#str(ratings_sample)
#summary(ratings_sample)
#head(ratings_sample)

#Function to sample data ensuring each user rates a certain number of movies 
#and each movie is rated at least a certain time
sample_ratings <- function(ratings, min_ratings_per_user, min_users_per_movie) {
  # Ensure each movie is rated at least min_users_per_movie
  movies_sample <- ratings %>%
    group_by(movieId) %>%
    summarise(n = n(), .groups = 'drop') %>%
    filter(n >= min_users_per_movie) %>%
    pull(movieId)
  
  # Ensure each user rates at least min_ratings_per_user movies
  users_sample <- ratings %>%
    filter(movieId %in% movies_sample) %>%
    group_by(userId) %>%
    summarise(n = n(), .groups = 'drop') %>%
    filter(n >= min_ratings_per_user) %>%
    pull(userId)
  
  # Get the final sample
  ratings_sample <- ratings %>%
    filter(userId %in% users_sample, movieId %in% movies_sample)
  
  return(ratings_sample)
}

# Sample: users rated at least 5 movies, movies get rated at least 5 times
ratings_sample <- sample_ratings(ratings_sample, 5, 5)

# Create the initial matrix
ratings_matrix <- dcast(ratings_sample, userId ~ movieId, value.var = "rating")
ratings_matrix <- as.matrix(ratings_matrix[,-1])  #remove first column userId
print(dim(ratings_matrix))   # users x  movies

#Implement softImpute
lambda0(ratings_matrix)
