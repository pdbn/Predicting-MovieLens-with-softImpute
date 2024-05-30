# Load necessary libraries
packages <- c("data.table",
              "reshape2", 
              "softImpute",
              "dplyr",
              "tidyr",
              "Matrix",
              "progress",
              "ggplot2", 
              "caret"
              )

packages_to_install <- packages[!packages %in% installed.packages()[,"Package"]]

if(length(packages_to_install)) install.packages(packages_to_install, repos = "https://cloud.r-project.org/")

lapply(packages, require, character.only = TRUE)

# Set working directory
setwd()

#Read CSV file
ratings <- fread("ratings.csv")
head(ratings)

set.seed(6290653)   #student number

#####-----------------------------------------------------------------------#####

# Filter data for ratings on or after February 18, 2003 (half-star scale)
ratings[, datetime := as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")]
ratings[, date := as.Date(datetime)]
ratings <- ratings[date >= as.Date("2003-02-18")]

# Sample 1% of data randomly 
sample_size <- 0.01
n_rows_to_sample <- floor(nrow(ratings) * sample_size)
sample_indices <- sample(1:nrow(ratings), size = n_rows_to_sample)
ratings_sample <- ratings[sample_indices, ]
head(ratings_sample)


# Print structure, summary of sampled data
str(ratings_sample)
summary(ratings_sample)
head(ratings_sample)

# Function to sample data ensuring each user rates a certain number of movies and each movie is rated at least a certain time
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

# Sample: users rate at least ... movies, movies get rated at least ... times
min_ratings_per_user <- 10
min_users_per_movie <- 10
ratings_sample <- sample_ratings(ratings_sample, min_ratings_per_user, min_users_per_movie)

#Plotting probability distribution of ratings (0.5 - 5 stars)
ggplot(ratings_sample, aes(x = rating)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) +
  labs(title = "Probability Distribution of Ratings", x = "Rating", y = "Probability") +
  theme_minimal()

#Distribution of ratings per user
ratings_sample %>%
  group_by(userId) %>%
  summarise(avg_rating = mean(rating), count = n()) %>%
  ggplot(aes(x = count, y = avg_rating)) +
  geom_point() +
  labs(title = "Average Rating by Number of Ratings per User", x = "Number of Ratings", y = "Average Rating") +
  theme_minimal()

# Rating Frequency
# Calculate the number of ratings each user has given
rating_counts_df <- ratings_sample %>%
  group_by(userId) %>%
  summarise(rating_count = n())

ggplot(rating_counts_df, aes(x = rating_count)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Number of Ratings per User", x = "Number of Ratings", y = "Count of Users") +
  theme_minimal()

# Average Rating over time
ratings_sample %>%
  mutate(six_month_period = floor_date(date, "6 months")) %>%
  group_by(six_month_period) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(x = six_month_period, y = avg_rating)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Rating Over Time (Every 6 Months)", x = "Date", y = "Average Rating") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

####------------------------------------------------------------------------------###
# Create the initial matrix
ratings_matrix <- dcast(ratings_sample, userId ~ movieId, value.var = "rating")
ratings_matrix <- as.matrix(ratings_matrix[,-1])  #remove first column userId
print(dim(ratings_matrix))   # users x  movies
print(paste("Number of users: ", nrow(ratings_matrix)))
print(paste("Number of movies:", ncol(ratings_matrix)))

# Report sparsity
na_count <- sum(is.na(ratings_matrix))
na_percentage <- (na_count / length(ratings_matrix)) * 100
print(paste("NAs: ", na_count, "values"))
print(paste("Percentage of NA values:", na_percentage, "%"))

# Convert ratings_matrix into Incomplete class
i <- row(ratings_matrix)[!is.na(ratings_matrix)]
j <- col(ratings_matrix)[!is.na(ratings_matrix)]
value <- ratings_matrix[!is.na(ratings_matrix)]
incomplete_matrix <- Incomplete(i, j, value)


####------------------------------------------------------------------------------####

# Initialize a test mask
test_mask <- matrix(FALSE, nrow=nrow(ratings_matrix), ncol=ncol(ratings_matrix))

# Identify indices of the observed (non-NA) entries
observed_indices <- which(!is.na(ratings_matrix), arr.ind=TRUE)

# Randomly select 20% of these indices for testing
sample_size <- floor(0.2 * nrow(observed_indices))
selected_indices <- sample(seq_len(nrow(observed_indices)), size = sample_size)
test_mask[observed_indices[selected_indices, "row"], observed_indices[selected_indices, "col"]] <- TRUE

# Create the training matrix
train_matrix <- ratings_matrix
train_matrix[test_mask] <- NA  # Set selected test entries to NA in the training matrix

# Step 5: Create the test matrix
test_matrix <- matrix(NA, nrow=nrow(ratings_matrix), ncol=ncol(ratings_matrix))
test_matrix[test_mask] <- ratings_matrix[test_mask]  # Fill test matrix with observed entries from the original matrix

#Probability distribution in train matrix
train_plotdf <- as.data.frame(train_matrix)
train_long <- pivot_longer(train_plotdf, cols = everything(), names_to = "movieId", values_to = "rating")
train_long <- train_long %>% filter(!is.na(rating))
ggplot(train_long, aes(x = rating)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) +
  labs(title = "Probability Distribution of Train Ratings", x = "Rating", y = "Probability") +
  theme_minimal()

###------------------------------------------------------------------------------###
# Define a function to perform 5-fold cross-validation
cv_softImpute <- function(matrix, lambda_seq, rank_seq, n_folds = 1) {
  # Create indices for the folds
  folds <- createFolds(which(!is.na(matrix)), k = n_folds, list = TRUE, returnTrain = FALSE)
  
  # Initialize variables to store the best parameters
  best_lambda <- NULL
  best_rank <- NULL
  best_error <- Inf
  
  # Perform grid search
  for (lambda in lambda_seq) {
    for (rank in rank_seq) {
      errors <- numeric(n_folds)
      
      for (fold in 1:n_folds) {
        test_indices <- folds[[fold]]
        train_matrix <- matrix
        train_matrix[test_indices] <- NA
        
        # Perform soft impute
        fit <- softImpute(train_matrix, rank.max = rank, lambda = lambda)
        completed_matrix <- complete(matrix, fit)
        
        # Calculate the error on the test set
        errors[fold] <- sqrt(mean((matrix[test_indices] - completed_matrix[test_indices])^2, na.rm = TRUE))
      }
      
      # Calculate the mean error for this combination of lambda and rank
      mean_error <- mean(errors)
      
      # Update the best parameters if the current mean error is lower
      if (mean_error < best_error) {
        best_lambda <- lambda
        best_rank <- rank
        best_error <- mean_error
      }
    }
  }
  
  return(list(best_lambda = best_lambda, best_rank = best_rank, best_error = best_error))
}

# Define the grid of parameters to search
lambda_seq <- seq(0.1, 5, by = 0.5)
rank_seq <- seq(50, 200, by = 25)

# Run the cross-validation to find the optimal parameters
result <- cv_softImpute(train_matrix, lambda_seq, rank_seq, n_folds = 1)

# Output the result
print(result)
