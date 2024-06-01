# Load necessary libraries
packages <- c("data.table",
              "reshape2", 
              "softImpute",
              "dplyr",
              "Matrix",
              "ggplot2",
              "lubridate",
              "caret")

packages_to_install <- packages[!packages %in% installed.packages()[,"Package"]]

if(length(packages_to_install)) install.packages(packages_to_install, repos = "https://cloud.r-project.org/")

lapply(packages, require, character.only = TRUE)

# Set working directory
setwd()

#Read CSV file
ratings <- fread("ratings.csv")

set.seed(6290653)   #student number

#####-----------------------------------------------------------------------#####
# Data Sampling & Scaling

# Filter data for ratings on or after February 18, 2003 (half-star scale)
ratings[, datetime := as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")]
ratings[, date := as.Date(datetime)]
ratings <- ratings[date >= as.Date("2003-02-18")]

# Sample ...% of data randomly 
sample_size <- 0.03
n_rows_to_sample <- floor(nrow(ratings) * sample_size)
sample_indices <- sample(1:nrow(ratings), size = n_rows_to_sample)
filtered_ratings<- ratings[sample_indices, ]
head(filtered_ratings)

# Function to sample data ensuring each user rates a certain number of movies and each movie is rated at least a certain time
sample_ratings <- function(ratings, min_ratings_per_user, min_users_per_movie) {
  repeat {
    # Filter movies that are rated by at least min_users_per_movie users
    movies_sample <- ratings %>%
      group_by(movieId) %>%
      summarise(n = n(), .groups = 'drop') %>%
      filter(n >= min_users_per_movie) %>%
      pull(movieId)
    
    # Filter users who have rated at least min_ratings_per_user movies
    users_sample <- ratings %>%
      filter(movieId %in% movies_sample) %>%
      group_by(userId) %>%
      summarise(n = n(), .groups = 'drop') %>%
      filter(n >= min_ratings_per_user) %>%
      pull(userId)
    
    # Filter the ratings based on the sampled movies and users
    new_ratings_sample <- ratings %>%
      filter(userId %in% users_sample, movieId %in% movies_sample)
    
    # Check if the new sample is the same as the previous sample
    if (nrow(new_ratings_sample) == nrow(ratings)) {
      break
    } else {
      ratings <- new_ratings_sample
    }
  }
  
  return(ratings)
}

# Sample: users rate at least ... movies, movies get rated at least ... times
min_ratings_per_user <- 20
min_users_per_movie <- 15
ratings_sample <- sample_ratings(filtered_ratings, min_ratings_per_user, min_users_per_movie)

# Create ratings matrix
ratings_matrix <- dcast(ratings_sample, userId ~ movieId, value.var = "rating")
ratings_matrix <- as.matrix(ratings_matrix[,-1])  #remove first column userId
print(paste("Number of users: ", nrow(ratings_matrix)))
print(paste("Number of movies:", ncol(ratings_matrix)))

# Function to report sparsity of a matrix
check_sparsity <- function(matrix){
  na_count <- sum(is.na(matrix))
  na_percentage <- (na_count / length(matrix)) * 100
  print(paste("NAs: ", na_count, "values"))
  print(paste("Percentage of NA values:", na_percentage, "%"))
}

# Function to check min number of ratings (columns and rows) of rating matrix
check_min_ratings <- function(matrix) {
  ratings_per_user <- rowSums(!is.na(matrix))
  ratings_per_movie <- colSums(!is.na(matrix))
  min_ratings_per_user <- min(ratings_per_user)
  min_ratings_per_movie <- min(ratings_per_movie)
  cat("Minimum number of ratings per user:", min_ratings_per_user, "\n")
  cat("Minimum number of ratings per movie:", min_ratings_per_movie, "\n")
}

# Report sparsity of ratings matrix
check_sparsity(ratings_matrix)

# Check min number of ratings for generated ratings matrix 
# (should >= set values)
check_min_ratings(ratings_matrix)

###-----------------------------------------------------------------------------###
# softImpute-specific transformation

# Function to transform ratings matrix into Incomplete class for computation efficiency
convert_Incomplete <- function(matrix){
  i <- row(matrix)[!is.na(matrix)]
  j <- col(matrix)[!is.na(matrix)]
  if (length(i) == 0 || length(j) == 0) {
    stop("The matrix dimensions are invalid.")
  }
  value <- matrix[!is.na(matrix)]
  incomplete_matrix <- Incomplete(i, j, value)
  return(incomplete_matrix)
}

#So far not used

ratings_incomplete <- convert_Incomplete(ratings_matrix)

# Scaling data 
ratings_scaled <- biScale(ratings_incomplete, trace = TRUE)  # Final transformed matrix

###-----------------------------------------------------------------------------###
# Initial plots and graphs for ratings sample/matrix

# Plot probability distribution of ratings (0.5 - 5 stars)
ggplot(ratings_sample, aes(x = rating)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) +
  labs(title = "Probability Distribution of Ratings", x = "Rating", y = "Probability") +
  theme_minimal()

# Plot distribution of ratings per user
ratings_sample %>%
  group_by(userId) %>%
  summarise(avg_rating = mean(rating), count = n()) %>%
  ggplot(aes(x = count, y = avg_rating)) +
  geom_point() +
  labs(title = "Average Rating by Number of Ratings per User", x = "Number of Ratings", y = "Average Rating") +
  theme_minimal()

# Plot rating frequency
rating_counts_df <- ratings_sample %>%
  group_by(userId) %>%
  summarise(rating_count = n())

ggplot(rating_counts_df, aes(x = rating_count)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Number of Ratings per User", x = "Number of Ratings", y = "Count of Users") +
  theme_minimal()

# Plot average rating over time (every 6 months)
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

####------------------------------------------------------------------------------####
# Generate train, test set to evaluate softImpute

# Step 1: Initialize a test mask
test_mask <- matrix(FALSE, nrow=nrow(ratings_matrix), ncol=ncol(ratings_matrix))

# Step 2: Identify indices of the observed (non-NA) entries
observed_indices <- which(!is.na(as.matrix(ratings_matrix)), arr.ind=TRUE)

# Step 3: Randomly select 10% of these indices for testing
sample_size <- floor(0.001 * nrow(observed_indices))
selected_indices <- observed_indices[sample(seq_len(nrow(observed_indices)), size = sample_size), ]

# Update test_mask in a vectorized manner
test_mask[selected_indices] <- TRUE

# Step 4: Create the training matrix
train_matrix <- as.matrix(ratings_matrix)
train_matrix[test_mask] <- NA  # Set selected test entries to NA in the training matrix

# Step 5: Create the test matrix
test_matrix <- matrix(NA, nrow=nrow(ratings_matrix), ncol=ncol(ratings_matrix))
test_matrix[test_mask] <- as.matrix(ratings_matrix)[test_mask] 

# Check sparsity of train matrix
check_sparsity(train_matrix)

# Check min number of ratings in train matrix
check_min_ratings(train_matrix)

###-----------------------------------------------------------------------------###
# Errors functions 

# Define RMSE calculation functions
calculate_rmse <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Define MAE calculation functions
calculate_mae <- function(true_ratings, predicted_ratings) {
  mean(abs(true_ratings - predicted_ratings))
}

# NMAE????

###-----------------------------------------------------------------------------###
#K-fold cross validation on train matrix for tuning parameters
cv_softImpute <- function(matrix, lambda_seq, rank_seq, n_folds) {
  # Create indices for the folds
  observed_indices <- which(!is.na(matrix))
  folds <- createFolds(observed_indices, k = n_folds, list = TRUE, returnTrain = FALSE)
  
  # Initialize variables to store the best parameters
  best_lambda_rmse <- NULL
  best_rank_rmse <- NULL
  best_rmse <- Inf
  
  best_lambda_mae <- NULL
  best_rank_mae <- NULL
  best_mae <- Inf
  
  # Initialize data frames to store results
  rmse_results <- data.frame(lambda = numeric(), rank = numeric(), rmse = numeric())
  mae_results <- data.frame(lambda = numeric(), rank = numeric(), mae = numeric())
  
  total_iterations <- length(lambda_seq) * length(rank_seq) * n_folds
  iteration <- 0
  start_time <- Sys.time()
  
  # Perform grid search
  for (lambda in lambda_seq) {
    for (rank in rank_seq) {
      rmse_errors <- numeric(n_folds)
      mae_errors <- numeric(n_folds)
      
      for (fold in 1:n_folds) {
        test_indices <- observed_indices[folds[[fold]]]
        train_set <- matrix
        train_set[test_indices] <- NA
        
        # Perform soft impute
        train_set <- convert_Incomplete(train_set)
        train_set <- biScale(train_set)
        fit <- softImpute(train_set,lambda = lambda, rank.max=rank, type = "als")
        completed_matrix <- softImpute::complete(train_set, fit)

        # Collect true and predicted ratings to calculate RMSE, MAE 
        true_ratings <- matrix[test_indices]
        predicted_ratings <- completed_matrix[test_indices]
      
        
        # Calculate RMSE and MAE
        rmse_errors[fold] <- calculate_rmse(true_ratings, predicted_ratings)
        mae_errors[fold] <- calculate_mae(true_ratings, predicted_ratings)
        elapsed_time <- Sys.time() - start_time
        remaining_time <- (total_iterations - iteration) * (elapsed_time / iteration)
        
        cat(sprintf("Iteration %d of %d completed. Estimated remaining time: %s\n", iteration, total_iterations, remaining_time))
        iteration <- iteration + 1
      }
      
      # Calculate the mean error for this combination of lambda and rank
      mean_rmse <- mean(rmse_errors, na.rm = TRUE)
      mean_mae <- mean(mae_errors, na.rm = TRUE)
      
      # Update the best parameters if the current mean RMSE or MAE is lower
      if (mean_rmse < best_rmse) {
        best_lambda_rmse <- lambda
        best_rank_rmse <- rank
        best_rmse <- mean_rmse
      }
      
      if (mean_mae < best_mae) {
        best_lambda_mae <- lambda
        best_rank_mae <- rank
        best_mae <- mean_mae
      }
      
      # Store the results
      rmse_results <- rbind(rmse_results, data.frame(lambda = lambda, rank = rank, rmse = mean_rmse))
      mae_results <- rbind(mae_results, data.frame(lambda = lambda, rank = rank, mae = mean_mae))
      
    }
  }
  
  # Print the best parameter combinations
  cat("Best parameters for RMSE:\n")
  cat("Lambda:", best_lambda_rmse, "Rank:", best_rank_rmse, "RMSE:", best_rmse, "\n\n")
  
  cat("Best parameters for MAE:\n")
  cat("Lambda:", best_lambda_mae, "Rank:", best_rank_mae, "MAE:", best_mae, "\n\n")
  
  return(list(best_lambda_rmse = best_lambda_rmse, best_rank_rmse = best_rank_rmse, best_rmse = best_rmse, 
              best_lambda_mae = best_lambda_mae, best_rank_mae = best_rank_mae, best_mae = best_mae, 
              rmse_results = rmse_results, mae_results = mae_results))
}

# Define the grid of parameters to search
lambda_seq <- seq(0.1, 2, by = 0.45)
rank_seq <- seq(50, 100, by = 20)


# Run the cross-validation to find the optimal parameters
result <- cv_softImpute(train_matrix, lambda_seq, rank_seq, n_folds = 3)

# Print the best parameters
cat("Best parameters for RMSE:\n")
cat("Lambda:", result$best_lambda_rmse, "Rank:", result$best_rank_rmse, "RMSE:", result$best_rmse, "\n\n")

cat("Best parameters for MAE:\n")
cat("Lambda:", result$best_lambda_mae, "Rank:", result$best_rank_mae, "MAE:", result$best_mae, "\n\n")

# Plot the RMSE results
ggplot(result$rmse_results, aes(x = lambda, y = rank, fill = rmse)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "RMSE for different Lambda and Rank combinations",
       x = "Lambda", y = "Rank", fill = "RMSE") +
  theme_minimal()

# Plot the MAE results
ggplot(result$mae_results, aes(x = lambda, y = rank, fill = mae)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "MAE for different Lambda and Rank combinations",
       x = "Lambda", y = "Rank", fill = "MAE") +
  theme_minimal()

###---------------------------------------------------------------------------###
# Fit to predict test
train_matrix <- convert_Incomplete(train_matrix)
train_matrix <- biScale(train_matrix, trace = TRUE)
fit <- softImpute(train_matrix, lambda = 1.9, rank.max = 90, trace.it = TRUE)
complete_train <- complete(train_matrix, fit)


# Collect true and predicted ratings
true_ratings <- numeric()
predicted_ratings <- numeric()
individual_rmse <- numeric()
individual_mae <- numeric()

observed_indices <- which(!is.na(test_matrix), arr.ind = TRUE)

for (idx in seq_len(nrow(observed_indices))) {
  i <- observed_indices[idx, 1]
  j <- observed_indices[idx, 2]
  
  true_ratings <- c(true_ratings, ratings_matrix[i, j])
  predicted_ratings <- c(predicted_ratings, complete_train[i, j])
  individual_rmse <- c(individual_rmse, calculate_rmse(ratings_matrix[i,j], complete_train[i, j]))
  individual_mae <- c(individual_mae, calculate_mae(ratings_matrix[i, j], complete_train[i, j]))
  }


# Calculate RMSE and MAE
rmse <- calculate_rmse(true_ratings, predicted_ratings)
mae <- calculate_mae(true_ratings, predicted_ratings)

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")

# Flatten the matrix to a vector for easier computation
ratings_vector <- as.vector(complete_train)

# Remove NA values if there are any
ratings_vector <- ratings_vector[!is.na(ratings_vector)]

# Calculate descriptive statistics
mean_rating <- mean(predicted_ratings)
median_rating <- median(predicted_ratings)
sd_rating <- sd(predicted_ratings)
min_rating <- min(predicted_ratings)
max_rating <- max(predicted_ratings)

# Print descriptive statistics
cat("Descriptive Statistics of Ratings:\n")
cat("Mean:", mean_rating, "\n")
cat("Median:", median_rating, "\n")
cat("Standard Deviation:", sd_rating, "\n")
cat("Minimum:", min_rating, "\n")
cat("Maximum:", max_rating, "\n")

ggplot(data.frame(rating = ratings_vector), aes(x = rating)) +
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Ratings",
       x = "Rating",
       y = "Frequency")

# Plot the distribution of predicted ratings for the observed indices
ggplot(data.frame(rating = predicted_ratings), aes(x = rating)) +
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Predicted Ratings at Test Indices",
       x = "Predicted Rating",
       y = "Frequency")

# Define functions to calculate individual RMSE and MAE
calculate_individual_rmse <- function(actual, predicted) {
  sqrt((actual - predicted)^2)
}

calculate_individual_mae <- function(actual, predicted) {
  abs(actual - predicted)
}

# Function to compute average RMSE and MAE for each true rating
compute_average_errors <- function(true_ratings, predicted_ratings) {
  results_df <- data.frame(
    true_rating = true_ratings,
    predicted_rating = predicted_ratings,
    rmse = mapply(calculate_individual_rmse, true_ratings, predicted_ratings),
    mae = mapply(calculate_individual_mae, true_ratings, predicted_ratings)
  )
  
  average_results_df <- results_df %>%
    group_by(true_rating) %>%
    summarise(
      avg_rmse = mean(rmse),
      avg_mae = mean(mae)
    )
  
  return(average_results_df)
}


# Compute average RMSE and MAE for each true rating
average_results_df <- compute_average_errors(true_ratings, predicted_ratings)

# Plot the average RMSE for each true rating
ggplot(average_results_df, aes(x = factor(true_rating), y = avg_rmse)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Average RMSE for Each True Rating",
       x = "True Rating",
       y = "Average RMSE")

# Plot the average MAE for each true rating
ggplot(average_results_df, aes(x = factor(true_rating), y = avg_mae)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Average MAE for Each True Rating",
       x = "True Rating",
       y = "Average MAE")

