# Load necessary libraries
packages <- c("data.table",
              "reshape2", 
              "softImpute",
              "dplyr",
              "Matrix",
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

set.seed(6290653)   #student number

#####-----------------------------------------------------------------------#####

# Filter data for ratings on or after February 18, 2003 (half-star scale)
ratings[, datetime := as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")]
ratings[, date := as.Date(datetime)]
ratings <- ratings[date >= as.Date("2003-02-18")]

# Sample 1% of data randomly 
sample_size <- 0.02
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
min_ratings_per_user <- 15
min_users_per_movie <- 10
ratings_sample <- sample_ratings(filtered_ratings, min_ratings_per_user, min_users_per_movie)

# Create matrix
ratings_matrix <- dcast(ratings_sample, userId ~ movieId, value.var = "rating")
ratings_matrix <- as.matrix(ratings_matrix[,-1])  #remove first column userId
print(paste("Number of users: ", nrow(ratings_matrix)))
print(paste("Number of movies:", ncol(ratings_matrix)))

# Report sparsity
na_count <- sum(is.na(ratings_matrix))
na_percentage <- (na_count / length(ratings_matrix)) * 100
print(paste("NAs: ", na_count, "values"))
print(paste("Percentage of NA values:", na_percentage, "%"))

test <- ratings_matrix
test2 <- biScale(test, trace = TRUE)
test2[13,11] <- NA
test1[2,7] <- NA
test1[19,8] <- NA
fit <- softImpute(test2, rank.max = 100, lambda = 2.5, type = "als", trace.it = TRUE)
completed_matrix <- complete(test2, fit)

print(completed_matrix[13,11])
print(completed_matrix[2,7])
print(completed_matrix[19,8])

# Convert the matrix to a vector
ratings_vector <- as.vector(completed_matrix)

# Create a data frame for plotting
ratings_data <- data.frame(Rating = ratings_vector)
# Plot the distribution
ggplot(ratings_data, aes(x = Rating)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Movie Ratings", x = "Ratings", y = "Frequency") +
  theme_minimal()

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

# Create the test matrix
test_matrix <- matrix(NA, nrow=nrow(ratings_matrix), ncol=ncol(ratings_matrix))
test_matrix[test_mask] <- ratings_matrix[test_mask]  # Fill test matrix with observed entries from the original matrix


# Report sparsity
na_count <- sum(is.na(train_matrix))
na_percentage <- (na_count / length(train_matrix)) * 100
print(paste("NAs: ", na_count, "values"))
print(paste("Percentage of NA values:", na_percentage, "%"))

# Calculate the number of ratings per user
ratings_per_user <- rowSums(!is.na(ratings_matrix))

# Calculate the number of ratings per movie
ratings_per_movie <- colSums(!is.na(ratings_matrix))

# Find the minimum number of ratings per user
min_ratings_per_user <- min(ratings_per_user)

# Find the minimum number of ratings per movie
min_ratings_per_movie <- min(ratings_per_movie)

# Print the results
cat("Minimum number of ratings per user:", min_ratings_per_user, "\n")
cat("Minimum number of ratings per movie:", min_ratings_per_movie, "\n")
###-----------------------------------------------------------------------------###

#New sampling
# Function to check if the conditions are met
check_conditions <- function(ratings, min_ratings_per_user, min_users_per_movie) {
  user_check <- ratings %>%
    group_by(userId) %>%
    summarise(n = n(), .groups = 'drop') %>%
    pull(n) >= min_ratings_per_user
  
  movie_check <- ratings %>%
    group_by(movieId) %>%
    summarise(n = n(), .groups = 'drop') %>%
    pull(n) >= min_users_per_movie
  
  return(all(user_check) && all(movie_check))
}

# Function to ensure the conditions are met
ensure_conditions <- function(ratings, min_ratings_per_user, min_users_per_movie) {
  repeat {
    movies_sample <- ratings %>%
      group_by(movieId) %>%
      summarise(n = n(), .groups = 'drop') %>%
      filter(n >= min_users_per_movie) %>%
      pull(movieId)
    
    users_sample <- ratings %>%
      filter(movieId %in% movies_sample) %>%
      group_by(userId) %>%
      summarise(n = n(), .groups = 'drop') %>%
      filter(n >= min_ratings_per_user) %>%
      pull(userId)
    
    new_ratings <- ratings %>%
      filter(userId %in% users_sample, movieId %in% movies_sample)
    
    if (check_conditions(new_ratings, min_ratings_per_user, min_users_per_movie)) {
      break
    }
  }
  
  return(new_ratings)
}

# Function to create a valid train matrix
create_train_matrix <- function(ratings_matrix, min_ratings_per_user, min_users_per_movie, initial_sample_fraction = 0.9, final_sample_fraction = 0.9) {
  repeat {
    observed_indices <- which(!is.na(ratings_matrix), arr.ind = TRUE)
    
    initial_sample_size <- floor(initial_sample_fraction * nrow(observed_indices))
    initial_selected_indices <- sample(seq_len(nrow(observed_indices)), size = initial_sample_size)
    initial_train_indices <- observed_indices[initial_selected_indices, ]
    
    train_matrix <- matrix(NA, nrow = nrow(ratings_matrix), ncol = ncol(ratings_matrix))
    train_matrix[initial_train_indices] <- ratings_matrix[initial_train_indices]
    
    train_ratings <- as.data.frame(as.table(train_matrix))
    colnames(train_ratings) <- c("userId", "movieId", "rating")
    train_ratings <- train_ratings %>% filter(!is.na(rating))
    
    final_train_ratings <- ensure_conditions(train_ratings, min_ratings_per_user, min_users_per_movie)
    
    if (check_conditions(final_train_ratings, min_ratings_per_user, min_users_per_movie)) {
      final_sample_size <- floor(final_sample_fraction * nrow(final_train_ratings))
      final_selected_indices <- sample(seq_len(nrow(final_train_ratings)), size = final_sample_size)
      final_train_ratings <- final_train_ratings[final_selected_indices, ]
      
      if (check_conditions(final_train_ratings, min_ratings_per_user, min_users_per_movie)) {
        break
      }
    }
  }
  
  final_train_matrix <- matrix(NA, nrow = nrow(ratings_matrix), ncol = ncol(ratings_matrix))
  final_train_matrix[cbind(final_train_ratings$userId, final_train_ratings$movieId)] <- final_train_ratings$rating
  
  return(final_train_matrix)
}

# Set the minimum ratings criteria
min_ratings_per_user <- 10
min_users_per_movie <- 5

# Time the creation of the train matrix
start_time <- Sys.time()
train_matrix <- create_train_matrix(ratings_matrix, min_ratings_per_user, min_users_per_movie, initial_sample_fraction = 0.9, final_sample_fraction = 0.8)
end_time <- Sys.time()

# Print the time taken
time_taken <- end_time - start_time
print(paste("Time taken for creating train matrix:", time_taken))

# Create the test matrix
test_matrix <- ratings_matrix
test_matrix[!is.na(train_matrix)] <- NA

# Check the final train matrix
train_ratings <- as.data.frame(as.table(train_matrix))
colnames(train_ratings) <- c("userId", "movieId", "rating")
train_ratings <- train_ratings %>% filter(!is.na(rating))

# Check the minimum number of ratings per movie in the train matrix
min_ratings_per_movie_in_train <- train_ratings %>%
  group_by(movieId) %>%
  summarise(n = n(), .groups = 'drop') %>%
  summarise(min_n = min(n))

print(min_ratings_per_movie_in_train)

# Check the minimum number of ratings per user in the train matrix
min_ratings_per_user_in_train <- train_ratings %>%
  group_by(userId) %>%
  summarise(n = n(), .groups = 'drop') %>%
  summarise(min_n = min(n))

print(min_ratings_per_user_in_train)

# Check the proportion of data in the train matrix
observed_entries <- sum(!is.na(ratings_matrix))
train_entries <- sum(!is.na(train_matrix))
print(paste("Proportion of data in train matrix:", train_entries / observed_entries))

###-----------------------------------------------------------------------------###

# Function to convert matrix to incomplete class 
convert_to_incomplete <- function(matrix) {
  i <- row(matrix)[!is.na(matrix)]
  j <- col(matrix)[!is.na(matrix)]
  value <- matrix[!is.na(matrix)]
  if (length(i) == 0 || length(j) == 0) {
    stop("The matrix dimensions are invalid.")
  }
  incomplete_matrix <- softImpute::Incomplete(i, j, value)
  
  return(incomplete_matrix)
}

# Define custom RMSE and MAE calculation functions
calculate_rmse <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

calculate_mae <- function(true_ratings, predicted_ratings) {
  mean(abs(true_ratings - predicted_ratings))
}

cv_softImpute <- function(matrix, lambda_seq, rank_seq, n_folds) {
  # Create indices for the folds
  observed_indices <- which(!is.na(matrix))
  folds <- createFolds(observed_indices, k = n_folds, list = TRUE, returnTrain = FALSE)
  
  # Ensure that each fold has at least one observed entry
  if (length(observed_indices) < n_folds) {
    stop("The number of observed entries is less than the number of folds.")
  }
  
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
        train_set <-  convert_to_incomplete(train_set)
        
        rank_max <- min(dim(train_set))
        rank <- min (rank, rank_max)
        if (rank_max < 1) {
          stop("rank.max must be at least 1.")
        }
        
        # Perform soft impute
        fit <- softImpute(train_set,lambda = lambda, rank.max=rank, type = "als")
        completed_matrix <- softImpute::complete(train_set, fit)
        if(lambda == 0.1 & rank ==50){
          View(completed_matrix)
        }
        # Collect true and predicted ratings
        true_ratings <- numeric()
        predicted_ratings <- numeric()
        for (i in seq_len(nrow(matrix))) {
          for (j in seq_len(ncol(matrix))) {
            index <- (i - 1) * ncol(matrix) + j
            if (index %in% test_indices) {
              if (!is.na(matrix[i, j])) {
                true_ratings <- c(true_ratings, matrix[i, j])
                predicted_ratings <- c(predicted_ratings, completed_matrix[i, j])
              }
            }
          }
        }
        
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
lambda_seq <- seq(0.01, 2, by = 1)
rank_seq <- seq(50, 100, by = 50)


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

sparse_train <- train_matrix
biscaled_train <- biScale(train_matrix, thresh = 1e-4, trace = TRUE)
train_fit <- softImpute(biscaled_train, lambda = 0.8, rank.max = 100, trace.it = TRUE, type = "als")
debiased_train_fit <- deBias(sparse_train, train_fit)
complete_train <- softImpute::complete(biscaled_train, train_fit)


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

