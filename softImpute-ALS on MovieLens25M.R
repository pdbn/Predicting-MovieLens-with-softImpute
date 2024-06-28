# Load necessary libraries
packages <- c("data.table",
              "reshape2", 
              "softImpute",
              "dplyr",
              "tidyr",
              "Matrix",
              "ggplot2",
              "lubridate",
              "caret",
              "gridExtra",
              "moments",
              "scales",
              "xtable")

packages_to_install <- packages[!packages %in% installed.packages()[,"Package"]]

if(length(packages_to_install)) install.packages(packages_to_install, repos = "https://cloud.r-project.org/")

lapply(packages, require, character.only = TRUE)

# Set working directory
setwd()

#Read CSV file
ratings <- fread("ratings.csv")
movies <- fread("movies.csv")

set.seed(6290653)   #student number

#####-----------------------------------------------------------------------#####
# Data Sampling & Scaling

# Filter data for ratings on or after February 18, 2003 (half-star scale)
ratings[, datetime := as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")]
ratings[, date := as.Date(datetime)]
ratings <- ratings[date >= as.Date("2003-02-18")]

# Sample 3% of data randomly 
sample_size <- 0.03
n_rows_to_sample <- floor(nrow(ratings) * sample_size)
sample_indices <- sample(1:nrow(ratings), size = n_rows_to_sample)
filtered_ratings<- ratings[sample_indices, ]

# Merge filtered_ratings with movies to get movie names and genres
filtered_ratings <- merge(filtered_ratings, movies, by = "movieId", all.x = TRUE)
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

# Sample: users rate at least 20 movies, movies get rated at least 15 times
min_ratings_per_user <- 20
min_users_per_movie <- 15
ratings_sample <- sample_ratings(filtered_ratings, min_ratings_per_user, min_users_per_movie)

# Create ratings matrix
ratings_matrix <- dcast(ratings_sample, userId ~ movieId, value.var = "rating")
movie_ids <- colnames(ratings_matrix)[-1]
ratings_matrix <- as.matrix(ratings_matrix[,-1])  #remove first column userId
print(paste("Number of users: ", nrow(ratings_matrix)))
print(paste("Number of movies:", ncol(ratings_matrix)))

# Get genres corresponding to movieIds
movie_genres <- sapply(movie_ids, function(id) {
  genres <- movies[movieId == as.integer(id), genres]
  if (length(genres) == 0) {
    return(NA)
  } else {
    return(genres)
  }
})

# Combine movieIds and genres into a single data frame
movie_info <- data.frame(movieId = movie_ids, genres = movie_genres, stringsAsFactors = FALSE)


# Display movie_info
print(movie_info)

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

###-----------------------------------------------------------------------------###
# Singular values Graph to verify low rank assumption
ratings_matrix1 <- ratings_matrix
ratings_matrix1[is.na(ratings_matrix1)] <- 0
svd_sample <- svd(ratings_matrix1)
singular_values <- svd_sample$d
num_top_singular_values <- ceiling(0.3 * length(singular_values))
top_singular_values_sum <- sum(singular_values[1:num_top_singular_values])
total_singular_values_sum <- sum(singular_values)
percentage_top_30 <- (top_singular_values_sum / total_singular_values_sum) * 100
cat("Top 30% singular values account for", round(percentage_top_30, 2), "% of the sum of all singular values.\n")

df_sample <- data.frame(
  singular_value_index = 1:length(singular_values),
  magnitude = singular_values
)
threshold_sample <- num_top_singular_values
ggplot(df_sample, aes(x = singular_value_index, y = magnitude)) +
  geom_line(color = "blue", linewidth = 1.15) +  # Make the blue line thicker
  geom_hline(yintercept = singular_values[threshold_sample], 
             linetype = "dashed", color = "red") +
  geom_vline(xintercept = threshold_sample, 
             linetype = "dashed", color = "red") +
  labs(title = "Singular Values of the Rating Matrix",
       x = "The i-th singular value",
       y = "Magnitude") +
  annotate("text", x = threshold_sample, y = max(singular_values), 
           label = "Top 30% singular values", color = "red", vjust = -0.5) +
  theme_bw()



# Plot probability distribution of ratings (0.5 - 5 stars)
ggplot(ratings_sample, aes(x = rating)) +
  geom_histogram(aes(y = after_stat(count) / sum(after_stat(count))), binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Proportion Distribution of Ratings", x = "Rating", y = "Proportion") +
  theme_bw()

# Calculate descriptive statistics
stats <- ratings_sample %>%
  summarize(
    mean = mean(rating),
    median = median(rating),
    sd = sd(rating),
    min = min(rating),
    max = max(rating),
    q1 = quantile(rating, 0.25),
    q3 = quantile(rating, 0.75),
    skewness = skewness(rating),
    kurtosis = kurtosis(rating)
  )

print(stats)

# Split genres into separate rows
genres_split <- filtered_ratings %>%
  separate_rows(genres, sep = "\\|")

# Print number of genres
unique_genres <- unique(genres_split$genres)
print(length(unique_genres))

# Calculate the total number of ratings for each genre
genre_popularity <- genres_split %>%
  group_by(genres) %>%
  summarise(total_ratings = n(), .groups = 'drop') %>%
  arrange(desc(total_ratings))

# Calculate the total number of movies for each genre
genre_movies <- genres_split %>%
  distinct(movieId, genres) %>%
  group_by(genres) %>%
  summarise(total_movies = n(), .groups = 'drop') %>%
  arrange(desc(total_movies))

# Convert to LaTeX table
latex_table <- xtable(genre_movies, caption = "Number of Movies in Each Genre")

# Print LaTeX table
print(latex_table, type = "latex", include.rownames = FALSE)

# Calculate the average rating for each genre
genre_avg_rating <- genres_split %>%
  group_by(genres) %>%
  summarise(average_rating = mean(rating), .groups = 'drop') %>%
  arrange(desc(average_rating))

# Display the plots
grid.arrange(
  ggplot(genre_popularity, aes(x = reorder(genres, -total_ratings), y = total_ratings)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    theme_bw() +
    labs(title = "Popularity of Movie Genres", x = "Genre", y = "Total Ratings") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  
  ggplot(genre_avg_rating, aes(x = reorder(genres, -average_rating), y = average_rating)) +
    geom_bar(stat = "identity", fill = "orange") +
    theme_bw() +
    labs(title = "Average Rating of Movie Genres", x = "Genre", y = "Average Rating") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(3, 4)),
  
  ncol = 1
)

# Extract the release year from the movie titles
movies$year <- as.numeric(sub(".*\\((\\d{4})\\).*", "\\1", movies$title))

# Merge the filtered_ratings with movies to include the release year
filtered_ratings <- merge(filtered_ratings, movies, by = "movieId", all.x = TRUE)

year_popularity <- filtered_ratings %>%
  group_by(year) %>%
  summarise(total_ratings = n(), .groups = 'drop') %>%
  arrange(year)

year_avg_rating <- filtered_ratings %>%
  group_by(year) %>%
  summarise(average_rating = mean(rating), .groups = 'drop') %>%
  arrange(year)

p1 <- ggplot(year_popularity, aes(x = year, y = total_ratings)) +
  geom_line(color = "steelblue") +
  theme_minimal() +
  labs(title = "Total Number of Ratings by Release Date", x = "Year", y = "Total Ratings")

p2 <- ggplot(year_avg_rating, aes(x = year, y = average_rating)) +
  geom_line(color = "darkorange") +
  theme_minimal() +
  labs(title = "Average Rating by Release Date", x = "Year", y = "Average Rating")

# Display the plots together
grid.arrange(p1, p2, ncol = 1)

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

ratings_sample %>%
  mutate(six_month_period = floor_date(date, "6 months")) %>%
  group_by(six_month_period) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(x = six_month_period, y = avg_rating)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Rating Over Time (Every 6 Months)", x = "Date", y = "Average Rating") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(limits = c(0.5, 5))

###-----------------------------------------------------------------------------###
# Generate train, test set to evaluate softImpute

# Step 1: Initialize a test mask
test_mask <- matrix(FALSE, nrow=nrow(ratings_matrix), ncol=ncol(ratings_matrix))

# Step 2: Identify indices of the observed (non-NA) entries
observed_indices <- which(!is.na(as.matrix(ratings_matrix)), arr.ind=TRUE)

# Step 3: Randomly select 20% of these indices for testing
sample_size <- floor(0.2 * nrow(observed_indices))
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

###-----------------------------------------------------------------------------###
#Parameters tuning 

# K-fold cross validation on train matrix for tuning parameters
cv_softImpute <- function(matrix, lambda_seq, rank_seq, n_folds) {
  # Create indices for the folds
  observed_indices <- which(!is.na(matrix))
  folds <- createFolds(observed_indices, k = n_folds, list = TRUE, returnTrain = FALSE)
  
  # Initialize variables to store the best parameters
  best_lambda_rmse <- NULL
  best_rank_rmse <- NULL
  best_rmse <- Inf
  
  # Initialize data frames to store results
  rmse_results <- data.frame(lambda = numeric(), rank = numeric(), rmse = numeric())
  
  # Perform grid search
  for (lambda in lambda_seq) {
    for (rank in rank_seq) {
      rmse_errors <- numeric(n_folds)
      
      for (fold in 1:n_folds) {
        test_indices <- observed_indices[folds[[fold]]]
        train_set <- matrix
        train_set[test_indices] <- NA
        
        # Perform soft impute
        train_set <- convert_Incomplete(train_set)
        train_set <- biScale(train_set, maxit = 30)   #scale data (necessary)
        fit <- softImpute(train_set,lambda = lambda, rank.max=rank, maxit = 150, trace.it = TRUE, type = "als")
        completed_matrix <- softImpute::complete(train_set, fit)
        
        # Collect true and predicted ratings to calculate RMSE, MAE 
        true_ratings <- matrix[test_indices]
        predicted_ratings <- completed_matrix[test_indices]
        
        # Calculate RMSE and MAE
        rmse_errors[fold] <- calculate_rmse(true_ratings, predicted_ratings)
      }
      
      # Calculate the mean error for this combination of lambda and rank
      mean_rmse <- mean(rmse_errors, na.rm = TRUE)
      
      # Update the best parameters if the current mean RMSE or MAE is lower
      if (mean_rmse < best_rmse) {
        best_lambda_rmse <- lambda
        best_rank_rmse <- rank
        best_rmse <- mean_rmse
      }
      
      # Store the results
      rmse_results <- rbind(rmse_results, data.frame(lambda = lambda, rank = rank, rmse = mean_rmse))
      
    }
  }
  
  # Print the best parameter combinations
  cat("Best parameters for RMSE:\n")
  cat("Lambda:", best_lambda_rmse, "Rank:", best_rank_rmse, "RMSE:", best_rmse, "\n\n")
  
  
  return(list(best_lambda_rmse = best_lambda_rmse, best_rank_rmse = best_rank_rmse, best_rmse = best_rmse, 
              rmse_results = rmse_results))
}

#lambda0
incomplete_train <- convert_Incomplete(train_matrix)
scaled_train <- biScale(incomplete_train)
lam0 <- lambda0(scaled_train, maxit = 150, trace.it = TRUE)
lam0

# Define the grid of parameters to search
lambda_seq <- seq(0.5, lam0, length.out = 5)
rank_seq <- seq(20, 100, by = 20)


# Run the cross-validation to find the optimal parameters
result <- cv_softImpute(train_matrix, lambda_seq, rank_seq, n_folds = 5)

# Print the best parameters
cat("Best parameters for RMSE:\n")
cat("Lambda:", result$best_lambda_rmse, "Rank:", result$best_rank_rmse, "RMSE:", result$best_rmse, "\n\n")

# Plot the RMSE results
ggplot(result$rmse_results, aes(x = lambda, y = rank, fill = rmse)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c( "lightblue", "skyblue", "deepskyblue", "dodgerblue", "blue", "darkblue"),
                       values = scales::rescale(c(min(result$rmse_results$rmse), 
                                                  quantile(result$rmse_results$rmse, 0.1),
                                                  quantile(result$rmse_results$rmse, 0.3),
                                                  quantile(result$rmse_results$rmse, 0.5),
                                                  quantile(result$rmse_results$rmse, 0.7),
                                                  quantile(result$rmse_results$rmse, 0.9),
                                                  max(result$rmse_results$rmse)))) +
  labs(title = "RMSE for Different Lambda and Rank Combinations",
       x = "Lambda", y = "Rank", fill = "RMSE") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
###---------------------------------------------------------------------------###
# Fit to predict test using tuned parameter
incomplete_train_matrix <- convert_Incomplete(train_matrix)
scaled_train_matrix <- biScale(incomplete_train_matrix, trace = TRUE)
fit <- softImpute(scaled_train_matrix, lambda = result$best_lambda_rmse, rank.max = result$best_rank_rmse, maxit = 150, trace.it = TRUE)
complete_train <- softImpute::complete(scaled_train_matrix, fit)

# Collect true and predicted ratings
true_ratings <- numeric()
predicted_ratings <- numeric()
transformed_predicted_ratings <- numeric()
observed_indices <- which(!is.na(test_matrix), arr.ind = TRUE)

for (idx in seq_len(nrow(observed_indices))) {
  i <- observed_indices[idx, 1]
  j <- observed_indices[idx, 2]
  
  true_ratings <- c(true_ratings, ratings_matrix[i, j])
  predicted_rating <- complete_train[i, j]
  predicted_ratings <- c(predicted_ratings, predicted_rating)
  if (predicted_rating < 0.5) {
    transformed_rating <- 0.5
  } else if (predicted_rating > 5) {
    transformed_rating <- 5
  } else {
    transformed_rating <- round(predicted_rating * 2) / 2
  }
  
  transformed_predicted_ratings <- c(transformed_predicted_ratings, transformed_rating)
}

###------------------------------------------------------------------------------###
#Naive model using median rating of movies
# Calculate the median rating for each movie from the training data
movie_medians <- apply(train_matrix, 2, function(x) median(x, na.rm = TRUE))

# Predict ratings in the test set using the median rating of each movie
predicted_ratings_naive <- matrix(NA, nrow = nrow(test_matrix), ncol = ncol(test_matrix))

observed_indices <- which(!is.na(test_matrix), arr.ind = TRUE)

for (idx in seq_len(nrow(observed_indices))) {
  i <- observed_indices[idx, 1]
  j <- observed_indices[idx, 2]
  predicted_ratings_naive[i, j] <- movie_medians[j]
}

# Collect true and predicted ratings
true_ratings_median <- numeric()
predicted_ratings_median <- numeric()

for (idx in seq_len(nrow(observed_indices))) {
  i <- observed_indices[idx, 1]
  j <- observed_indices[idx, 2]
  
  true_ratings_median <- c(true_ratings_median, test_matrix[i, j])
  predicted_ratings_median <- c(predicted_ratings_median, predicted_ratings_naive[i, j])
}

# Calculate RMSE for the naive model
calculate_rmse <- function(true, predicted) {
  sqrt(mean((true - predicted) ^ 2, na.rm = TRUE))
}

rmse_naive <- calculate_rmse(true_ratings_median, predicted_ratings_median)

# Print the RMSE for the naive median-based model
cat("Naive Median-Based Model RMSE:", rmse_naive, "\n")

###-----------------------------------------------------------------------------###

# Function to calculate segment-wise RMSE and MAE
calculate_segment_performance <- function(true_ratings, predicted_ratings, segment_indices) {
  segment_true_ratings <- true_ratings[segment_indices]
  segment_predicted_ratings <- predicted_ratings[segment_indices]
  
  segment_rmse <- calculate_rmse(segment_true_ratings, segment_predicted_ratings)
 
  list(rmse = segment_rmse)
}

# Define segments
user_ids <- unique(observed_indices[, 1])
movie_ids <- unique(observed_indices[, 2])

# High and low rating users
high_rating_users <- user_ids[rowMeans(ratings_matrix[user_ids, ], na.rm = TRUE) > 3.5]
low_rating_users <- user_ids[rowMeans(ratings_matrix[user_ids, ], na.rm = TRUE) <= 3.5]

high_rating_indices <- which(observed_indices[, 1] %in% high_rating_users)
low_rating_indices <- which(observed_indices[, 1] %in% low_rating_users)

# Users with more ratings vs. fewer ratings
user_ratings_count <- rowSums(!is.na(ratings_matrix))
more_ratings_users <- user_ids[user_ratings_count[user_ids] > quantile(user_ratings_count, 0.75)]
fewer_ratings_users <- user_ids[user_ratings_count[user_ids] <= quantile(user_ratings_count, 0.25)]

more_ratings_indices <- which(observed_indices[, 1] %in% more_ratings_users)
fewer_ratings_indices <- which(observed_indices[, 1] %in% fewer_ratings_users)

# Movies with more ratings vs. fewer ratings
movie_ratings_count <- colSums(!is.na(ratings_matrix))
more_ratings_movies <- movie_ids[movie_ratings_count[movie_ids] > quantile(movie_ratings_count, 0.75)]
fewer_ratings_movies <- movie_ids[movie_ratings_count[movie_ids] <= quantile(movie_ratings_count, 0.25)]

more_ratings_movies_indices <- which(observed_indices[, 2] %in% more_ratings_movies)
fewer_ratings_movies_indices <- which(observed_indices[, 2] %in% fewer_ratings_movies)

# Movies with higher average ratings vs. lower average ratings
high_avg_rating_movies <- movie_ids[colMeans(ratings_matrix[, movie_ids], na.rm = TRUE) > 3.5]
low_avg_rating_movies <- movie_ids[colMeans(ratings_matrix[, movie_ids], na.rm = TRUE) <= 3.5]

high_avg_rating_movies_indices <- which(observed_indices[, 2] %in% high_avg_rating_movies)
low_avg_rating_movies_indices <- which(observed_indices[, 2] %in% low_avg_rating_movies)

# Calculate performance for each segment
high_rating_performance <- calculate_segment_performance(true_ratings, predicted_ratings, high_rating_indices)
low_rating_performance <- calculate_segment_performance(true_ratings, predicted_ratings, low_rating_indices)
more_ratings_users_performance <- calculate_segment_performance(true_ratings, predicted_ratings, more_ratings_indices)
fewer_ratings_users_performance <- calculate_segment_performance(true_ratings, predicted_ratings, fewer_ratings_indices)
more_ratings_movies_performance <- calculate_segment_performance(true_ratings, predicted_ratings, more_ratings_movies_indices)
fewer_ratings_movies_performance <- calculate_segment_performance(true_ratings, predicted_ratings, fewer_ratings_movies_indices)
high_avg_rating_movies_performance <- calculate_segment_performance(true_ratings, predicted_ratings, high_avg_rating_movies_indices)
low_avg_rating_movies_performance <- calculate_segment_performance(true_ratings, predicted_ratings, low_avg_rating_movies_indices)

processed_performance <- calculate_rmse(true_ratings, transformed_predicted_ratings)

# Print the results
# Print the results
cat("Overall RMSE:", calculate_rmse(true_ratings, predicted_ratings), "\n")
cat("High Rating Users RMSE:", high_rating_performance$rmse, "\n")
cat("Low Rating Users RMSE:", low_rating_performance$rmse, "\n")
cat("Users with More Ratings RMSE:", more_ratings_users_performance$rmse, "\n")
cat("Users with Fewer Ratings RMSE:", fewer_ratings_users_performance$rmse, "\n")
cat("Movies with More Ratings RMSE:", more_ratings_movies_performance$rmse, "\n")
cat("Movies with Fewer Ratings RMSE:", fewer_ratings_movies_performance$rmse, "\n")
cat("High Average Rating Movies RMSE:", high_avg_rating_movies_performance$rmse, "\n")
cat("Low Average Rating Movies RMSE:", low_avg_rating_movies_performance$rmse, "\n")

###------------------------------------------------------------------------------###

# Calculate squared errors
squared_errors <- (true_ratings - predicted_ratings)^2

# Create a data frame for plotting
errors_data <- data.frame(SquaredErrors = squared_errors)

# Density Plot of Squared Errors
ggplot(errors_data, aes(x = SquaredErrors)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Squared Errors", x = "Squared Errors", y = "Density") +
  theme_bw()

# Flatten the matrix to a vector for easier computation for plotting
ratings_vector <- as.vector(complete_train)
ratings_vector <- ratings_vector[!is.na(ratings_vector)]

# Calculate descriptive statistics
mean_rating <- mean(predicted_ratings)
median_rating <- median(predicted_ratings)
sd_rating <- sd(predicted_ratings)
min_rating <- min(predicted_ratings)
max_rating <- max(predicted_ratings)
skewness_rating <- skewness(predicted_ratings)
kurtosis_rating <- kurtosis(predicted_ratings)

# Print descriptive statistics
cat("Descriptive Statistics of Predicted Ratings:\n")
cat("Mean:", mean_rating, "\n")
cat("Median:", median_rating, "\n")
cat("Standard Deviation:", sd_rating, "\n")
cat("Minimum:", min_rating, "\n")
cat("Maximum:", max_rating, "\n")
cat("Skewness:", skewness_rating, "\n")
cat("Kurtosis:", kurtosis_rating, "\n")

# Count observations that fall below 0 and above 5
below_zero_count <- sum(predicted_ratings < 0.5)
above_five_count <- sum(predicted_ratings > 5)

# Print the counts
cat("Number of ratings below 0.5:", below_zero_count, "\n")
cat("Number of ratings above 5:", above_five_count, "\n")

ggplot(data.frame(rating = predicted_ratings), aes(x = rating)) +
  geom_density(fill = "lightblue", color = "black", alpha = 0.7) +
  theme_bw() +
  labs(title = "Density Plot of Predicted Ratings at Test Indices",
       x = "Predicted Rating",
       y = "Density")

###-------------------------------------------------------------------------------###
# Calculate prediction bias
prediction_bias <- data.frame(
  true_rating = true_ratings,
  predicted_rating = predicted_ratings,
  bias = predicted_ratings - true_ratings
)

# Summarize bias statistics
bias_summary <- prediction_bias %>%
  summarise(
    mean_bias = mean(bias),
    median_bias = median(bias),
    min_bias = min(bias),
    max_bias = max(bias),
    sd_bias = sd(bias)
  )

print(bias_summary)


###-----------------------------------------------------------------------------###
# Ratings unclipped & unrounded

# Plot the distribution of prediction bias
ggplot(prediction_bias, aes(x = bias)) +
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Prediction Bias",
       x = "Prediction Bias (Predicted - True)",
       y = "Frequency")

# Plot bias as a function of true ratings
ggplot(prediction_bias, aes(x = true_rating, y = bias)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Prediction Bias by True Rating",
       x = "True Rating",
       y = "Prediction Bias (Predicted - True)")


###----------------------------------------------------------------------------### 

#Confusion matrix (clipped & rounded to nearest half integer)

# Round predicted and true ratings to nearest half integer, bound them between 0.5 and 5
rounded_predicted_ratings <- pmin(pmax(round(predicted_ratings * 2) / 2, 0.5), 5)
rounded_true_ratings <- true_ratings

# Calculate prediction bias for the rounded ratings
prediction_bias_rounded <- data.frame(
  true_rating = rounded_true_ratings,
  predicted_rating = rounded_predicted_ratings,
  bias = rounded_predicted_ratings - rounded_true_ratings
)

# Summarize bias statistics for the rounded ratings
bias_summary_rounded <- prediction_bias_rounded %>%
  summarise(
    mean_bias = mean(bias),
    median_bias = median(bias),
    min_bias = min(bias),
    max_bias = max(bias),
    sd_bias = sd(bias)
  )

print(bias_summary_rounded)

# Plot the distribution of prediction bias for the rounded ratings
ggplot(prediction_bias_rounded, aes(x = bias, y = after_stat(count / sum(count)))) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  theme_bw() +
  labs(title = "Distribution of Prediction Bias for Rounded Ratings",
       x = "Prediction Bias (Predicted - True)",
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format())

# Plot bias as a function of true ratings for the rounded ratings
ggplot(prediction_bias_rounded, aes(x = as.factor(true_rating), y = bias)) +
  geom_violin(fill = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Prediction Bias by True Rating for Rounded Ratings",
       x = "True Rating",
       y = "Prediction Bias (Predicted - True)")


# Create the confusion matrix
conf_matrix_2 <- confusionMatrix(
  factor(rounded_predicted_ratings, levels = seq(0.5, 5, by = 0.5)),
  factor(rounded_true_ratings, levels = seq(0.5, 5, by = 0.5))
)

# Print the confusion matrix
print(conf_matrix_2)

# Convert the confusion matrix to a data frame for visualization
conf_matrix_df_2 <- as.data.frame(conf_matrix_2$table)
colnames(conf_matrix_df_2) <- c("Predicted", "True", "Frequency")

# Reverse the levels for Predicted ratings
conf_matrix_df_2$Predicted <- factor(conf_matrix_df_2$Predicted, levels = rev(levels(conf_matrix_df_2$Predicted)))

# Visualize the confusion matrix with the adjusted levels
ggplot(data = conf_matrix_df_2, aes(x = True, y = Predicted, fill = Frequency)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frequency), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  theme_minimal() +
  labs(title = "Confusion Matrix",
       x = "True Rating",
       y = "Predicted Rating",
       fill = "Frequency")

###--------------------------------------------------------------------------###
transform_predicted_matrix <- function(predicted_matrix) {
  transformed_matrix <- predicted_matrix
  transformed_matrix[predicted_matrix < 0.5] <- 0.5
  transformed_matrix[predicted_matrix > 5] <- 5
  transformed_matrix[predicted_matrix >= 0.5 & predicted_matrix <= 5] <- round(predicted_matrix[predicted_matrix >= 0.5 & predicted_matrix <= 5])
  return(transformed_matrix)
}

# Apply the transformation function to the predicted matrix
transformed_predicted_matrix <- transform_predicted_matrix(complete_train)
# Function to calculate prediction bias

calculate_prediction_bias <- function(predicted_matrix, true_matrix) {
  # Ensure the matrices have the same dimensions
  if (!all(dim(predicted_matrix) == dim(true_matrix))) {
    stop("Matrices must have the same dimensions")
  }
  
  # Calculate the bias only at the indices where true_matrix is not NA
  bias_matrix <- matrix(NA, nrow = nrow(true_matrix), ncol = ncol(true_matrix))
  bias_indices <- !is.na(true_matrix)
  bias_matrix[bias_indices] <- predicted_matrix[bias_indices] - true_matrix[bias_indices]
  
  return(bias_matrix)
}

predicted_matrix <- complete_train
true_matrix <- test_matrix
bias_matrix <- calculate_prediction_bias(transformed_predicted_matrix, test_matrix)

# Convert bias_matrix to a data frame for easier handling
bias_df <- melt(bias_matrix, varnames = c("userId", "movieId"), value.name = "bias")
bias_df <- bias_df[!is.na(bias_df$bias), ]  # Remove NA values

# Merge with movie_info to get genres
movie_info$movieId <- as.integer(movie_info$movieId)  # Ensure movieId is an integer
bias_with_genres <- merge(bias_df, movie_info, by = "movieId")

# Split genres into separate rows
bias_genres_split <- bias_with_genres %>%
  separate_rows(genres, sep = "\\|")

# Group by genres and calculate average bias
average_bias_by_genre <- bias_genres_split %>%
  group_by(genres) %>%
  summarise(average_bias = mean(bias), .groups = 'drop') %>%
  arrange(desc(average_bias))

# Plot the average bias by genre 
ggplot(average_bias_by_genre, aes(x = reorder(genres, -average_bias), y = average_bias, fill = average_bias >= 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "orange")) +
  theme_bw() +
  labs(title = "Average Prediction Bias by Genre", x = "Genre", y = "Average Bias") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")  # This line removes the legend for the fill

# Display the result
print(average_bias_by_genre)

# Group by genres and calculate average, min, and max bias
bias_summary_by_genre <- bias_genres_split %>%
  group_by(genres) %>%
  summarise(
    average_bias = mean(bias),
    min_bias = min(bias),
    max_bias = max(bias),
    .groups = 'drop'
  ) %>%
  arrange(desc(average_bias))

print(bias_summary_by_genre)

# Count the frequency of overpredicting and underpredicting per genre
bias_frequency_by_genre <- bias_genres_split %>%
  mutate(overpredict = ifelse(bias > 0, 1, 0),
         underpredict = ifelse(bias < 0, 1, 0)) %>%
  group_by(genres) %>%
  summarise(
    overpredict_count = sum(overpredict),
    underpredict_count = sum(underpredict),
    .groups = 'drop'
  ) %>%
  arrange(desc(overpredict_count))

# Melt the data frame for easier plotting
bias_frequency_melted <- bias_frequency_by_genre %>%
  pivot_longer(cols = c(overpredict_count, underpredict_count), 
               names_to = "prediction_type", 
               values_to = "count")

# Plot the frequency of overpredicting and underpredicting by genre using a grouped bar chart
ggplot(bias_frequency_melted, aes(x = reorder(genres, -count), y = count, fill = prediction_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("overpredict_count" = "darkgreen", "underpredict_count" = "darkred"), 
                    labels = c("Overpredict", "Underpredict")) +
  theme_minimal() +
  labs(title = "Frequency of Overpredicting and Underpredicting by Genre", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Prediction Type"))

# Display the result
print(bias_frequency_by_genre)

# Calculate the proportion of overprediction and underprediction by genre
bias_proportion_by_genre <- bias_genres_split %>%
  mutate(overpredict = ifelse(bias > 0, 1, 0),
         underpredict = ifelse(bias < 0, 1, 0)) %>%
  group_by(genres) %>%
  summarise(
    total = n(),
    overpredict_proportion = sum(overpredict) / total,
    underpredict_proportion = sum(underpredict) / total,
    .groups = 'drop'
  ) %>%
  arrange(desc(overpredict_proportion))

# Melt the data frame for easier plotting
bias_proportion_melted <- bias_proportion_by_genre %>%
  pivot_longer(cols = c(overpredict_proportion, underpredict_proportion), 
               names_to = "prediction_type", 
               values_to = "proportion")

bias_proportion_melted$genres_spaced <- factor(bias_proportion_melted$genres, 
                                               levels = unique(bias_proportion_melted$genres),
                                               labels = paste0(unique(bias_proportion_melted$genres), " "))

# Plot the proportion of overpredicting and underpredicting by genre using a grouped bar chart
ggplot(bias_proportion_melted, aes(x = reorder(genres_spaced, -proportion), y = proportion, fill = prediction_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("overpredict_proportion" = "darkgreen", "underpredict_proportion" = "darkred"), 
                    labels = c("Overpredict", "Underpredict")) +
  theme_bw() +
  labs(title = "Proportion of Overpredicting and Underpredicting by Genre", x = "Genre", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Prediction Type"))

# Calculate the frequency and magnitude of overpredicting, underpredicting, and correct predictions per genre
bias_proportion_magnitude_by_genre <- bias_genres_split %>%
  mutate(overpredict = ifelse(bias > 0, 1, 0),
         underpredict = ifelse(bias < 0, 1, 0),
         correct_predict = ifelse(bias == 0, 1, 0),
         overpredict_magnitude = ifelse(bias > 0, bias, NA),
         underpredict_magnitude = ifelse(bias < 0, bias, NA)) %>%
  group_by(genres) %>%
  summarise(
    total = n(),
    overpredict_count = sum(overpredict),
    overpredict_proportion = sum(overpredict) / total,
    average_overpredict_magnitude = mean(overpredict_magnitude, na.rm = TRUE),
    underpredict_count = sum(underpredict),
    underpredict_proportion = sum(underpredict) / total,
    average_underpredict_magnitude = mean(underpredict_magnitude, na.rm = TRUE),
    correct_predict_count = sum(correct_predict),
    correct_predict_proportion = sum(correct_predict) / total,
    .groups = 'drop'
  ) %>%
  arrange(desc(overpredict_proportion))

# Create a LaTeX table
latex_table <- xtable(bias_proportion_magnitude_by_genre)
