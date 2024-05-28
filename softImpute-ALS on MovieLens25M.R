# Load necessary libraries
packages <- c("data.table",
              "reshape2", 
              "softImpute",
              "dplyr",
              "tidyr",
              "Matrix",
              "progress",
              "ggplot2", 
              "lubridate",
              "foreach"
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

###-----------------------------------------------------------------------------###

#Grid search to find optimal parameters

calculate_rmse <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

calculate_nmae <- function(true_ratings, predicted_ratings) {
  mean(abs(true_ratings - predicted_ratings)) / mean(abs(true_ratings))
}

cv_softimpute <- function(train_matrix, lambda_seq, max_rank_seq, nfolds = 5) {
  # Create folds
  folds <- sample(rep(1:nfolds, length.out = nrow(train_matrix)))
  
  # Grid search over lambda and max.rank
  results <- expand.grid(lambda = lambda_seq, max_rank = max_rank_seq)
  results$rmse <- NA
  results$nmae <- NA
  
  # Initialize the progress bar
  total_iterations <- nrow(results) * nfolds
  pb <- progress_bar$new(
    format = "  Progress [:bar] :percent in :elapsed",
    total = total_iterations,
    clear = FALSE,
    width = 60
  )
  
  # Perform K-fold cross-validation
  for (i in 1:nrow(results)) {
    lambda <- results$lambda[i]
    max_rank <- results$max_rank[i]
    
    # Initialize RMSE and NMAE for this parameter set
    cv_rmse <- numeric(nfolds)
    cv_nmae <- numeric(nfolds)
    
    for (k in 1:nfolds) {
      # Split the data into training and validation sets
      train_fold <- train_matrix
      train_fold[folds == k, ] <- NA
      val_fold <- train_matrix[folds == k, , drop = FALSE]
      
      # Apply softImpute on the training fold
      fit <- softImpute(train_fold, lambda = lambda, rank.max = max_rank, type = "als")
      
      # Impute the validation fold
      completed_matrix <- complete(train_fold, fit)
      
      # Evaluate on the validation set
      true_ratings <- numeric()
      predicted_ratings <- numeric()
      for (row in 1:nrow(val_fold)) {
        for (col in 1:ncol(val_fold)) {
          if (!is.na(val_fold[row, col])) {
            true_ratings <- c(true_ratings, val_fold[row, col])
            predicted_ratings <- c(predicted_ratings, completed_matrix[row, col])
          }
        }
      }
      
      # Calculate RMSE and NMAE
      cv_rmse[k] <- calculate_rmse(true_ratings, predicted_ratings)
      cv_nmae[k] <- calculate_nmae(true_ratings, predicted_ratings)
      
      # Update the progress bar
      pb$tick()
    }
    
    # Average RMSE and NMAE across all folds
    avg_rmse <- mean(cv_rmse)
    avg_nmae <- mean(cv_nmae)
    
    # Store the results
    results$rmse[i] <- avg_rmse
    results$nmae[i] <- avg_nmae
  }
  
  # Return the results as a data frame
  return(results)
}



#Grid of parameters
lambda_seq <- seq(0.1, 5, by = 0.1)
max_rank_seq <- seq(50, 200, by = 25)

results <- cv_softimpute(train_matrix, lambda_seq, max_rank_seq, nfolds = 5)

# Print the results
print(results)

# Find the best parameters
best_params <- results[which.min(results$rmse), ]
print(best_params)
