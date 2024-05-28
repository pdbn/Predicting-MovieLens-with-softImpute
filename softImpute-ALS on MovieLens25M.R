# Load necessary libraries
packages <- c("data.table",
              "reshape2", 
              "softImpute",
              "dplyr",
              "tidyr",
              "Matrix",
              "progress",
              "ggplot2")

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
#str(ratings_sample)
#summary(ratings_sample)
#head(ratings_sample)

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

# Example: Distribution of ratings per user
ratings_sample %>%
  group_by(userId) %>%
  summarise(avg_rating = mean(rating), count = n()) %>%
  ggplot(aes(x = count, y = avg_rating)) +
  geom_point() +
  labs(title = "Average Rating by Number of Ratings per User", x = "Number of Ratings", y = "Average Rating") +
  theme_minimal()

#Temporal changes
ratings_sample %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(x = year, y = avg_rating)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Rating Over Time", x = "Year", y = "Average Rating") +
  theme_minimal()

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

# Step 1: Initialize a test mask
test_mask <- matrix(FALSE, nrow=nrow(ratings_matrix), ncol=ncol(ratings_matrix))

# Step 2: Identify indices of the observed (non-NA) entries
observed_indices <- which(!is.na(ratings_matrix), arr.ind=TRUE)

# Step 3: Randomly select 20% of these indices for testing
sample_size <- floor(0.2 * nrow(observed_indices))
set.seed(123)  # Ensure reproducibility
selected_indices <- sample(seq_len(nrow(observed_indices)), size = sample_size)
test_mask[observed_indices[selected_indices, "row"], observed_indices[selected_indices, "col"]] <- TRUE

# Step 4: Create the training matrix
train_matrix <- ratings_matrix
train_matrix[test_mask] <- NA  # Set selected test entries to NA in the training matrix

# Step 5: Create the test matrix
test_matrix <- matrix(NA, nrow=nrow(ratings_matrix), ncol=ncol(ratings_matrix))
test_matrix[test_mask] <- ratings_matrix[test_mask]  # Fill test matrix with observed entries from the original matrix

#Probability distribution in train and test matrix
train_plotdf <- as.data.frame(train_matrix)
train_long <- pivot_longer(train_plotdf, cols = everything(), names_to = "movieId", values_to = "rating")
train_long <- train_long %>% filter(!is.na(rating))
ggplot(train_long, aes(x = rating)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) +
  labs(title = "Probability Distribution of Train Ratings", x = "Rating", y = "Probability") +
  theme_minimal()


###------------------------------------------------------------------------------###
# Generate a sequence of lambda values
lambdas <- seq(0.1, 5, length.out = 100)
rmse_results <- data.frame(lambda = numeric(), rmse = numeric())
nmae_results <- data.frame(lambda = numeric(), nmae = numeric())

# Initialize progress bar
pb <- progress_bar$new(total = 100, format = "[:bar] :percent :elapsed ETA: :eta", clear = FALSE)

# Function to calculate RMSE
calculate_rmse <- function(true, predicted) {
  sqrt(mean((true - predicted)^2, na.rm = TRUE))
}

#Function to calculate NMAE
calculate_nmae <- function(true, predicted) {
  mean(abs(true - predicted), na.rm = TRUE) / (max(true, na.rm = TRUE) - min(true, na.rm = TRUE))
}

#Find smallest value for lambda such that softImpute returns zero solution
#all nonzero solutions would require a smaller lambda
lambda0(ratings_matrix)

# Apply SoftImpute for different lambda values
for (lambda in lambdas) {
  softImpute_model <- softImpute(train_matrix, lambda=lambda, rank.max=150, type = "als")
  completed_matrix <- complete(train_matrix, softImpute_model)

  
  if (!is.matrix(completed_matrix)) {
    completed_matrix <- as.matrix(completed_matrix)
  }
  
  # Evaluate on the test set
  true_ratings <- numeric()
  predicted_ratings <- numeric()
  for (i in seq_len(nrow(test_matrix))) {
    for (j in seq_len(ncol(test_matrix))) {
      if (!is.na(test_matrix[i, j])) {
        true_ratings <- c(true_ratings, test_matrix[i, j])
        predicted_ratings <- c(predicted_ratings, completed_matrix[i, j])
      }
    }
  }
  
  # Calculate RMSE
  rmse <- calculate_rmse(true_ratings, predicted_ratings)
  nmae <- calculate_nmae(true_ratings, predicted_ratings)
  rmse_results <- rbind(rmse_results, data.frame(lambda = lambda, rmse = rmse))
  nmae_results <- rbind(nmae_results, data.frame(lambda = lambda, nmae = nmae))
  
  pb$tick()
}

# Plot RMSE vs. Lambda
ggplot(rmse_results, aes(x = lambda, y = rmse)) +
  geom_line() +
  geom_point() +
  labs(title = "RMSE vs. Lambda", x = "Lambda", y = "RMSE") +
  theme_minimal()

ggplot(nmae_results, aes(x = lambda, y = nmae)) +
  geom_line() +
  geom_point() +
  labs(title = "NMAE vs. Lambda", x = "Lambda", y = "NMAE") +
  theme_minimal()


#Fix lambda = 1.3
#Change ranks
ranks <- seq(50, 200, length.out = 10)
rmse_results <- data.frame(rank = numeric(), rmse = numeric())
nmae_results <- data.frame(rank = numeric(), nmae = numeric())

pb <- progress_bar$new(total = 10, format = "[:bar] :percent :elapsed ETA: :eta", clear = FALSE)

for (rank in ranks) {
  softImpute_model <- softImpute(train_matrix, lambda= 1.3 , rank.max= rank, type = "als")
  completed_matrix <- complete(train_matrix, softImpute_model)
  
  
  if (!is.matrix(completed_matrix)) {
    completed_matrix <- as.matrix(completed_matrix)
  }
  
  # Evaluate on the test set
  true_ratings <- numeric()
  predicted_ratings <- numeric()
  for (i in seq_len(nrow(test_matrix))) {
    for (j in seq_len(ncol(test_matrix))) {
      if (!is.na(test_matrix[i, j])) {
        true_ratings <- c(true_ratings, test_matrix[i, j])
        predicted_ratings <- c(predicted_ratings, completed_matrix[i, j])
      }
    }
  }
  
  # Calculate RMSE
  rmse <- calculate_rmse(true_ratings, predicted_ratings)
  nmae <- calculate_nmae(true_ratings, predicted_ratings)
  rmse_results <- rbind(rmse_results, data.frame(rank = rank, rmse = rmse))
  nmae_results <- rbind(nmae_results, data.frame(rank = rank, nmae = nmae))
  
  pb$tick()
}

# Plot RMSE vs. Rank
ggplot(rmse_results, aes(x = rank, y = rmse)) +
  geom_line() +
  geom_point() +
  labs(title = "RMSE vs. Rank", x = "Rank", y = "RMSE") +
  theme_minimal()

ggplot(nmae_results, aes(x = rank, y = nmae)) +
  geom_line() +
  geom_point() +
  labs(title = "NMAE vs. Rank", x = "Rank", y = "NMAE") +
  theme_minimal()
