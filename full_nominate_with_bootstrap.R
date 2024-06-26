# Load necessary packages
library(readr)  # For reading TSV files
library(wnominate)
library(dplyr)
library(moments)

# Read the TSV file
file_path <- "C:/Eindproject_master/df_member_votes_fv_with_topics_3.tsv"
data <- read_tsv(file_path)

# Define bimodality coefficient function
bimodality_coefficient <- function(x) {
  n <- length(x)
  skewness_x <- skewness(x)
  kurtosis_x <- kurtosis(x) - 3
  BC <- (skewness_x^2 + 1) / (kurtosis_x + 3 * (n-1)^2 / ((n-2)*(n-3)))
  return(BC)
}

adjust_orientation <- function(scores, party_labels) {
  # Ensure party_labels is a factor with levels "Democratic Party" and "Republican Party"
  party_labels <- factor(party_labels, levels = c("Democratic Party", "Republican Party"))
  
  # Calculate mean scores for each party
  mean_democrat <- mean(scores[party_labels == "Democratic Party"], na.rm = TRUE)
  mean_republican <- mean(scores[party_labels == "Republican Party"], na.rm = TRUE)
  
  # Check if Democrats have a positive mean score (indicating a flip)
  if (mean_democrat > mean_republican) {
    scores <- -scores  # Flip the scores
  }
  
  return(scores)
}

# Function to find a valid polarity legislator
find_polarity_legislator <- function(subset_data, rc) {
  total_votes <- ncol(rc$votes)
  if (total_votes < 30) {
    return(NULL)
  }
  
  vote_counts <- table(subset_data$nameparty_id)
  valid_legislators <- names(vote_counts[vote_counts >= 20])
  
  if (length(valid_legislators) == 0) {
    return(NULL)
  }
  
  for (legislator in valid_legislators) {
    if (sum(rc$votes[rownames(rc$votes) == legislator, ] != 0) >= 20) {
      return(legislator)
    }
  }
  
  return(NULL)
}

# Bootstrap function to estimate standard error
bootstrap_bimodality <- function(scores, n_bootstrap) {
  bootstrap_results <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    boot_sample <- sample(scores, replace = TRUE)
    bc_value <- bimodality_coefficient(boot_sample)
    bootstrap_results[i] <- bc_value
  }
  
  standard_error <- sd(bootstrap_results, na.rm = TRUE)
  
  return(standard_error)
}

# Get unique topics and congresses
topics <- unique(data$topic_with_coherence)
congresses <- unique(data$congress)

# Prepare to store results
bimodality_results <- list()

# Ensure correct data types
data$topic_with_coherence <- as.character(data$topic_with_coherence)
data$congress <- as.integer(data$congress)

# Calculate W-NOMINATE and Bimodality Coefficient for each topic and congress
for (topic_iterator in topics) {
  for (congress_iterator in congresses) {
    print(paste("Processing Topic:", topic_iterator, "Congress:", congress_iterator))
    
    subset_data <- subset(data, topic_with_coherence == topic_iterator & congress == congress_iterator)
    
    reshaped_data <- subset_data %>%
      select(nameparty_id, party, bill_id, cast_code)
    
    pivot_data <- reshape2::dcast(reshaped_data, nameparty_id + party ~ bill_id, value.var = "cast_code", fill = 0)
    
    matrix_data <- as.matrix(pivot_data[, -c(1, 2)])  # Exclude first two columns (nameparty_id, party)
    ids <- pivot_data$nameparty_id
    legData <- matrix(pivot_data$party, nrow(matrix_data), 1)
    colnames(legData) <- "party"
    
    rc <- rollcall(matrix_data, yea = c(1, 2, 3), nay = c(4, 5, 6), missing = c(7, 8, 9),
                   notInLegis = 0, legis.names = ids, legis.data = legData)
    
    polarity_legislator <- find_polarity_legislator(subset_data, rc)
    
    total_votes <- ncol(rc$votes)
    
    if (is.null(polarity_legislator)) {
      print(paste("No valid legislator found for Topic:", topic_iterator, "Congress:", congress_iterator))
      bimodality_results[[paste0(topic_iterator, "_", congress_iterator)]] <- list(
        bc_value = NA,
        bootstrap_error = NA
      )
      next
    }
    
    # Perform W-NOMINATE with the valid polarity legislator
    nominate_result <- wnominate(rc, dims = 1, polarity = polarity_legislator)
    
    # Extract W-NOMINATE scores
    nominate_scores <- nominate_result$legislators$coord1D
    nominate_scores <- na.omit(nominate_scores)
    nominate_scores <- as.numeric(nominate_scores)
    
    # Adjust orientation based on party labels
    adjusted_scores <- adjust_orientation(nominate_scores, subset_data$party)
    
    # Calculate original Bimodality Coefficient
    bc_value <- bimodality_coefficient(adjusted_scores)
    
    # Perform bootstrap to estimate error margin
    if (length(nominate_scores) > 1) {  # Check if more than one legislator
      bootstrap_error <- bootstrap_bimodality(nominate_scores, n_bootstrap = 1000)
    } else {
      bootstrap_error <- NA
    }
    
    # Store results in list
    bimodality_results[[paste0(topic_iterator, "_", congress_iterator)]] <- list(
      bc_value = bc_value,
      bootstrap_error = bootstrap_error
    )
  }
}

# Convert results to data frames
bimodality_results_df <- data.frame(
  topic_congress = names(bimodality_results),
  bc_value = sapply(bimodality_results, function(x) x$bc_value),
  bootstrap_error = sapply(bimodality_results, function(x) x$bootstrap_error)
)

# Write results to CSV files
write.csv(bimodality_results_df, "C:/Eindproject_master/bimodality_coefficients_bootstrap.csv", row.names = TRUE)
write.csv(vote_counts_results_df, "C:/Eindproject_master/vote_counts_bootstrap.csv", row.names = TRUE)