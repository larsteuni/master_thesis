# Load necessary packages
library(readr)  # For reading TSV files
library(data.table)
library(wnominate)
library(dplyr)
library(tidyr)
library(moments)
library(ggplot2)

# Read the TSV file
file_path <- "C:/Eindproject_master/df_member_votes_fv_with_topics_3.tsv"
data <- read_tsv(file_path)

# Define bimodality coefficient function
bimodality_coefficient <- function(x) {
  n <- length(x)
  skewness_x <- skewness(x)
  kurtosis_x <- kurtosis(x) - 3
  BC <- (skewness_x^2 + 1) / (kurtosis_x + 3 * (n-1)^2 / ((n-2)*(n-3)))
  print(BC)
  return(BC)
}

adjust_orientation <- function(scores, party_labels) {
  # Ensure party_labels is a factor with levels "Democrat" and "Republican"
  party_labels <- factor(party_labels, levels = c("Democratic Party", "Republican Party"))
  
  # Calculate mean scores for each party
  mean_democrat <- mean(scores[party_labels == "Democratic Party"], na.rm = TRUE)
  mean_republican <- mean(scores[party_labels == "Republican Party"], na.rm = TRUE)
  
  # Check if Democrats have a positive mean score (indicating a flip)
  if (mean_democrat > mean_republican) {
    scores <- -scores  # Flip the scores
    print("scores flipped!")
  }
  
  return(scores)
}

# Function to find a valid polarity legislator (placeholder)
find_polarity_legislator <- function(subset_data, rc) {
  # Ensure there are at least 30 votes
  total_votes <- ncol(rc$votes)
  if (total_votes < 30) {
    print("Fewer than 30 total votes, skipping this topic and congress.")
    return(NULL)
  }
  
  # Count the number of votes per legislator
  vote_counts <- table(subset_data$nameparty_id)
  
  # Filter legislators with at least 20 votes
  valid_legislators <- names(vote_counts[vote_counts >= 20])
  
  if (length(valid_legislators) == 0) {
    return(NULL)
  }
  
  # Verify that the legislator has at least 20 valid votes after processing
  for (legislator in valid_legislators) {
    if (sum(rc$votes[rownames(rc$votes) == legislator, ] != 0) >= 20) {
      print(paste("Legislator:", legislator, "- Votes:", vote_counts[legislator]))
      return(legislator)
    }
  }
  
  return(NULL)
}


# Get unique topics and congresses
topics <- unique(data$topic_with_coherence)
congresses <- unique(data$congress)

# Prepare to store results
bimodality_results <- matrix(NA, nrow = length(topics), ncol = length(congresses))
colnames(bimodality_results) <- congresses
rownames(bimodality_results) <- topics

vote_counts_results <- matrix(NA, nrow = length(topics), ncol = length(congresses))
colnames(vote_counts_results) <- congresses
rownames(vote_counts_results) <- topics

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
    
    pivot_data <- reshaped_data %>% 
      pivot_wider(names_from = bill_id, values_from = cast_code, values_fill = 0)
    
    matrix_data <- as.matrix(pivot_data)
    
    ids <- matrix_data[,1]
    legData <- matrix(matrix_data[,2], length(matrix_data[,2]), 1)
    colnames(legData) <- "party"
    matrix_data <- matrix_data[,-c(1,2)]
    
    rc <- rollcall(matrix_data, yea=c(1,2,3), nay=c(4,5,6), missing=c(7,8,9),
                   notInLegis=0, legis.names = ids, legis.data=legData)
    
    # Find a valid polarity legislator after rollcall processing
    polarity_legislator <- find_polarity_legislator(subset_data, rc)
    
    total_votes <- ncol(rc$votes)
    vote_counts_results[topic_iterator, as.character(congress_iterator)] <- total_votes
    
    if (is.null(polarity_legislator)) {
      print(paste("No valid legislator found for Topic:", topic_iterator, "Congress:", congress_iterator))
      next
    }
    
    # Perform W-NOMINATE with the valid polarity legislator
    nominate_result <- wnominate(rc, dims = 1, polarity = polarity_legislator)
    
    # Extract W-NOMINATE scores
    nominate_scores <- nominate_result$legislators$coord1D
    
    # Remove NAs if present
    nominate_scores <- na.omit(nominate_scores)
    
    # Ensure all values are numeric
    nominate_scores <- as.numeric(nominate_scores)
    
    # Adjust orientation based on party labels
    adjusted_scores <- adjust_orientation(nominate_scores, subset_data$party)
    
    # Calculate Bimodality Coefficient
    bc_value <- bimodality_coefficient(adjusted_scores)
    
    # Store bimodality coefficient result
    bimodality_results[topic_iterator, as.character(congress_iterator)] <- bc_value
  }
}

# Convert results to data frames
bimodality_results_df <- as.data.frame(bimodality_results, row.names = topics)
vote_counts_results_df <- as.data.frame(vote_counts_results, row.names = topics)

# Write results to CSV files
write.csv(bimodality_results_df, "C:/Eindproject_master/bimodality_coefficients.csv", row.names = TRUE)
write.csv(vote_counts_results_df, "C:/Eindproject_master/vote_counts.csv", row.names = TRUE)