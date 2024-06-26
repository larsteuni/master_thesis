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

# Get unique topics and congresses
topics <- unique(data$topic_with_coherence)
congresses <- unique(data$congress)

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

print(topics[1])
print(congresses[25])
#subset
subset_data <- subset(data, topic_with_coherence == topics[1] & congress == congresses[25])
#create rc
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

# Run 1D W-NOMINATE
result_1D <- wnominate(rc, dims=1, polarity=2)
str(result_1D)
# Plot W-NOMINATE results
plot(result_1D)

# Extract W-NOMINATE scores for 1D
wnom_scores_1D <- result_1D$legislators$coord1D

# Debugging: Check for non-numeric values or NAs
summary(wnom_scores_1D)

# Remove NAs if present
wnom_scores_1D <- na.omit(wnom_scores_1D)

# Ensure all values are numeric
wnom_scores_1D <- as.numeric(wnom_scores_1D)

# Adjust orientation based on party labels
adjusted_scores <- adjust_orientation(wnom_scores_1D, subset_data$party)

skewness_value <- skewness(wnom_scores_1D)
kurtosis_value <- kurtosis(wnom_scores_1D)

# Debugging: Check the distribution
hist(wnom_scores_1D, main="Histogram of W-NOMINATE Scores", xlab="W-NOMINATE Score")

# Define bimodality coefficient function
bimodality_coefficient <- function(x) {
  n <- length(x)
  skewness_x <- skewness(x)
  kurtosis_x <- kurtosis(x) - 3
  BC <- (skewness_x^2 + 1) / (kurtosis_x + 3 * (n-1)^2 / ((n-2)*(n-3)))
  print(BC)
  return(BC)
}

# Calculate Bimodality Coefficient
bimodality_1D <- bimodality_coefficient(wnom_scores_1D)

# Check the result
print(paste("Bimodality Coefficient for 1D W-NOMINATE:", bimodality_1D))

# Create a data frame for ggplot
plot_data_1D <- data.frame(
  Dimension1 = wnom_scores_1D
)

p1D <- ggplot(plot_data_1D, aes(x = Dimension1)) +
  geom_histogram(bins = 30, fill = 'blue', alpha = 0.7) +
  labs(title = paste("Histogram of Dimension 1\nBimodality Coefficient:", round(bimodality_1D, 3)),
       x = "Dimension 1", y = "Frequency") +
  theme_minimal()

# Display plot
print(p1D)