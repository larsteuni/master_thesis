# Load necessary library
library(dplyr)

# Read the CSV file
input_csv_file <- "C:/Eindproject_master/df_bills_fv_with_topics_3.csv"
df <- read.csv(input_csv_file, sep = '\t')

# Define the function to map topics to categories
map_topics_to_categories <- function(df, topic_column) {
  # Ensure the topic column exists in the data frame
  if (!topic_column %in% names(df)) {
    stop("The specified topic column does not exist in the data frame.")
  }
  
  # Map the topic values to category names using base R
  df$topic <- sapply(df[[topic_column]], function(x) {
    switch(as.character(x),
           '0' = 'Environment and Natural Resources',
           '1' = 'Infrastructure and Development',
           '2' = 'Government Budget and Administration',
           '3' = 'Defense and Military',
           '4' = 'International Relations and Government',
           '5' = 'Legislation and Policy',
           '6' = 'Social Services and Public Welfare',
           'unknown')  # Default case
  })
  
  return(df)
}
df <- map_topics_to_categories(df, "topic")

# Count the number of rows per topic
topic_counts <- table(df$topic)
print(topic_counts)

# Sample 5 random rows per topic
sampled_rows <- df %>%
  group_by(df$topic) %>%
  sample_n(5)

# Convert to a new data frame
sampled_df <- as.data.frame(sampled_rows)
# Keep only the 'full_summary' and 'topic' columns
sampled_df <- sampled_df[, c("full_summary", "topic")]
