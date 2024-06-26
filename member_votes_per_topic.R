# Load necessary packages
library(readr)  # For reading TSV files
library(dplyr)  # For data manipulation
library(stringr) # For string manipulation

# Read the TSV file
file_path <- "C:/Eindproject_master/df_member_votes_fv_with_topics_3.tsv"
data <- read_tsv(file_path)

# Split the data frame by 'topic_with_coherence'
data_split <- split(data, data$topic_with_coherence)

# Function to create a safe file name from a topic string
make_safe_filename <- function(topic) {
  safe_name <- gsub(":", "-", topic)    # Replace colons with hyphens
  safe_name <- gsub(" ", "_", safe_name) # Replace spaces with underscores
  return(paste0("C:/Eindproject_master/df_member_votes_", safe_name, ".tsv"))
}

# Write each subset of data to a new TSV file
for (topic in names(data_split)) {
  topic_data <- data_split[[topic]]
  file_name <- make_safe_filename(topic)
  write_tsv(topic_data, file_name)
  cat("Data for topic:", topic, "written to file:", file_name, "\n")
}