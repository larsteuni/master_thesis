# Load necessary packages
library(readr)  # For reading TSV files
library(data.table)
library(dplyr)
library(tidyr)

# Read the TSV file
file_path_1 <- "C:/Eindproject_master/df_members_v1.tsv"
file_path_2 <- "C:/Eindproject_master/df_member_votes_v1.tsv"
file_path_3 <- "C:/Eindproject_master/df_bills_v1.tsv"
data_1 <- read_tsv(file_path_1)
data_2 <- read_tsv(file_path_2)
data_3 <- read_tsv(file_path_3)

#View(data_1)
#View(data_2)
#View(data_3)

# Convert to data.frame for base R compatibility
data_1 <- as.data.frame(data_1)

# Select only the relevant columns
df_data_subset <- data_1[, c("bioname", "member_id", "party", "congress")]

# Calculate the number of unique members per congress
members_per_congress <- aggregate(member_id ~ congress, data = df_data_subset, FUN = function(x) length(unique(x)))

# Rename the column for clarity
colnames(members_per_congress)[2] <- "num_members"

# Calculate the average number of members per congress
average_members_per_congress <- mean(members_per_congress$num_members)
cat("Average number of members per congress:", average_members_per_congress, "\n")


# Find the largest congress
largest_congress <- members_per_congress[which.max(members_per_congress$num_members), ]
cat("Largest congress:", largest_congress$congress, "with", largest_congress$num_members, "members\n")

# Find the smallest congress
smallest_congress <- members_per_congress[which.min(members_per_congress$num_members), ]
cat("Smallest congress:", smallest_congress$congress, "with", smallest_congress$num_members, "members\n")


# Count the unique number of congresses each name_id appears in
name_congress_counts <- table(data_1$name_id)

# Find the top 3 name_ids with the most occurrences
top_3_name_ids <- names(head(sort(name_congress_counts, decreasing = TRUE), 3))

# Create a subset of the data for the top 3 name_ids
top_3_data <- data_1[data_1$name_id %in% top_3_name_ids, ]

# Find the corresponding bionames for the top 3 name_ids
top_3_bionames <- unique(top_3_data$bioname)

# Print the result
cat("Top 3 bionames of the name_ids found in the most different congresses:\n")
print(top_3_bionames)