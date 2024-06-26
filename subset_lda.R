# Load necessary library
library(data.table)

# Set the file path
file_path <- "C:/Eindproject_master/df_bills_final.tsv"

# Read the TSV file
df_data <- fread(file_path)

# Create subset of data
subset_df <- df_data[df_data$congress >= 111 & df_data$congress <= 113, ]


# Tokenize
library(NLP)
library(tm)
library(proxy)
library(topicmodels)

# Voorbeeld: teksttokenisatie met tm-pakket
text_corpus <- Corpus(VectorSource(subset_df$summary))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removeWords, stopwords("en"))

dtm <- DocumentTermMatrix(text_corpus)

words_to_remove <- c("the", "and", "for", "sec", "that", "such", "from", "with", "act", 
                     "election", "speaker", "accelerated", "access", "accompany", "secretary",
                     "requires", "program", "federal", "national", "prohibits", "directs", 
                     "authorizes", "use", "specified", "states", "title", "state", "available", 
                     "programs", "including", "congress", "certain", "activities", "amends", "requirements",
                     "department", "agency", "office", "united", "year", "authority", "subtitle",
                     "service", "provide", "information", "made", "project", "services", 
                     "within")
text_corpus <- tm_map(text_corpus, removeWords, words_to_remove)

# Controleer op lege documenten
empty_docs <- rowSums(as.matrix(dtm)) == 0
# Verwijder lege documenten
dtm <- dtm[!empty_docs, ]

num_topics <- seq(5, 12, by = 1)
# LDA Modelling
# Initialize a vector to store cosine similarity scores for each model
similarity_scores <- numeric(length(num_topics))

# Define the ground truth topic distribution (based on policy column)
policy_corpus <- Corpus(VectorSource(subset_df$policy))
policy_corpus <- tm_map(policy_corpus, content_transformer(tolower))
policy_corpus <- tm_map(policy_corpus, removePunctuation)
policy_corpus <- tm_map(policy_corpus, removeNumbers)
policy_corpus <- tm_map(policy_corpus, removeWords, stopwords("en"))

dtm_control <- DocumentTermMatrix(policy_corpus)
# Controleer op lege documenten
empty_docs <- rowSums(as.matrix(dtm_control)) == 0
# Verwijder lege documenten
dtm_control <- dtm_control[!empty_docs, ]
dtm_matrix <- as.matrix(dtm_control)
# Calculate the sum of word frequencies for each policy document
doc_word_freq <- rowSums(dtm_matrix)
# Normalize the document vectors to obtain the probability distribution of words within each document
doc_prob_distribution <- dtm_matrix / rowSums(dtm_matrix)
# Calculate the average distribution to obtain the ground truth topic distribution
ground_truth <- colMeans(doc_prob_distribution)

# Loop over different numbers of topics
for (i in seq_along(num_topics)) {
  # Build the LDA model
  lda_model <- LDA(dtm, k = num_topics[i])
  
  # Get the topic distribution for each document
  doc_topic_dist <- posterior(lda_model)$topics
  
  # Compute cosine similarity between inferred topic distributions and ground truth
  similarity <- sapply(1:nrow(doc_topic_dist), function(j) {
    cosine_similarity <- sum(doc_topic_dist[j, ] * ground_truth) / (sqrt(sum(doc_topic_dist[j, ]^2)) * sqrt(sum(ground_truth^2)))
    return(cosine_similarity)
  })
  
  # Average cosine similarity across all documents
  average_similarity <- mean(similarity)
  
  # Store the similarity score
  similarity_scores[i] <- average_similarity
}

# Plot the similarity scores for different numbers of topics
plot(num_topics, similarity_scores, type = "b", xlab = "Number of Topics", ylab = "Average Cosine Similarity", main = "LDA Model Performance")
# Haal de topwoorden voor elk onderwerp op