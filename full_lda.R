# Load necessary library
library(data.table)
library(doParallel)

# Set the file path
file_path <- "C:/Eindproject_master/df_bills_fv.tsv"

# Read the TSV file
df_data <- fread(file_path)

# Tokenize
library(NLP)
library(tm)
library(proxy)
library(topicmodels)

# Voorbeeld: teksttokenisatie met tm-pakket
text_corpus <- Corpus(VectorSource(df_data$full_summary))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removeWords, stopwords("en"))

words_to_remove <- c("the", "and", "for", "sec", "that", "such", "from", "with", "act", 
                     "election", "speaker", "accelerated", "access", "accompany", "secretary",
                     "requires", "program", "federal", "national", "prohibits", "directs", 
                     "authorizes", "use", "specified", "states", "title", "state", "available", 
                     "programs", "including", "congress", "certain", "activities", "amends", "requirements",
                     "department", "agency", "office", "united", "year", "authority", "subtitle",
                     "service", "provide", "information", "made", "project", "services", 
                     "within")
text_corpus <- tm_map(text_corpus, removeWords, words_to_remove)

dtm <- DocumentTermMatrix(text_corpus)

# Controleer op lege documenten
empty_docs <- rowSums(as.matrix(dtm)) == 0
# Verwijder lege documenten
dtm <- dtm[!empty_docs, ]

# Set number of CPU cores to use
num_cores <- detectCores()
# Register parallel backend
registerDoParallel(cores = num_cores)
# Define function to run LDA on a subset of the data
run_lda <- function(dtm_subset, num_topics, iter, burnin) {
  lda_model <- LDA(dtm_subset, k = num_topics, method = "Gibbs", 
                   control = list(iter = iter, burnin = burnin))
  return(lda_model)
}

# Get dimensions of DTM
num_docs <- nrow(dtm)
chunk_size <- ceiling(num_docs / num_cores)

# Split the indices into chunks
chunk_indices <- split(1:num_docs, ceiling(seq_along(1:num_docs) / chunk_size))
# Parallelize LDA computation using foreach
lda_models <- foreach(i = 1:num_cores, .packages = c("topicmodels")) %dopar% {
  library(topicmodels)  # Load topicmodels inside the parallel worker
  dtm_subset <- dtm[(i-1)*floor(nrow(dtm)/num_cores) + 1:i*floor(nrow(dtm)/num_cores), ]
  lda_model <- LDA(dtm_subset, k = 10, method = "Gibbs", control = list(iter = 1000, burnin = 1000))
  lda_model
}
# Stop parallel backend
stopImplicitCluster()

# Get topics
topics <- terms(lda_models, 10)  # Get top 10 terms for each topic


# Build the LDA model
#lda_model <- LDA(dtm, k = 7)
# Get topics
#topics <- as.matrix(topics(lda_model))
# Save topics into a variable
#lda_topics <- topics
# Print topics
#print(lda_topics)
