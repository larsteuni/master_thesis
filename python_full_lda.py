import pandas as pd
import numpy as np
import nltk
from sklearn.decomposition import LatentDirichletAllocation as cuLDA
from sklearn.feature_extraction.text import TfidfVectorizer
from gensim.models import CoherenceModel
from gensim.corpora import Dictionary
from gensim.models.ldamodel import LdaModel
import re
from nltk.corpus import stopwords

# Preprocess the text
def preprocess_text(text):
    nltk.download('stopwords')

    stop_words = set(stopwords.words('english'))
    additional_stop_words = set([
        "the", "and", "for", "sec", "that", "such", "from", "with", "act", 
        "election", "speaker", "accelerated", "access", "accompany", "secretary",
        "requires", "program", "federal", "national", "prohibits", "directs", 
        "authorizes", "use", "specified", "states", "title", "state", "available", 
        "programs", "including", "congress", "certain", "activities", "amends", "requirements",
        "department", "agency", "office", "united", "year", "authority", "subtitle",
        "service", "provide", "information", "made", "project", "services", 
        "within", "appropriations"
    ])
    stop_words = stop_words.union(additional_stop_words)

    text = text.lower()  # Convert to lowercase
    text = re.sub(r'\d+', '', text)  # Remove numbers
    text = re.sub(r'[^\w\s]', '', text)  # Remove punctuation
    text = ' '.join([word for word in text.split() if word not in stop_words])  # Remove stopwords
    return text

if __name__ == "__main__":
    # Set the file path
    file_path = "C:/Eindproject_master/df_bills_fv.tsv"

    # Read the TSV file into a Pandas DataFrame
    df_data = pd.read_csv(file_path, sep='\t')

    # Preprocess the text
    df_data['processed_summary'] = df_data['full_summary'].apply(lambda x: preprocess_text(x) if pd.notnull(x) else "")

    # Use TF-IDF to vectorize the text
    vectorizer = TfidfVectorizer(max_features=5000)
    X = vectorizer.fit_transform(df_data['processed_summary'])

    # Run LDA with scikit-learn
    num_topics = 7
    lda = cuLDA(n_components=num_topics, max_iter=1000, verbose=1)
    lda.fit(X)

    # Extract the top 10 terms for each topic
    terms = vectorizer.get_feature_names_out()
    topics = lda.components_.argsort(axis=1)[:, :-11:-1] #also used -20

    # Display the topics with the top 10 terms
    topic_terms = []
    for topic in topics:
        topic_terms.append([terms[i] for i in topic])

    topic_terms_df = pd.DataFrame(topic_terms, columns=[f'Term{i+1}' for i in range(10)]) #also used 19
    print(topic_terms_df)

    # Get the topic distribution for each document
    topic_distribution = lda.transform(X)

    # Assign each document to the topic with the highest probability
    df_data['topic'] = topic_distribution.argmax(axis=1)

    # Now we use gensim for coherence score
    texts = [text.split() for text in df_data['processed_summary']]
    dictionary = Dictionary(texts)
    corpus = [dictionary.doc2bow(text) for text in texts]

    gensim_lda = LdaModel(corpus=corpus, id2word=dictionary, num_topics=num_topics, passes=10)
    coherence_model_lda = CoherenceModel(model=gensim_lda, texts=texts, dictionary=dictionary, coherence='c_v')
    coherence_scores = coherence_model_lda.get_coherence_per_topic()

    print("Coherence Scores: ", coherence_scores)

    # Assign each document to the topic with the highest coherence score
    def assign_topic_with_coherence(doc_topic_dist, coherence_scores):
        weighted_scores = doc_topic_dist * coherence_scores
        return np.argmax(weighted_scores)

    df_data['topic_with_coherence'] = df_data.apply(
        lambda row: assign_topic_with_coherence(topic_distribution[row.name], coherence_scores), axis=1
    )

    # Save or display the DataFrame with the new topic column
    print(df_data.head())
    df_data.to_csv("C:/Eindproject_master/df_bills_fv_with_topics_3.csv", sep='\t', index=False)