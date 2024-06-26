import pandas as pd

# Load the CSV file
input_csv_file = "C:/Eindproject_master/df_bills_fv_with_topics_2.csv"
output_csv_file = "C:/Eindproject_master/df_bills_fv_with_topics_2.csv"

df = pd.read_csv(input_csv_file)

# Mapping dictionary
category_mapping = {
    0: 'foreign policy',
    1: 'healthcare & education',
    2: 'lawmaking',
    3: 'military',
    4: 'house operations',
    5: 'energy & resources',
    6: 'housing & agriculture'
}

# Apply the mapping to the specified column
df['topic'] = df['topic'].map(category_mapping)

# Save the DataFrame as a TSV file
df.to_csv(output_csv_file, sep='\t', index=False)