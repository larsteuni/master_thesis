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
file_path_2 <- "C:/Eindproject_master/df_bills_fv.tsv"
votes <- read_tsv(file_path)
bills <- read_tsv(file_path_2)


# 1. Identifying the most active voters
most_active_voters <- votes %>%
  group_by(bioname) %>%
  summarize(total_votes = n()) %>%
  arrange(desc(total_votes)) %>%
  top_n(10, total_votes)

print(most_active_voters)

# 2. Analyzing the success rate of bills by different sponsors
# Assuming each bill has a sponsor_id
bill_success_by_sponsor <- bills %>%
  group_by(sponsor_name) %>%
  summarize(
    total_bills = n(),
    passed_bills = sum(vote_result == 'Passed', na.rm = TRUE),
    success_rate = passed_bills / total_bills
  ) %>%
  arrange(desc(passed_bills))

print(bill_success_by_sponsor)

# 4. Determining the most controversial bills (those with the closest vote margins)
bill_controversy <- bills %>%
  select(bill_id, yea_count, nay_count) %>%
  mutate(vote_margin = abs(yea_count - nay_count),
         total_votes = yea_count + nay_count) %>%
  arrange(vote_margin) %>%
  top_n(-10, vote_margin)

print(bill_controversy)

# 5. Trends over time in voting patterns or bill passage rates
# Assuming 'vote_date' and 'bill_date' are the dates of votes and bills respectively

# Voting patterns over time
voting_trends <- votes %>%
  group_by(year) %>%
  summarize(total_votes = n()) %>%
  ggplot(aes(x = year, y = total_votes)) +
  geom_line() +
  geom_point() +
  labs(title = "Voting Trends Over Time", x = "Year", y = "Total Votes")

print(voting_trends)

# Bill passage rates over time
passage_trends <- bills %>%
  group_by(year) %>%
  summarize(
    total_bills = n(),
    passed_bills = sum(vote_result == 'Passed', na.rm = TRUE),
    passage_rate = passed_bills / total_bills
  ) %>%
  ggplot(aes(x = year, y = passage_rate)) +
  geom_line() +
  geom_point() +
  labs(title = "Bill Passage Rates Over Time", x = "Year", y = "Passage Rate")

print(passage_trends)