# Load necessary packages
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(gridExtra)

# Read the CSV file into a dataframe
file_path <- "C:/Eindproject_master/bimodality_coefficients_bootstrap.csv"
bimodality_data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

# Convert the topic_congress column to character type if it is not already
bimodality_data$topic_congress <- as.character(bimodality_data$topic_congress)

# Extract the congress number from the topic_congress column and create a new column called congress
bimodality_data <- bimodality_data %>%
  mutate(congress = as.numeric(sub(".*_", "", topic_congress)))

# Check the structure of the dataframe to ensure the new column is added correctly
str(bimodality_data)

# Convert the congress column to numeric
bimodality_data$congress <- as.numeric(bimodality_data$congress)

# Calculate the threshold for red color in the heatmap
bimodality_data <- bimodality_data %>%
  mutate(threshold = 0.555 + bootstrap_error)

# Melt the dataframe into a long format suitable for ggplot2
bimodality_long <- bimodality_data %>%
  select(topic_congress, bc_value, congress, bootstrap_error, threshold) %>%
  separate(topic_congress, into = c("topic", "congress"), sep = "_") %>%
  mutate(congress = as.numeric(congress))

# Rename columns for better readability
colnames(bimodality_long) <- c("topic", "congress", "bimodality_coefficient", "bootstrap_error", "threshold")

# Create the heatmap
helper <- bimodality_long %>%
  mutate(fill_color = case_when(
    is.na(bimodality_coefficient) ~ "grey",
    bimodality_coefficient < threshold ~ "lightblue",
    TRUE ~ "brown"
  ))

heatmap_plot <- ggplot(helper, aes(x = congress, y = topic, fill = fill_color)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("lightblue" = "lightblue", "brown" = "brown", "grey" = "grey50"),
                    labels = c("lightblue" = "Non-polarized", "brown" = "Polarized", "grey" = "No information"),
                    name = "Bimodality Coefficient") +
  labs(title = "Bimodality Coefficients Across Topics and Congresses",
       x = "Congress",
       y = "Topic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1/2)  # Adjust aspect ratio for wider plot

# Display the heatmap
print(heatmap_plot)

# Create the line plot with standard error bars
line_plot <- ggplot(bimodality_long, aes(x = congress, y = bimodality_coefficient, color = topic, group = topic)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = bimodality_coefficient - bootstrap_error, ymax = bimodality_coefficient + bootstrap_error), width = 0.2) +
  labs(title = "Bimodality Coefficients Across Topics and Congresses",
       x = "Congress",
       y = "Bimodality Coefficient",
       color = "Topic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")

# Display the line plot
print(line_plot)

# Create a directory to save the plots if it doesn't exist
output_dir <- "C:/Eindproject_master/plots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Function to create and save the plot for each topic
create_plot <- function(topic_data, topic_name) {
  p <- ggplot(topic_data, aes(x = congress, y = bimodality_coefficient)) +
    geom_line(color = "lightgray") +
    geom_smooth(method = "lm", color = "black") +
    geom_errorbar(aes(ymin = bimodality_coefficient - bootstrap_error, ymax = bimodality_coefficient + bootstrap_error), width = 0.2) +
    labs(title = paste("Bimodality Coefficient for", topic_name),
         x = "Congress",
         y = "Bimodality Coefficient") +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = paste0(output_dir, "/", topic_name, "_bimodality_coefficient.png"), plot = p, width = 10, height = 6)
  
  # Print the plot to the console (optional)
  print(p)
}

# Loop through each topic and create the plot
unique_topics <- unique(bimodality_long$topic)
for (topic in unique_topics) {
  topic_data <- filter(bimodality_long, topic == !!topic)
  create_plot(topic_data, topic)
}

# Calculate the fitted regression values at Congress 93 and Congress 118 for each topic, and add MSE
calculate_fitted_values <- function(data) {
  lm_fit <- lm(bimodality_coefficient ~ congress, data = data)
  congress_93 <- predict(lm_fit, newdata = data.frame(congress = 93))
  congress_118 <- predict(lm_fit, newdata = data.frame(congress = 118))
  delta <- congress_118 - congress_93
  mse <- mean(lm_fit$residuals^2)
  return(list(start = congress_93, end = congress_118, delta = delta, mse = mse))
}

fitted_values_table <- data.frame(
  topic = character(),
  start_value = numeric(),
  end_value = numeric(),
  delta = numeric(),
  mse = numeric(),
  stringsAsFactors = FALSE
)

for (topic in unique_topics) {
  topic_data <- filter(bimodality_long, topic == !!topic)
  fitted_values <- calculate_fitted_values(topic_data)
  fitted_values_table <- rbind(fitted_values_table, data.frame(topic = topic, start_value = fitted_values$start, end_value = fitted_values$end, delta = fitted_values$delta, mse = fitted_values$mse))
}
# Save the fitted values table to a CSV file
output_file_path <- "C:/Eindproject_master/bimodality_fitted_values.csv"
write.csv(fitted_values_table, file = output_file_path, row.names = FALSE)

# Convert the fitted values table to a graphical table using gridExtra
table_grob <- tableGrob(fitted_values_table)

# Save the table as an image
ggsave(filename = "C:/Eindproject_master/result visualizations/bimodality_fitted_values_table_with_MSE.png", plot = table_grob, width = 10, height = 6)

# Print the fitted_values_table dataframe to check the results
print(fitted_values_table)