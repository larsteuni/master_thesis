library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(gridExtra)

# Read the CSV file into a dataframe
file_path <- "C:/Eindproject_master/bimodality_coefficients.csv"
bimodality_data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, row.names = 1)

# Print the dataframe to check its structure
print(bimodality_data)

# Convert the dataframe to a matrix
bimodality_matrix <- as.matrix(bimodality_data)

# Melt the matrix into a long format dataframe suitable for ggplot2
bimodality_long <- melt(bimodality_matrix)

# Rename columns for better readability
colnames(bimodality_long) <- c("topic", "congress", "bimodality_coefficient")

# Create the heatmap ########################################################################### heatmap with all topics
# Create a new column to categorize the bimodality coefficient values
helper <- bimodality_long %>%
  mutate(fill_color = case_when(
    is.na(bimodality_coefficient) ~ "grey",
    bimodality_coefficient < 0.555 ~ "green",
    TRUE ~ "red"
  ))

heatmap_plot <- ggplot(helper, aes(x = congress, y = topic, fill = fill_color)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("green" = "green", "red" = "red", "grey" = "grey50"),
                    labels = c("green" = "Non-polarized", "red" = "Polarized", "grey" = "No information"),
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

############################################################################################### graph with all topics
# Convert the congress column to numeric
bimodality_long$congress <- as.numeric(bimodality_long$congress)

# Create the line plot
line_plot <- ggplot(bimodality_long, aes(x = congress, y = bimodality_coefficient, color = topic, group = topic)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
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

############################################################################################## graphs per topic
# Convert 'congress' column to numeric
bimodality_long$congress <- as.numeric(as.character(bimodality_long$congress))

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
    labs(title = paste("Bimodality Coefficient for", topic_name),
         x = "Congress",
         y = "Bimodality Coefficient") +
    scale_x_continuous(breaks = 1:length(colnames(bimodality_matrix)), labels = colnames(bimodality_matrix)) +
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

############################################################################################## table for topic deltas
# Calculate the fitted regression values at Congress 93 and Congress 118 for each topic, and add MSE
calculate_fitted_values <- function(data) {
  lm_fit <- lm(bimodality_coefficient ~ congress, data = data)
  congress_93 <- predict(lm_fit, newdata = data.frame(congress = 1))
  congress_118 <- predict(lm_fit, newdata = data.frame(congress = 26))
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