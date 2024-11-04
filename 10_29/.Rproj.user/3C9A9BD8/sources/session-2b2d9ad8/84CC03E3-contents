# # Tidy Tuesday's-Monster Movies

# 02. Data Analysis

# Libraries
library(haven)
library(dplyr)
library(modelsummary)
library(stargazer)
library(ggplot2)
library(tidyr)

# Load data 

# Load the data set

data_table   <- read_dta(file.path(data_path, "Data/Intermediate/monster_movies.dta"))

data_table2   <- read_dta(file.path(data_path, "Data/Intermediate/monsters_genres.dta"))

data_table3   <- read_dta(file.path(data_path, "Data/Intermediate/join_data.dta"))


#Transform data sets to data.table 


data_table <- as.data.table(data_table)

data_table2 <- as.data.table(data_table2)

data_table3 <- as.data.table(data_table3)

# Graphs ----

# Bar graph number for genres

output_path_graphs <- file.path("C:/WBG/GitHub/Tidy_Tuesday-s/10_29/Outputs/graph_genres.png")

data_table_plot <- data_table %>%
  ggplot(aes(x = genre_1)) + 
  geom_bar(fill = "#B3CDE3") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +  # Add count labels
  labs(title = "Genres Distribution", x = "Genres", y = "Count") +
  theme_minimal()

# Print the plot
print(data_table_plot)

# Save the plot
ggsave(output_path_graphs, plot = data_table_plot, width = 8, height = 6)


# Bar graph number for number of votes vs rating per genre

output_path_graphs <- file.path("C:/WBG/GitHub/Tidy_Tuesday-s/10_29/Outputs/graph_genres_votes_rating.png")

p <- data_table[, list(
  average_rating = mean(average_rating, na.rm = TRUE),  # Mean average_rating
  num_votes = mean(num_votes, na.rm = TRUE)  # Mean num_votes
), by = genre_1] %>%
  ggplot(aes(x = average_rating, y = num_votes, color = genre_1)) +  # Use genre_1 for color
  geom_point(size = 5) +  # Adjust the size of the points if needed
  theme_bw() +
  labs(title = "Average Rating vs Number of Votes per Genre Movie",
       x = "Average Rating",
       y = "Number of Votes per Genre Movie",
       color = "Genre")


# Print the plot
print(p)

# Save the plot
ggsave(output_path_graphs, plot = p, width = 8, height = 6)


# Bar graph number of projects for projects types

output_path_graphs <- file.path("C:/WBG/GitHub/Tidy_Tuesday-s/10_29/Outputs/graph_genres_numvotes.png")

# Filter and plot using ggplot2
p <- data_table %>%  
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  geom_point() +
  theme_bw()

# Print the plot
print(p)

# Save the plot
ggsave(output_path_graphs, plot = data_table_plot, width = 8, height = 6)


# Graph for projects and time

output_path_graphs <- file.path("C:/WBG/GitHub/Bangladesh/Outputs/graph_project_time.png")

data_table3_plot <- data_table3 %>%
  ggplot(aes(x = fiscal_year_id)) + 
  geom_bar(fill = "#B3CDE3") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +  # Add count labels
  labs(title = "Project Time Distribution", x = "Fiscal Year", y = "Count") +
  theme_minimal()


# Save the plot
ggsave(output_path_graphs, plot = data_table3_plot, width = 8, height = 6)