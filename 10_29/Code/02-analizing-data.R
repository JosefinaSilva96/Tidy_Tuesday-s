# # Tidy Tuesday's-Monster Movies

# 02. Data Analysis

# Libraries
library(haven)
library(dplyr)
library(modelsummary)
library(stargazer)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

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

#Graph number of genres per year

pastel_colors <- c("#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", "#BAE1FF",
                   "#FFABAB", "#FFC3A0", "#FF677D", "#D4A5A5", "#392F5A",
                   "#B9FBC0", "#C1C8E4", "#FFC4D6", "#F6B93B", "#B1E1FF",
                   "#A6D8D4", "#FFE156", "#D3C6E5", "#B1C3E7", "#F0A500",
                   "#F9AFAF", "#A6D7C0")  # Adding two more colors
# Summarize the number of movies by year and genre

summary_data <- data_table[, .(num_movies = .N), by = .(year, genre_1)] 

# Create the plot with pastel colors
p1 <- ggplot(summary_data, aes(x = year, y = num_movies, color = genre_1)) +
  geom_point(size = 3) +        # Use points for each year
  theme_minimal() +             # Clean theme
  labs(title = "Number of Movies by Genre Across Years",
       x = "Year",
       y = "Number of Movies",
       color = "Genre") +
  scale_color_manual(values = pastel_colors)  # Using custom pastel colors
  
# Print the plot
print(p1)
