# Tidy Tuesday's-Monster Movies
# 01. Data processing

### Libraries

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(readxl)
library(data.table)
library(lubridate)
library(tidytuesdayR)


### Loading data ----

# Load the data sets

# Option 1: tidytuesdayR package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2024-10-29')

## OR
#tuesdata <- tidytuesdayR::tt_load(2024, week = 44)

monster_movie_genres <- tuesdata$monster_movie_genres #1291 obs

monster_movies <- tuesdata$monster_movies #630 obs

#Transform the data sets into a data.table

data_table <- as.data.table(monster_movie_genres)

data_table2 <- as.data.table(monster_movies)
  
#View data


View(data_table)
head(data_table)
n_distinct(data_table)
nrow(data_table) # 1291 observations 
glimpse(data_table)


View(data_table2)
head(data_table2)
n_distinct(data_table2)
nrow(data_table2) # 630 observations 
glimpse(data_table2)

#Check Duplicates 

### Remove duplicates based on Code

data_table <- data_table %>% distinct(tconst, .keep_all=TRUE) # 0 duplicates

data_table2 <- data_table2 %>% distinct(tconst, .keep_all=TRUE) # 0 duplicates


### Data cleaning ----


# Separating column genre in more columns in the data_table2


data_table2[, c("genre_1", "genre_2", "genre_3") := tstrsplit(genres, ",")]

#Join data sets

join_data_table <- data_table[data_table2, on = .(tconst), nomatch = 0] #630 obs

# Replace dots with underscores in column names

setnames(join_data_table, old = names(join_data_table), new = gsub("\\.", "_", names(join_data_table)))



#Transform data.table to a data.frame

data_table<- as.data.frame(data_table)

data_table2<- as.data.frame(data_table2)

join_data_table<- as.data.frame(join_data_table)


# Save the project data 

write_dta(data_table, file.path(data_path, "Data/Intermediate/monsters_genres.dta"))

write_dta(data_table2, file.path(data_path, "Data/Intermediate/monster_movies.dta"))

write_dta(join_data_table, file.path(data_path, "Data/Intermediate/join_data.dta"))

