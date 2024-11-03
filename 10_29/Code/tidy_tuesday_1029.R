###Tidy Tuesday 10/29 
#Created by: Josefina Silva- World Bank 


### Libraries
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(DT)
library(maps)
library(mapdata)
library(leaflet)
library(DT)
library(rnaturalearth)
library(sf)
library(tidytuesdayR)


### INITIAL COMMANDS ----

#Load data set 

tuesdata <- tidytuesdayR::tt_load('2024-10-29')
#tuesdata <- tidytuesdayR::tt_load(2024, week = 44) #option 2 

monster_movie_genres <- tuesdata$monster_movie_genres
monster_movies <- tuesdata$monster_movies

#Transform the data set into a data.table

data_table <- as.data.table(monster_movies)
data_table2 <- as.data.table(monster_movie_genres)


### EXPLORE THE DATA ----




















