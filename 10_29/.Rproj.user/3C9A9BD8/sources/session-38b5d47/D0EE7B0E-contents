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

### Loading data ----

# Load the data sets

data <- read_excel("C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/Bangladesh_Request/Data/Project_Data_for_WorldBank_PIM_Analysis.xlsx", 
                   sheet = "Project Basic Info")

data2 <- read_excel("C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/Bangladesh_Request/Data/Project_Data_for_WorldBank_PIM_Analysis.xlsx", 
                    sheet = "Project Location")

data3 <- read_excel("C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/Bangladesh_Request/Data/Project_Data_for_WorldBank_PIM_Analysis.xlsx", 
                    sheet = "Allocation")

data4 <- read_excel("C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/Bangladesh_Request/Data/Project_Data_for_WorldBank_PIM_Analysis.xlsx", 
                    sheet = "Release")

data5<- read_excel("C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/Bangladesh_Request/Data/Project_Data_for_WorldBank_PIM_Analysis.xlsx", 
                   sheet = "Expenditure")

#Transform the data sets into a data.table

data_table <- as.data.table(data)

data_table2 <- as.data.table(data2)

data_table3 <- as.data.table(data3)

data_table4 <- as.data.table(data4)

data_table5 <- as.data.table(data5)

#View data


View(data_table)
head(data_table)
n_distinct(data_table)
nrow(data_table) # 2637 observations 
glimpse(data_table)


View(data_table2)
head(data_table2)
n_distinct(data_table2)
nrow(data_table2) # 8822 observations 
glimpse(data_table2)


View(data_table3)
head(data_table3)
n_distinct(data_table3)
nrow(data_table3) # 7035 observations 
glimpse(data_table3)


View(data_table4)
head(data_table4)
n_distinct(data_table4)
nrow(data_table4) # 5080 observations 
glimpse(data_table4)

View(data_table5)
head(data_table5)
n_distinct(data_table5)
nrow(data_table5) # 8855 observations 
glimpse(data_table5)


# Rename columns with small letters

setnames(data_table, tolower(names(data_table)))

setnames(data_table2, tolower(names(data_table2)))

setnames(data_table3, tolower(names(data_table3)))

setnames(data_table4, tolower(names(data_table4)))

setnames(data_table5, tolower(names(data_table5)))

#Check Duplicates 

### Remove duplicates based on Code

data_table <- data_table %>% distinct(code, .keep_all=TRUE) # 0 duplicates

data_table3 <- data_table3 %>% distinct(code, .keep_all=TRUE) # 2 duplicates


### Data cleaning ----


# Replace special characters columns

setnames(data_table, gsub("[^[:alnum:]_]", "_", names(data_table)))

setnames(data_table2, gsub("[^[:alnum:]_]", "_", names(data_table2)))

setnames(data_table3, gsub("[^[:alnum:]_]", "_", names(data_table3)))

setnames(data_table4, gsub("[^[:alnum:]_]", "_", names(data_table4)))

setnames(data_table5, gsub("[^[:alnum:]_]", "_", names(data_table5)))


## Adding quarter column for dates of

data_table[, year_quarter_i := paste0(year(date_of_commencement), "-Q", quarter(date_of_commencement))]

data_table[, year_quarter_f := paste0(year(date_of_completion), "-Q", quarter(date_of_completion))]

data_table4[, quarters_name := gsub("\\D", "", quarters_name)]

data_table4[, year_quarter := paste0(fiscal_year_id, "-Q", quarters_name)]

data_table5[, year_quarter := paste0(fiscal_year_id, "-Q", month)]



## Drop column call "name bangle" only first data set

data_table[, name_bangle := NULL]


## Erase the special characters in the column code

data_table[, code := gsub("-", "", code)]

data_table2[, code := gsub("-", "", code)]

data_table3[, code := gsub("-", "", code)]

data_table4[, code := gsub("-", "", code)]

data_table5[, code := gsub("-", "", code)]

## Erase the special characters in the column ministry/division

data_table[, ministry__division := gsub("[-']", "", ministry__division)] #only first data table



#Ministry Division

data_table<-data_table[, ministry :=ministry__division]


#Transform data.table to a data.frame

project_basic<- as.data.frame(data_table)

project_location<- as.data.frame(data_table2)

project_allocation<- as.data.frame(data_table3)

project_release<- as.data.frame(data_table4)

project_expenditure<- as.data.frame(data_table5)

# Save the project data 

write_dta(project_basic, file.path(data_path, "Intermediate/project_basic.dta"))

write_dta(project_location, file.path(data_path, "Intermediate/project_location.dta"))

write_dta(project_allocation, file.path(data_path, "Intermediate/project_allocation.dta"))

write_dta(project_release, file.path(data_path, "Intermediate/project_release.dta"))

write_dta(project_expenditure, file.path(data_path, "Intermediate/project_expenditure.dta"))