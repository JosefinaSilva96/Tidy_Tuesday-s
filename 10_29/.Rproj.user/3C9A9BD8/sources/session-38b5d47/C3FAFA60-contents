# Tidy Tuesdays Monster Movies
# 02. Data construction


# Load the dataset

data  <- read_dta(file.path(data_path, "Data/Intermediate/monsters_genres.dta"))

data2  <- read_dta(file.path(data_path, "Data/Intermediate/monster_movies.dta"))

data_join  <- read_dta(file.path(data_path, "Data/Intermediate/join_data.dta"))


#Transform data.frame to data.table

data_table <- as.data.table(data)

data_table2 <- as.data.table(data2)

data_table_join <- as.data.table(data_join)


#  Handle outliers ----

# customized function:
winsor_function <- function(dataset, var, min = 0.00, max = 0.95){
  var_sym <- sym(var)
  
  percentiles <- quantile(
    dataset %>% pull(!!var_sym), probs = c(min, max), na.rm = TRUE
  )
  
  min_percentile <- percentiles[1]
  max_percentile <- percentiles[2]
  
  dataset %>%
    mutate(
      !!paste0(var, "_w") := case_when(
        is.na(!!var_sym) ~ NA_real_,
        !!var_sym <= min_percentile ~ percentiles[1],
        !!var_sym >= max_percentile ~ percentiles[2],
        TRUE ~ !!var_sym
      )
    )
}


# Winsorize selected variables in the dataset
win_vars <- c("runtime_minutes", "average_rating", "num_votes")

# Apply the custom winsor_function to each variable in win_vars
for (var in win_vars) {
  data2 <- winsor_function(data2, var)
}

# Update the labels to reflect that winsorization was applied
data2 <- data2 %>%
  mutate(across(ends_with("_w"), 
                ~ labelled(.x, label = paste0(attr(.x, "label"), 
                                              " (Winsorized 0.05)"))))

# Exercise 6: Save final dataset ----

# Save the final merged data for analysis
write_dta(final_hh_data, file.path(data_path, "Final/TZA_CCT_analysis.dta"))

# Save the final secondary data for analysis
write_dta(secondary_data, file.path(data_path, "Final/TZA_amenity_analysis.dta"))