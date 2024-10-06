
# Loading required libraries

# Note: Install them if they are not installed 
library(tidyverse)
library(janitor)
library(writexl)


# Load data
# Note: Change the file path to reflect where the file is located on your machine
df <- read_csv("C:/Users/tty8/Desktop/clustering_results_summary.csv")


# --> clean up column names
df <- df |> 
  janitor::clean_names() 


# Define a Mode function
Mode <- function(x) {
  ux <- unique(x[!is.na(x)])  # ignore NA values for mode calculation
  if(length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}


# df <- df %>%
#   mutate(across(.cols = where(is.numeric),
#                 .fns = ~case_when(
#                   . <= quantile(., 0.25, na.rm = TRUE) ~ "Low",
#                   . > quantile(., 0.25, na.rm = TRUE) & . <= quantile(., 0.50, na.rm = TRUE) ~ "Moderate",
#                   . > quantile(., 0.50, na.rm = TRUE) & . <= quantile(., 0.75, na.rm = TRUE) ~ "Moderately High",
#                   . > quantile(., 0.75, na.rm = TRUE) ~ "High",
#                   TRUE ~ NA_character_),
#                 .names = "percentile_{.col}")) |>
#   mutate(row_id = row_number())


df <- df %>%
  mutate(across(.cols = where(is.numeric),
                .fns = ~case_when(
                  . <= quantile(., 0.25, na.rm = TRUE) ~ "Low",
                  . > quantile(., 0.25, na.rm = TRUE) & . <= quantile(., 0.75, na.rm = TRUE) ~ "Moderate",
                  . > quantile(., 0.50, na.rm = TRUE) ~ "High",
                  TRUE ~ NA_character_),
                .names = "percentile_{.col}")) |>
  mutate(row_id = row_number())



# Calculate mode for each row's percentile columns
mode_df <- df %>%
  pivot_longer(cols = starts_with("percentile_"), names_to = "percentile", values_to = "value") %>%
  group_by(row_id) %>%
  summarize(mode_value = Mode(value), .groups = 'drop')  # Ensure groups are dropped after summarization

# Join back with the original data
df <- left_join(df, mode_df, by = "row_id")

# Optionally remove the row_id column if no longer needed
df <- select(df, -row_id)
write_xlsx(df, "C:/Users/tty8/Desktop/cluster_groupings_3.xlsx")
