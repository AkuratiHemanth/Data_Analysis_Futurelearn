# Load necessary libraries


library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)

# Define paths
enrolments_data_path <- "data/raw/enrolments/"
arch_type_data_path <- "data/raw/arch_type/"
leaving_survey_data_path <- "data/raw/leaving_survey/"

# Define output paths
cleaned_enrolments_file <- "data/cleaned_data/enrolments.csv"
cleaned_arch_type_file <- "data/cleaned_data/arch_type.csv"
cleaned_leaving_survey_file <- "data/cleaned_data/leaving_survey.csv"

# 1. Combine and Clean Enrolment Data
# Load and combine all enrolment CSV files
enrolments_combined <- list.files(path = enrolments_data_path, pattern = "*.csv", full.names = TRUE) %>%
    lapply(read_csv, show_col_types = FALSE) %>%
    bind_rows()

# Convert specific placeholder values to NA
enrolments_combined[enrolments_combined %in% c("Unknown", "N/A", "null")] <- NA

# Filter out rows with no participation or purchase data
cleaned_enrolments <- enrolments_combined %>%
    filter(!is.na(fully_participated_at) | !is.na(purchased_statement_at))

# Save cleaned enrolments data to CSV
write_csv(cleaned_enrolments, cleaned_enrolments_file)
cat("Enrolments data combined and cleaned. Saved to:", cleaned_enrolments_file, "\n")

# 2. Combine and Clean Arch Type Data
# Load and combine all arch type CSV files with consistent column types
arch_type_combined <- list.files(path = arch_type_data_path, pattern = "*.csv", full.names = TRUE) %>%
    lapply(function(file) read_csv(file, col_types = cols(.default = "c"))) %>%  # Set all columns to character type
    bind_rows()

# Save combined arch type data to CSV
write.csv(arch_type_combined, cleaned_arch_type_file, row.names = FALSE)
cat("Arch type data combined and saved to:", cleaned_arch_type_file, "\n")

# 3. Combine and Clean Leaving Survey Data
# Load and combine all leaving survey CSV files with consistent column types
leaving_survey_combined <- list.files(path = leaving_survey_data_path, pattern = "*.csv", full.names = TRUE) %>%
    lapply(function(file) read_csv(file, col_types = cols(.default = "c"))) %>%  # Set all columns to character type
    bind_rows()

# Save combined leaving survey data to CSV
write.csv(leaving_survey_combined, cleaned_leaving_survey_file, row.names = FALSE)
cat("Leaving survey data combined and saved to:", cleaned_leaving_survey_file, "\n")

# Data Cleaning for Merged Dataset
# 4. Load cleaned data
cleaned_enrolments <- read.csv(cleaned_enrolments_file)
arch_type_combined <- read.csv(cleaned_arch_type_file)
leaving_survey_combined <- read.csv(cleaned_leaving_survey_file)

# Data Cleaning for Archetypes
archetypes_clean <- arch_type_combined %>%
    rename(learner_id = learner_id) %>%
    drop_na(archetype)

# Data Cleaning for Enrollments
enrollments_clean <- cleaned_enrolments %>%
    mutate(enrolled_at = as.Date(enrolled_at), unenrolled_at = as.Date(unenrolled_at)) %>%
    mutate(fully_participated = !is.na(fully_participated_at))

# Data Cleaning for Leaving Survey
leaving_survey_clean <- leaving_survey_combined %>%
    rename(learner_id = learner_id) %>%
    mutate(left_at = as.Date(left_at),
           last_completed_step_at = as.Date(last_completed_step_at),
           early_leaver = ifelse(last_completed_week_number <= 3, TRUE, FALSE)) # Assuming course duration > 9 weeks

# Merge Data
merged_data <- enrollments_clean %>%
    left_join(archetypes_clean, by = "learner_id") %>%
    left_join(leaving_survey_clean, by = "learner_id")

# Save the final merged data
save(merged_data, file = "cache/merged_data.RData")
cat("Merged data saved to cache/merged_data.RData\n")
