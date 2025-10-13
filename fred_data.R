library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# Read the main FRED dataset
fred_data <- read_csv("data/fred_data.csv")

# Remove the "Transform" row
fred_data <- fred_data %>%
  filter(!grepl("Transform", sasdate, ignore.case = TRUE))

# Rename 'sasdate' to 'date'
fred_data <- fred_data %>%
  rename(date = sasdate)

# Convert date to proper Date format (YYYY-MM-DD)
fred_data <- fred_data %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

# Filter for date range 1993-01-01 to 2025-06-01 ---
fred_data <- fred_data %>%
  filter(date >= as.Date("1993-01-01") & date <= as.Date("2025-06-01"))

# Read the appendix with mappings
appendix <- read_csv("data/fred_md_updated_appendix.csv")

# Create rename mapping
rename_map <- appendix %>%
  mutate(
    fred = toupper(fred), # Ensure FRED codes are uppercase for matching
    clean_name = description %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:][:space:]]+", "_")
  ) %>%
  select(fred, clean_name)

# Apply renaming
names(fred_data) <- ifelse(names(fred_data) == "date",
                           "date",
                           toupper(names(fred_data))) # Ensure all columns except date are uppercase for matching
valid_map <- rename_map %>%
  filter(fred %in% names(fred_data))

fred_data <- fred_data %>%
  rename_at(vars(valid_map$fred), ~ valid_map$clean_name[match(., valid_map$fred)])

# Save cleaned version
write_csv(fred_data, "data/fred_data_clean.csv")
