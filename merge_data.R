# Load libraries
library(tidyverse)

# Read and rename datasets
df1 <- read_csv("data/BOPTEXP.csv") %>% 
  rename(date = observation_date, exports_of_gs = BOPTEXP)

df2 <- read_csv("data/BOPTIMP.csv") %>% 
  rename(date = observation_date, imports_of_gs = BOPTIMP)

df3 <- read_csv("data/brent_spot_prices.csv") %>%
  rename(date = Date)

df4 <- read_csv("data/OPEC_crude_oil_capacity.csv") %>%
  rename(date = period, opec_crude_oil_capacity = crude_oil_capacity)

df5 <- read_csv("data/OPEC_crude_oil_production.csv") %>%
  rename(date = period, opec_crude_oil_production = crude_oil_production)

df6 <- read_csv("data/PPIACO.csv") %>%
  rename(date = observation_date, producer_price_index = PPIACO)

df7 <- read_csv("data/TOTCI.csv") %>%
  rename(date = observation_date, commercial_and_industrial_loans = TOTCI)

df8 <- read_csv("data/USD_index.csv") %>%
  rename(usd_index = US_dollar_index)

df9 <- read_csv("data/USREC.csv") %>%
  rename(date = observation_date, us_recession_indicator = USREC)

df10 <- read_csv("data/IGREA.csv") %>%
  rename(date = observation_date, global_real_economic_activity_index = IGREA)

df11 <- read_csv("data/fred_data_clean.csv")

# Combine all dataframes into a list
dfs <- list(df11, df1, df2, df3, df4, df5, df6, df7, df8, df9, df10)

# Convert all 'date' columns to Date type
dfs <- lapply(dfs, function(x) {
  x$date <- as.Date(x$date)
  x
})

# Merge all by 'date'
df_merged <- reduce(dfs, function(x, y) inner_join(x, y, by = "date"))

# Sort by date
df_merged <- df_merged %>% arrange(date)

# Print head and count rows
print(head(df_merged))
cat("Number of rows:", nrow(df_merged), "\n")

# Save merged dataset
write_csv(df_merged, "data/merged_df.csv")
