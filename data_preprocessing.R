# Load libraries
library(tidyverse)

# Read and rename datasets
df1 <- read_csv("data/BOPTEXP.csv") %>% 
  rename(date = observation_date, exports_of_gs = BOPTEXP)

df2 <- read_csv("data/BOPTIMP.csv") %>% 
  rename(date = observation_date, imports_of_gs = BOPTIMP)

df3 <- read_csv("data/brent_spot_prices.csv") %>%
  rename(date = Date)

df4 <- read_csv("data/BUSINV.csv") %>%
  rename(date = observation_date, total_business_inventories = BUSINV)

df5 <- read_csv("data/CE16OV.csv") %>%
  rename(date = observation_date, civilian_employment = CE16OV)

df6 <- read_csv("data/CLF16OV.csv") %>%
  rename(date = observation_date, civilian_labor_force = CLF16OV)

df7 <- read_csv("data/CPIAUCSL.csv") %>%
  rename(date = observation_date, consumer_price_index = CPIAUCSL)

df8 <- read_csv("data/DFF.csv") %>%
  rename(date = observation_date, federal_funds_rate = DFF)

df9 <- read_csv("data/DPCERAM1M225NBEA.csv") %>%
  rename(date = observation_date, real_personal_consumption_expenditures = DPCERAM1M225NBEA)

df10 <- read_csv("data/OPEC_crude_oil_capacity.csv") %>%
  rename(date = period, opec_crude_oil_capacity = crude_oil_capacity)

df11 <- read_csv("data/OPEC_crude_oil_production.csv") %>%
  rename(date = period, opec_crude_oil_production = crude_oil_production)

df12 <- read_csv("data/PPIACO.csv") %>%
  rename(date = observation_date, producer_price_index = PPIACO)

df13 <- read_csv("data/RPI_PC1.csv") %>%
  rename(date = observation_date, real_personal_income = RPI_PC1)

df14 <- read_csv("data/SP500_dividend_yield.csv") %>%
  rename(date = Date, sp500_dividend_yield = Dividend_Yield)

df15 <- read_csv("data/SP500_PE_ratio.csv") %>%
  rename(date = Date, sp500_pe_ratio = PE_ratio)

df16 <- read_csv("data/TOTCI.csv") %>%
  rename(date = observation_date, commercial_and_industrial_loans = TOTCI)

df17 <- read_csv("data/UMCSENT.csv") %>%
  rename(date = observation_date, consumer_sentiment_index = UMCSENT)

df18 <- read_csv("data/UNRATE.csv") %>%
  rename(date = observation_date, unemployment_rate = UNRATE)

df19 <- read_csv("data/USD_index.csv") %>%
  rename(usd_index = US_dollar_index)

df20 <- read_csv("data/USREC.csv") %>%
  rename(date = observation_date, us_recession_indicator = USREC)

df21 <- read_csv("data/VIX_index.csv") %>%
  rename(vix_index = VIX)

# Combine all dataframes into a list
dfs <- list(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10,
            df11, df12, df13, df14, df15, df16, df17, df18, df19, df20, df21)

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
