install.packages("usethis")

usethis::edit_r_environ()
Sys.getenv("CENSUS_API_KEY")

install.packages("tidycensus")
library(tidycensus)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

df <- get_acs(
  geography = "tract",
  variables = c(
    commute_time = "B08136_001",  # aggregate travel time
    total_workers = "B08101_001", # workers 16+
    median_income = "B19013_001",
    pop_density = "B01003_001",
    transit_users = "B08301_010", # public transit commuters
    disability = "B18101_001"     # population with disability
  ),
  state = "NY",
  county = c("New York", "Kings", "Queens", "Bronx", "Richmond"),
  year = 2022,
  output = "wide"
)
head(df)
df$mean_commute <- df$commute_timeE / df$total_workersE
df$pct_transit <- df$transit_usersE / df$total_workersE
df$pct_disability <- df$disabilityE / df$pop_densityE

df_clean <- df[, c("GEOID", "NAME", "mean_commute", "median_incomeE", 
                   "pop_densityE", "pct_transit", "pct_disability")]

df2 <- get_acs(
  geography = "tract",
  variables = c(
    mean_commute = "B08135_001",
    total_workers = "B08101_001",
    median_income = "B19013_001",
    total_pop = "B01003_001",
    transit_users = "B08301_010",
    disability = "B18101_001",
    disability_count = "C18108_002"  # total with any disability
  ),
  state = "NY",
  county = c("New York", "Kings", "Queens", "Bronx", "Richmond"),
  year = 2022,
  output = "wide"
)

df2$mean_commute <- df2$mean_commuteE / df2$total_workersE
df2$pct_transit <- df2$transit_usersE / df2$total_workersE
df2$pct_disability <- df2$disability_countE / df2$total_popE
summary(df2$pct_disability)

df_model <- df2[, c("GEOID", "NAME", "mean_commute", "median_incomeE", 
                    "total_popE", "pct_transit", "pct_disability")]

df_model <- na.omit(df_model)
nrow(df_model)
summary(df_model)
cor(df_model[, c("mean_commute", "median_incomeE", "total_popE", "pct_transit", "pct_disability")])

