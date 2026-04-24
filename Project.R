#install.packages("usethis")

usethis::edit_r_environ()
Sys.getenv("CENSUS_API_KEY")

#install.packages("tidycensus")
library(tidycensus)
library(MASS)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

df <- get_acs(
  geography = "tract",
  variables = c(
    aggregate_commute = "B08135_001",
    total_workers = "B08101_001",
    median_income = "B19013_001",
    total_pop = "B01003_001",
    transit_users = "B08301_010",
    disability_count = "C18108_002",  # total with any disability
    median_rent = "B25031_001",
    median_age = "B06002_001"
  ),
  state = "NY",
  county = c("New York", "Kings", "Queens", "Bronx", "Richmond"),
  year = 2022,
  output = "wide"
)

df$mean_commute <- df$aggregate_commuteE / df$total_workersE
df$pct_transit <- df$transit_usersE / df$total_workersE
df$pct_disability <- df$disability_countE / df$total_popE
summary(df$pct_disability)

df_model <- df[, c("GEOID", "NAME", "mean_commute", "median_incomeE", 
                    "total_popE", "pct_transit", "pct_disability","median_ageE")]

df_model <- na.omit(df_model)
nrow(df_model)
summary(df_model)
cor(df_model[, c("mean_commute", "median_incomeE", "total_popE", "pct_transit", "pct_disability", 'median_ageE')])

cor_matrix <- cor(df_model[, c("mean_commute", "median_incomeE", 
                               "total_popE", "pct_transit", "pct_disability")])
round(cor_matrix, 2)

# graph all variables
hist(df_model$mean_commute, xlab="Mean commute", main = "Histogram of mean commute")
hist(df_model$median_incomeE, xlab="Median income", main = "Histogram of median income")
hist(df_model$total_popE, xlab="total population", main = "Histogram of total population")
hist(df_model$pct_transit, xlab="Percent transit", main = "Histogram of percent transit")
hist(df_model$pct_disability, xlab="Percent disability", main = 'Histogram of percent disability')
hist(df_model$median_ageE, xlab="Median age", main = 'Histogram of median age')

# simple linear regression
lmdisability <- lm(mean_commute ~ pct_disability, data = df_model)
summary(lmdisability)
plot(df_model$pct_disability, df_model$mean_commute,
     xlab = "Percent Disability",
     ylab = "Mean Commute Time (minutes)",
     main = "Commute Time vs Disability Rate")
abline(lmdisability, col = "red")


#multivariable linear regression
lr <- lm(mean_commute ~ median_incomeE + total_popE + pct_transit + pct_disability + median_ageE, data = df_model)
summary(lr)
plot(fitted(lr), rstandard(lr), xlab = "Fitted",ylab = "Standardized Residuals", col = "blue")  
abline(h=0, col = "red") 
# errors are heteroskedastic - transform

par (mfrow = c (1,2)) 
qqnorm(rstandard(lr), main = "")  
abline(0,1, col = "red")  
hist (rstandard(lr), main = "", xlab = "Standardized Residuals")
par (mfrow = c (1,1))
# qq plot looks good

obj <- boxcox(lr, plotit = TRUE)
obj$x[which.max(obj$y)]
lrTrans <- lm(mean_commute^(0.5) ~  median_incomeE + total_popE + pct_transit + pct_disability, data = df_model) # Fit a new regression​
summary(lrTrans)
plot(fitted(lrTrans), rstandard(lrTrans), xlab = "Fitted",ylab = "Standardized Residuals", col = "blue")  
abline(h=0, col = "red")
# qqplot and standardized residuals
par (mfrow = c (1,2)) 
qqnorm(rstandard(lrTrans), main = "")  
abline(0,1, col = "red")  
hist (rstandard(lrTrans), main = "", xlab = "Standardized Residuals")
par (mfrow = c (1,1))
# qq plot is worse
# transformation is not effective

#interaction term
lrinteraction <- lm(mean_commute ~ median_incomeE + total_popE + pct_transit + pct_disability + median_ageE + median_ageE*pct_disability, data = df_model)
summary(lrinteraction)
plot(fitted(lrinteraction), rstandard(lrinteraction), xlab = "Fitted",ylab = "Standardized Residuals", col = "blue")  
abline(h=0, col = "red") 

AIC(lr, lrinteraction)
BIC(lr, lrinteraction)
# both AIC and BIC agree that the model with the interaction is better

rstandard(lrinteraction)
#19, 367

#find large leverages
hatv <- hatvalues(lrinteraction)
head(hatv)  
# listing large leverages:
hatv [hatv > 2 * mean (hatv)]
