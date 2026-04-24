#install.packages("usethis")

#usethis::edit_r_environ()
Sys.getenv("CENSUS_API_KEY")

#install.packages("tidycensus")
library(tidycensus)
library(MASS)
library(faraway)

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
plot(df_model$median_incomeE, df_model$mean_commute)



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

# find largest studentized residuals
stud <- rstudent(lrinteraction)
stud[which.max(abs(stud))]

#Compute Bonferroni critical value: qt(1-alpha/(2n), n-p-1)
p <- length (coefficients(lrinteraction))
n <- nrow(model.matrix(lrinteraction))

qt(1-.05/(n*2),n-p-1)
which(abs(stud) > qt(1-.05/(n*2),n-p-1))
# no outliers detected after bonferroni correction

#cooks distance
cooks <- cooks.distance(lrinteraction)
h <- hatvalues(lrinteraction)
n <- nrow(df_model)
high_cooks <- cooks > quantile(cooks, 0.95)
summary(high_cooks)

plot(fitted(lrinteraction), df_model$mean_commute,
     xlab = "Fitted values",
     ylab = "Mean commute",
     col = "blue",
     pch = 19,
     xlim = c(0, 65),
     ylim = c(0, 65),
     asp = 1,
     xaxs = "i",
     yaxs = "i",
     main="Actual vs. Predicted Mean Commute Times (N=561), Adj. R-sq = 0.5152")
abline(a = 0, b = 1, col = "black", lwd = 2, lty = 2)
labels <- paste0(
  "Lev=", round(h[high_cooks], 3),
  "  D=", round(cooks[high_cooks], 3)
)
text(fitted(lrinteraction)[high_cooks],
     df_model$mean_commute[high_cooks] + 0.5,  # optional vertical offset
     labels = labels,
     pos = 4,
     cex = 0.7,
     col = "red")
high_meancommute <- names(high_cooks)[high_cooks]
high_meancommute
sort(cooks, decreasing = TRUE)[1:10]

halfnorm(cooks, nlab = 4, ylab = "Cook's distances")
# 0 points have a cooks distance above 0.5

#confidence intervals
confint(lrinteraction)
summary(lrinteraction)

#prediction intervals
new_data <- data.frame(
  median_incomeE = mean(df_model$median_incomeE),
  total_popE = mean(df_model$total_popE),
  pct_transit = mean(df_model$pct_transit),
  pct_disability = mean(df_model$pct_disability),
  median_ageE = mean(df_model$median_ageE)
)
predict(lrinteraction, new_data, interval = "confidence")
predict(lrinteraction, new_data, interval = "prediction")
summary(new_data)

