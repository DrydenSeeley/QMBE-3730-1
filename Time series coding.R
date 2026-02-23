sales <- read_excel("/Users/drydenseeley1/Desktop/Sales.xlsx")


sales <- c( 
  17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 20, 20, 17, 24, 21, 22, 17, 24, 23, 26, 23, 23, 24, 20, 20, 15, 20, 17 
  ) 

# Create a vector of week numbers 
weeks <- 1:length(sales) 

# Check the length (should be 29) 
length(sales)

library(zoo) 
# 3-week moving average: each value is the mean of the previous 3 weeks 
ma3_raw <- rollmean(sales, k = 3, align = "right") 

# 4-week moving average: mean of the previous 4 weeks 
ma4_raw <- rollmean(sales, k = 4, align = "right") 

# 5-week moving average: mean of the previous 5 weeks 
ma5_raw <- rollmean(sales, k = 5, align = "right") 

# Pad with NA at the beginning so that the vectors line up with weeks 1–29 
ma3 <- c(rep(NA, 3 - 1), ma3_raw) # NA for weeks 1–2, then MA(3) from week 3 
ma4 <- c(rep(NA, 4 - 1), ma4_raw) # NA for weeks 1–3, then MA(4) from week 4 
ma5 <- c(rep(NA, 5 - 1), ma5_raw) # NA for weeks 1–4, then MA(5) from week 5 

# Combine into a data frame to see the moving averages by week 
ma_df <- data.frame( 
  Week = weeks, 
  Sales = sales, 
  MA3 = ma3, 
  MA4 = ma4, 
  MA5 = ma5
)

# View the moving averages table 
ma_df

#B
# Set the smoothing parameter alpha 
alpha_fixed <- 0.5 

# Initialize a vector to store the exponential smoothing forecasts 
es_fixed <- numeric(length(sales)) 

# Common initialization: set the first forecast equal to the first observation 
es_fixed[1] <- sales[1] 

# Recursively compute exponential smoothing forecasts 
for (t in 2:length(sales)) { 
  # F_t = alpha * Y_{t-1} + (1 - alpha) * F_{t-1} 
  es_fixed[t] <- alpha_fixed * sales[t - 1] + (1 - alpha_fixed) * es_fixed[t - 1] } 

# Put the exponential smoothing results into a data frame 
es_df <- data.frame( 
  Week = weeks, 
  Sales = sales, 
  ES_alpha_0.5 = es_fixed ) 

# View the exponential smoothing forecasts 
es_df
  
#C
# Define helper functions for error metrics 
mse <- function(actual, forecast) { 
  # Mean Squared Error 
  mean((actual - forecast)^2, na.rm = TRUE) } 

mae <- function(actual, forecast) { 
  # Mean Absolute Error 
  mean(abs(actual - forecast), na.rm = TRUE) } 

mape <- function(actual, forecast) { 
  # Mean Absolute Percentage Error (in percent) 
  mean(abs((actual - forecast) / actual), na.rm = TRUE) * 100 } 

# For moving averages, forecasts start only after enough data exist. 
# We align actual and forecast by dropping the initial NA values. 

# 3-week MA errors (weeks 3–29) 
actual_ma3 <- sales[3:length(sales)] # actual sales from week 3 onward 
forecast_ma3 <- ma3[3:length(ma3)] # corresponding 3-week MA forecasts 
mse_ma3 <- mse(actual_ma3, forecast_ma3) 
mae_ma3 <- mae(actual_ma3, forecast_ma3) 
mape_ma3 <- mape(actual_ma3, forecast_ma3)

# 4-week MA errors (weeks 4–29) 
actual_ma4 <- sales[4:length(sales)] # actual sales from week 4 onward 
forecast_ma4 <- ma4[4:length(ma4)] # corresponding 4-week MA forecasts 
mse_ma4 <- mse(actual_ma4, forecast_ma4) 
mae_ma4 <- mae(actual_ma4, forecast_ma4) 
mape_ma4 <- mape(actual_ma4, forecast_ma4) 

# 5-week MA errors (weeks 5–29) 
actual_ma5 <- sales[5:length(sales)] # actual sales from week 5 onward 
forecast_ma5 <- ma5[5:length(ma5)] # corresponding 5-week MA forecasts 
mse_ma5 <- mse(actual_ma5, forecast_ma5) 
mae_ma5 <- mae(actual_ma5, forecast_ma5) 
mape_ma5 <- mape(actual_ma5, forecast_ma5) 

# Exponential smoothing with alpha = 0.5 
# We usually evaluate errors from t = 2 onward (first forecast is initialization) 
actual_es_fixed <- sales[2:length(sales)] # actual sales from week 2 onward 
forecast_es_fixed <- es_fixed[2:length(es_fixed)] # ES forecasts from week 2 onward 
mse_es_fixed <- mse(actual_es_fixed, forecast_es_fixed) 
mae_es_fixed <- mae(actual_es_fixed, forecast_es_fixed) 
mape_es_fixed <- mape(actual_es_fixed, forecast_es_fixed) 

# Summarize all error metrics in one table 
errors_summary <- data.frame( 
  Method = c("MA(3)", "MA(4)", "MA(5)", "ES(alpha=0.5)"), 
  MSE = c(mse_ma3, mse_ma4, mse_ma5, mse_es_fixed), 
  MAE = c(mae_ma3, mae_ma4, mae_ma5, mae_es_fixed), 
  MAPE = c(mape_ma3, mape_ma4, mape_ma5, mape_es_fixed) )

# View the error summary 
errors_summary

#D
# Function to compute MSE for a given alpha using exponential smoothing 
es_mse_for_alpha <- function(alpha, y) { 
  # y is the sales vector 
  n <- length(y) 
  f <- numeric(n) 
  
  # Initialize first forecast with first observation 
  f[1] <- y[1] 
  
  # Compute ES forecasts for t = 2,...,n 
  for (t in 2:n) { 
    # F_t = alpha * Y_{t-1} + (1 - alpha) * F_{t-1} 
    f[t] <- alpha * y[t - 1] + (1 - alpha) * f[t - 1] } 
  
  # Compute MSE from t = 2 onward 
  actual <- y[2:n] 
  forecast <- f[2:n] 
  mse(actual, forecast) } 

# Use optimize to find alpha in [0, 1] that minimizes MSE 
opt_result <- optimize( 
  f = es_mse_for_alpha, 
  interval = c(0, 1), 
  y = sales )

# Extract the best alpha and its MSE 
best_alpha <- opt_result$minimum # alpha that minimizes MSE 
best_mse <- opt_result$objective # corresponding minimum MSE 
best_alpha 
best_mse

#E
# Recompute exponential smoothing using the optimal alpha 
alpha_opt <- best_alpha 
es_opt <- numeric(length(sales)) 
es_opt[1] <- sales[1] # initialize with first observation 
for (t in 2:length(sales)) { 
  es_opt[t] <- alpha_opt * sales[t - 1] + (1 - alpha_opt) * es_opt[t - 1] } 

# Compute error metrics for ES with best alpha 
actual_es_opt <- sales[2:length(sales)] 
forecast_es_opt <- es_opt[2:length(es_opt)] 

mse_es_opt <- mse(actual_es_opt, forecast_es_opt) 
mae_es_opt <- mae(actual_es_opt, forecast_es_opt) 
mape_es_opt <- mape(actual_es_opt, forecast_es_opt) 

# Extend the error summary to include ES with best alpha 
errors_summary_extended <- rbind( 
  errors_summary, 
  data.frame( 
    Method = "ES(alpha=best)", 
    MSE = mse_es_opt, 
    MAE = mae_es_opt, 
    MAPE = mape_es_opt ) ) 

# View the extended error summary to see which method is best 
errors_summary_extended

























