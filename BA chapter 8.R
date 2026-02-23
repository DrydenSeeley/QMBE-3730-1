sales <- read_excel("/Users/drydenseeley1/Desktop/Vintage.xlsx")

sales <- c( 
  242,232,234,244,245,247,257,258,260,263,266,270, # Year 1 
  238,228,236,243,239,248,253,254,255,256,263,269, # Year 2 
  255,231,240,253,254,255,260,263,266,270,274,275 # Year 3
  )

months <- 1:36

#A Time Series Plot
plot(months, sales, type="l", lwd=2, col="blue", 
     xlab="Month", ylab="Sales (000s)", 
     main="Vintage Restaurant Monthly Sales")

#B Trend-Only Regression
trend_model <- lm(sales ~ months) 
summary(trend_model) 

# Forecast months 37–48 (Year 4) 
future_months <- 37:48 
trend_forecast <- predict(trend_model, newdata=list(months=future_months)) 
trend_forecast

#C Trend + Seasonal Regression
# Create month factor (1–12 repeating) 
month_factor <- factor(rep(1:12, 3)) 

seasonal_model <- lm(sales ~ months + month_factor) 
summary(seasonal_model)

# Forecast Year 4 months with seasonality 
future_month_factor <- factor(rep(1:12, 1), levels=1:12)

seasonal_forecast <- predict(seasonal_model, 
                             newdata=list( 
                               months=future_months, 
                               month_factor=future_month_factor ))
seasonal_forecast

#D Forecast Error for January of Year 4
actual_jan4 <- 295 
trend_error <- actual_jan4 - trend_forecast[1] 
seasonal_error <- actual_jan4 - seasonal_forecast[1] 

trend_error 
seasonal_error

#E Combine Forecasts into a Table
forecast_table <- data.frame( 
  Month = month.abb, 
  Trend_Forecast = round(trend_forecast,1), 
  Seasonal_Forecast = round(seasonal_forecast,1) ) 

forecast_table





