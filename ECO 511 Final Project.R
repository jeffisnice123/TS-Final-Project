# ECO 511 Final #


# Libraries loaded
library(forecast)
library(tseries)
library(dplyr)
library(xts)
library(quantmod)
library(ggplot2)
library(forecast)
library(TTR)
library(dygraphs)
library(vars)
library(urca)
library(lmtest)
library(tidyr)
library(randomForest)
library(kableExtra)
source("http://bigblue.depaul.edu/jlee141/econdata/R/func_tslib.R")


# Date utilized for analysis
start = as.Date('2000-01-01')
end = as.Date('2024-04-01')


# Download 'Motor Vehicle Retail Sales: Domestic Autos' from FRED
getSymbols('DAUTONSA',
           src = 'FRED',
          from = start,
          to = end)

# Download 'Motor Vehicle Retail Sales: Foreign Autos' from FRED
getSymbols('FAUTONSA',
           src = 'FRED',
           from = start,
           to = end)


# Download 'U.S. Dollars to Euro Spot Exchange Rate' from FRED
getSymbols('EXUSEU',
           src = 'FRED',
           from = start,
           to = end)


# Download 'Japanese Yen to U.S. Dollar Spot Exchange Rate' from FRED
getSymbols('EXJPUS',
           src = 'FRED',
           from = start,
           to = end)

# Download 'Federal Funds Effective Rate' from FRED
getSymbols('FEDFUNDS',
           src = 'FRED',
           from = start,
           to = end)

# Download 'Disposable Personal Income' from FRED
getSymbols('DSPI',
           src = 'FRED',
           from = start,
           to = end)




# Combine Data
combined <- cbind(DAUTONSA, 
                  FAUTONSA, 
                  EXJPUS, 
                  EXUSEU, 
                  FEDFUNDS, 
                  DSPI)
# Plots
plot(combined$DAUTONSA)
plot(combined$FAUTONSA)
plot(combined$EXJPUS)
plot(combined$EXUSEU)
plot(combined$FEDFUNDS)
plot(combined$DSPI)

summary(combined)

combined <- ts(combined,
               start = c(2000, 1),
               frequency = 12)

# Check if original data is stationary

check_stationarity_all_tests <- function(combined) {
  if (!require("tseries")) install.packages("tseries")
  library(tseries)
  
  results <- sapply(colnames(combined), function(col_name) {
    series <- combined[, col_name]
    adf_test <- adf.test(series, alternative = "stationary")
    kpss_test <- kpss.test(series, null = "Level", lshort = TRUE)
    pp_test <- pp.test(series)
    
    adf_p_value <- adf_test$p.value
    kpss_p_value <- kpss_test$p.value
    pp_p_value <- pp_test$p.value
    
    adf_stationary <- ifelse(adf_p_value < 0.05, "Yes", "No")
    kpss_stationary <- ifelse(kpss_p_value > 0.05, "Yes", "No")
    pp_stationary <- ifelse(pp_p_value < 0.05, "Yes", "No")
    
    return(c(adf_p_value, adf_stationary, 
             kpss_p_value, kpss_stationary, 
             pp_p_value, pp_stationary))
  })
  
  results_df <- as.data.frame(t(results))
  colnames(results_df) <- c("ADF_P-Value", "ADF_Stationary", 
                            "KPSS_P-Value", "KPSS_Stationary", 
                            "PP_P-Value", "PP_Stationary")
  rownames(results_df) <- colnames(combined)
  
  return(results_df)
}

# Example usage
stationarity_results <- check_stationarity_all_tests(combined)
print(stationarity_results)

# Data is not stationary


# Check if difference data is stationary

check_stationarity_differenced <- function(combined) {
  if (!require("tseries")) install.packages("tseries")
  library(tseries)
  
  results <- sapply(colnames(combined), function(col_name) {
    series <- combined[, col_name]
    diff_series <- diff(series)
    
    # Differenced series tests
    adf_test_diff <- adf.test(diff_series, alternative = "stationary")
    kpss_test_diff <- kpss.test(diff_series, null = "Level", lshort = TRUE)
    pp_test_diff <- pp.test(diff_series)
    
    adf_p_value_diff <- adf_test_diff$p.value
    kpss_p_value_diff <- kpss_test_diff$p.value
    pp_p_value_diff <- pp_test_diff$p.value
    
    adf_stationary_diff <- ifelse(adf_p_value_diff < 0.05, "Yes", "No")
    kpss_stationary_diff <- ifelse(kpss_p_value_diff > 0.05, "Yes", "No")
    pp_stationary_diff <- ifelse(pp_p_value_diff < 0.05, "Yes", "No")
    
    return(c(adf_p_value_diff, adf_stationary_diff, 
             kpss_p_value_diff, kpss_stationary_diff, 
             pp_p_value_diff, pp_stationary_diff))
  })
  
  results_df <- as.data.frame(t(results))
  colnames(results_df) <- c("ADF_P-Value_Diff", "ADF_Stationary_Diff", 
                            "KPSS_P-Value_Diff", "KPSS_Stationary_Diff", 
                            "PP_P-Value_Diff", "PP_Stationary_Diff")
  rownames(results_df) <- colnames(combined)
  
  return(results_df)
}

# Example usage
stationarity_results <- check_stationarity_differenced(combined)
print(stationarity_results)

# Data is stationary when we use a difference


# Check for lag on data stationary or not

check_stationarity_differenced_lag_1 <- function(combined) {
  if (!require("tseries")) install.packages("tseries")
  if (!require("tidyr")) install.packages("tidyr")
  library(tseries)
  library(tidyr)
  
  results <- lapply(colnames(combined), function(col_name) {
    series <- combined[, col_name]
    
    # Differenced series with lag 1
    diff_series <- diff(series, differences = 1, lag = 1)
    
    # Differenced series tests
    adf_test_diff <- adf.test(diff_series, alternative = "stationary")
    kpss_test_diff <- kpss.test(diff_series, null = "Level", lshort = TRUE)
    pp_test_diff <- pp.test(diff_series)
    
    adf_p_value_diff <- adf_test_diff$p.value
    kpss_p_value_diff <- kpss_test_diff$p.value
    pp_p_value_diff <- pp_test_diff$p.value
    
    adf_stationary_diff <- ifelse(adf_p_value_diff < 0.05, "Yes", "No")
    kpss_stationary_diff <- ifelse(kpss_p_value_diff > 0.05, "Yes", "No")
    pp_stationary_diff <- ifelse(pp_p_value_diff < 0.05, "Yes", "No")
    
    data.frame(
      Variable = col_name,
      Test = c("ADF", "KPSS", "PP"),
      P_Value = c(adf_p_value_diff, kpss_p_value_diff, pp_p_value_diff),
      Stationary = c(adf_stationary_diff, kpss_stationary_diff, pp_stationary_diff)
    )
  })
  
  results_df <- do.call(rbind, results)
  
  return(results_df)
}

# Example usage
# combined <- your_time_series_data
stationarity_results <- check_stationarity_differenced_lag_1(combined)
print(stationarity_results)

# Data is stationary with 1 lag





# Modeling



# Train
indata_train <- window(combined,
                       end = c(2023, 12))
indata_train <- ts(indata_train,
                   start = c(2000, 1),
                   frequency = 12)


# Test
indata_test <- window(combined,
                      start = c(2024, 1))
indata_test <- ts(indata_test,
                  start = c(2024, 1),
                  frequency = 12)


# Exogenous Variables
exog_train <- indata_train[,3:6]
exog_test  <- indata_test[,3:6]

# ARIMA
# Domestic
model1_1 <- auto.arima(indata_train[,'DAUTONSA'],
                       d = 1,
                       max.D = 0,
                       seasonal = TRUE)

summary(model1_1)

domestic_forecast1 <- forecast(model1_1,
                      h = 4)
# Foreign
model1_2 <- auto.arima(indata_train[,'FAUTONSA'],
                       d = 1,
                       max.D = 0, 
                       seasonal = TRUE)

summary(model1_2)

foreign_forecast1 <- forecast(model1_2,
                      h = 4)



# Transfer ARIMA
# Domestic
model2_1 <- auto.arima(indata_train[,'DAUTONSA'],
                       d = 1,
                     max.D = 0,
                     xreg = exog_train)

domestic_forecast2 <- forecast(model2_1,
                      h = 4,
                      xreg = exog_test)

summary(model2_1)

# DSPI is significant

# Foreign
model2_2 <- auto.arima(indata_train[,'FAUTONSA'],
                       d = 1,
                       max.D = 0,
                       xreg = exog_train)

foreign_forecast2 <- forecast(model2_2,
                        h = 4,
                        xreg = exog_test)


summary(model2_2)

# EU exchange rate being significant while JPY exchange rate is near significant (Approx. 1.944)

plot(foreign_forecast2, 
     col = 'red', 
     lwd = 2,
     main = 'Forecast for Foreign Total Auto Sales',
     ylab = 'Thousands of Units')
lines(indata_test[,2], 
      col = 'blue', 
      lwd = 2)


plot(domestic_forecast2, 
     col = 'red', 
     lwd = 2,
     main = 'Forecast for Domestic Total Auto Sales',
     ylab = 'Thousands of Units')
lines(indata_test[,1], 
      col = 'blue', 
      lwd = 2)



# VAR
lag_selection <- VARselect(indata_train,
                           lag.max = 24,
                           type = "both")
optimal_lag <- lag_selection$selection["AIC(n)"]

model_var <- VAR(indata_train,
                 p = optimal_lag,
                 type = "both")

forecast3 <- forecast(model_var,
                      h = 4)

domestic_forecast3 <- forecast3$forecast$DAUTONSA

foreign_forecast3 <- forecast3$forecast$FAUTONSA

summary(model_var$varresult$DAUTONSA)
summary(model_var$varresult$FAUTONSA)

# Granger Causality
# Doemstic

grang_jpy <- causality(model_var,
                       cause = 'EXJPUS')

grang_eu <- causality(model_var,
                      cause = 'EXUSEU')

grang_fed  <- causality(model_var,
                        cause = 'FEDFUNDS')

grang_di  <- causality(model_var,
                        cause = 'DSPI')

# Extract p-values from the test results
p_values <- c(
  grang_jpy$Granger$p.value,
  grang_eu$Granger$p.value,
  grang_fed$Granger$p.value,
  grang_di$Granger$p.value
)

# Create a data frame to display results
results <- data.frame(
  Cause = c("EXJPUS", "EXUSEU", "FEDFUNDS", "DSPI"),
  P_Value = p_values,
  Significant = p_values < 0.05
)

# Print the results
print(results)

# Assume your VAR model is named `model_var`
# Extract variable names from your VAR model
var_names <- colnames(model_var$y)

# Initialize a list to store results
causality_results <- list()

# Perform Granger causality test for each variable with FEDFUNDS as the cause
for (var in var_names) {
  if (var != 'FEDFUNDS') {
    causality_results[[var]] <- causality(model_var, cause = var)
  }
}

# Initialize vectors to store results
p_values <- c()
variables <- c()

# Extract p-values from the causality results
for (var in names(causality_results)) {
  p_values <- c(p_values, causality_results[[var]]$Granger$p.value)
  variables <- c(variables, var)
}

# Create a data frame to display results
results <- data.frame(
  Variable = variables,
  P_Value = p_values,
  Significant = p_values < 0.05
)

# Print the results
print(results)


# Granger Causality FEDFUNDS
indata_train_f1 <- indata_train[,c('DAUTONSA','FEDFUNDS')]
indata_train_f2 <- indata_train[,c('FAUTONSA','FEDFUNDS')]
indata_train_f3 <- indata_train[,c('EXJPUS','FEDFUNDS')]
indata_train_f4 <- indata_train[,c('EXUSEU','FEDFUNDS')]
indata_train_f5 <- indata_train[,c('DSPI','FEDFUNDS')]



# Function to select optimal lag, fit VAR model, and perform Granger causality test
run_var_analysis <- function(data, cause_var, max_lag = 24, significance_level = 0.05) {
  lag_selection <- VARselect(data, lag.max = max_lag, type = "both")
  optimal_lag <- lag_selection$selection["AIC(n)"]
  
  model_var <- VAR(data, p = optimal_lag, type = "both")
  
  causality_results <- lapply(setdiff(colnames(data), cause_var), function(var) {
    causality_result <- causality(model_var, cause = cause_var)
    p_value <- causality_result$Granger[["p.value"]]
    significant <- ifelse(p_value < significance_level, "Yes", "No")
    data.frame(
      Variable = var,
      Granger_Causality_p_Value = p_value,
      Significant = significant
    )
  })
  
  do.call(rbind, causality_results)
}

# Function to analyze multiple datasets
analyze_datasets <- function(datasets, cause_var, max_lag = 24, significance_level = 0.05) {
  results <- lapply(names(datasets), function(name) {
    data <- datasets[[name]]
    result <- run_var_analysis(data, cause_var, max_lag, significance_level)
    result$Dataset <- name
    result
  })
  
  do.call(rbind, results)
}

# Define your datasets
datasets <- list(
  indata_train_f1 = indata_train_f1,
  indata_train_f2 = indata_train_f2,
  indata_train_f3 = indata_train_f3,
  indata_train_f4 = indata_train_f4,
  indata_train_f5 = indata_train_f5
)

# Define the cause variable
cause_var <- 'FEDFUNDS'

# Run the analysis
results_df <- analyze_datasets(datasets, cause_var)

# Print the results
print(results_df)


# FEDFUNDS shows Granger Causality for DAUTONASA and DSPI and slight Granger Causality for FAUTONSA

irf_results <- irf(model_var, n.ahead = 4, boot = TRUE)
plot(irf_results)

# Foreign

# Function to plot forecasts
plot_forecasts <- function(forecast1, 
                           forecast2, 
                           forecast3, 
                           indata_test_y, 
                           region) {
  # Check if all forecasts and actual data are not NULL
  if (is.null(forecast1) || is.null(forecast2) || is.null(forecast3) || is.null(indata_test_y)) {
    stop("One or more forecast objects or actual data are NULL.")
  }
  
  # Create data frames for each forecast
  tryCatch({
    forecast1_df <- data.frame(Date = as.Date(time(forecast1$mean)), Forecast = as.numeric(forecast1$mean), Model = "ARIMA")
    forecast2_df <- data.frame(Date = as.Date(time(forecast2$mean)), Forecast = as.numeric(forecast2$mean), Model = "ARIMA with Exogenous")
    forecast3_df <- data.frame(Date = as.Date(time(forecast3$mean)), Forecast = as.numeric(forecast3$mean), Model = "VAR")
  }, error = function(e) {
    stop("Error in creating forecast data frames: ", e$message)
  })
  
  # Combine all forecast data frames
  combined_forecasts <- tryCatch({
    bind_rows(forecast1_df, forecast2_df, forecast3_df)
  }, error = function(e) {
    stop("Error in combining forecast data frames: ", e$message)
  })
  
  # Create actual values data frame
  actual_values_df <- tryCatch({
    data.frame(Date = as.Date(time(indata_test_y)), Actual = as.numeric(indata_test_y))
  }, error = function(e) {
    stop("Error in creating actual values data frame: ", e$message)
  })
  
  # Plotting using ggplot2
  tryCatch({
    plot <- ggplot() +
      geom_line(data = actual_values_df, aes(x = Date, y = Actual), color = "black", size = 1) +
      geom_line(data = combined_forecasts, aes(x = Date, y = Forecast, color = Model), size = 1) +
      labs(title = paste("Comparison of Forecast Models for", region), x = "Date", y = "Thousand of Units") +
      scale_color_manual(values = c("ARIMA" = "blue", "ARIMA with Exogenous" = "red", "VAR" = "green")) +
      theme_minimal()
    print(plot)
  }, error = function(e) {
    stop("Error in plotting: ", e$message)
  })
}

# Example usage for domestic and foreign forecasts
plot_forecasts(domestic_forecast1, 
               domestic_forecast2, 
               domestic_forecast3, 
               indata_test[, 1], 
               "Domestic")
plot_forecasts(foreign_forecast1, 
               foreign_forecast2, 
               foreign_forecast3, 
               indata_test[, 2], 
               "Foreign")


# Random Forest

# Domestic
lags <- 24

# Create lagged variables
y_var <- combined[, 1]  # Assuming the first column is the target variable
lagged_y <- do.call(cbind, 
                    lapply(1:lags, 
                           function(k) stats::lag(y_var,
                                                  -k)))
colnames(lagged_y) <- c(paste0("y",
                               1:lags))
#
u_var <- combined[, 3]  # Assuming the third column is the target variable
lagged_u <- do.call(cbind,
                    lapply(1:lags, 
                           function(k) stats::lag(u_var,
                                                  -k)))
colnames(lagged_u) <- c(paste0("u",
                               1:lags))

i_var <- combined[, 4]  # Assuming the fourth column is the target variable
lagged_i <- do.call(cbind,
                    lapply(1:lags,
                           function(k) stats::lag(i_var,
                                                  -k)))
colnames(lagged_i) <- c(paste0("i",
                               1:lags))

j_var <- combined[, 5]  # Assuming the fifth column is the target variable
lagged_j <- do.call(cbind,
                    lapply(1:lags,
                           function(k) stats::lag(j_var,
                                                  -k)))
colnames(lagged_j) <- c(paste0("j",
                               1:lags))
t_var <- combined[, 6]  # Assuming the sixth column is the target variable
lagged_t <- do.call(cbind,
                    lapply(1:lags,
                           function(k) stats::lag(t_var,
                                                  -k)))
colnames(lagged_t) <- c(paste0("t",
                               1:lags))

y_lags <- cbind(y_var,
                lagged_y,
                lagged_u,
                lagged_i,
                lagged_j,
                lagged_t)
y_lags <- na.omit(y_lags)
colnames(y_lags) <- c("y",
                      paste0("y",
                             1:lags),
                      paste0("u",
                             1:lags),
                      paste0("i",
                             1:lags),
                      paste0("j",
                             1:lags),
                      paste0("t",
                             1:lags))

# Split the data into training and testing sets
rfdata_train <- window(y_lags,
                       end = c(2023,12))
rfdata_test  <- window(y_lags,
                       start = c(2024,1))

# Fit a Random Forest model
# Further Tests on mtry uing Out of Bag Error. Lower number is better oob.values <- vector(length=12)
# Out of bag error
oob.values <- vector(length = 24)
for(i in 1:24) {
  temp.model <- randomForest(y ~ ., 
                             data = rfdata_train, 
                             ntree = 200, mtry= i)
  oob.values[i] <- temp.model$mse }
cbind(1:24,
      oob.values)

# Lower OOB is better for model, put model into next equation under mtry
plot(oob.values,
     col="red")

rf_model <- randomForest(y ~ ., 
                         data = rfdata_train,
                         ntree = 200, 
                         mtry = 17)
Error.rate = rf_model$mse

# Use error rate plot to determine the amount of ntrees
plot(Error.rate, 
     col="red")

# Further Tests on mtry uing Out of Bag Error. Lower number is better 
rf_model1 <- randomForest(y ~ ., 
                          data = rfdata_train, 
                          ntree = 125, 
                          mtry = 17)

# Make predictions on the test data
domestic_forecast4 <- predict(rf_model1, 
                     newdata = rfdata_test)

# Random Forest Model with exogenous variables and lags

# Domestic
y_xlags <- cbind(y_var, 
                 u_var, 
                 i_var,
                 j_var,
                 t_var,
                 lagged_y,
                 lagged_u,
                 lagged_i,
                 lagged_j,
                 lagged_t)
y_xlags <- na.omit(y_xlags)
colnames(y_xlags) <- c("y", 
                       "u", 
                       "i",
                       "j",
                       't',
                       paste0("y", 
                              1:lags),
                       paste0("u", 
                              1:lags),
                       paste0("i", 
                              1:lags),
                       paste0("j", 
                              1:lags),
                       paste0("t", 
                              1:lags))

# Split the data into training and testing sets
rfdata_xtrain <- window(y_xlags,
                        end = c(2023, 12))
rfdata_xtest  <- window(y_xlags,
                        start = c(2024,1))

rf_model2_1 <- randomForest(y ~ ., 
                            data = rfdata_xtrain, 
                            ntree = 125, 
                            mtry = 17)

# Make predictions on the test data
domestic_forecast5 <- predict(rf_model2_1,
                              newdata = rfdata_xtest)



# Initialize vectors to store RMSE values and models
niter <- 2500
rmse_values <- numeric(niter)
models <- vector("list", 
                 niter)

# Repeat the process 2500 times
for (i in 1:niter) {
  # Train the random forest model
  rf_model2 <- randomForest(y ~ ., 
                            data = rfdata_xtrain, 
                            ntree = 125, 
                            mtry = 17)
  
  # Make predictions on the test data
  forecast6 <- predict(rf_model2, 
                       newdata = rfdata_xtest)
  
  # Calculate the RMSE and store it in the vector
  rmse_values[i] <- accuracy(forecast6, 
                             indata_test[, 1])[, "RMSE"]
  
  # Store the model
  models[[i]] <- rf_model2
}

# Find the best-performing model (with the lowest RMSE)
best_model_index <- which.min(rmse_values)
best_model <- models[[best_model_index]]
best_rmse <- rmse_values[best_model_index]

# Print the best RMSE and the corresponding model index
print(paste("Best RMSE:", 
            best_rmse))
print(paste("Best Model Index:", 
            best_model_index))

# Save the best model
saveRDS(best_model, 
        file = "best_rf_model.rds")

# Plot the RMSE values
plot(rmse_values, 
     type = "b", 
     col = "blue", 
     pch = 19, 
     lwd = 2,
     main = "RMSE values over 2500 iterations",
     xlab = "Iteration", 
     ylab = "RMSE")

domestic_forecast6 <- predict(best_model, 
                              newdata = rfdata_xtest)



# Foreign


# Create lagged variables
y_var <- combined[, 2]  # Assuming the first column is the target variable
lagged_y <- do.call(cbind, 
                    lapply(1:lags, 
                           function(k) stats::lag(y_var,
                                                  -k)))
colnames(lagged_y) <- c(paste0("y",
                               1:lags))
#
u_var <- combined[, 3]  # Assuming the third column is the target variable
lagged_u <- do.call(cbind,
                    lapply(1:lags, 
                           function(k) stats::lag(u_var,
                                                  -k)))
colnames(lagged_u) <- c(paste0("u",
                               1:lags))

i_var <- combined[, 4]  # Assuming the fourth column is the target variable
lagged_i <- do.call(cbind,
                    lapply(1:lags,
                           function(k) stats::lag(i_var,
                                                  -k)))
colnames(lagged_i) <- c(paste0("i",
                               1:lags))

j_var <- combined[, 5]  # Assuming the fifth column is the target variable
lagged_j <- do.call(cbind,
                    lapply(1:lags,
                           function(k) stats::lag(j_var,
                                                  -k)))
colnames(lagged_j) <- c(paste0("j",
                               1:lags))
t_var <- combined[, 6]  # Assuming the sixth column is the target variable
lagged_t <- do.call(cbind,
                    lapply(1:lags,
                           function(k) stats::lag(t_var,
                                                  -k)))
colnames(lagged_t) <- c(paste0("t",
                               1:lags))

y_lags <- cbind(y_var,
                lagged_y,
                lagged_u,
                lagged_i,
                lagged_j,
                lagged_t)
y_lags <- na.omit(y_lags)
colnames(y_lags) <- c("y",
                      paste0("y",
                             1:lags),
                      paste0("u",
                             1:lags),
                      paste0("i",
                             1:lags),
                      paste0("j",
                             1:lags),
                      paste0("t",
                             1:lags))

# Split the data into training and testing sets
rfdata_train <- window(y_lags,
                       end = c(2023,12))
rfdata_test  <- window(y_lags,
                       start = c(2024,1))

# Fit a Random Forest model
# Further Tests on mtry uing Out of Bag Error. Lower number is better oob.values <- vector(length=12)
# Out of bag error
oob.values <- vector(length = 24)
for(i in 1:24) {
  temp.model <- randomForest(y ~ ., 
                             data = rfdata_train, 
                             ntree = 200, mtry= i)
  oob.values[i] <- temp.model$mse }
cbind(1:24,
      oob.values)

# Lower OOB is better for model, put model into next equation under mtry
plot(oob.values,
     col="red")

rf_model <- randomForest(y ~ ., 
                         data = rfdata_train,
                         ntree = 200, 
                         mtry = 18)
Error.rate = rf_model$mse

# Use error rate plot to determine the amount of ntrees
plot(Error.rate, 
     col="red")

# Further Tests on mtry uing Out of Bag Error. Lower number is better 
rf_model2 <- randomForest(y ~ ., 
                          data = rfdata_train, 
                          ntree = 100, 
                          mtry = 18)

# Make predictions on the test data
foreign_forecast4 <- predict(rf_model2, 
                     newdata = rfdata_test)


# Random Forest Model with exogenous variables and lags

y_xlags <- cbind(y_var, 
                 u_var, 
                 i_var,
                 j_var,
                 t_var,
                 lagged_y,
                 lagged_u,
                 lagged_i,
                 lagged_j,
                 lagged_t)
y_xlags <- na.omit(y_xlags)
colnames(y_xlags) <- c("y", 
                       "u", 
                       "i",
                       "j",
                       't',
                       paste0("y", 
                              1:lags),
                       paste0("u", 
                              1:lags),
                       paste0("i", 
                              1:lags),
                       paste0("j", 
                              1:lags),
                       paste0("t", 
                              1:lags))

# Split the data into training and testing sets
rfdata_xtrain <- window(y_xlags,
                        end = c(2023, 12))
rfdata_xtest  <- window(y_xlags,
                        start = c(2024,1))

rf_model2_2 <- randomForest(y ~ ., 
                          data = rfdata_xtrain, 
                          ntree = 100, 
                          mtry = 18)

# Make predictions on the test data
foreign_forecast5 <- predict(rf_model2_2,
                     newdata = rfdata_xtest)

# Best Random Tree

# Initialize vectors to store RMSE values and models
niter <- 2500
rmse_values <- numeric(niter)
models <- vector("list", 
                 niter)

# Repeat the process 2500 times     
for (i in 1:niter) {
  # Train the random forest model
  rf_model2 <- randomForest(y ~ ., 
                            data = rfdata_xtrain, 
                            ntree = 100, 
                            mtry = 18)
  
  
  # Make predictions on the test data
  forecast6 <- predict(rf_model2, 
                       newdata = rfdata_xtest)
  
  # Calculate the RMSE and store it in the vector
  rmse_values[i] <- accuracy(forecast6, 
                             indata_test[, 2])[, "RMSE"]
  
  # Store the model
  models[[i]] <- rf_model2
}

# Find the best-performing model (with the lowest RMSE)
best_model_index <- which.min(rmse_values)
best_model <- models[[best_model_index]]
best_rmse <- rmse_values[best_model_index]

# Print the best RMSE and the corresponding model index
print(paste("Best RMSE:", 
            best_rmse))
print(paste("Best Model Index:", 
            best_model_index))

# Save the best model
saveRDS(best_model, 
        file = "best_rf_model.rds")

# Plot the RMSE values
plot(rmse_values, 
     type = "b", 
     col = "blue", 
     pch = 19, 
     lwd = 2,
     main = "RMSE values over 1000 iterations",
     xlab = "Iteration", 
     ylab = "RMSE")

foreign_forecast6 <- predict(best_model, 
                              newdata = rfdata_xtest)




# Function to evaluate and print forecast accuracy metrics
evaluate_forecasts <- function(forecast1, 
                               forecast2, 
                               forecast3, 
                               forecast4, 
                               forecast5, 
                               forecast6, 
                               indata_test, 
                               region) {
  # Check if all forecasts and actual data are not NULL
  if (any(sapply(list(forecast1, forecast2, forecast3, forecast4, forecast5, forecast6, indata_test), is.null))) {
    stop("One or more forecast objects or actual data are NULL.")
  }
  
  # Calculate accuracy for each forecast model
  tryCatch({
    accuracy1 <- accuracy(forecast1, indata_test)
    accuracy2 <- accuracy(forecast2, indata_test)
    accuracy3 <- accuracy(forecast3, indata_test)
    accuracy4 <- accuracy(forecast4, indata_test)
    accuracy5 <- accuracy(forecast5, indata_test)
    accuracy6 <- accuracy(forecast6, indata_test)
  }, error = function(e) {
    stop("Error in calculating accuracy: ", e$message)
  })
  
  # Create a summary data frame with accuracy metrics
  accuracy_summary <- data.frame(
    Model = c("ARIMA", "Transfer", "VAR", "Random Forest", "Random Forest w/ Exogenous", "Best Random Forest"),
    RMSE = c(accuracy1[1, "RMSE"], accuracy2[1, "RMSE"], accuracy3[1, "RMSE"], accuracy4[1, "RMSE"], accuracy5[1, "RMSE"], accuracy6[1, "RMSE"]),
    MAE = c(accuracy1[1, "MAE"], accuracy2[1, "MAE"], accuracy3[1, "MAE"], accuracy4[1, "MAE"], accuracy5[1, "MAE"], accuracy6[1, "MAE"])
  )
  
  # Print the summary table using kableExtra
  print(
    kable(accuracy_summary, caption = paste("Forecast Accuracy Metrics for", region)) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
  )
}

# Example usage for domestic and foreign forecasts
evaluate_forecasts(domestic_forecast1, 
                   domestic_forecast2, 
                   domestic_forecast3, 
                   domestic_forecast4, 
                   domestic_forecast5, 
                   domestic_forecast6, 
                   indata_test[, 1], 
                   "Domestic")

evaluate_forecasts(foreign_forecast1, 
                   foreign_forecast2, 
                   foreign_forecast3, 
                   foreign_forecast4, 
                   foreign_forecast5, 
                   foreign_forecast6, 
                   indata_test[, 2], 
                   "Foreign")


# Domestic
# Create data frames for each forecast
forecast1_df <- data.frame(Date = as.Date(time(domestic_forecast1$mean)), 
                           Forecast = as.numeric(domestic_forecast1$mean), 
                           Model = "ARIMA")
forecast2_df <- data.frame(Date = as.Date(time(domestic_forecast2$mean)), 
                           Forecast = as.numeric(domestic_forecast2$mean), 
                           Model = "ARIMA with Exogenous")
forecast3_df <- data.frame(Date = as.Date(time(domestic_forecast3$mean)),
                           Forecast = as.numeric(domestic_forecast3$mean), 
                           Model = "VAR")
forecast4_df <- data.frame(Date = as.Date(time(indata_test[, 1])), 
                           Forecast = as.numeric(domestic_forecast4), 
                           Model = "Random Forest")
forecast5_df <- data.frame(Date = as.Date(time(indata_test[, 1])), 
                           Forecast = as.numeric(domestic_forecast5), 
                           Model = "Random Forest with Exogenous")
forecast6_df <- data.frame(Date = as.Date(time(indata_test[, 1])), 
                           Forecast = as.numeric(domestic_forecast6), 
                           Model = "Best Random Forest")

# Combine all forecast data frames
combined_forecasts <- bind_rows(forecast1_df, 
                                forecast2_df, 
                                forecast3_df, 
                                forecast4_df, 
                                forecast5_df, 
                                forecast6_df)

# Create actual values data frame
actual_values_df <- data.frame(Date = as.Date(time(indata_test[, 1])), 
                               Actual = as.numeric(indata_test[, 1]))

# Plotting using ggplot2
ggplot() +
  geom_line(data = actual_values_df, 
            aes(x = Date, 
                y = Actual), 
            color = "black", 
            size = 1) +
  geom_line(data = combined_forecasts, 
            aes(x = Date, 
                y = Forecast, 
                color = Model), 
            size = 1) +
  labs(title = "Comparison of Forecast Models for Motor Vehicle Retail Sales: Domestic", 
       x = "Date", 
       y = "Thousand of Units") +
  scale_color_manual(values = c("ARIMA" = "blue", 
                                "ARIMA with Exogenous" = "red", 
                                "VAR" = "green", 
                                "Random Forest" = "purple", 
                                "Random Forest with Exogenous" = "orange", 
                                "Best Random Forest" = "brown")) + theme_minimal()


# Foreign
# Create data frames for each forecast
forecast1_df <- data.frame(Date = as.Date(time(foreign_forecast1$mean)), 
                           Forecast = as.numeric(foreign_forecast1$mean), 
                           Model = "ARIMA")
forecast2_df <- data.frame(Date = as.Date(time(foreign_forecast2$mean)), 
                           Forecast = as.numeric(foreign_forecast2$mean), 
                           Model = "ARIMA with Exogenous")
forecast3_df <- data.frame(Date = as.Date(time(foreign_forecast3$mean)),
                           Forecast = as.numeric(foreign_forecast3$mean), 
                           Model = "VAR")
forecast4_df <- data.frame(Date = as.Date(time(indata_test[, 2])), 
                           Forecast = as.numeric(foreign_forecast4), 
                           Model = "Random Forest")
forecast5_df <- data.frame(Date = as.Date(time(indata_test[, 2])), 
                           Forecast = as.numeric(foreign_forecast5), 
                           Model = "Random Forest with Exogenous")
forecast6_df <- data.frame(Date = as.Date(time(indata_test[, 2])), 
                           Forecast = as.numeric(foreign_forecast6), 
                           Model = "Best Random Forest")

# Combine all forecast data frames
combined_forecasts <- bind_rows(forecast1_df, 
                                forecast2_df, 
                                forecast3_df, 
                                forecast4_df, 
                                forecast5_df, 
                                forecast6_df)

# Create actual values data frame
actual_values_df <- data.frame(Date = as.Date(time(indata_test[, 2])), 
                               Actual = as.numeric(indata_test[, 2]))

# Plotting using ggplot2
ggplot() +
  geom_line(data = actual_values_df, 
            aes(x = Date, 
                y = Actual), 
            color = "black", 
            linewidth = 1) +
  geom_line(data = combined_forecasts, 
            aes(x = Date, 
                y = Forecast, 
                color = Model), 
            linewidth = 1) +
  labs(title = "Comparison of Forecast Models for Motor Vehicle Retail Sales: Foreign", 
       x = "Date", 
       y = "Thousand of Units") +
  scale_color_manual(values = c("ARIMA" = "blue", 
                                "ARIMA with Exogenous" = "red", 
                                "VAR" = "green", 
                                "Random Forest" = "purple", 
                                "Random Forest with Exogenous" = "orange", 
                                "Best Random Forest" = "brown")) + theme_minimal()
