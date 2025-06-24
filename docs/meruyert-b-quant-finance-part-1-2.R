library(readxl)
Project_Data <- read_excel("~/Desktop/AD 685/Project_Data.xlsx")
View(Project_Data) 

#Downloading necessary packages
library(dplyr)
library(urca)
library(dynlm)
library(lmtest)
library(sandwich)
library(lubridate)
library(car)
library(zoo)

#Creating variables from Column names from Project_Data excel file
#Original column name started with a number and had a space, I changed it to Treasury_Yield_monthly_average
#and using it as 'tyma'
#Downloaded CPI for All Urban Consumers, All Items in U.S. City Average into Project data, 1990-01 to 2023-06

excess_return <- Project_Data$excess_return
unemployment_rate <- Project_Data$unemployment_rate
tyma <- Project_Data$Treasury_Yield_monthly_average
VIX <- Project_Data$vix_level_monthly_average
CPI <- Project_Data$CPI

#Furnishing date format 
Project_Data$date <- as.Date(as.character(Project_Data$date), format = "%Y%m")

#Creating lags
excess_return_lag1 <- lag(excess_return,1)
unemployment_rate_lag1 <- lag(unemployment_rate, 1)
unemployment_rate_lag2 <- lag(unemployment_rate, 2)
unemployment_rate_lag3 <- lag(unemployment_rate, 3)
unemployment_rate_lag4 <- lag(unemployment_rate, 4)
unemployment_rate_lag5 <- lag(unemployment_rate, 5)
unemployment_rate_lag6 <- lag(unemployment_rate, 6)
tyma_lag1 <- lag(tyma, 1)
CPI_lag1 <- lag(CPI, 1)
CPI_lag2 <- lag(CPI, 2)
CPI_lag3 <- lag(CPI, 3)
CPI_lag4 <- lag(CPI, 4)
CPI_lag5 <- lag(CPI, 5)
CPI_lag6 <- lag(CPI, 6)
excess_return_lag2 <- lag(excess_return,2)
unemployment_rate_lag7 <- lag(unemployment_rate, 7)
unemployment_rate_lag8 <- lag(unemployment_rate, 8)
unemployment_rate_lag9 <- lag(unemployment_rate, 9)
unemployment_rate_lag10 <- lag(unemployment_rate, 10)
unemployment_rate_lag11 <- lag(unemployment_rate, 11)
unemployment_rate_lag12 <- lag(unemployment_rate, 12)
tyma_lag2 <- lag(tyma, 2)


#Created difference in VIX 'diff_VIX' variable
diff_VIX = VIX - lag(VIX)
diff_VIX_lag1 <- lag(diff_VIX, 1)
diff_VIX_lag2 <- lag(diff_VIX, 2)

#Part 1a) ADL(1,6,1,1)
data1 <- cbind(data.frame(excess_return, excess_return_lag1, unemployment_rate_lag1, unemployment_rate_lag2, unemployment_rate_lag3, unemployment_rate_lag4, unemployment_rate_lag5, unemployment_rate_lag6, tyma_lag1, diff_VIX_lag1, CPI_lag1, CPI_lag2, CPI_lag3, CPI_lag4, CPI_lag5, CPI_lag6))
data1 <- na.omit(data1)
eq01 <- lm(excess_return ~ excess_return_lag1 + unemployment_rate_lag1 + unemployment_rate_lag2 + unemployment_rate_lag3 + unemployment_rate_lag4 + unemployment_rate_lag5 + unemployment_rate_lag6 + tyma_lag1 + diff_VIX_lag1, data = data1)
summary(eq01)

#Getting heteroskedastic standard errors 
model01 <- coeftest(eq01, vcov = vcovHC(eq01, type = "HC1"))
model01

#Part 1a) ADL(2,12,2,2)
data2 <- cbind(data.frame(excess_return, excess_return_lag1, excess_return_lag2, unemployment_rate_lag1, unemployment_rate_lag2, unemployment_rate_lag3, unemployment_rate_lag4, unemployment_rate_lag5, unemployment_rate_lag6, unemployment_rate_lag7, unemployment_rate_lag8, unemployment_rate_lag9, unemployment_rate_lag10, unemployment_rate_lag11, unemployment_rate_lag12, tyma_lag1, tyma_lag2, diff_VIX_lag1,  diff_VIX_lag2, CPI_lag1, CPI_lag2, CPI_lag3, CPI_lag4, CPI_lag5, CPI_lag6))
data2 <- na.omit(data2)
eq02 <- lm(excess_return ~ excess_return_lag1 + excess_return_lag2 + unemployment_rate_lag1 + unemployment_rate_lag2 + unemployment_rate_lag3 + unemployment_rate_lag4 + unemployment_rate_lag5 + unemployment_rate_lag6 + unemployment_rate_lag7 + unemployment_rate_lag8 + unemployment_rate_lag9 + unemployment_rate_lag10 + unemployment_rate_lag11 + unemployment_rate_lag12 + tyma_lag1 + tyma_lag2 + diff_VIX_lag1 + diff_VIX_lag2  , data = data2)
summary(eq02)

#Getting heteroskedastic standard errors 
model02 <- coeftest(eq02, vcov = vcovHC(eq02, type = "HC1"))
model02

#Part 1d
     #Adding CPI of lag 1 (eq03) and lag up to 6 (eq04) into ADL(1,6,1,1)
eq03 <- lm(excess_return ~ excess_return_lag1 + unemployment_rate_lag1 + unemployment_rate_lag2 + unemployment_rate_lag3 + unemployment_rate_lag4 + unemployment_rate_lag5 + unemployment_rate_lag6 + tyma_lag1 + diff_VIX_lag1  + CPI_lag1, data = data1)
eq04 <- lm(excess_return ~ excess_return_lag1 + unemployment_rate_lag1 + unemployment_rate_lag2 + unemployment_rate_lag3 + unemployment_rate_lag4 + unemployment_rate_lag5 + unemployment_rate_lag6 + tyma_lag1 + diff_VIX_lag1  + CPI_lag1 + CPI_lag2 + CPI_lag3 + CPI_lag4 + CPI_lag5 + CPI_lag6, data = data1)
summary(eq03)
summary(eq04)

    #Adding CPI of lag 1 (eq05) and lag up to 6 (eq06) into ADL(2,12,2,2)

eq05 <- lm(excess_return ~ excess_return_lag1 + excess_return_lag2 + unemployment_rate_lag1 + unemployment_rate_lag2 + unemployment_rate_lag3 + unemployment_rate_lag4 + unemployment_rate_lag5 + unemployment_rate_lag6 + unemployment_rate_lag7 + unemployment_rate_lag8 + unemployment_rate_lag9 + unemployment_rate_lag10 + unemployment_rate_lag11 + unemployment_rate_lag12 + tyma_lag1 + tyma_lag2 + diff_VIX_lag1 + diff_VIX_lag2  + CPI_lag1  , data = data2)
eq06 <- lm(excess_return ~ excess_return_lag1 + excess_return_lag2 + unemployment_rate_lag1 + unemployment_rate_lag2 + unemployment_rate_lag3 + unemployment_rate_lag4 + unemployment_rate_lag5 + unemployment_rate_lag6 + unemployment_rate_lag7 + unemployment_rate_lag8 + unemployment_rate_lag9 + unemployment_rate_lag10 + unemployment_rate_lag11 + unemployment_rate_lag12 + tyma_lag1 + tyma_lag2 + diff_VIX_lag1 + diff_VIX_lag2  + CPI_lag1 + CPI_lag2 + CPI_lag3 + CPI_lag4 + CPI_lag5 + CPI_lag6, data = data2)
summary(eq05)
summary(eq06)

    #Testing if CPI with lag1-6 jointly significant

linearHypothesis(eq04,c("CPI_lag1", "CPI_lag2", "CPI_lag3", "CPI_lag4", "CPI_lag5", "CPI_lag6"))
linearHypothesis(eq06,c("CPI_lag1", "CPI_lag2", "CPI_lag3", "CPI_lag4", "CPI_lag5", "CPI_lag6"))

squared_errors <- (excess_return)^2
rmsfe_zero <- sqrt(mean(squared_errors))
rmsfe_zero

#Part 1e
#Zero rmsfe
squared_errors <- (excess_return)^2
rmsfe_zero <- sqrt(mean(squared_errors))
rmsfe_zero

# Define the rolling_forecast_constant function
rolling_forecast_constant <- function(window_size, data) {
  forecasts <- numeric()
  for (i in window_size:length(data$date)) {
    window_data <- data[i - window_size + 1:i, ]
    eq <- lm(formula = as.formula("excess_return ~ 1"), data = window_data)
    forecasts[i] <- eq$coefficients[1]
  }
  return(forecasts)
}

# Create the Constant Forecast using a rolling window
constant_forecasts <- rolling_forecast_constant(120, Project_Data)

# Trim constant_forecasts to match original_returns' length
constant_forecasts <- constant_forecasts[120:length(constant_forecasts)]

# Calculate the Root Mean Squared Forecast Error (RMSFE) for the Constant Forecast
original_returns <- Project_Data$excess_return[120:length(Project_Data$excess_return)]

squared_errors <- (original_returns - constant_forecasts)^2
rmsfe_constant <- sqrt(mean(na.omit(squared_errors)))

# Print RMSFE for Constant Forecast
cat("RMSFE for Constant Forecast:", rmsfe_constant, "\n")







# Define the rolling_forecast_adl function
rolling_forecast_adl <- function(window_size, data) {
  forecasts <- numeric(length(data$date))
  for (i in window_size:length(data$date)) {
    window_data <- data[(i - window_size + 1):i, ]
    eq <- lm(excess_return ~ excess_return_lag1 + excess_return_lag2 + unemployment_rate_lag1 + unemployment_rate_lag2 + unemployment_rate_lag3 + unemployment_rate_lag4 + unemployment_rate_lag5 + unemployment_rate_lag6 + unemployment_rate_lag7 + unemployment_rate_lag8 + unemployment_rate_lag9 + unemployment_rate_lag10 + unemployment_rate_lag11 + unemployment_rate_lag12 + tyma_lag1 + tyma_lag2 + diff_VIX_lag1 + diff_VIX_lag2, data = window_data)
    forecasts[i] <- predict(eq, newdata = data[i, , drop = FALSE])
  }
  return(forecasts)
}

# Create the ADL(2,12,2,2) Forecast using a rolling window
adl_forecasts <- rolling_forecast_adl(132, data2)

# Trim adl_forecasts to match original_returns' length
adl_forecasts <- adl_forecasts[132:length(adl_forecasts)]

# Calculate the Root Mean Squared Forecast Error (RMSFE) for the ADL(2,12,2,2) Forecast
squared_errors <- (original_returns - adl_forecasts)^2
rmsfe_adl <- sqrt(mean(na.omit(squared_errors)))

# Print RMSFE for ADL(2,12,2,2) Forecast
cat("RMSFE for ADL(2,12,2,2) Forecast:", rmsfe_adl, "\n")
