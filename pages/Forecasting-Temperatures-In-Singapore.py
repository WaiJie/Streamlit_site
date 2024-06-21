import streamlit as st
st.set_page_config(layout="wide",initial_sidebar_state="collapsed")

r_code = """# 1.1 - Data Preparation 

## Set working directory ##
setwd("C:{set to your working directory}")

# Load the SG Temperature and Rainfall data set
# readxl library allows R to read excel files
library(readxl)

# Reading only the excel cells "A10:BL20" which contains the data
# using the range argument
SG_temp_rainfall <- read_excel("SG_Enviroment_TemperatureRainfall.xlsx",range = "A10:BL20")

# Check data types assigned by R. Data from 1983 and before are in character format
str(SG_temp_rainfall)

# Library will be used for data manipulation using pipe operator and
# mutate function
library(dplyr)
# Library used for the pivot_longer() function to pivot from wide to long format
library(tidyr)
# Library for ARIMA models and stl decomposition
library(forecast)
# Pivot the original table from wide format to long format.
# Mutate function was used to format all columns to character format
# This prevents type mismatch issues when pivoting columns with different data types
# The character data type is due to older data having "na" for some records
SG_temp_rainfall_long <- SG_temp_rainfall %>% 
                         mutate(across(everything(),as.character)) %>%
                         pivot_longer(cols= -"Data Series",
                                      names_to = "Year",
                                      values_to = "values") 

# Check values after transformation to wide format
head(SG_temp_rainfall_long)

# Extract the rows corresponding to the average of the daily highest temperature
# These rows are known by "Air Temperature Means Daily Maximum (Degree Celsius)"
# After that, the time series values column was renamed to "DailyAvgMaxTemp"
# Since there is only one time series, the "Data Series" column can be dropped
# Lastly,convert the format of the time series values back to numeric
SG_temp_avg_max <- SG_temp_rainfall_long %>%
                   filter(`Data Series` == "Air Temperature Means Daily Maximum (Degree Celsius)") %>%
                   rename("DailyAvgMaxTemp" = values) %>%
                   subset(select = c("Year","DailyAvgMaxTemp"))%>%
                   mutate(across(everything(),as.numeric))

head(SG_temp_avg_max)

# Repeating the same data preparation steps for the average of the daily lowest temperature
# This time I am filtering for rows with "Air Temperature Means Daily Minimum (Degree Celsius)"
# Similarly, renaming of the values column and dropping of the "Data Series" column is performed
# The format was also converted to numeric
SG_temp_avg_min <- SG_temp_rainfall_long %>%
                   filter(`Data Series` == "Air Temperature Means Daily Minimum (Degree Celsius)") %>%
                   rename("DailyAvgMinTemp" = values) %>%
                   subset(select = c("Year","DailyAvgMinTemp"))%>%
                   mutate(across(everything(),as.numeric))

head(SG_temp_avg_min)

# Reverse the sequence of both the time series 
# This is done by sorting the data by year (in ascending order)
# Renaming them to max_series and min_series to shorten the names.
max_series <- SG_temp_avg_max %>% 
              arrange(Year)
min_series <- SG_temp_avg_min %>% 
              arrange(Year)

# Print first 6 rows for prepared series
head(max_series)
head(min_series)

# Data Quality - Check for missing values in the entire dataset
any(is.na(max_series))
any(is.na(min_series))

# 1.2 - Exploring the time series 

# Plotting the time series

plot(max_series,type="l",
     main="Average Daily Maximum Temperature",
     ylab="Temperature (Degree Celcius)")

plot(min_series,type="l",
     main="Average Daily Minimum Temperature",
     ylab="Temperature (Degree Celcius)")

# Linear model to confirm that trend exists
# as both series appear to show an upwards trend in the plots

# Fitting a linear model for the average daily maximum series
max_series.lm<- lm(DailyAvgMaxTemp~Year,data=max_series)

# Checking the results of the linear model for the average daily maximum series
summary(max_series.lm)

# Fitting a linear model for the average daily minimum series
min_series.lm <- lm(DailyAvgMinTemp~Year,data=min_series)

# Checking the results of the linear model for the avg daily min series
summary(min_series.lm)

# Coefficient of Year is statistically significant. (P-value below 0.05)
# There is a positive trend for both time series.

# Plot the time series with the trend shown in red (for Average daily maximum series)
plot(max_series,type="l",main="Positive trend in the Average Daily Maximum Temperature")
abline(max_series.lm,col="red")

# Plot the time series with the trend shown in red (for Average daily minimum series)
plot(min_series,type="l",main="Positive trend in the Average Daily Minimum Temperature")
abline(min_series.lm,col="red")

max_series.lm.pred <-predict(max_series.lm)
min_series.lm.pred <-predict(min_series.lm)

plot(max_series,type="l",main="Positive trend in the Average Daily Maximum Temperature")
lines(x = max_series$Year, y = max_series.lm.pred,col="blue")

# Extract the residuals from the linear model results
max_series.lm.res<- residuals(max_series.lm)
min_series.lm.res<- residuals(min_series.lm)

# check on the normality of residuals
plot(max_series.lm.res)
qqnorm(max_series.lm.res)
qqline(max_series.lm.res, col = "red")

plot(min_series.lm.res)
qqnorm(min_series.lm.res)
qqline(min_series.lm.res, col = "blue")

# Statistical test to confirm that residuals are normally distributed
shapiro.test(max_series.lm.res)
shapiro.test(min_series.lm.res)

# Check on independence of residuals using ACF plots
acf(max_series.lm.res)
acf(min_series.lm.res)

# Statistical test for independence of residuals 
# using the Durbin Watson test

library(car)
durbinWatsonTest(max_series.lm)
durbinWatsonTest(min_series.lm)

# P values for both linear models are less than 0.05. 
# Reject the null hypothesis that residuals are independent
# Residuals in the linear model are autocorrelated.

# Linear model with seasonality
# Check for seasonality at the decade level

max_series2 <- max_series %>% 
               mutate(season = rep(1:10,length.out=nrow(max_series)))
max_series2$season <- as.factor(max_series2$season)

# fit linear model with season as dummy variable
max_series.sesonalitylm <- lm(DailyAvgMaxTemp ~ Year + season,data = max_series2)

# Check model results
summary(max_series.sesonalitylm)
anova(max_series.sesonalitylm)

# Repeat for the average daily minimum series
min_series2 <- min_series %>% 
               mutate(season = rep(1:10,length.out=nrow(min_series)))
min_series2$season <- as.factor(min_series2$season)

# fit linear model with season as dummy variable
min_series.sesonalitylm <- lm(DailyAvgMinTemp ~ Year + season,data = min_series2)

# Check model results
summary(min_series.sesonalitylm)
anova(min_series.sesonalitylm)

# no seasonality was found
# Set both series as R time series data type using ts()
max_series$DailyAvgMaxTemp <- ts(data = max_series$DailyAvgMaxTemp,start=1960,frequency=1)
min_series$DailyAvgMinTemp <- ts(data = min_series$DailyAvgMinTemp,start=1960,frequency=1)

# Since there is no reasonable seasonal cycles for both of the time series
# The decompose() function cannot be used as period is less than 2
# mstl() can be used to decompose the series without a seasonal component
max_series.decomposed <- mstl(max_series$DailyAvgMaxTemp)
plot(max_series.decomposed)

min_series.decomposed <- mstl(min_series$DailyAvgMinTemp)
plot(min_series.decomposed)

# Preparation to fit ARIMA models
max_series$DailyAvgMaxTemp <- ts(data = max_series$DailyAvgMaxTemp,start=1960,frequency = 1)
acf(max_series$DailyAvgMaxTemp)
pacf(max_series$DailyAvgMaxTemp)

min_series$DailyAvgMinTemp <- ts(data = min_series$DailyAvgMinTemp,start=1960,frequency = 1)
acf(min_series$DailyAvgMinTemp)
pacf(min_series$DailyAvgMinTemp)

# Both series show significant autocorrelation even at large number of lags
# Differencing is performed to make the time series stationary
max_series.d1 <- diff(max_series$DailyAvgMaxTemp, lag = 1)
max_series.d1.acf<- acf(max_series.d1,lag.max = 20)
max_series.d1.pacf<- pacf(max_series.d1,lag.max = 20)

min_series.d1 <- diff(min_series$DailyAvgMinTemp, lag = 1)
min_series.d1.acf<- acf(min_series.d1,lag.max = 20)
min_series.d1.pacf<- pacf(min_series.d1,lag.max = 20)
#--------------------------------------------------------------------#
# Model selection process for average daily maximum series
#--------------------------------------------------------------------#
# Try ARIMA(0,1,1) first
max_series.arima011 <- arima(max_series.d1, order = c(0, 0, 1)) 
print(max_series.arima011)
param <- max_series.arima011$coef # Extract coefficient estimates
se <- sqrt(diag(max_series.arima011$var.coef)) # Extract standard errors of the coefficient estimates
df <- max_series.arima011$nobs - length(param) # Degrees of freedom
pt(abs(param) / se, df = df, lower.tail = FALSE) * 2 # Compute p-values

# Try ARIMA(1,1,1) 
max_series.arima111 <- arima(max_series.d1, order = c(1, 0, 1)) 
print(max_series.arima111)
param <- max_series.arima111$coef # Extract coefficient estimates
se <- sqrt(diag(max_series.arima111$var.coef)) # Extract standard errors of the coefficient estimates
df <- max_series.arima111$nobs - length(param) # Degrees of freedom
pt(abs(param) / se, df = df, lower.tail = FALSE) * 2 # Compute p-values

# Try ARIMA(1,1,2) 
max_series.arima112 <- arima(max_series.d1, order = c(1, 0, 2)) 
print(max_series.arima112)
param <- max_series.arima112$coef # Extract coefficient estimates
se <- sqrt(diag(max_series.arima112$var.coef)) # Extract standard errors of the coefficient estimates
df <- max_series.arima112$nobs - length(param) # Degrees of freedom
pt(abs(param) / se, df = df, lower.tail = FALSE) * 2 # Compute p-values

# Try ARIMA(2,1,1) 
max_series.arima211 <- arima(max_series.d1, order = c(2, 0, 1)) 
print(max_series.arima211)
param <- max_series.arima211$coef # Extract coefficient estimates
se <- sqrt(diag(max_series.arima211$var.coef)) # Extract standard errors of the coefficient estimates
df <- max_series.arima211$nobs - length(param) # Degrees of freedom
pt(abs(param) / se, df = df, lower.tail = FALSE) * 2 # Compute p-values

# Candidate models are ARIMA(0,1,1) and ARIMA(1,1,1)
summary(max_series.arima011)
summary(max_series.arima111)

# ARIMA(1,1,1) model has the smaller AIC

# Extract the residuals of the models
max_series.arima011.res <- residuals(max_series.arima011)
max_series.arima111.res <- residuals(max_series.arima111)

# Calculate MAD
mean(abs(max_series.arima011.res))
mean(abs(max_series.arima111.res))

# ARIMA(1,1,1) model has lower MAD

# Try double exponential smoothing model
# Double exponential smoothing for average maximum temp series
max_series.desm <- holt(max_series$DailyAvgMaxTemp,h=10)
summary(max_series.desm)

# Extract the residuals of the models
max_series.desm.res <- max_series.desm$residuals

# Calculate MAD
mean(abs(max_series.desm.res))
# Accuracy is poor compared to ARIMA models.

# ARIMA(1,1,1) is chosen as the champion model for 
# average maximum temperature series

# Model diagnosis for chosen ARIMA(1,1,1) model.
# Model diagnosis 1 - Independence of residuals
acf(max_series.arima111.res)

# Model diagnosis 2 - Normality
hist(max_series.arima111.res)
qqnorm(max_series.arima111.res)
qqline(max_series.arima111.res, col = "red")
shapiro.test(max_series.arima111.res) 
# P-value is 0.30 which is >0.05. Do not reject the null hypothesis. 
# Therefore residuals are normally distributed.

# Model diagnosis 3 - Zero mean and constant variance
max_series.arima111.pred <- max_series.d1 - max_series.arima111.res

# converting to numeric for plotting 
max_series.arima111.pred<- as.numeric(max_series.arima111.pred)
max_series.arima111.res<- as.numeric(max_series.arima111.res)
plot(x = max_series.arima111.pred, y = max_series.arima111.res)
# residuals randomly distributed around zero mean except for one outlier 
# which has a high value of above 1
#--------------------------------------------------------------------#
# Repeat model selection process for average daily minimum series
#--------------------------------------------------------------------#
# Try ARIMA(0,1,1) first
min_series.arima011 <- arima(min_series.d1, order = c(0, 0, 1)) 
print(min_series.arima011)
param <- min_series.arima011$coef # Extract coefficient estimates
se <- sqrt(diag(min_series.arima011$var.coef)) # Extract standard errors of the coefficient estimates
df <- min_series.arima011$nobs - length(param) # Degrees of freedom
pt(abs(param) / se, df = df, lower.tail = FALSE) * 2 # Compute p-values

# Try ARIMA(0,1,2) 
min_series.arima012 <- arima(min_series.d1, order = c(0, 0, 2)) 
print(min_series.arima012)
param <- min_series.arima012$coef # Extract coefficient estimates
se <- sqrt(diag(min_series.arima012$var.coef)) # Extract standard errors of the coefficient estimates
df <- min_series.arima012$nobs - length(param) # Degrees of freedom
pt(abs(param) / se, df = df, lower.tail = FALSE) * 2 # Compute p-values

# Try ARIMA(1,1,1) 
min_series.arima111 <- arima(min_series.d1, order = c(1, 0, 1)) 
print(min_series.arima111)
param <- min_series.arima111$coef # Extract coefficient estimates
se <- sqrt(diag(min_series.arima111$var.coef)) # Extract standard errors of the coefficient estimates
df <- min_series.arima111$nobs - length(param) # Degrees of freedom
pt(abs(param) / se, df = df, lower.tail = FALSE) * 2 # Compute p-values

# Try ARIMA(1,1,2) 
min_series.arima112 <- arima(min_series.d1, order = c(1, 0, 2)) 
print(min_series.arima112)
param <- min_series.arima112$coef # Extract coefficient estimates
se <- sqrt(diag(min_series.arima112$var.coef)) # Extract standard errors of the coefficient estimates
df <- min_series.arima112$nobs - length(param) # Degrees of freedom
pt(abs(param) / se, df = df, lower.tail = FALSE) * 2 # Compute p-values

# Try ARIMA(2,1,1) 
min_series.arima211 <- arima(min_series.d1, order = c(2, 0, 1)) 
print(min_series.arima211)
param <- min_series.arima211$coef # Extract coefficient estimates
se <- sqrt(diag(min_series.arima211$var.coef)) # Extract standard errors of the coefficient estimates
df <- min_series.arima211$nobs - length(param) # Degrees of freedom
pt(abs(param) / se, df = df, lower.tail = FALSE) * 2 # Compute p-values

# Candidate models are ARIMA(0,1,2) and ARIMA(1,1,1)
# ARIMA(1,1,1) AIC = 27.03
# ARIMA(0,1,2) AIC = 26.34 

summary(min_series.arima111)
summary(min_series.arima012)

# Extract the residuals of the models
min_series.arima111.res <- residuals(min_series.arima111)
min_series.arima012.res <- residuals(min_series.arima012)

# Calculate MAD
mean(abs(min_series.arima111.res))
mean(abs(min_series.arima012.res))

# ARIMA(1,1,1) model has slightly lower MAD but ARIMA(0,1,2) has lower AIC

# Double exponential smoothing for average minimum temp series
min_series.desm <- holt(min_series$DailyAvgMinTemp,h=10)
summary(min_series.desm)

min_series.desm$residuals
# Extract the residuals of the models
min_series.desm.res <- min_series.desm$residuals

# Calculate MAD
mean(abs(min_series.desm.res))

# Model diagnosis 1 - Independence of residuals
acf(min_series.arima012.res)

# Model diagnosis 2 - Normality
hist(min_series.arima012.res)
qqnorm(min_series.arima012.res)
qqline(min_series.arima012.res, col = "blue")
shapiro.test(min_series.arima012.res) 
# P-value is 0.30 which is >0.05. Do not reject the null hypothesis. 
# Therefore residuals are normally distributed.

# Model diagnosis 3 - Zero mean and constant variance
min_series.arima012.pred <- min_series.d1 - min_series.arima012.res

# converting to numeric for plotting 
min_series.arima012.pred<- as.numeric(min_series.arima012.pred)
min_series.arima012.res<- as.numeric(min_series.arima012.res)
plot(x = min_series.arima012.pred, y = min_series.arima012.res)
#--------------------------------------------------------------------#
# 1.4 Forecast future values - average maximum temperature series
#--------------------------------------------------------------------#
# Generate the future forecasts using the predict function.
# The n.ahead parameter controls the number of periods for forecasting.
max_series.arima111.fc <- predict(max_series.arima111, n.ahead = 10)

# Append the forecasted values to differenced series
max_series.fcval <- c(max_series.d1, max_series.arima111.fc$pred)
max_series.fcval <- ts(max_series.fcval , start = 1960, frequency = 1)

# Revert the 1-time differencing effect from the forecasted value. The first
# observation is appended as the first value. This reversion is done by summing
# up the first observation and the cumulative sum of previous differenced
# values
new.max_series.fcval <- c(max_series$DailyAvgMaxTemp[1],
                          max_series$DailyAvgMaxTemp[1] + cumsum(max_series.fcval))
max_series.arima111.fc <- ts(new.max_series.fcval, start = 1960, frequency = 1)

# Series with original scale and forecasted values have been obtained
print(max_series.arima111.fc)

# Plot the series value and forecasted future values as a red line. 
plot(max_series.arima111.fc, type = "l", col = "red",
     main= "Forecast of Average Daily Maximum Temperature",
     xlab = "Year",
     ylab ="Temperature (Degree Celcius)")
# Overlay the original series value as a black line.
lines(max_series$DailyAvgMaxTemp, col = "black")
mtext("ARMIMA (1,1,1) model", side = 3)
#--------------------------------------------------------------------#
# 1.4 Forecast future values - Repeat for average minimum temperature series
#--------------------------------------------------------------------#
# Generate the future forecasts using the predict function.
# The n.ahead parameter controls the number of periods for forecasting.
min_series.arima012.fc <- predict(min_series.arima012, n.ahead = 10)

# Append the forecasted values to differenced series
min_series.fcval <- c(min_series.d1, min_series.arima012.fc$pred)
min_series.fcval <- ts(min_series.fcval , start = 1960, frequency = 1)

# Revert the 1-time differencing effect from the forecasted value. The first
# observation is appended as the first value. This reversion is done by summing
# up the first observation and the cumulative sum of previous differenced
# values

new.min_series.fcval <- c(min_series$DailyAvgMinTemp[1], 
                          min_series$DailyAvgMinTemp[1] + cumsum(min_series.fcval))
min_series.arima012.fc <- ts(new.min_series.fcval, start = 1960, frequency = 1)

# Series with original scale and forecasted values have been obtained
print(min_series.arima012.fc)

# Plot the series value and forecasted future values as a red line. 
plot(min_series.arima012.fc, type = "l", col = "red",
     main= "Forecast of Average Daily Minimum Temperature",
     xlab = "Year",
     ylab ="Temperature (Degree Celcius)")
# Overlay the original series value as a black line.
lines(min_series$DailyAvgMinTemp, col = "black")
mtext("ARMIMA (0,1,2) model", side = 3)
#--------------------------------------------------------------------#
# Get the index of the outlier in the residuals
#--------------------------------------------------------------------#
position <- which(max_series.arima111.res >1)
# Calculate the year number based on start year and index 
position <- position + 1960
# Print the position
print(position)
"""

st.subheader("R code for Forecasting Temperatures in Singapore")
st.code(r_code,"r")