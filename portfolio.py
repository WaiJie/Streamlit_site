import streamlit as st
st.set_page_config(layout="wide")

st.header("Code repository")
st.write("This page showcases some of the code from the assignments/Projects highlighted in my portfolio website.")
st.write("These are mainly part of the assignements done for the Master of Analytics and Visualisation (MAVI) Programme")
tab1, tab2, tab3 = st.tabs(["Forecasting Temperatures in Singapore with R", 
                            "Data Consolidation & Transformation with Python", 
                            "Visualising Singaporean Attitudes towards the US and China"])



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

python_code = """# Import required modules
import pandas as pd
import os 
import pymysql
from sqlalchemy import create_engine
from datetime import datetime

# Parameters for the sqlalchemy engine
# Using f-strings to input the username, password and database parameters in db_uri
mysqlid = 'root'
mysqlpw = 'xxxxxxxx'
mysqldb = 'xxxxxxxx'
db_uri = f"mysql+pymysql://{mysqlid}:{mysqlpw}@localhost:3306/{mysqldb}"

# initialise the sqlalchemy engine
engine = create_engine(db_uri)

# Get the filepath of current working directory
path = os.getcwd() 

# Assuming the raw folder is in the current working directory as well
# Define the filepath of the 'raw' folder
data_path = os.path.join(path, 'raw')

# Get a list of files in the 'raw' folder
file_list = os.listdir(data_path)

# list containing all possible sheetnames of interest
# If the sheetnames changes, adding to this list will include them in the data extraction
target_sheetnames = ['inpatient', 'warded', 'ip', 'in','inp' ]

# User written function to figure out the name of the sheet to extract from
# Check if the standardised sheet names are present in the list of sheet names of interest
# Then return the original sheet name given in the excel worksheet
def get_sheetname(excel_sheetnames,target_sheetnames):
    for sheet_name in excel_sheetnames:
        # Standardise sheet names to lower case to match our target sheet names
        standardised_sheet_name = sheet_name.lower()
        # Remove remove leading/trailing blanks
        standardised_sheet_name = standardised_sheet_name.strip()

        # If sheet name in excel matches the list of sheet names of interest
        # Then return that original sheet name 
        if standardised_sheet_name in target_sheetnames:
            return sheet_name

# User written function to convert the abbreviated month name to month number
# E.g. Jan will be 01 , Feb will be 02
def month_to_number(excel_name):
    # Split the excel name string by the '.' delimiter
    # String will be split by the '.' delimiter to output a list
    # The first part is the abbreviated month which is indexed by [0]. E.g. jan
    # The second part is 'xlsx' and is not required. 
    month = excel_name.split('.')[0]

    # Convert month to datetime object using datetime.strptime
    # The %b format represents abbreviated month names such as Jan, Feb
    month= datetime.strptime(month, "%b")
    
    # Convert datetime object to month number with string datatype
    # The %m format represents month as a zero padded number, such as 01,02
    month_num = datetime.strftime(month, "%m")
    
    return month_num

# Main loop to extract data from excel files and load into MySQL database.
for file in file_list:
    # String manipulation to create filepath of the excel files
    # Excel file name is concatenated with the folder directory 
    excel_path = data_path + '\\' + file
    
    # Replace all backslashes with forward slashes as backslash is an escape character in Windows
    # Only 1 forward slash is needed and it is more readable
    excel_path = excel_path.replace('\\','/')

    # Read excel file metadata such as sheet names 
    excel_file = pd.ExcelFile(excel_path)
    
    # Extract names of all sheets in the current excel file
    # Save the sheet names from the excel file in a list
    sheet_names_excel = excel_file.sheet_names

    # Each excel file in this loop has a different sheet to extract data from
    # Use user written function to figure out the name of the sheet to extract from
    # by checking if the standardised sheet names are present in the list of sheet names of interest
    # output in sheetname variable will only be one sheet which matches the target sheetnames 
    sheetname = get_sheetname(sheet_names_excel,target_sheetnames)
    
    # Print out the name of the sheet to be extracted
    print(sheetname)

    # Use pandas to read the excel file with sheetname into a pandas dataframe
    df = pd.read_excel(excel_path, sheet_name = sheetname) 
    
    # Use user written function to convert the abbreviated month name to month number
    month_number = month_to_number(file)

    # Load dataframe into MySQL database using .to_sql and specify con = engine
    # The month number is used as the table name to indicate which month data belongs to
    # This is done by using f-strings to insert the month number behind the word "in"
    df.to_sql(name=f"in{month_number}",con=engine,if_exists='replace')
    
    # Print out confirmation that tables have been created
    print(f"Table in{month_number} created") 
    
    """
   

R_viz_code = """# load packages

library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(foreign)

# import survey data set

df<- foreign::read.spss("Pew Research Center Global Attitudes Spring 2022 Dataset.sav")
class(df)

#convert df to dataframe
df<- as.data.frame(df)

class(df)
# Explore data set
str(df)
head(df)

# As expected, it is a large data set with many columns. Due to the scope of the
# TMA, only data from Singapore is required. Columns that we want either contain
# Singapore in their column headers or contain information about the
# characteristics of the survey respondents. By consulting the documentation on
# the survey questions, the main columns that are needed : 
# fav_us,fav_china,growinflu_us,growinflu_china, as well as demographic
# information like age, race and gender


df%>% filter(country == "Singapore") %>% 
  select(id,"fav_us","fav_china","age","sex",contains("singapore")) %>% view()

# After confirming these are the columns needed, save it into a new dataframe

df%>% filter(country == "Singapore") %>% 
  select(id,"fav_us","fav_china","age","sex",contains("singapore")) -> df2

df2 %>% pivot_longer(cols = c(2,3,6,7), names_to = "Question", values_to = "Response") %>% view()

# Firstly, pivot the table so we can plot the general sentiment of Singaporeans towards US and China

df2 %>% pivot_longer(cols = c(2,3,6,7), names_to = "Question", values_to = "Response") %>% 
  filter(Question %in% c("fav_us","fav_china")) -> df_fav

# view counts of each category
df_fav %>% group_by(Response,Question)  %>% summarise(Data.Count = n()) %>%
view()

# checking the data types
str(df_fav)

# All the categorical variables are factors which are appropriate, but age is also a factor
# Convert age to numeric
df_fav %>% filter(Question %in% c("fav_china","fav_us")) %>% 
  mutate(age = as.character(age)) %>% 
  mutate(age = as.numeric(age))-> df_fav

# here , we can begin to plot the responses of Singaporeans

# plot_data dataframe was created to prevent always subsetting the dataset

plot_data <- subset(df_fav,!Response %in% c("Refused (DO NOT READ)","Don’t know (DO NOT READ)"))


# Fig 1 :  opinion towards US and China

p_opinion <- ggplot(plot_data, aes(x = Response, fill = Question)) +
  geom_bar(position = "dodge",mapping = aes(y = after_stat(prop), group = Question)) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values=c("#de2910","#3C3B6E"), breaks = c("fav_china","fav_us"),
                    labels = c("fav_china" = "China", "fav_us" = "US")) +
  labs(x = "Opinion", y = "Percentage of Singaporeans",
       title = "Opinion Towards The US and China",
       caption = "Source: Pew Research Center Global Attitudes Spring 2022 survey",
       fill=NULL)+
  theme(title = element_text(size=14), 
        axis.text.x = element_text(size = 12,margin = margin(b = 10)), 
        axis.text.y = element_text(size = 12,margin = margin(l = 10)),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, margin = margin(b = 10)),
        legend.position ="top")

p_opinion

ggsave("Fig1_Opinion Towards The US and China.png",plot = p_opinion)      

# Looking into the demographics of the people who have a favourable view towards China (demo1 to demo4)

# Vector created to label all the facet plots
facet_names <- c(fav_china = "China",fav_us = "US")


# Fig 2 : Age distribution of respondents
p_demo2 <- ggplot(plot_data, aes(x= age,fill=Question)) +
  facet_wrap(~Response) + 
  geom_density(alpha = 0.6) + 
  scale_fill_manual(values=c("#de2910","#3C3B6E"), breaks = c("fav_china","fav_us"),
                    labels = c("fav_china" = "China", "fav_us" = "US")) +
  labs(x = "Age", y = "Density",
       title = "Age Distribution of Respondents",
       subtitle = "Views Towards : ",
       caption = "Source: Pew Research Center Global Attitudes Spring 2022 survey",fill=NULL) + 
  theme(title = element_text(size=14), 
        axis.text.x = element_text(size = 12,margin = margin(b = 5)), 
        axis.text.y = element_text(size = 12,margin = margin(l = 10)),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, margin = margin(b = 10)),
        legend.position ="top")

p_demo2


ggsave("Age Distribution of Respondents.png",plot = p_demo2)    

# Fig 3a : Gender distribution of respondents
p_demo3 <- ggplot(plot_data, aes(x= Response,fill=sex)) + 
  geom_bar(position="dodge",mapping = aes(y = after_stat(prop),group = sex)) + 
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Question,ncol=1,labeller = as_labeller(facet_names)) +
  labs(x = "", y = "Percentage of respondents",
       title = "Gender distribution of opinions towards the US and China",
       caption = "Source: Pew Research Center Global Attitudes Spring 2022 survey") +
  theme(title = element_text(size=14), 
        axis.text.y = element_text(size = 12,margin = margin(l = 10)),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, margin = margin(b = 10))) 
                                                                                                        
p_demo3
ggsave("Gender Distribution of Respondents.png",plot = p_demo3)  


# Fig 3b : Age,Gender distribution of respondents (unused in report)
p_demo1 <- ggplot(plot_data, aes(x= Response, y = age,fill=sex)) +
  geom_boxplot() + facet_wrap(~Question,ncol=1,labeller = as_labeller(facet_names)) +
  labs(x = "", y = "Age of respondents",
       title = "Age, Gender of Respondents and their opinion on US and China",
       caption = "Source: Pew Research Center Global Attitudes Spring 2022 survey") +
  theme(title = element_text(size=14), 
        axis.text.y = element_text(size = 12,margin = margin(l = 10)),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, margin = margin(b = 10))) 
p_demo1
ggsave("Age, Gender of Respondents and their opinion on US and China.png",plot = p_demo1)    

# Fig 4 : Ethnicity of respondents

p_demo4 <- ggplot(plot_data, aes(x= Response,fill=d_identity_singapore)) + 
  geom_bar(position="dodge",mapping = aes(y = after_stat(prop),group = d_identity_singapore)) +
  facet_wrap(~Question,ncol=1,labeller = as_labeller(facet_names)) + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete (labels=c ('Chinese', 'Malay','Indian','Others')) +
  labs(x = "", y = "Percentage of respondents",
       title = "Ethnicity of respondents",
       caption = "Source: Pew Research Center Global Attitudes Spring 2022 survey",
       subtitle = "Attitudes towards US and China",
       fill = "Ethnicity" ) +
  theme(title = element_text(size=14), 
        axis.text.y = element_text(size = 12,margin = margin(l = 10)),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, margin = margin(b = 10))) 
p_demo4
ggsave("Ethnicity of respondents.png",plot = p_demo4)  

lookup_df <- data.frame(
  key = 1:10,
  d_educ_singapore = unique(plot_data$d_educ_singapore))

lookup_df %>%
  mutate(edu_level = case_when(key %in% c(2,7) ~ "Primary and below",
                               key %in% c(5,6) ~ "Secondary school",
                               key %in% c(1,8) ~ "Post Secondary",
                               key == 3 ~ "Bachelor's Degree",
                               key == 4 ~ "Master's Degree",
                               key == 10 ~ "Doctorate",
                               key == 9 ~ ""
        )) -> lookup_df

plot_data %>% left_join(y = lookup_df,by = "d_educ_singapore") ->plot_data 

# Fig 5 : Education level of respondents
p_edu_level <- ggplot(subset(plot_data,!d_educ_singapore == "Refused (DO NOT READ)"),aes(x= Response,fill = edu_level)) + 
  geom_bar(position="dodge",mapping = aes(y = after_stat(prop),group = edu_level)) + 
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Question,ncol =1, labeller = as_labeller(facet_names)) + theme(legend.position = "right") +
  labs(x = "", y = "Percentage of respondents",
       title = "Education level of respondents",
       subtitle = "Attitudes towards US and China",
       caption = "Source: Pew Research Center Global Attitudes Spring 2022 survey",
       fill = "Education Level" ) +
  theme(title = element_text(size=14), 
        axis.text.y = element_text(size = 12,margin = margin(l = 10)),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, margin = margin(b = 10))) 

p_edu_level

ggsave("Education level of respondents.png",plot = p_edu_level) 

unique(plot_data$d_income_singapore)"""

with tab1:
    st.subheader("R code for Forecasting Temperatures in Singapore")
    st.code(r_code,"r")

with tab2:
    st.subheader("Python Code for Data Consolidation & Transformation with Python")
    st.code(python_code,"python")

with tab3:
    st.subheader("R code for Visualising Singaporean Attitudes towards the US and China")
    st.code(R_viz_code,"r")