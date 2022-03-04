# 19BDS0117
# Author - Pranjal Kesarwani

# The dataset that will be uploaded is for city LUCKNOW which is the capital of my State Uttar Pradesh

# The weather data for LUCKNOW will be extracted from an api which is a real time data provided by https://www.weatherapi.com/
# The dataset is completely unique to the best of my knowledge, the source being weatherapi.com

# The data is expected to have 24 x 6 Rows and it possesses 35 columns which needs to be filtered accordingly in the Data preprocessing stage
# The rows will consist of weather data for every hour from 11-Nov-2021 to 16-Nov-2021 which is a 6 day period

# Level 1 data is uploaded in the given link
# And the rest will be uploaded on 23rd Nov

# Column names and description for the dataset

# Column 1 -> Area - Lucknow(State Capital)	
# Column 2 -> Epoch Time - UNIX time is the number of seconds that have elapsed since January 1, 1970 (midnight)
# Column 3 -> Time - (Date & time when the weather conditions are recorded)	
# Column 4 -> Temp in C	
# Column 5 -> Temp in F	
# Column 6 -> Is_Day - (Binary Value day or not)	
# Column 7 -> Condition	Icon - Icon for day and night	
# Column 8 -> Code 	
# Column 9 -> Wind Speed(mph)	
# Column 10 -> Wind Speed(kph)	
# Column 11 -> Wind Degree	
# Column 12 -> Wind Direction (8 directions)	
# Column 13 -> Pressure in MB	
# Column 15 -> Pressure in IN	
# Column 16 -> Precipitation in MM	
# Column 17 -> Precipitation in IN	
# Column 18 -> Humidity percentage	
# Column 19 -> Cloud (Binary value)	
# Column 20 -> FeelsLike in C 	
# Column 21 -> FeelsLike in F	
# Column 22 -> Windchill in C	
# Column 23 -> Windchill in F	
# Column 24 -> HeatIndex in C	
# Column 25 -> HeatIndex in F	
# Column 26 -> Dewpoint in C	
# Column 27 -> Dewpoint in F	
# Column 28 -> Will_it_rain - Binary value for rain or not	
# Column 29 -> Chance_of_rain - Binary value for chance of rain	
# Column 30 -> Will_it_snow - Binary value for snow or not	
# Column 31 -> Chance _of_snow - Binary value for chance of snow	
# Column 32 -> Visibility in KM	
# Column 33 -> Visibility in Miles	
# Column 34 -> Gust in Mph	- Strong sudden winds
# Column 35 -> Gust in Kph - Strong sudden winds

# Some columns need to be left out in preprocessing which is done below

# Data preprocessing is a methodology in which we transform raw data in a effective and useful manner.

#Retrieving the dataset from Github

library(utils)

df <- read.csv("https://raw.githubusercontent.com/anthoniraj/dsr_datasets_final/main/19BDS0117.csv",sep=",")
head(df)

# Changing column names for a meaningful dataset with no special characters

colnames(df) <- c("Area","Epoch_Time","Time","Temp_in_C","Temp_in_F","Is_Day","Condition","Icon","Code","Wind_Speed(mph)","Wind_Speed_kph",
                  "Wind_Degree","Wind_Direction","Pressure_in_MB","Pressure_in_IN","Precipitation_in_MM","Precipitation_in_IN",
                  "Humidity","Cloud","FeelsLike_in_C","FeelsLike_in_F",
                  "Windchill_in_C","Windchill_in_F","HeatIndex_in_C","HeatIndex_in_F","Dewpoint_in_C","Dewpoint_in_F","Will_it_rain",
                  "Chance_of_rain","Will_it_snow","Chance_of_snow","Visibility_in_KM","Visibility_in_Miles","Gust_in_Mph","Gust_in_Kph")


# Checking for missing values (if any)
# Step 1 - Data Cleaning

library(naniar) # For missing data visualisations

miss_var_summary(df)

# No missing data found so no need for imputations

# Step 2 - Data Reduction
# We have a lot of attributes which are of no use like Area, Time so we need to filter the dataset

df<-df[c(4,6,7,11,12,13,14,18,20,32,35)] 
head(df)

tail(df)
str(df)

# Recursive Partitioning and Regression Trees to fit rpart model - rpart
# Moving window statistics - caTools

library(caTools)
library(rpart)

# Splitting data into training and test data
# Keeping a split ratio of 67:33 ie. training_data:test_data

split <- sample.split(df, SplitRatio=0.66)

# Subsetting train and test data according to the true and false values created with sample.split

train_data <- subset(df,split==TRUE)
test_data <- subset(df,split==FALSE)



# Decision Tree regression
# Taking method=anova for regression

# Considering Temp_in_C as the Dependent Variable

# Using Is_Day, Condition, Wind_Speed_kph, Wind_Degree, Wind_Direction, Pressure_in_MB, 
# Humidity, FeelsLike_in_C, Visibility_in_KM, Gust_in_Kph as independent variables
# ~ is a tilde operator also called the relationship operator
# '.' after tilde shows that we are using all the columns to fit in the regression model

model_fit <- rpart(Temp_in_C ~ Is_Day + Condition + Wind_Speed_kph
                   + Humidity + Visibility_in_KM + Gust_in_Kph, 
             method = "anova", data = train_data)

# Decision Tree Regression Model

model_fit

# Predicted test data from Decision Tree Regression Model

y_predicted <- predict(model_fit, newdata = test_data)

# Original Temperature in C

y_true <- test_data$Temp_in_C

#  Evaluation metrics for regression model

# The basic concept of accuracy evaluation is to compare the original target
# with the predicted one according to certain metrics.

# Mean Absolute Error (MAE) is the averaged value of absoluted difference between
# predicted temperature and original temperature for test data

diff <- abs(y_true - y_predicted)
mae <- mean(diff)
sprintf("The Mean Absolute Error is %f",mae)

# Converting to percentage
sprintf("The Mean Absolute Error is %f percent",(mae/100))

# Mean Squared Error (RMSE)

mse <- mean(diff^2)
sprintf("The Mean Squared Error is %f",mse)

# Converting to percentage
sprintf("The Mean Squared Error is %f percent",(mse/100)) 

# Root Mean Squared Error (RMSE)

rmse <- sqrt(mse)
sprintf("The Root Mean Squared Error is %f",rmse)

# Converting to percentage
sprintf("The Root Mean Squared Error is %f percent",(rmse/100)) 

# R-squared (Coefficient of determination) represents the coefficient of
# how well the values fit compared to the original values. 
# The value from 0 to 1 interpreted as percentages. 
# The higher the value is, the better the model is.
# R2 = 1-(sum((d)^2)/sum((original-mean(original))^2))

r2 <- 1-(sum((diff)^2)/sum((y_true-mean(y_true))^2))
sprintf("The R squared coefficient is %f",(r2))

# Data Visualization is an integral part of data science
# So we choose a suitable model
# Histogram which represents the frequencies of values of a variable bucketed into ranges
# Real element in X - axis
# Frequency in Y - axis

hist(train_data$Temp_in_C, main = 'Temperature in C', xlab = 'Temperature', col = blues9)

# The data is not normally distributed
# We get a multimodal histogram







