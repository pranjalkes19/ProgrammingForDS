#Importing required libraries library(utils)
library(dplyr)
library(stats)
#Reading the dataset from github
df <- read.csv('https://raw.githubusercontent.com/anthoniraj/pds_lab_fat/main/ nation_level_daily.csv?token=AAHLUWVMXTKS76C6RPXO2NLBW3XVC') #head(df)
# Metadata -
# Column names and description # Date - Date
  
 # Daily Confirmed - Daily COVID19 confirmed cases # Daily Recovered - Daily COVID19 recovered cases # Daily.Deceased - Daily COVID19 deceased cases
# Author : Sir Anthoniraj (Lab FAT)
# Converting the chr column to Date format
df[['Date']] <- as.Date(df[['Date']],format = '%d-%m-%Y')
#str(df)
# Making new columns for separated day and month
Day <- as.numeric(format(df$Date, format="%d")) Month <- as.numeric(format(df$Date, format="%m"))
df_new <- cbind(Day,Month,df)
#head(df_new)
# Dropping Date column from df_new to meet the required specs
drop <- c("Date")
df_new = df_new[,!(names(df_new)%in%drop)]
head(df_new)
# df_new is having the dataset with extracted day and month
# Making three different columns for Total Confirmed, Recovered and Deceased and adding it to dataframe using cbind
Total_Confirmed <- cumsum(df$Daily.Confirmed) Total_Recovered <- cumsum(df$Daily.Recovered) Total_Deceased <- cumsum(df$Daily.Deceased)
df_new2 <- cbind(df,Total_Confirmed,Total_Recovered,Total_Deceased) # Reordering columns according to question
df_new2 <- df_new2[, c(1,2,5,3,6,4,7)] head(df_new2)
# Plot and Analysis (Line Chart)
19BDS0117

19BDS0117
# Considering Daily Confirmed and Daily Recovered for the Line chart with overplotted argument
plot(df$Daily.Confirmed, df$Daily.Recovered, type="o", xlab="Daily Recovered", ylab="Daily Confirmed", main="Line Chart 19BDS0117")
# Line chart is the most appropriate for comparing the 2 samples.
# Analysis
# The data is not related to each other as seen from the line chart
# There is a constant growth in both the data as time increases but it cannot be related
# There are outliers in the data which leads to anonymous points on the plotted graph
# Hypothesis Testing
# We choose T-Test for 2 different sample set
# Applying T-Test on Daily COnfirmed and Daily recovered taking 100 samples at random at 5% LOS
# Null hypothesis H0 : There is no significant difference between Daily Confirmed and Daily Recovered
# Alternative Hypo H1 : There is a significant difference between the 2 samples
t.test(sample(df$Daily.Confirmed),sample(df$Daily.Recovered),var.equal=T) # pvalue - 0.0175 (random run)
# The P value is less that 5% ie. 0.05 so we reject the null hypothesis mean(df$Daily.Confirmed)
mean(df$Daily.Recovered)
# There is a significant difference between the mean as seen from result
# We have enough evidence to go with H1
# There is a significant difference between the 2 samples
print("Conclusion - There is a significant difference between the 2 samples") print(" ")
