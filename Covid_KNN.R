#19BDS0117
#PRANJAL KESARWANI

library(httr)
library(utils)
library(dplyr)

library(naniar) # for missing value operations and functions

# Retrieving the raw dataset from my own repository uploaded on Github, with , as delimeter
# R script to write data in a CSV file - https://github.com/pranjalkes19/Lab3PDS/blob/main/zer_writing.R

dfm <- read.csv("https://raw.githubusercontent.com/pranjalkes19/Lab3PDS/main/19BDS0117temp.csv",sep=",")

# dfm consists of 561 rows and 8 columns

# The dataset obtained from covid 19 website had 0's which cannot be replaced with NA's
# The dataset is made to introduce missing values using a loop, with dataset and percentage as paramaters

# Daily.Vaccinated2 is introduced with 60% random NA values

myvars <- c("Daily.Vaccinated2")
newdata <- dfm[myvars]
percentage <- 60

# Extracting the column in a new list and performing operations which is appended later
while(sum(is.na(newdata) == TRUE) < (nrow(newdata) * ncol(newdata) * percentage/100)){
  newdata[sample(nrow(newdata),1), sample(ncol(newdata),1)] <- NA
}

# Total.Vaccinated1 & Total.Vaccinated2 is introduced with 20% random NA values

myvars2 <- c("Total.Vaccinated1" , "Total.Vaccinated2")
newdata2 <- dfm[myvars2]
percentage<-20

# Extracting the column in a new list and performing operations which is appended later
while(sum(is.na(newdata2) == TRUE) < (nrow(newdata2) * ncol(newdata2) * percentage/100)){
  newdata2[sample(nrow(newdata2),1), sample(ncol(newdata2),1)] <- NA
}

# Taking subset and adding the new columns with random missing values

dfm<-dfm[1:5]
dfm["Daily.Vaccinated2"] <- newdata["Daily.Vaccinated2"]
dfm[c("Total.Vaccinated1","Total.Vaccinated2")] <- newdata2[c("Total.Vaccinated1","Total.Vaccinated2")]
#miss_var_summary(dfm)

# Removing rows with equal to and more than 60% NA values

dfm<-dfm[which(rowMeans(!is.na(dfm)) >= 0.6), ]

# Removing columns with more than 60% missing data
# Column Daily.Vaccinated2 show pct_miss > 60% which is removed by the following code

dfm<-dfm[which(colMeans(!is.na(dfm)) > 0.4)]
#miss_var_summary(dfm)



# The column with missing values > 60% are removed

# Median imputation on column Total.Vaccinated1 because we have a good amount of 0's
# which is better than replacing with Mean

dfm$Total.Vaccinated1[is.na(dfm$Total.Vaccinated1)] <- median(dfm$Total.Vaccinated1[!is.na(dfm$Total.Vaccinated1)],na.rm = T)

miss_var_summary(dfm)
head(dfm)
calculate_median<-function(temp){
  
  temp<-sort(temp)
  cnt<-length(temp)
  if(cnt%%2==0){
    first<-cnt/2
    sec<-first+1
    return ((temp[first]+temp[sec])/2)
  }
  else{
    first<-cnt/2
    return (temp[round(first)])
  }
}

calculate_median(x)

library(VIM) #Visualization and Imputation of Missing Values 

# Using MICE and other algorithms would be inappropriate as data is Missing Completely at Random (MCAR)
# For these kind of data, whose value is independent of any observation in the data set
# are imputed using KNN or deep learning models

# Using K - Nearest Neighbour algorithm which is a Standard Algorithm and seems to be the best fit according to our values
# We have an unlabelled data which needs to be classified in one of the several labelled groups
# Using K as 5 ie. the most appropriate among 5 nearest values will be used

dfm <- kNN(dfm, variable = "Total.Vaccinated2",k = 5)

# An extra logical column gets added which needs to be removed

dfm<-subset(dfm,select = Area:Total.Vaccinated2)

# Function to delete rows with given number of NAs
# didn't use it because it was given percentage in the description
# delete.na <- function(DF, n=0) {
#   DF[rowSums(is.na(DF)) <= n,]
# }
head(dfm,n=10L)

