########################
# ANUKRITI BAIJAL
# EDA Year 2014
########################
library(caTools)
library(stats)
library(rpart)
library(tidyverse)
library(dplyr)
library(caret)
library(glmnet)
library(ggplot2)
library(ggcorrplot)
library(corrplot)

# Import the dataset
df_2014<- read.csv(file.choose(), sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Checking for missing values
print(sum(is.na(df_2014)))

# Summary of the data for the year 2014
summary(df_2014)

# Creating a new column that contains approximate total payment for year 2014
df_2014['total_payments_approx']=df_2014['total_discharges']*df_2014['average_total_payments']

# Creating a new column that contains approximate total covered charges for year 2014
df_2014['total_covered_charges_approx']=df_2014['total_discharges']*df_2014['average_covered_charges']

# Calculating mean of total payments for year 2014
df_2014_avg_total=mean(df_2014$total_payments_approx)

# Encode the string column 'hospital_referral_region_description' using label encoding for the year 2014
df_2014$hospital_referral_region_description_encoded=as.integer(factor(df_2014$hospital_referral_region_description))

# Encode the string column 'provider_city' using label encoding for year 2014
df_2014$provider_city_encoded=as.integer(factor(df_2014$provider_city))

# Calculating CMI for year 2014
df_2014$cmi=round((df_2014$total_payments_approx / df_2014_avg_total), digits = 2)

# Distribution of average_medicare_payments
ggplot(df_2014, aes(x = average_medicare_payments)) + geom_histogram(binwidth = 5000, fill = "red", color = "black") + xlab("Average Medicare Payments ($)") + ylab("Count") + ggtitle("Distribution of Average Medicare Payments")

# Check normality of value per sq ft
qqnorm(df_2014$average_medicare_payments, pch=16, col="red", main=expression("Check Average Medicare Payments Normality"))

# Add blue line to check normality
qqline(df_2014$average_medicare_payments, col="blue")  

# Distribution of total_payments_approx
ggplot(df_2014, aes(x = total_payments_approx)) + geom_histogram(binwidth = 1000000, fill = "green", color = "black") + xlab("Approximate Total Payments ($)") + ylab("Count") + ggtitle("Distribution of Approximate Total Payments")

# Distribution of total_covered_charges_approx
ggplot(df_2014, aes(x = total_covered_charges_approx)) + geom_histogram(binwidth = 2500000, fill = "orange", color = "black") + xlab("Approximate Total Covered Charges ($)") + ylab("Count") + ggtitle("Distribution of Approximate Total Covered Charges")

# Subset of rows where DRG is 291
sub_df2014 = df_2014[df_2014$drg_definition == "291 - HEART FAILURE & SHOCK W MCC",]

# Selecting sample from the dataset
sample_2014 = sub_df2014%>% sample_n(1000)

# Scatter Plot for Total Discharges vs. Average Medicare Payments for DRG 291
ggplot(data = sample_2014, aes(x=`total_discharges`, y=`average_medicare_payments`))+
  geom_point(color = "purple")+
  stat_smooth(method="lm")+
  labs(title = "Scatter Plot for Total Discharges vs. Average Medicare Payments for DRG 291",
       x = "Total Discharges",
       y = "Average Total Payments ($)") +
  theme_minimal()

# Scatter Plot for Total Payments Approx vs. Average Medicare Payments for DRG 291
ggplot(data = sample_2014, aes(x=`total_payments_approx`, y=`average_medicare_payments`))+
  geom_point(color = "maroon")+
  stat_smooth(method="lm")+
  labs(title = "Scatter Plot for Total Payments Approx vs. Average Medicare Payments for DRG 291",
       x = "Total Payments Approx ($)",
       y = "AAverage Medicare Payments ($)") +
  theme_minimal()

# Scatter Plot for Hospital Referral Region vs. Average Medicare Payments for DRG 291
ggplot(data = sample_2014, aes(x=`hospital_referral_region_description_encoded`, y=`average_medicare_payments`))+
  geom_point(color = "darkblue")+
  stat_smooth(method="lm")+
  labs(title = "Scatter Plot for Hospital Referral Region vs. Average Medicare Payments for DRG 291",
       x = "Hospital Referral Region",
       y = "Average Medicare Payments ($)") +
  theme_minimal()

# Scatter Plot for Provider City vs. Average Medicare Payments for DRG 291
ggplot(data = sample_2014, aes(x=`provider_city`, y=`average_medicare_payments`))+
  geom_point(color = "orange")+
  stat_smooth(method="lm")+
  labs(title = "Scatter Plot for Provider City vs. Average Medicare Payments for DRG 291",
       x = "Provider City",
       y = "Average Medicare Payments ($)") +
  theme_minimal()

# Scatter Plot for CMI vs. Average Medicare Payments for DRG 291
ggplot(data = sample_2014, aes(x=`cmi`, y=`average_medicare_payments`))+
  geom_point(color = "darkgreen")+
  stat_smooth(method="lm")+
  labs(title = "Scatter Plot for CMI vs. Average Medicare Payments for DRG 291",
       x = "CMI",
       y = "Average Medicare Payments ($)") +
  theme_minimal()

########################
# MODEL BUILDING
########################

# Combining all the years of data frames into one data frame 
df<-rbind(df_2011, df_2012, df_2013)

# Creating a new column that contains approximate total payment
df['total_payments_approx']=df['total_discharges']*df['average_total_payments']

# Creating a new column that contains approximate total covered charges
df['total_covered_charges_approx']=df['total_discharges']*df['average_covered_charges']

# Summary of the data frame
summary(df)

# Creating new data frame with filtered rows where the 'drg_definition' column contains '291'
df_291=df[grepl("291", df$drg_definition), ]

# Creating new data frame with filtered rows where the 'drg_definition' column contains '292'
df_292=df[grepl("292", df$drg_definition), ]

# Creating new data frame with filtered rows where the 'drg_definition' column contains '293'
df_293=df[grepl("293", df$drg_definition), ]

# Calculating mean of total payments for DRG 291
df_291_avg_total=mean(df_291$total_payments_approx)

# Calculating mean of total payments for DRG 291
df_292_avg_total=mean(df_292$total_payments_approx)

# Calculating mean of total payments for DRG 291
df_293_avg_total=mean(df_293$total_payments_approx)

# Calculating CMI for DRG 291
df_291$cmi=round((df_291$total_payments_approx / df_291_avg_total), digits = 2)

# Calculating CMI for DRG 292
df_292$cmi=round((df_292$total_payments_approx / df_292_avg_total), digits = 2)

# Calculating CMI for DRG 293
df_293$cmi=round((df_293$total_payments_approx / df_293_avg_total), digits = 2)

# Encode the string column 'hospital_referral_region_description' using label encoding for DRG 291
df_291$hospital_referral_region_description_encoded=as.integer(factor(df_291$hospital_referral_region_description))

# Encode the string column 'hospital_referral_region_description' using label encoding for DRG 292
df_292$hospital_referral_region_description_encoded=as.integer(factor(df_292$hospital_referral_region_description))

# Encode the string column 'hospital_referral_region_description' using label encoding for DRG 293
df_293$hospital_referral_region_description_encoded=as.integer(factor(df_293$hospital_referral_region_description))

# Encode the string column 'provider_city' using label encoding for DRG 291
df_291$provider_city_encoded=as.integer(factor(df_291$provider_city))

# Encode the string column 'provider_city' using label encoding for DRG 292
df_292$provider_city_encoded=as.integer(factor(df_292$provider_city))

# Encode the string column 'provider_city' using label encoding for DRG 293
df_293$provider_city_encoded=as.integer(factor(df_293$provider_city))

# Setting seed value 
#set.seed(10)

#df_2=rbind(df_291, df_292, df_293)

# Split the data into features and target
#X_train <- df_2[, c('hospital_referral_region_description_encoded', 'total_covered_charges_approx', 'total_payments_approx', 'cmi', 'provider_city_encoded')]

#y_train <- df_2$average_medicare_payments

#X_test=df_2014[c('hospital_referral_region_description_encoded', 'total_covered_charges_approx', 'total_payments_approx', 'cmi', 'provider_city_encoded')]

#########################
# Model 1: Regression Model
#########################

# Create a linear regression model
#regressor <- lm(y_train ~ ., data = df_2)

# Predict on the test set
#y_pred=predict(regressor, newdata = X_test)

# Calculate Mean Squared Error (MSE)
#mse=mean((y_pred - y_test)^2)

# Print the Mean Squared Error
#cat("MSE:", mse, "\n")

# Calculate R-squared (R2) as a measure of accuracy
#r2=1 - (sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2))

# Print the R-squared value
#cat("R-squared (Accuracy):", round(r2 * 100, 2), "%\n")
