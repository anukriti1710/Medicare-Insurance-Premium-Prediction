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

#Read the Result CSV file
df_2011<- read.csv(file.choose(), sep = ",", header = TRUE, stringsAsFactors = FALSE)
df_2012<- read.csv(file.choose(), sep = ",", header = TRUE, stringsAsFactors = FALSE)
df_2013<- read.csv(file.choose(), sep = ",", header = TRUE, stringsAsFactors = FALSE)
df_2014<- read.csv(file.choose(), sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Adding a column to indicate years
df_2011['year']=2011
df_2012['year']=2012
df_2013['year']=2013
df_2014['year']=2014

# Combining all the years of data frames into one data frame
df_main<-rbind(df_2011, df_2012, df_2013, df_2014)

# Adding a column to indicate the region
df_main <- df_main %>%
  mutate(region = case_when(
    provider_state %in% c("CT", "DE", "ME", "MD", "MA", "NH", "NJ", "NY", "PA", "RI", "VT") ~ "Northeast",
    provider_state %in% c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "SC", "TN", "VA", "WV") ~ "Southeast",
    provider_state %in% c("AK", "ID", "MT", "OR", "WA") ~ "Northwest",
    provider_state %in% c("AZ", "CA", "CO", "HI", "NV", "NM", "OK", "TX", "UT") ~ "Southwest",
    TRUE ~ "Other"
  ))
# Duplicating to avoid accidental change of the main data frame
df<-df_main

# Creating new data frame for the DRG 193, 194, 195
df_193=df[grepl("193", df$drg_definition), ]
df_194=df[grepl("194", df$drg_definition), ]
df_195=df[grepl("195", df$drg_definition), ]

# Combining all the DRGs related to Simple Pneumonia into one data frame 
df_drg<-rbind(df_193, df_194, df_195)

# Redefining the data frames for different years with only DRG 193, 194, 195 included
df2011 <- df_drg %>% filter(year == 2011)
df2012 <- df_drg %>% filter(year == 2012)
df2013 <- df_drg %>% filter(year == 2013)
df2014 <- df_drg %>% filter(year == 2014)

# Number of Hospitals in different HRR treats DRGS 193, 194, 195
hrr_2011 <- df2011 %>%
  group_by(hospital_referral_region_description) %>%
  summarize(number_of_hospitals = n())

hrr_2012 <- df2012 %>%
  group_by(hospital_referral_region_description) %>%
  summarize(number_of_hospitals = n())

hrr_2013 <- df2013 %>%
  group_by(hospital_referral_region_description) %>%
  summarize(number_of_hospitals = n())

hrr_2014 <- df2014 %>%
  group_by(hospital_referral_region_description) %>%
  summarize(number_of_hospitals = n())

# Arrange the data in descending order of the number of hospitals
hrr_2011_sorted <- hrr_2011 %>%
  arrange(desc(number_of_hospitals))

# Print the top 10 and bottom 10
top_10 <- head(hrr_2011_sorted, 10)
bottom_10 <- tail(hrr_2011_sorted, 10)

# Assuming you want to create two classes: high and low payments
threshold_2011 <- median(df2011$average_medicare_payments)
threshold_2012 <- median(df2012$average_medicare_payments)
threshold_2013 <- median(df2013$average_medicare_payments)
threshold_2014 <- median(df2014$average_medicare_payments)

total_threshold<-data.frame(
  year = c(2011, 2012, 2013, 2014),
  threshold = c(threshold_2011,threshold_2012, threshold_2013, threshold_2014)
)

# Convert to binary classification problem
df2011$payment_class <- ifelse(df2011$average_medicare_payments > threshold_2011, "high", "low")
df2012$payment_class <- ifelse(df2012$average_medicare_payments > threshold_2012, "high", "low")
df2013$payment_class <- ifelse(df2013$average_medicare_payments > threshold_2013, "high", "low")
df2014$payment_class <- ifelse(df2014$average_medicare_payments > threshold_2014, "high", "low")

# Merge the hospital count data with the original data 
df2011 <- left_join(df2011, hrr_2011, by = c("hospital_referral_region_description" = "hospital_referral_region_description"))
df2012 <- left_join(df2012, hrr_2012, by = c("hospital_referral_region_description" = "hospital_referral_region_description"))
df2013 <- left_join(df2013, hrr_2013, by = c("hospital_referral_region_description" = "hospital_referral_region_description"))
df2014 <- left_join(df2014, hrr_2014, by = c("hospital_referral_region_description" = "hospital_referral_region_description"))

# Renaming the new columns for the data frames 
#df_2011 <- df_2011 %>%
#  rename(number_of_hospitals = number_of_hospitals.x)

# Deleting the duplicate columns for the data frames
#df_2011 <- df_2011 %>%
#  select(-number_of_hospitals.y)

# Getting distinct values from the columns to be able to convert into binary
print(unique(df2011$hospital_type))
print(unique(df2011$hospital_ownership))

# Summary of the data frames
summary(df2011)
summary(df2012)
summary(df2013)
summary(df2014)

# Bar plot for top 10 HRRs with most number of hospitals
ggplot(top_10, aes(x = reorder(hospital_referral_region_description, -number_of_hospitals), y = number_of_hospitals)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  geom_text(aes(label = number_of_hospitals), vjust = -0.5, size = 3) +
  labs(title = "Top 10 HRRs by Number of Hospitals",
       x = "Hospital Referral Region",
       y = "Number of Hospitals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for bottom 10 HRRs with the least number of hospitals
ggplot(bottom_10, aes(x = reorder(hospital_referral_region_description, number_of_hospitals), y = number_of_hospitals)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = number_of_hospitals), vjust = -0.5, size = 3) +
  labs(title = "Bottom 10 HRRs by Number of Hospitals",
       x = "Hospital Referral Region",
       y = "Number of Hospitals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total cases for DRG 193-Simple Pneumonia W/O MCC W/O CC for different years
# Histogram of the Average Medicare Payments
hist((df_drg$average_medicare_payments), main = "Histogram of Average Medicare Payments for Simple Pneumonia cases", xlab = "Values", col = "turquoise", border = "black")

# Bar plot for Total Discharges for Simple Pneumonia in Different Regions (2011)
ggplot(df2011, aes(x = region, y = total_discharges, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Simple Pneumonia Discharges in Different Regions (2011)",
       x = "Region",
       y = "Total Discharges") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for Total Discharges for Simple Pneumonia in Different Regions (2012)
ggplot(df2012, aes(x = region, y = total_discharges, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Simple Pneumonia Discharges in Different Regions (2012)",
       x = "Region",
       y = "Total Discharges") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for Total Discharges for Simple Pneumonia in Different Regions (2013)
ggplot(df2013, aes(x = region, y = total_discharges, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Simple Pneumonia Discharges in Different Regions (2013)",
       x = "Region",
       y = "Total Discharges") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for Total Discharges for Simple Pneumonia in Different Regions (2014)
ggplot(df2014, aes(x = region, y = total_discharges, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Simple Pneumonia Discharges in Different Regions (2014)",
       x = "Region",
       y = "Total Discharges") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box Plot for Average Medicare Payments for the Region (2011)
plot_ly(data = df2011, x = ~region, y = ~average_medicare_payments,
        type = 'box', hoverinfo = 'text', text = ~region,
        marker = list(color = 'lightblue', line = list(color = 'darkblue'))) %>%
  layout(title = "Average Medicare Payments for the Region (2011)",
         xaxis = list(title = "Regions"),
         yaxis = list(title = "Average Medicare Payments"))

# Box Plot for Average Medicare Payments for the Regions (2012)
plot_ly(data = df2012, x = ~region, y = ~average_medicare_payments,
        type = 'box', hoverinfo = 'text', text = ~region,
        marker = list(color = 'lightblue', line = list(color = 'darkblue'))) %>%
  layout(title = "Average Medicare Payments for the Regions (2012)",
         xaxis = list(title = "Regions"),
         yaxis = list(title = "Average Medicare Payments"))

# Box Plot for Average Medicare Payments for the Regions (2013)
plot_ly(data = df2013, x = ~region, y = ~average_medicare_payments,
        type = 'box', hoverinfo = 'text', text = ~region,
        marker = list(color = 'lightblue', line = list(color = 'darkblue'))) %>%
  layout(title = "Average Medicare Payments for the Regions (2013)",
         xaxis = list(title = "Regions"),
         yaxis = list(title = "Average Medicare Payments"))

# Box Plot for Average Medicare Payments for the Regions (2014)
plot_ly(data = df2014, x = ~region, y = ~average_medicare_payments,
        type = 'box', hoverinfo = 'text', text = ~region,
        marker = list(color = 'lightblue', line = list(color = 'darkblue'))) %>%
  layout(title = "Average Medicare Payments for the Region (2014)",
         xaxis = list(title = "Regions"),
         yaxis = list(title = "Average Medicare Payments"))

##########################
# DRG: 193, 194, 195
##########################
# Box Plot for Average Medicare Payments for DRGs 193, 194, 195 for all 4 years
plot_ly(data = df_drg, x = ~drg_definition, y = ~average_medicare_payments,
        type = 'box', hoverinfo = 'text', text = ~drg_definition,
        marker = list(color = 'lightblue', line = list(color = 'darkblue'))) %>%
  layout(title = "Average Medicare Payments for DRGs 193, 194, 195 for all 4 years",
         xaxis = list(title = "DRG"),
         yaxis = list(title = "Average Medicare Payments"))

# Bar plot for Threshold values for Simple Pneumonia for different years (2011-2014)
ggplot(total_threshold, aes(x = year, y = threshold, fill = year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = threshold), vjust = -0.5, color = "darkblue", size = 3) +
  labs(title = "Threshold values for Simple Pneumonia for different years (2011-2014)",
       x = "Years",
       y = "Threshold value ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################
# Prediction Model
###################
# Converting categorical into numerical
df2011$emergency_services <- as.factor(df2011$emergency_services)
df2012$emergency_services <- as.factor(df2012$emergency_services)
df2013$emergency_services <- as.factor(df2013$emergency_services)
df2014$emergency_services <- as.factor(df2014$emergency_services)

df2011$hospital_ownership <- as.factor(df2011$hospital_ownership)
df2012$hospital_ownership <- as.factor(df2012$hospital_ownership)
df2013$hospital_ownership <- as.factor(df2013$hospital_ownership)
df2014$hospital_ownership <- as.factor(df2014$hospital_ownership)

df2011$region <- as.factor(df2011$region)
df2012$region <- as.factor(df2012$region)
df2013$region <- as.factor(df2013$region)
df2014$region <- as.factor(df2014$region)

df2011$hospital_referral_region_description <- as.factor(df2011$hospital_referral_region_description)
df2012$hospital_referral_region_description <- as.factor(df2012$hospital_referral_region_description)
df2013$hospital_referral_region_description <- as.factor(df2013$hospital_referral_region_description)
df2014$hospital_referral_region_description <- as.factor(df2014$hospital_referral_region_description)

# Combining all the years of data frames into one data frame
df<-rbind(df2011, df2012, df2013, df2014)

# Setting seed value
set.seed(1000)

#################
# YEAR 2011
#################
# Split the data
splitIndex <- createDataPartition(df$payment_class, p = 0.8, list = FALSE)
train_data <- df[splitIndex, ]
test_data <- df[-splitIndex, ]

# Converting payment class into factors 
train_data$payment_class <- as.factor(train_data$payment_class)
test_data$payment_class <- as.factor(test_data$payment_class)

###################
# Decision Tree
###################
# Create the model
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

# Fit the model
tree_fit <- tree_spec %>%
  fit(payment_class ~ region + total_discharges + hospital_ownership + emergency_services + average_covered_charges, data = train_data)

# Make predictions
predictions <- tree_fit %>%
  predict(test_data) %>%
  pull(.pred_class)

# Calculate RMSE and R-Squared
#metrics <- metric_set(rmse, rsq)
#model_performance <- test_data %>%
#  mutate(predictions = predictions) %>%
#  metrics(truth = average_medicare_payments, estimate = predictions)

#print(model_performance)

# Calculate accuracy
accuracy <- mean(predictions == test_data$payment_class)

cat("Accuracy:", round(accuracy*100,2), "%\n")

#################
# Naive Bayes
#################
# Specify the features used for prediction
features <- c("total_discharges", "number_of_hospitals", "hospital_ownership", "emergency_services", "hospital_referral_region_description","average_covered_charges")

# Train a Naive Bayes model
nb_model <- naiveBayes(train_data[, features], train_data$payment_class)

# Make predictions on the test data
predictions2 <- predict(nb_model, newdata = test_data[, features])

# Calculate accuracy
accuracy2 <- mean(predictions2 == test_data$payment_class)

# Display accuracy
cat("Accuracy:", round(accuracy2 * 100, 2), "%")
