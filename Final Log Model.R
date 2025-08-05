library(tidyverse)
library(gmodels)
library(readr)
library(survival)
library(caret)
library(car)
library(dplyr)
library(lubridate)
library(ROSE)

#Original Data *************************************************************
df <- read_csv("C:/Users/jcalonzo/Desktop/Carlos/DestAirport_withblocks.csv")


#Create Arrival Date/Time  *************************************************************
datatotransform <- df %>% select(FlightDate,DepTime,ArrTime) 
datatotransform <- datatotransform %>%
  mutate(
    # Pad times to HHMM string format (e.g., 518 → "0518")
    DepTime_str = sprintf("%04d", DepTime),
    ArrTime_str = sprintf("%04d", ArrTime),
    
    # Extract hours and minutes
    Dep_Hour = as.integer(substr(DepTime_str, 1, 2)),
    Dep_Min  = as.integer(substr(DepTime_str, 3, 4)),
    Arr_Hour = as.integer(substr(ArrTime_str, 1, 2)),
    Arr_Min  = as.integer(substr(ArrTime_str, 3, 4)),
    
    # Build full POSIXct datetime for departure and arrival
    DepDateTime = make_datetime(year = year(FlightDate),
                                month = month(FlightDate),
                                day = day(FlightDate),
                                hour = Dep_Hour,
                                min = Dep_Min),
    
    ArrDateTime_raw = make_datetime(year = year(FlightDate),
                                    month = month(FlightDate),
                                    day = day(FlightDate)
                                    ),
    
    # Adjust arrival if it occurred after midnight (i.e., next day)
    ArrDateTime = if_else(ArrDateTime_raw < DepDateTime,
                          ArrDateTime_raw + days(1),
                          ArrDateTime_raw)
  )

#Save results to df  *************************************************************
df$ArrDateTime <- as.Date(datatotransform$ArrDateTime, format = "%m/%d/%Y")
df$FlightDate <- as.Date(datatotransform$FlightDate, format = "%m/%d/%Y")

#Load weather data  *************************************************************
df2 <- read_csv("C:/Users/jcalonzo/Desktop/Carlos/AirportDailyWeather.csv")
df2$time <- as.Date(df2$time, format = "%m/%d/%Y")

#Merge weather data  *************************************************************
Model_data <- df %>%
  select(ArrDel15, Operating_Airline, 
         Distance,ArrDateTime, Dest,FlightDate,
         TIME_OF_DAY)%>%
  drop_na()
df2 <- df2 %>% 
  select(Airport, time, `temperature_2m_mean (°F)`, `wind_speed_10m_mean (mp/h)`) %>%
  rename(
    Arr_temp   = `temperature_2m_mean (°F)`,
    Arr_wndspd = `wind_speed_10m_mean (mp/h)`
  )
merged_df <- left_join(Model_data, df2, 
                       by = c("ArrDateTime" = "time", 
                              "Dest"       = "Airport"))





#Clean model data and check for NAs  *************************************************************
df_with_na <- merged_df[!complete.cases(merged_df), ]
Model_data <-merged_df %>% select(-FlightDate,-ArrDateTime)

#Create Prediction Data  *************************************************************
df3 <- read_csv("C:/Users/jcalonzo/Desktop/Carlos/AirportDailyWeather23_24.csv")
df3$time <- as.Date(df3$time, format = "%m/%d/%Y")


datapredict<- read.csv("C:/users/jcalonzo/Desktop/Carlos/data_to_predict.csv")
datapredict$FL_DATE <- as.Date(datapredict$FL_DATE,format = "%m/%d/%Y")

#Transform dates and create arrival data  *************************************************************
datatotransform <- datapredict %>% select(FL_DATE,DEP_TIME,ARR_TIME)
datatotransform <- datatotransform %>%
  mutate(
    # Pad times to HHMM string format (e.g., 518 → "0518")
    DepTime_str = sprintf("%04d", DEP_TIME),
    ArrTime_str = sprintf("%04d", ARR_TIME),
    
    # Extract hours and minutes
    Dep_Hour = as.integer(substr(DepTime_str, 1, 2)),
    Dep_Min  = as.integer(substr(DepTime_str, 3, 4)),
    Arr_Hour = as.integer(substr(ArrTime_str, 1, 2)),
    Arr_Min  = as.integer(substr(ArrTime_str, 3, 4)),
    
    # Build full POSIXct datetime for departure and arrival
    DepDateTime = make_datetime(year = year(FL_DATE),
                                month = month(FL_DATE),
                                day = day(FL_DATE),
                                hour = Dep_Hour,
                                min = Dep_Min),
    
    ArrDateTime_raw = make_datetime(year = year(FL_DATE),
                                    month = month(FL_DATE),
                                    day = day(FL_DATE)
    ),
    
    # Adjust arrival if it occurred after midnight (i.e., next day)
    ArrDateTime = if_else(ArrDateTime_raw < DepDateTime,
                          ArrDateTime_raw + days(1),
                          ArrDateTime_raw)
  )
datapredict$ArrDateTime <- as.Date(datatotransform$ArrDateTime, format = "%m/%d/%Y")
#Predict_data <-merged_df %>% select(-FL_DATE,-ArrDateTime,-DEP_TIME,-DEP_DEL15,-ARR_TIME,
     #                               -CANCELLED,-DIVERTED,-ORIGIN,-ARR_DEL15)

#Merge weather data to prediction data  *************************************************************
df3 <- df3 %>% 
  select(Airport, time, `temperature_2m_mean (°F)`, `wind_speed_10m_mean (mp/h)`) %>%
  rename(
    Arr_temp   = `temperature_2m_mean (°F)`,
    Arr_wndspd = `wind_speed_10m_mean (mp/h)`
  )
merged_df <- left_join(datapredict, df3, 
                       by = c("ArrDateTime" = "time", 
                              "DEST"       = "Airport"))
Predict_data <-merged_df %>% select(-TAIL_NUM,-FL_DATE,-ArrDateTime,-DEP_TIME,-DEP_DEL15,-ARR_TIME,
                                    -CANCELLED,-DIVERTED,-ORIGIN,-ARR_DEL15,-CRS_DEP_TIME,-SCHEDULED_HOUR)

#Train Split Data DONT RUN  *************************************************************
set.seed(123)
train_indices <- createDataPartition(
  y = Model_data$ArrDel15,
  p = 0.7,
  list = FALSE,
  times = 1
)
train_data <- Model_data[train_indices, ]
test_data <- Model_data[-train_indices, ]

# NEW SPLIT DATA *************************************************************
# Assuming Model_data is already loaded and DepDel15 is your target variable

# --- 1. Initial Split: Create an 80/20 split (Train+Validation vs. Test) ---
set.seed(123) # Use your preferred seed for reproducibility of the first split
train_val_indices <- createDataPartition(
  y = Model_data$ArrDel15,
  p = 0.8,    # 80% for training + validation
  list = FALSE,
  times = 1
)

train_val_data <- Model_data[train_val_indices, ]
test_data <- Model_data[-train_val_indices, ] # The remaining 20% is your test set

# --- 2. Second Split: Divide Train+Validation into 75/25 (60/20 overall) ---
# Calculate the 'p' for this step: 60% (desired train) / 80% (train_val_data size) = 0.75
set.seed(456) # Use a different seed for reproducibility of the second split
train_indices <- createDataPartition(
  y = train_val_data$ArrDel15,
  p = 0.75,   # 75% of train_val_data (which is 60% of original)
  list = FALSE,
  times = 1
)

train_data <- train_val_data[train_indices, ]
validation_data <- train_val_data[-train_indices, ] # The remaining 25% of train_val_data (which is 20% of original)
# --- 3. Verify the Splits (Important!) ---

cat("Original Data Dimensions:", dim(Model_data), "\n")
cat("Training Data Dimensions:", dim(train_data), "\n")
cat("Validation Data Dimensions:", dim(validation_data), "\n")
cat("Test Data Dimensions:", dim(test_data), "\n\n")

cat("Number of rows in original data:", nrow(Model_data), "\n")
cat("Number of rows in training data:", nrow(train_data), " (approx.", round(nrow(train_data)/nrow(Model_data)*100), "%)\n")
cat("Number of rows in validation data:", nrow(validation_data), " (approx.", round(nrow(validation_data)/nrow(Model_data)*100), "%)\n")
cat("Number of rows in test data:", nrow(test_data), " (approx.", round(nrow(test_data)/nrow(Model_data)*100), "%)\n\n")


cat("Proportions of DepDel15 in Training Data:\n")
print(prop.table(table(train_data$ArrDel15)))
cat("\nProportions of DepDel15 in Validation Data:\n")
print(prop.table(table(validation_data$ArrDel15)))
cat("\nProportions of DepDel15 in Test Data:\n")
print(prop.table(table(test_data$ArrDel15)))

# You can also check the total counts
cat("\nCounts of DepDel15 in Test Data:\n")
print(table(test_data$ArrDel15))

#Load proper vars to train and test  *************************************************************
train_data <- train_data |>
  dplyr::mutate(
    dplyr::across(c(Operating_Airline, Dest, TIME_OF_DAY), as.factor) #deleted origin here 
  )
test_data <- test_data |>
  dplyr::mutate(
    dplyr::across(c(Operating_Airline, Dest, TIME_OF_DAY), as.factor) #deleted origin here 
  )
validation_data <- validation_data |>
  dplyr::mutate(
    dplyr::across(c(Operating_Airline, Dest, TIME_OF_DAY), as.factor) #deleted origin here 
  )
train_data<- drop_na(train_data)
test_data<- drop_na(test_data)
validation_data <- drop_na(validation_data)

#Resampling using ROSE  *************************************************************
X <- train_data[, setdiff(names(train_data), "ArrDel15")]
y <- as.factor(train_data$ArrDel15)
X_encoded <- model.matrix(~ . - 1, data = X)  # removes intercept column
X_encoded <- as.data.frame(X_encoded)

data_for_rose <- cbind(X_encoded, target = y)
smote_rose_result <- ROSE(target ~ ., data = data_for_rose, N = 977225, p = 0.5) # Example: N=desired total samples, p=desired minority proportion
smote_data <- smote_rose_result$data
table(smote_data$target)
smote_data <- smote_data %>% select(-Operating_AirlineAA) #drop for reference level
#Create Log Model with GLM  *************************************************************
Flights_logit <- glm(target ~ ., data = smote_data, family = binomial) #SMOTE
data_for_rose <- data_for_rose %>% select(-Operating_AirlineAA)
Flights_logit <- glm(target ~ ., data = data_for_rose, family = binomial) #No SMOTE
#Results  *************************************************************
summary(Flights_logit)
100 * (exp(cbind(coef(Flights_logit), confint.default(Flights_logit))) - 1)
X_test_matrix <- model.matrix(~ . - 1, data = test_data[, setdiff(names(test_data), "ArrDel15")])
X_test_encoded <- as.data.frame(X_test_matrix)
test_prob <- predict(Flights_logit, newdata = X_test_encoded, type = "response")
test_pred <- ifelse(test_prob > 0.5, 1, 0)
confusionMatrix(factor(test_pred, levels = c(0,1)),
                factor(test_data$ArrDel15, levels = c(0,1)),
                positive = "1")
survival::concordance(Flights_logit)
print(vif(Flights_logit))


#Finding best thresholds ** OPTIONAL  *************************************************************
# Define a range of thresholds to test
thresholds <- seq(0.05, 0.95, by = 0.05) # From 0.05 to 0.95 in steps of 0.05

# Initialize a data frame to store results
results_threshold_analysis <- data.frame(
  Threshold = numeric(),
  Accuracy = numeric(),
  Precision = numeric(),
  Recall = numeric(),
  F1_Score = numeric(),
  Specificity = numeric()
)

# Loop through each threshold
for (t in thresholds) {
  # Convert probabilities to binary predictions based on the current threshold
  predicted_classes <- ifelse(test_prob >= t, 1, 0)
  
  # Create a confusion matrix for the current threshold
  cm <- caret::confusionMatrix(factor(predicted_classes, levels = c(0, 1)), # Predicted classes
                               factor(test_data$ArrDel15, levels = c(0, 1)),  # Actual classes
                               positive = "1") # Specify '1' as the positive class
  
  # Extract relevant metrics
  accuracy <- cm$overall["Accuracy"]
  precision <- cm$byClass["Pos Pred Value"]
  recall <- cm$byClass["Sensitivity"]
  f1 <- cm$byClass["F1"]
  specificity <- cm$byClass["Specificity"]
  
  # Add results to the data frame
  results_threshold_analysis <- rbind(results_threshold_analysis,
                                      data.frame(Threshold = t,
                                                 Accuracy = accuracy,
                                                 Precision = precision,
                                                 Recall = recall,
                                                 F1_Score = f1,
                                                 Specificity = specificity))
}

# Print the results table
print(results_threshold_analysis)

# **********************************************************************************
# --- OPTIONAL: Visualize the Trade-off ---
# **********************************************************************************

cat("\n--- Generating Performance Plots ---\n")

# Reshape the data for plotting with ggplot2
results_long <- pivot_longer(results_threshold_analysis,
                             cols = c(Accuracy, Precision, Recall, F1_Score, Specificity),
                             names_to = "Metric",
                             values_to = "Value")

# Plot Precision, Recall, F1-Score, Accuracy, and Specificity vs. Threshold
ggplot(results_long, aes(x = Threshold, y = Value, color = Metric)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Model Performance Metrics Across Different Prediction Thresholds",
       x = "Prediction Threshold",
       y = "Metric Value") +
  scale_color_brewer(palette = "Set1") + # Use a color palette for better distinction
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray", alpha = 0.7) + # Mark default 0.5 threshold
  geom_vline(xintercept = 0.4, linetype = "dotted", color = "blue", alpha = 0.7) # Mark your current 0.4 threshold
#.4 - .5
# Generate a Precision-Recall Curve (if you want to focus on that trade-off)
ggplot(results_threshold_analysis, aes(x = Recall, y = Precision)) +
  geom_line(size = 1, color = "purple") +
  geom_point(size = 2, color = "purple") +
  labs(title = "Precision-Recall Curve",
       x = "Recall (Sensitivity)",
       y = "Precision (Pos Pred Value)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")) +
  # Add threshold labels to points on the PR curve if desired
  geom_text(aes(label = sprintf("%.2f", Threshold)), vjust = -0.8, hjust = 0.5, size = 3, color = "black")

#Prep predict data and fix col names  *************************************************************
Predict_data <- Predict_data %>% mutate(OP_UNIQUE_CARRIER = as.factor(OP_UNIQUE_CARRIER),
                                        DEST = as.factor(DEST), 
                                        TIME_OF_DAY = as.factor(TIME_OF_DAY)) %>% 
  rename(Operating_Airline = OP_UNIQUE_CARRIER, Dest = DEST, Distance = DISTANCE)
#Predict_data <- Predict_data %>% 
 # rename(Dest = dest, Distance = distance)

#Look and replace any NAs  *************************************************************
na_summary <- sapply(Predict_data, function(x) sum(is.na(x)))
print(na_summary[na_summary > 0]) # Print columns with NAs
Predict_data <- Predict_data %>%
  filter(!is.na(TIME_OF_DAY ))

#Prediction Results for 23-24  *************************************************************
X_pred_matrix <- model.matrix(~ .-1, data = Predict_data[, setdiff(names(Predict_data), "ArrDel15")])
X_pred_encoded <- as.data.frame(X_pred_matrix)
pred_prob <- predict(Flights_logit, newdata = X_pred_encoded, type = "response")
pred_pred <- ifelse(pred_prob > 0.5, 1, 0)


pred_link_with_se <- predict(Flights_logit, newdata = X_pred_encoded, type = "link", se.fit = TRUE)
pred_log_odds <- pred_link_with_se$fit
pred_se <- pred_link_with_se$se.fit
z_score <- qnorm(0.975) # For 95% CI, we want the 97.5th percentile for a two-tailed test

# 3. Calculate the lower and upper bounds of the confidence interval on the link scale
lower_bound_log_odds <- pred_log_odds - z_score * pred_se
upper_bound_log_odds <- pred_log_odds + z_score * pred_se

# 4. Transform these bounds back to the probability (response) scale using the inverse logit function
# The inverse logit function is: exp(x) / (1 + exp(x))
inverse_logit <- function(x) {
  exp(x) / (1 + exp(x))
}

pred_prob_lower_ci <- inverse_logit(lower_bound_log_odds)
pred_prob_upper_ci <- inverse_logit(upper_bound_log_odds)



#Load predictions back to explanatory dataset  *************************************************************
Predict_data$predicted_prob <- pred_prob
Predict_data$predicted_ArrDel15 <- pred_pred
Predict_data$prob_lower_CI <- pred_prob_lower_ci
Predict_data$prob_upper_CI <- pred_prob_upper_ci

#head(prediction_results)
#*Prediction interval add for user*
colnames(merged_df)
#Join final data with rest of cols for dashboard analysis  *************************************************************
Predict_data <- Predict_data %>% mutate(Operating_Airline = as.character(Operating_Airline),Dest = as.character(Dest),
                                        TIME_OF_DAY = as.double(TIME_OF_DAY))
Finaldata <- left_join(Predict_data, merged_df, 
                       by = c("Operating_Airline" = "OP_UNIQUE_CARRIER", 
                              "Dest"       = "DEST",
                              "Distance" = "DISTANCE",
                              "TIME_OF_DAY" = "TIME_OF_DAY",
                              "Arr_temp"="Arr_temp",
                              "Arr_wndspd" = "Arr_wndspd"))

#Write to csv  *************************************************************
output_file_path <- "C:/Users/jcalonzo/Desktop/Carlos/Finaldata.csv"
write.csv(Predict_data, file = output_file_path, row.names = FALSE)

cat("CSV file saved successfully using write.csv() to:", output_file_path, "\n")















## old ************************************************* following is old code




df <- data.frame(100 * (exp(cbind(coef(Flights_logit), confint.default(Flights_logit))) - 1))

df <- df %>% rownames_to_column(var = "Predictor")
df <- df %>%
  rename(
    `Log Odds` = V1,         # New name = Old name
    `Lower CI` = X2.5..,     # New name = Old name (using R's sanitized name)
    `Upper CI` = X97.5..      # New name = Old name (using R's sanitized name)
  )

library(stringr) 

df <- df %>%
  mutate(Predictor = str_remove(Predictor, "factor\\([A-Za-z_]+\\)")) %>% # Removes "factor(something)"
  mutate(Predictor = str_trim(Predictor)) # Removes any leading/trailing whitespace left after removal

print(df[2:9,])
df_plot_airline<- df[2:10,]
df_plot_airport<-df[11:19,]
df_plot_time<-df[20:24,]

ggplot(data = df_plot_time) +
  geom_bar(mapping = aes(x = Predictor, y = `Log Odds`,,fill = Predictor), stat = "identity") +
  geom_errorbar(data = df_plot_time, aes(x = Predictor, ymin = `Lower CI`, ymax = `Upper CI`), width = 0.2) +
  labs(
    title = "Percentage Change in Odds of Delay by Time Block",
    x = "Predictor Variable",
    y = "Percentage Change in Odds"
  ) + scale_fill_manual(values = c("6" = "red3",
                                   "5" = "yellow3",
                                   "4" = "green4",
                                   "3" = "turquoise",
                                   "2" = "royalblue"
                                   )) +
  theme_minimal() +
  coord_flip()
ggplot(data = df_plot_airport) +
  geom_bar(mapping = aes(x = Predictor, y = `Log Odds`,,fill = Predictor), stat = "identity") +
  geom_errorbar(data = df_plot_airport, aes(x = Predictor, ymin = `Lower CI`, ymax = `Upper CI`), width = 0.2) +
  labs(
    title = "Percentage Change in Odds of Delay by Airport",
    x = "Predictor Variable",
    y = "Percentage Change in Odds"
  ) + scale_fill_manual(values = c("PHX" = "red3",
                                   "ORD" = "yellow3",
                                   "MIA" = "green4",
                                   "MCO" = "turquoise",
                                   "LAX" = "royalblue",
                                   "LAS" = "purple",
                                   "DFW" = "hotpink",
                                   "DEN" = "khaki1",
                                   "CLT" = "orange"
  )) +
  theme_minimal() +
  coord_flip()
ggplot(data = df_plot_airline) +
  geom_bar(mapping = aes(x = Predictor, y = `Log Odds`,,fill = Predictor), stat = "identity") +
  geom_errorbar(data = df_plot_airline, aes(x = Predictor, ymin = `Lower CI`, ymax = `Upper CI`), width = 0.2) +
  labs(
    title = "Percentage Change in Odds of Delay by Airline",
    x = "Predictor Variable",
    y = "Percentage Change in Odds"
  ) + scale_fill_manual(values = c("WN" = "red3",
                                   "UA" = "yellow3",
                                   "NK" = "green4",
                                   "HA" = "turquoise",
                                   "G4" = "royalblue",
                                   "F9" = "purple",
                                   "DL" = "hotpink",
                                   "B6" = "khaki1",
                                   "AS" = "orange"
  )) +
  theme_minimal() +
  coord_flip()
df_plot_airline
df_plot_airport
df_plot_time

write.csv(df_plot_airline, file = "df_plot_airline.csv", row.names = FALSE)
# Step wise selection *****************************************
Flights_logit <- glm(class ~ ., data = smote_data, family = binomial)

full.model <- glm(class ~ . , data = smote_data,family = binomial)
empty.model <- glm(class ~ 1, data = smote_data,family = binomial)

for.model<- step(empty.model,
                 scope= list(lower= formula(empty.model),
                             upper=formula(full.model)),
                 direction = "forward", k = 2)

survival::concordance(for.model)
test_terms   <- terms(for.model)
# Predict
test_prob <- predict(for.model, newdata = X_test_encoded, type = "response")
test_pred <- ifelse(test_prob > 0.5, 1, 0)

# Evaluate
truth <- factor(test_data$ArrDel15, levels = c(0, 1))
pred  <- factor(test_pred, levels = c(0, 1))
confusionMatrix(pred, truth, positive = "1")