### Set up ----

# load libraries
library(tidyverse) # tidyverse

library(broom) # to clean model outputs

library(GGally) # for pairs plots with ggplot

library(caret) # train/test splits and cross validation splits

library(pROC) # Sampling-over and under, ROC and AUC curve

library(margins) # for average marginal effects

library(readxl) # read excel files

library(car)

library(reshape2) 

library(corrplot)

library(caret)

library(dplyr)

library(ggplot2)

library(pROC)

library(openxlsx)


# Set seed for reproducibility
set.seed(9067)

### Load Data ----
employee_data <- read_excel("Employee_Data_Project.xlsx")


# check data structure
str(employee_data)
summary(employee_data)

# check missing values
employee_data |>
  summarize(
    across(everything(), function(x) sum(is.na(x)))
  )


### Step 1: Create a train/test split ----

test_idx <- createDataPartition(
  employee_data$Attrition,
  p = 0.3
)

employee_test <- employee_data[test_idx[[1]], ]

employee_train <- employee_data[-test_idx[[1]], ]

validation_idx <- createDataPartition(
  employee_data$Attrition, # no stratification
  p = 0.3
)

employee_validation <- employee_data[validation_idx[[1]], ]

employee_train <- employee_train[-validation_idx[[1]], ]

keep_idx <- c(
  which(employee_train$Attrition == "Yes"), # indices of positive class
  sample(which(employee_train$Attrition != "Yes"), 300)
)


### Step 2: Data Exploration ----

summary(employee_train)

#### Heatmap w/ Numeric Data----
# Step 1: Transform Attrition to numeric binary format
employee_train <- employee_train %>%
  mutate(Attrition_num = ifelse(Attrition == "Yes", 1, 0))

numeric_cols <- sapply(employee_train, is.numeric)
employee_train_numeric <- employee_train[, numeric_cols]

# Compute the correlation matrix
cor_matrix <- cor(employee_train_numeric, use = "complete.obs")

# Melt the correlation matrix 
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap
heatmap_plot <- ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap Including Attrition",
       x = "Variables",
       y = "Variables")

print(heatmap_plot)


####GGPlot ----
employee_train |>
  mutate(
    Attrition = factor(Attrition)
  ) |>
  select(
    -TotalWorkingYears
  )|>
  ggpairs()

employee_train |>
  mutate(
    Attrition = factor(Attrition)
  ) |>
  select(
    -TotalWorkingYears
  )|>
  ggpairs(aes(color = Attrition, alpha = 0.3))

####Individual Analysis----
##### Attrition by Martial Status----
ggplot(employee_train[keep_idx, ], aes(x = MaritalStatus, fill = Attrition)) +
  geom_bar(position = "dodge") +  
  labs(
    title = "Attrition by Marital Status",
    x = "Marital Status",
    y = "Count",
    fill = "Attrition")+
      geom_text(
        stat = "count", 
        aes(label = ..count..), 
        position = position_dodge(width = 0.9),  
        vjust = -0.5  
      ) +
  theme_minimal(base_size = 14) 

##### Attrition by Business Travel----
ggplot(employee_train[keep_idx, ], aes(x = BusinessTravel, fill = Attrition)) +
  geom_bar(position = "dodge") +  
  labs(
    title = "Attrition by Business Travel",
    x = "Business Travel",
    y = "Count",
    fill = "Attrition")+
  geom_text(
    stat = "count", 
    aes(label = ..count..), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5 
  ) +
  theme_minimal(base_size = 14)+ scale_fill_brewer(palette = "Set2")

##### Attrition by Years with Current Manager----
ggplot(employee_train[keep_idx, ], aes(x = YearsWithCurrManager, fill = Attrition)) +
  geom_bar(position = "dodge") +  
  labs(
    title = "Attrition by Years with Current Manager",
    x = "Years with Current Manager",
    y = "Count",
    fill = "Attrition")+
  geom_text(
    stat = "count", 
    aes(label = ..count..), 
    position = position_dodge(width = 0.9),  
    vjust = -0.5  
  ) +
  theme_minimal(base_size = 14)+ scale_fill_brewer(palette = "Set3")

##### Attrition by Numbers of Companies Worked----
ggplot(employee_train[keep_idx, ], aes(x = NumCompaniesWorked, fill = Attrition)) +
  geom_bar(position = "dodge") +  
  labs(
    title = "Attrition by Number of Companies Worked",
    x = "Number of Companies Worked",
    y = "Count",
    fill = "Attrition")+
  geom_text(
    stat = "count", 
    aes(label = ..count..), 
    position = position_dodge(width = 0.9),  
    vjust = -0.5  
  ) +
  theme_minimal(base_size = 14)+ scale_fill_brewer(palette = "Set1")

### Step 3: Pre-Processing ----
employee_train <- 
  employee_train |>
  mutate(
    Attrition = factor(Attrition)
  )

####Create data frames for each attempt at the logistic model ----
training_1<-employee_train
training_2<-employee_train
training_3<-employee_train
training_4<-employee_train
training_5<-employee_train
training_6<-employee_train

### Step 4: Fit Logistics Model ----

#### Attempt 1----
f1 <- glm(
  Attrition ~ ., 
  data = training_1[keep_idx, ],
  family = binomial("logit")
)

summary(f1)
plot(f1)

summary_linear_model1 <- summary(f1)
# Convert the summary to a data frame
summary_df1 <- data.frame(
  Variable = rownames(summary_linear_model1$coefficients),
  Estimate = summary_linear_model1$coefficients[, "Estimate"],
  Std.Error = summary_linear_model1$coefficients[, "Std. Error"],
  z.value = summary_linear_model1$coefficients[, "z value"],
  Pr = summary_linear_model1$coefficients[, "Pr(>|z|)"]
)

# Write the data frame to an Excel file
write.xlsx(summary_df1, file = "model1.xlsx", sheetName = "Summary", rowNames = FALSE)


##### Get the in-sample AUC ----
f_roc <- tibble(
  actual = training_1$Attrition,
  predicted = predict(f1, training_1, type = "response")
) |>
  roc("actual", "predicted")

plot(f_roc)
f_roc$auc
# use stepwise regression with direction = "both" to select variables automatically
f_step <- step(
  f1,
  direction = "both"
)

summary(f_step)

# get the in-sample AUC
f_step_roc <- tibble(
  actual = training_1_cleaned$Attrition,
  predicted = predict(f_step, training_1_cleaned, type = "response")
) |>
  roc("actual", "predicted")

plot(f_step_roc)

f_step_roc$auc 

# which model works better?

f_step_roc$auc < f_roc$auc # kitchen sink works better

coefs1 <- tidy(f1) |>
  mutate(
    odds = exp(estimate),
    odds_mfx = odds - 1
  )

View(coefs1)



#### Attempt 2: Better fit Logistics Model ----

# Remove the outliers from the dataset
training_2_cleaned <- training_2 %>%
  mutate(
    Age_squared = Age^2,
    Income_squared = Income^2
  ) %>%
  filter(
    !(row_number() %in% c(552, 562, 593))
  ) %>%
  select(
    -TotalWorkingYears,
    -StandardHours,
    -JobLevel,
    -DistanceFromHome,
    -Education,
    -EmployeeID,
    -Gender,
    -YearsAtCompany
  )

# Fit the logistic regression model
f3_cleaned <- glm(
  Attrition ~ . + Age_squared + Income_squared,
  data = training_2_cleaned[keep_idx, ],
  family = binomial("logit")
)

# Display model summary
summary(f3_cleaned)

# Plot diagnostics
plot(f3_cleaned)


# Calculate Variance Inflation Factor (VIF)
vif_values <- vif(f3_cleaned)
print(vif_values)


summary_linear_model3 <- summary(f3_cleaned)
# Convert the summary to a data frame
summary_df3 <- data.frame(
  Variable = rownames(summary_linear_model3$coefficients),
  Estimate = summary_linear_model3$coefficients[, "Estimate"],
  Std.Error = summary_linear_model3$coefficients[, "Std. Error"],
  z.value = summary_linear_model3$coefficients[, "z value"],
  Pr = summary_linear_model3$coefficients[, "Pr(>|z|)"]
)

# Write the data frame to an Excel file
write.xlsx(summary_df3, file = "model3.xlsx", sheetName = "Summary", rowNames = FALSE)


##### Coefficients for Model----
# Coefficients for Model 
coefs3 <- tidy(f3_cleaned) %>%
  mutate(
    odds = exp(estimate), 
    odds_mfx = odds - 1 ) 
# View the data frame
View(coefs3)

##### Plot Model----
training_2 |>
  ggplot(aes(
    x = Attrition, 
    y = 1 / (1 + exp(-1 * (coefs3$estimate[1] + coefs3$estimate[7] * Income + coefs3$estimate[29] * Income^2)))
  )) + 
  geom_line() + 
  xlab("Income") +
  ylab("P(default|income, excluding other variables)")

training_2 |>
  ggplot(aes(
    x = Attrition, 
    y = 1 / (1 + exp(-1 * (coefs3$estimate[1] + coefs3$estimate[2] * Age + coefs3$estimate[28] * Age^2)))
  )) + 
  geom_line() + 
  xlab("Age") +
  ylab("P(default|age, excluding other variables)")

#####ROC for Best Fit----
f_roc3 <- tibble(
  actual = training_2_cleaned$Attrition,
  predicted = predict(f3_cleaned, training_2_cleaned, type = "response")
) |>
  roc("actual", "predicted")

plot(f_roc3)
f_roc3$auc


### Model Validation for Best Fit----
# Step 1: Transform the Validation Data
validate_2 <- employee_validation %>% mutate( Age_squared = Age^2, Income_squared = Income^2 )
# Step 2: Generate Predictions 
preds_validation3 <- predict(f3_cleaned, validate_2, type = "response") 
# Step 3: Calculate ROC and AUC 
roc_validation3 <- roc( data = tibble( actual = validate_2 %>%
                                         select(Attrition) %>% 
                                         unlist(),
                                       predicted = preds_validation3 
                                       ), "actual", "predicted" ) 
# Plot the ROC Curve
plot(roc_validation3, main = "ROC Curve for Model Validation") 
# Print the AUC value
auc_value <- auc(roc_validation3) 
print(paste("AUC:",auc_value))

### Distribution Estimates: Marginal Effects ----
mfx3 <- margins(f3_cleaned)
View(mfx3)

mfx3

### Distribution Estimates: Marginal Effects Without Logistic Model ----
attrition_counts<-table(training_4_cleaned$Attrition)
marginal_distribution<-prop.table(attrition_counts)*100

print(marginal_distribution)

### Distribution Estimates: Calculate With Two Ages----
younger = 1 / (1 + exp(-1 * (coefs3$estimate[1] + coefs3$estimate[2] * 18)))
older = 1 / (1 + exp(-1 * (coefs3$estimate[1] + coefs3$estimate[2] * 60)))
print(younger) 
print(older)

### Graphical Representation----
f4 <- glm(
  Attrition~Age, 
  data=training_3[keep_idx, ], 
  family=binomial("logit")
)
summary(f4)
plot(f4)
summary_linear_model4 <- summary(f4)
# Convert the summary to a data frame
summary_df4 <- data.frame(
  Variable = rownames(summary_linear_model4$coefficients),
  Estimate = summary_linear_model4$coefficients[, "Estimate"],
  Std.Error = summary_linear_model4$coefficients[, "Std. Error"],
  z.value = summary_linear_model4$coefficients[, "z value"],
  Pr = summary_linear_model4$coefficients[, "Pr(>|z|)"]
)


# Write the data frame to an Excel file
write.xlsx(summary_df4, file = "model4.xlsx", sheetName = "Summary", rowNames = FALSE)

####Table Statistics for Age Variable----
# Calculate summary statistics
age_summary <- training_3 %>%
  group_by(Attrition) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE),
    Min_Age = min(Age, na.rm = TRUE),
    Max_Age = max(Age, na.rm = TRUE)
  )

# Display the table
print(age_summary)


coefs4 <- tidy(f4)

coefs4 <- 
  coefs4 |>
  mutate(
    odds_estimate = exp(estimate),
    odds_mfx = odds_estimate - 1
  )

coefs4
training_3 |>
  ggplot(aes(x = Attrition, y = Age)) + 
  geom_point() + 
  geom_line(aes(y = 1 / (1 + exp(-1 * (coefs4$estimate[1] + coefs4$estimate[2] * Age))))) + #logistic function that is in the slides
  ylab("") + 
  labs(
    title = "Propensity to Attrition by Age",
    subtitle = "Actual (points) vs. Fitted (curve)"
  )
View(coefs4)

#####ROC for Best Fit----

f_roc4 <- tibble(
  actual = training_3$Attrition,
  predicted = predict(f4, training_3, type = "response")
) |>
  roc("actual", "predicted")

plot(f_roc4)
f_roc4$auc


##### Model Validation for Best Fit----
# Step 1: Transform the Validation Data
validate_3 <- employee_validation 
# Step 2: Generate Predictions 
preds_validation4 <- predict(f4, validate_3, type = "response") 
# Step 3: Calculate ROC and AUC 
roc_validation4 <- roc( data = tibble( actual = validate_3 %>%
                                         select(Attrition) %>% 
                                         unlist(),
                                       predicted = preds_validation4 
), "actual", "predicted" ) 
# Plot the ROC Curve
plot(roc_validation4, main = "ROC Curve for Model Validation") 
# Print the AUC value
auc_value4 <- auc(roc_validation4) 
print(paste("AUC:",auc_value4))


### Model Comparsion ---- 

#### Model 5----
# Remove the outliers from the dataset
training_4_cleaned <- training_4 %>%
  filter(
    !(row_number() %in% c(566, 398, 534))
  )

# Fit the logistic regression model
f5_cleaned <- glm(
  Attrition ~ Age,
  data = training_4_cleaned[keep_idx, ],
  family = binomial("logit")
)

# Display model summary
summary(f5_cleaned)

# Plot diagnostics
plot(f5_cleaned)

summary_linear_model5 <- summary(f5_cleaned)
# Convert the summary to a data frame
summary_df5 <- data.frame(
  Variable = rownames(summary_linear_model5$coefficients),
  Estimate = summary_linear_model5$coefficients[, "Estimate"],
  Std.Error = summary_linear_model5$coefficients[, "Std. Error"],
  z.value = summary_linear_model5$coefficients[, "z value"],
  Pr = summary_linear_model5$coefficients[, "Pr(>|z|)"]
)

# Write the data frame to an Excel file
write.xlsx(summary_df5, file = "model5.xlsx", sheetName = "Summary", rowNames = FALSE)


#### Model 6----
# Remove the outliers from the dataset
training_4_cleaned <- training_4 %>%
  filter(
    !(row_number() %in% c(566, 398, 534))
  )

# Fit the logistic regression model
f6_cleaned <- glm(
  Attrition ~ Age+Gender,
  data = training_4_cleaned[keep_idx, ],
  family = binomial("logit")
)

# Display model summary
summary(f6_cleaned)

# Plot diagnostics
plot(f6_cleaned)

summary(f6_cleaned)
summary_linear_model6 <- summary(f6_cleaned)
# Convert the summary to a data frame
summary_df6 <- data.frame(
  Variable = rownames(summary_linear_model6$coefficients),
  Estimate = summary_linear_model6$coefficients[, "Estimate"],
  Std.Error = summary_linear_model6$coefficients[, "Std. Error"],
  z.value = summary_linear_model6$coefficients[, "z value"],
  Pr = summary_linear_model6$coefficients[, "Pr(>|z|)"]
)

# Write the data frame to an Excel file
write.xlsx(summary_df6, file = "model6.xlsx", sheetName = "Summary", rowNames = FALSE)


#### Model 7----
# Remove the outliers from the dataset
training_4_cleaned <- training_4 %>%
  filter(
    !(row_number() %in% c(566, 398, 534))
  )

# Fit the logistic regression model
f7_cleaned <- glm(
  Attrition ~ Age+Gender+JobSatisfaction,
  data = training_4_cleaned[keep_idx, ],
  family = binomial("logit")
)

# Display model summary
summary(f7_cleaned)

# Plot diagnostics
plot(f7_cleaned)

summary_linear_model7 <- summary(f7_cleaned)
# Convert the summary to a data frame
summary_df7 <- data.frame(
  Variable = rownames(summary_linear_model7$coefficients),
  Estimate = summary_linear_model7$coefficients[, "Estimate"],
  Std.Error = summary_linear_model7$coefficients[, "Std. Error"],
  z.value = summary_linear_model7$coefficients[, "z value"],
  Pr = summary_linear_model7$coefficients[, "Pr(>|z|)"]
)

# Write the data frame to an Excel file
write.xlsx(summary_df7, file = "model7.xlsx", sheetName = "Summary", rowNames = FALSE)



#### Model 8----
# Remove the outliers from the dataset
training_4_cleaned <- training_4 %>%
  filter(
    !(row_number() %in% c(566, 398, 534))
  )

# Fit the logistic regression model
f8_cleaned <- glm(
  Attrition ~ Age + factor(Gender) + JobSatisfaction + Income + factor(Gender):Income,
  data = training_4_cleaned[keep_idx, ],
  family = binomial("logit")
)

# Display model summary
summary(f8_cleaned)


# Plot diagnostics
plot(f8_cleaned)

summary_linear_model8 <- summary(f8_cleaned)
# Convert the summary to a data frame
summary_df8 <- data.frame(
  Variable = rownames(summary_linear_model8$coefficients),
  Estimate = summary_linear_model8$coefficients[, "Estimate"],
  Std.Error = summary_linear_model8$coefficients[, "Std. Error"],
  z.value = summary_linear_model8$coefficients[, "z value"],
  Pr = summary_linear_model8$coefficients[, "Pr(>|z|)"]
)

# Write the data frame to an Excel file
write.xlsx(summary_df8, file = "model8.xlsx", sheetName = "Summary", rowNames = FALSE)


#### Compare Models---- 
##### AIC Value----
aic_values <- data.frame( Model = c("Model 3","Model 5", "Model 6", "Model 7", "Model 8"),
AIC = c(AIC(f3_cleaned),AIC(f5_cleaned), AIC(f6_cleaned), AIC(f7_cleaned), AIC(f8_cleaned)) )
ggplot(aic_values, aes(x = Model, y = AIC, fill = Model)) + geom_bar(stat = "identity") +
  geom_text(aes(label = round(AIC, 2)), vjust = -0.3, color = "black", size = 3.5) +
  theme_minimal() + 
  labs(title = "Model Comparison Based on AIC", x = "Model", y = "AIC Value") + 
  scale_fill_manual(values = c("Model 3" = "blue", "Model 5" = "purple", "Model 6" = "orange", "Model 7" = "red", "Model 8" = "brown"))

##### AUC Value----
f_roc5 <- tibble(
  actual = training_4_cleaned$Attrition,
  predicted = predict(f5_cleaned, training_4_cleaned, type = "response")
) |>
  roc("actual", "predicted")

plot(f_roc5)
f_roc5$auc


f_roc6 <- tibble(
  actual = training_4_cleaned$Attrition,
  predicted = predict(f6_cleaned, training_4_cleaned, type = "response")
) |>
  roc("actual", "predicted")

plot(f_roc6)
f_roc6$auc

f_roc7 <- tibble(
  actual = training_4_cleaned$Attrition,
  predicted = predict(f7_cleaned, training_4_cleaned, type = "response")
) |>
  roc("actual", "predicted")

plot(f_roc7)
f_roc7$auc

f_roc8 <- tibble(
  actual = training_4_cleaned$Attrition,
  predicted = predict(f8_cleaned, training_4_cleaned, type = "response")
) |>
  roc("actual", "predicted")

plot(f_roc8)
f_roc8$auc


##### Precision & recall (Confusion Matrix)----
#####Confusion Matrix Model 5----
# Check lengths
print(length(f5_cleaned$fitted.values))  # Length of predicted probabilities
print(length(training_4_cleaned[keep_idx, ]$Attrition))  # Length of actual labels

# Create predicted labels using case_when
predicted_labels5 <- case_when(
  f5_cleaned$fitted.values >= 0.5 ~ "1",
  TRUE ~ "0"
) %>%
  factor(levels = c("0", "1"))

# Create reference labels
reference_labels <- training_4_cleaned[keep_idx, ]$Attrition %>%
  factor(levels = c("0", "1"))

# Check lengths again
print(length(predicted_labels5))  # Should match length of reference_labels
print(length(reference_labels))  # Should match length of predicted_labels5

# Generate Confusion Matrix
confusion5 <- confusionMatrix(
  data = predicted_labels5,
  reference = reference_labels
)

# Print the confusion matrix and class-wise statistics
print(confusion5$table)
print(confusion5$byClass)

#####Confusion Matrix Model 6----
# Check lengths
print(length(f6_cleaned$fitted.values))  # Length of predicted probabilities
print(length(training_4_cleaned[keep_idx, ]$Attrition))  # Length of actual labels

# Create predicted labels using case_when
predicted_labels6 <- case_when(
  f6_cleaned$fitted.values >= 0.5 ~ "1",
  TRUE ~ "0"
) %>%
  factor(levels = c("0", "1"))

# Create reference labels
reference_labels <- training_4_cleaned[keep_idx, ]$Attrition %>%
  factor(levels = c("0", "1"))

# Check lengths again
print(length(predicted_labels6))  # Should match length of reference_labels
print(length(reference_labels))  # Should match length of predicted_labels5

# Generate Confusion Matrix
confusion6 <- confusionMatrix(
  data = predicted_labels6,
  reference = reference_labels
)

# Print the confusion matrix and class-wise statistics
print(confusion6$table)
print(confusion6$byClass)

#####Confusion Matrix Model 7----
# Check lengths
print(length(f7_cleaned$fitted.values))  # Length of predicted probabilities
print(length(training_4_cleaned[keep_idx, ]$Attrition))  # Length of actual labels

# Create predicted labels using case_when
predicted_labels7 <- case_when(
  f6_cleaned$fitted.values >= 0.5 ~ "1",
  TRUE ~ "0"
) %>%
  factor(levels = c("0", "1"))

# Create reference labels
reference_labels <- training_4_cleaned[keep_idx, ]$Attrition %>%
  factor(levels = c("0", "1"))

# Check lengths again
print(length(predicted_labels7))  # Should match length of reference_labels
print(length(reference_labels))  # Should match length of predicted_labels5

# Generate Confusion Matrix
confusion7 <- confusionMatrix(
  data = predicted_labels7,
  reference = reference_labels
)

# Print the confusion matrix and class-wise statistics
print(confusion6$table)
print(confusion6$byClass)

#####Confusion Matrix Model 8----
# Check lengths
print(length(f8_cleaned$fitted.values))  # Length of predicted probabilities
print(length(training_4_cleaned[keep_idx, ]$Attrition))  # Length of actual labels

# Create predicted labels using case_when
predicted_labels8 <- case_when(
  f8_cleaned$fitted.values >= 0.5 ~ "1",
  TRUE ~ "0"
) %>%
  factor(levels = c("0", "1"))

# Create reference labels
reference_labels <- training_4_cleaned[keep_idx, ]$Attrition %>%
  factor(levels = c("0", "1"))

# Check lengths again
print(length(predicted_labels8))  # Should match length of reference_labels
print(length(reference_labels))  # Should match length of predicted_labels5

# Generate Confusion Matrix
confusion8 <- confusionMatrix(
  data = predicted_labels8,
  reference = reference_labels
)

# Print the confusion matrix and class-wise statistics
print(confusion8$table)
print(confusion8$byClass)

### Test Model----
test_data1 <- employee_test %>% mutate( Age_squared = Age^2, Income_squared = Income^2 )
# Model 3: f3
preds_test_f3 <- predict(f3_cleaned, test_data1, type = "response")
# Model 5: f5
preds_test_f5 <- predict(f5_cleaned, employee_test, type = "response")
# Model 6:f6
preds_test_f6 <- predict(f6_cleaned, employee_test, type = "response") 
# Model 7: f7 
preds_test_f7 <- predict(f7_cleaned, employee_test, type = "response") 
# Model 8: f8 
preds_test_f8 <- predict(f8_cleaned, employee_test, type = "response") 
# Combine predicted probabilities into a data frame 
predicted_probs_comb <- employee_test %>% 
  select(Age, Income, MaritalStatus, BusinessTravel, NumCompaniesWorked,YearsWithCurrManager) %>%
  mutate( Model3 = preds_test_f3, Model5 = preds_test_f5, Model6 = preds_test_f6, 
                               Model7 = preds_test_f7, Model8 = preds_test_f8 ) 
# Display the predicted probabilities print
print(predicted_probs_comb)

view(predicted_probs_comb)

# Write the data frame to an Excel file
write.xlsx(predicted_probs_comb, file = "Predicted_Probabilites.xlsx", sheetName = "Summary", rowNames = FALSE)

###Plot ROC Curves----
ggroc(list(Model_3 = f_roc3, Model_5 = f_roc5, Model_6 = f_roc6, Model_7 = f_roc7,Model_8 = f_roc8)) +
  ggtitle("ROC Curves for Models 3, 5,6,7 and 8") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "purple", "orange","red","brown"))

### Comparison Between Linear and Logistic Regression ----
# Prepare the data by creating the necessary squared terms and selecting relevant columns
training_6 <- training_6 %>%
  mutate(
    Age_squared = Age^2,
    Income_squared = Income^2
  ) %>%
  select(
    -TotalWorkingYears,
    -StandardHours,
    -JobLevel,
    -DistanceFromHome,
    -Education,
    -EmployeeID,
    -Gender,
    -YearsAtCompany
  )
training_6 <- training_6 %>% mutate(Attrition = ifelse(Attrition == "Yes", 1, 0))
# Fit the linear regression model
linear_model <- lm(
  Attrition ~ . + Age_squared + Income_squared,
  data = training_6[keep_idx, ]
)

# Display model summary
summary(linear_model)

summary_linear_model <- summary(linear_model)
# Convert the summary to a data frame
summary_df <- data.frame(
  Variable = rownames(summary_linear_model$coefficients),
  Estimate = summary_linear_model$coefficients[, "Estimate"],
  Std.Error = summary_linear_model$coefficients[, "Std. Error"],
  t.value = summary_linear_model$coefficients[, "t value"],
  Pr = summary_linear_model$coefficients[, "Pr(>|t|)"]
)

# Write the data frame to an Excel file
write.xlsx(summary_df, file = "model1.xlsx", sheetName = "Summary", rowNames = FALSE)


# let's plot all three
employee_train |>
  ggplot(aes(x = Age, y = Attrition)) + 
  geom_point() + 
  geom_line(aes(y = 1 / (1 + exp(-1 * (coefs3$estimate[1] + coefs3$estimate[2] * Age))))) + 
  geom_line(aes(y = 1.360 - .03685 * Age), color = "blue", lty = 3) +
  ylab("") + 
  labs(
    title = "Propensity to Lose Employee By Age",
    subtitle = "Logistic (black) vs. Linear (blue)"
  )
