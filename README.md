# Employee Attrition Analysis

## Overview
This project focuses on analyzing employee attrition using logistic regression models. The analysis includes data preprocessing, feature engineering, model fitting, and evaluation. Various visualizations and marginal effect calculations provide insights into factors influencing employee attrition.

## Features
- **Data Visualization:**
  - Correlation heatmaps.
  - Attrition trends by marital status, business travel, years with current manager, and number of companies worked.
- **Logistic Regression Models:**
  - Multiple iterations of logistic regression to test different features and improve the model's predictive power.
  - Stepwise regression for feature selection.
  - Diagnostic plots for model validation.
- **Model Evaluation:**
  - Receiver Operating Characteristic (ROC) curves.
  - Area Under Curve (AUC) values for performance metrics.
  - Marginal effects analysis for interpretation of coefficients.

## Key Visualizations
1. **Correlation Heatmap:**
   - Displays the Pearson correlation between variables, including attrition.
   - Custom color gradients for easier interpretation.

2. **Individual Analysis:**
   - Attrition trends by:
     - Marital Status
     - Business Travel
     - Years with Current Manager
     - Number of Companies Worked

3. **Marginal Effects:**
   - Marginal probability of attrition based on variables such as income and age.

## Workflow
1. **Data Preprocessing:**
   - Factorization of categorical variables.
   - Removal of outliers and unnecessary features.
   - Feature engineering (e.g., squared terms for non-linear relationships).

2. **Model Development:**
   - Initial logistic regression (kitchen sink approach).
   - Feature selection via stepwise regression.
   - Improved models with cleaned and transformed data.

3. **Model Evaluation:**
   - Comparison of models using AUC.
   - Validation with new datasets.

4. **Insights:**
   - Analysis of marginal probabilities to determine high-risk groups.

## Notable Code Snippets
### Correlation Heatmap
```r
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
```r
###Logistic Regression Model
f1 <- glm(
  Attrition ~ ., 
  data = training_1[keep_idx, ],
  family = binomial("logit")
)
summary(f1)
plot(f1)

##ROC Curve for Model Validation
roc_validation3 <- roc(
  data = tibble(
    actual = validate_2 %>% select(Attrition) %>% unlist(),
    predicted = preds_validation3
  ), 
  "actual", "predicted"
)
plot(roc_validation3, main = "ROC Curve for Model Validation")
auc_value <- auc(roc_validation3) 
print(paste("AUC:", auc_value))
