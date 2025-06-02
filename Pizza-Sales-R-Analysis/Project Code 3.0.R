install.packages(c("dplyr", "tidyr", "ggplot2", "lpSolve", "arules", "readr", "lubridate", "stringr", "rpart"))
library(dplyr)


data <- read.csv("C:/Users/LH032/OneDrive - Mass General Brigham/Documents/pizza sales doc.csv")

#SUMMARY OF DATA, For personal understanding.
str(data)

summary(data)

sapply(data, function(x) sum(is.na(x)))

colnames(data)

#Data Cleaning by removing columns, and removing missing values

# Drop wasteful columns

cleaned_data <- data %>%
  select(-c(Row.Labels, Count.of))  

cleaned_data$order_date <- as.Date(cleaned_data$order_date, format = "%m/%d/%Y")
cleaned_data$order_time <- format(strptime(cleaned_data$order_time, "%H:%M:%S"), "%H:%M:%S")

# Filter out missing values 
cleaned_data <- cleaned_data %>%
  filter(!is.na(order_date) & !is.na(order_time) & !is.na(total_price))

head(cleaned_data)


#EDA in relation to problem statement

# Frequency of ingrediants 
ingredient_columns <- grep("Ingrediant", names(cleaned_data), value = TRUE)

# data cleaning further
library(tidyr)
ingredients_long <- cleaned_data %>%
  select(all_of(ingredient_columns)) %>%
  pivot_longer(cols = everything(), names_to = "Ingredient_Column", values_to = "Ingredient") %>%
  filter(!is.na(Ingredient))

# Ingredient frequency in new format
ingredient_frequency <- ingredients_long %>%
  group_by(Ingredient) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

# Visualizing ingredient frequency
library(ggplot2)
ggplot(ingredient_frequency, aes(x = reorder(Ingredient, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Ingredient Frequency", x = "Ingredient", y = "Frequency")


top_10_ingredients <- ingredient_frequency %>%
  slice_max(Frequency, n = 10)

# Visualizing the top 10 ingredients
ggplot(top_10_ingredients, aes(x = reorder(Ingredient, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Ingredients by Frequency",
    x = "Ingredient",
    y = "Frequency"
  )

# Ingediants vs Sales 
ingredient_columns <- grep("Ingrediant", names(cleaned_data), value = TRUE)

ingredients_long <- cleaned_data %>%
  select(all_of(ingredient_columns), total_price) %>%
  pivot_longer(cols = all_of(ingredient_columns), names_to = "Ingredient_Column", values_to = "Ingredient") %>%
  filter(!is.na(Ingredient) & Ingredient != "")

ingredient_sales_correlation <- ingredients_long %>%
  group_by(Ingredient) %>%
  summarise(
    Total_Sales = sum(total_price, na.rm = TRUE),
    Frequency = n()
  ) %>%
  arrange(desc(Frequency))

print("Ingredient Sales Correlation:")
head(ingredient_sales_correlation)

ggplot(ingredient_sales_correlation, aes(x = Frequency, y = Total_Sales)) +
  geom_point(color = "red") +
  theme_minimal() +
  labs(
    title = "Correlation Between Ingredient Frequency and Sales",
    x = "Frequency of Ingredient Usage",
    y = "Total Sales"
  )

#Top Ingrediants by Sales

top_ingredients <- ingredient_sales_correlation %>%
  arrange(desc(Total_Sales)) %>%
  slice(1:15)

library(ggplot2)

ggplot(top_ingredients, aes(x = reorder(Ingredient, Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 15 Ingredients by Total Sales",
    x = "Ingredient",
    y = "Total Sales"
  )


#Profitable Ingredients 

ingredient_costs <- data.frame(
  Ingredient = unique(ingredients_long$Ingredient),
  Cost_Per_Unit = runif(length(unique(ingredients_long$Ingredient)), min = 0.5, max = 5)  # Example costs
)

ingredient_profitability <- ingredients_long %>%
  group_by(Ingredient) %>%
  summarise(
    Total_Sales = sum(total_price, na.rm = TRUE),
    Usage_Count = n()
  ) %>%
  inner_join(ingredient_costs, by = "Ingredient") %>%
  mutate(
    Total_Cost = Usage_Count * Cost_Per_Unit,
    Profit = Total_Sales - Total_Cost
  ) %>%
  arrange(desc(Profit))

top_profitable_ingredients <- ingredient_profitability %>%
  slice(1:15)

library(ggplot2)

ggplot(top_profitable_ingredients, aes(x = reorder(Ingredient, Profit), y = Profit)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 15 Most Profitable Ingredients",
    x = "Ingredient",
    y = "Profit (Sales - Cost)"
  )

# Sales by category
ggplot(data, aes(x = pizza_category, y = total_price)) +
  geom_boxplot() +
  theme_minimal()

# Popular sizes
ggplot(data, aes(x = pizza_size)) +
  geom_bar(fill = "blue") +
  theme_minimal()

# Sales by date
data$order_date <- as.Date(data$order_date, format = "%d/%m/%Y")
sales_trend <- data %>%
  group_by(order_date) %>%
  summarise(total_sales = sum(total_price, na.rm = TRUE))

ggplot(sales_trend, aes(x = order_date, y = total_sales)) +
  geom_line() +
  theme_minimal()


# OUTLIER DETECTION

Q1 <- quantile(ingredient_profitability$Profit, 0.25, na.rm = TRUE)
Q3 <- quantile(ingredient_profitability$Profit, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

outliers <- ingredient_profitability %>%
  filter(Profit < (Q1 - 1.5 * IQR) | Profit > (Q3 + 1.5 * IQR))

print("Outliers in Profit:")
print(outliers)

#FEATURE ENGINEERING - Further to what was done in EDA

ingredient_profitability <- ingredient_profitability %>%
  mutate(
    Frequency_Category = ifelse(Usage_Count > median(Usage_Count), "High", "Low"),
    Profit_Cost_Ratio = Profit / Total_Cost
  )

print(head(ingredient_profitability))

# Frequency and Total Sales
ingredient_profitability <- ingredients_long %>%
  group_by(Ingredient) %>%
  summarise(
    Total_Sales = sum(total_price, na.rm = TRUE),
    Frequency = n()
  )

# Ingredient Costs 
ingredient_costs <- data.frame(
  Ingredient = unique(ingredients_long$Ingredient),
  Cost_Per_Unit = runif(length(unique(ingredients_long$Ingredient)), min = 0.5, max = 5)  
)

# Merge Ingredient Costs and Calculate Profit
ingredient_profitability <- ingredient_profitability %>%
  inner_join(ingredient_costs, by = "Ingredient") %>%
  mutate(
    Total_Cost = Frequency * Cost_Per_Unit,
    Profit = Total_Sales - Total_Cost
  )

# Profit Margin
ingredient_profitability <- ingredient_profitability %>%
  mutate(Profit_Margin = Profit / Total_Sales)

# Popularity Score
ingredient_profitability <- ingredient_profitability %>%
  mutate(Popularity_Score = Frequency * Total_Sales)

# Ingredient Density (Cost and Profit Per Unit)
ingredient_profitability <- ingredient_profitability %>%
  mutate(
    Avg_Cost_Per_Unit = Total_Cost / Frequency,
    Avg_Profit_Per_Unit = Profit / Frequency
  )

# Transaction Column for Market Basket Analysis
ingredient_transactions <- cleaned_data %>%
  select(order_id, starts_with("Ingrediant")) %>%
  mutate_all(as.character) %>%
  pivot_longer(cols = starts_with("Ingrediant"), names_to = "Ingredient_Column", values_to = "Ingredient") %>%
  filter(!is.na(Ingredient) & Ingredient != "") %>%
  group_by(order_id) %>%
  summarise(Transaction = paste(Ingredient, collapse = ","))

# Ingredient Dependency
ingredient_dependency <- ingredient_transactions %>%
  separate_rows(Transaction, sep = ",") %>%
  group_by(Transaction) %>%
  summarise(Combination_Count = n()) %>%
  rename(Ingredient = Transaction) %>%
  inner_join(ingredient_profitability, by = "Ingredient")

# Sales Contribution
ingredient_profitability <- ingredient_profitability %>%
  mutate(Sales_Contribution = Total_Sales / sum(Total_Sales))

# Top 10 pizzas by total sales
top_pizzas <- cleaned_data %>%
  group_by(pizza_name) %>%
  summarise(Total_Sales = sum(total_price)) %>%
  arrange(desc(Total_Sales)) %>%
  slice(1:10)

# Combine Transactions and Pizza Names
ingredient_transactions <- cleaned_data %>%
  select(pizza_name, starts_with("Ingrediant")) %>%
  mutate_all(as.character) %>%
  pivot_longer(cols = starts_with("Ingrediant"), names_to = "Ingredient_Column", values_to = "Ingredient") %>%
  filter(!is.na(Ingredient) & Ingredient != "") %>%
  group_by(pizza_name) %>%
  summarise(Transaction = paste(Ingredient, collapse = ","))

# Filter
favorite_ingredients <- ingredient_transactions %>%
  filter(pizza_name %in% top_pizzas$pizza_name) %>%
  separate_rows(Transaction, sep = ",") %>%
  group_by(Transaction) %>%
  summarise(Favorite_Count = n()) %>%
  rename(Ingredient = Transaction)

print("Favorite Ingredients:")
print(head(favorite_ingredients))

# Merge favorite ingredient counts into ingredient_profitability
ingredient_profitability <- ingredient_profitability %>%
  left_join(favorite_ingredients, by = "Ingredient") %>%
  mutate(Favorite_Count = replace_na(Favorite_Count, 0))

print(head(ingredient_profitability))


# Model Developingg
library(glmnet)
library(ggplot2)
library(caret)

ingredient_profitability$Is_Favorite <- ifelse(ingredient_profitability$Favorite_Count > 0, 1, 0)

ingredient_dependency <- ingredients_long %>%
  group_by(Ingredient) %>%
  summarise(Combination_Count = n_distinct(Ingredient_Column), .groups = "drop")

ingredient_profitability <- ingredient_profitability %>%
  left_join(ingredient_dependency, by = "Ingredient")

# Customer Favorites Model
x_favorites <- model.matrix(Is_Favorite ~ Profit_Margin + Popularity_Score + Sales_Contribution + Combination_Count, 
                            data = ingredient_profitability)[, -1]
y_favorites <- ingredient_profitability$Is_Favorite

# Regularized Logistic Regression for Customer Favorites
set.seed(123)
regularized_favorites <- cv.glmnet(x_favorites, y_favorites, family = "binomial", alpha = 0.5)

best_lambda_favorites <- regularized_favorites$lambda.min

print("Coefficients for Regularized Customer Favorites Model:")
print(coef(regularized_favorites, s = best_lambda_favorites))

ingredient_profitability$Predicted_Probabilities_Favorites <- predict(
  regularized_favorites, 
  newx = x_favorites, 
  s = best_lambda_favorites, 
  type = "response"
)

ingredient_profitability$Predicted_Favorites <- ifelse(
  ingredient_profitability$Predicted_Probabilities_Favorites > 0.5, 1, 0
)

# confusion matrix
confusion_favorites <- confusionMatrix(
  factor(ingredient_profitability$Predicted_Favorites),
  factor(ingredient_profitability$Is_Favorite)
)
print("Confusion Matrix for Customer Favorites Model:")
print(confusion_favorites)


# Line graph for predicted probabilities
ggplot(ingredient_profitability, aes(x = Predicted_Probabilities_Favorites, color = as.factor(Is_Favorite))) +
  geom_density(size = 1) +
  theme_minimal() +
  labs(
    title = "Regularized Logistic Regression: Predicted vs. Actual Customer Favorites",
    x = "Predicted Probability",
    y = "Density",
    color = "Actual Favorite (1 = Yes, 0 = No)"
  ) +
  theme(legend.position = "top")

## High vs Low

ingredient_profitability$High_Profit <- ifelse(ingredient_profitability$Profit > median(ingredient_profitability$Profit, na.rm = TRUE), 1, 0)


x_high_profit <- model.matrix(High_Profit ~ Profit_Margin + Popularity_Score + Sales_Contribution + Combination_Count, 
                              data = ingredient_profitability)[, -1]
y_high_profit <- ingredient_profitability$High_Profit

set.seed(123)
regularized_high_profit <- cv.glmnet(x_high_profit, y_high_profit, family = "binomial", alpha = 0.5)

best_lambda_high_profit <- regularized_high_profit$lambda.min

print("Coefficients for Regularized High vs. Low Profitability Model:")
print(coef(regularized_high_profit, s = best_lambda_high_profit))

ingredient_profitability$Predicted_Probabilities_High_Profit <- predict(
  regularized_high_profit, 
  newx = x_high_profit, 
  s = best_lambda_high_profit, 
  type = "response"
)

ingredient_profitability$Predicted_High_Profit <- ifelse(
  ingredient_profitability$Predicted_Probabilities_High_Profit > 0.5, 1, 0
)

# confusion matrix
confusion_high_profit <- confusionMatrix(
  factor(ingredient_profitability$Predicted_High_Profit),
  factor(ingredient_profitability$High_Profit)
)
print("Confusion Matrix for High vs. Low Profitability Model:")
print(confusion_high_profit)

# Bar graph for predicted probabilities
ggplot(ingredient_profitability, aes(x = Predicted_Probabilities_High_Profit, fill = as.factor(High_Profit))) +
  geom_histogram(binwidth = 0.05, position = "dodge", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Regularized Logistic Regression: Predicted vs. Actual High vs. Low Profitability",
    x = "Predicted Probability",
    y = "Frequency",
    fill = "Actual High Profit (1 = Yes, 0 = No)"
  ) +
  theme(legend.position = "top")


# Linear Regression: Predicting Profit

colnames(ingredient_profitability)
head(ingredient_profitability)

# Usage_Count 
ingredient_usage <- ingredients_long %>%
  group_by(Ingredient) %>%
  summarise(Usage_Count = n())
# Merge Usage_Count into ingredient_profitability
ingredient_profitability <- ingredient_profitability %>%
  left_join(ingredient_usage, by = "Ingredient")


linear_model <- lm(
  Profit ~ Popularity_Score + Avg_Cost_Per_Unit + Usage_Count + Sales_Contribution,
  data = ingredient_profitability
)

summary(linear_model)

# Linear Regressin: Predicting Ingredient Demand

demand_model <- lm(
  Usage_Count ~ Popularity_Score + Combination_Count + Sales_Contribution,
  data = ingredient_profitability
)

summary(demand_model)


# KMeans for Ingredients 

clustering_data <- ingredient_profitability %>%
  select(Profit_Margin, Avg_Profit_Per_Unit, Sales_Contribution) %>%
  na.omit()

set.seed(123)
kmeans_model <- kmeans(clustering_data, centers = 3)

ingredient_profitability$Cluster <- kmeans_model$cluster

ggplot(ingredient_profitability, aes(x = Profit_Margin, y = Avg_Profit_Per_Unit, color = as.factor(Cluster))) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "K-Means Clustering of Ingredients",
    x = "Profit Margin",
    y = "Average Profit Per Unit",
    color = "Cluster"
  )
# each cluster insight
ingredient_profitability$Cluster <- kmeans_model$cluster  # Use the variable from the first K-Means code

ingredients_first_kmeans <- ingredient_profitability %>%
  select(Ingredient, Cluster) %>%
  arrange(Cluster)

print(ingredients_first_kmeans)

write.csv(ingredients_first_kmeans, "ingredients_first_kmeans_clusters.csv", row.names = FALSE)

# KMeans: Ingredients by Customer Preference 

preference_data <- ingredient_profitability %>%
  select(Favorite_Count, Profit_Margin) %>%
  na.omit()

set.seed(123)
preference_kmeans <- kmeans(preference_data, centers = 3)

ingredient_profitability$Preference_Cluster <- preference_kmeans$cluster

ggplot(ingredient_profitability, aes(x = Favorite_Count, y = Profit_Margin, color = as.factor(Preference_Cluster))) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Clustering Ingredients by Customer Preference and Profit Margin",
    x = "Favorite Count",
    y = "Profit Margin",
    color = "Cluster"
  )
# cluster summary
cluster_summary <- ingredient_profitability %>%
  group_by(Preference_Cluster) %>%
  summarise(
    Avg_Favorite_Count = mean(Favorite_Count, na.rm = TRUE),
    Avg_Profit_Margin = mean(Profit_Margin, na.rm = TRUE),
    Ingredient_Count = n()
  )
print(cluster_summary)

# each cluster insight

ingredients_in_clusters <- ingredient_profitability %>%
  select(Ingredient, Preference_Cluster) %>%
  arrange(Preference_Cluster)

print(ingredients_in_clusters)
# to view all cluster breakdown
write.csv(ingredients_in_clusters, "ingredients_in_clusters.csv", row.names = FALSE)

# Market Basket: Dependancy

#  Transactions
ingredient_transactions <- cleaned_data %>%
  select(starts_with("Ingrediant")) %>%
  mutate_all(as.character) %>%  # Ensure all ingredient values are character
  unite("Transaction", everything(), sep = ",", remove = TRUE) %>%  # Combine into a single column
  filter(Transaction != "")  # Remove empty transactions if any

transaction_list <- strsplit(ingredient_transactions$Transaction, ",")

library(arules)
transactions <- as(transaction_list, "transactions")

# market basket fr
rules <- apriori(
  transactions,
  parameter = list(supp = 0.005, conf = 0.4, minlen = 2)  # Adjusted thresholds
)

if (length(rules) == 0) {
  stop("No rules generated. Consider lowering support or confidence thresholds.")
}

rules_df <- as(rules, "data.frame")

rules_df$lhs <- as.character(labels(lhs(rules)))
rules_df$rhs <- as.character(labels(rhs(rules)))

top_rules_df <- rules_df %>%
  arrange(desc(lift)) %>%
  slice(1:10)

library(ggplot2)

ggplot(top_rules_df, aes(x = reorder(lhs, lift), y = lift, fill = rhs)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Association Rules by Lift",
    x = "Rule Antecedent (lhs)",
    y = "Lift",
    fill = "Rule Consequent (rhs)"
  )


# Masket Basket: Ingredients driving top sales

top_pizza_transactions <- cleaned_data %>%
  filter(pizza_name %in% top_pizzas$pizza_name) %>%
  select(starts_with("Ingrediant")) %>%
  mutate_all(as.character) %>%
  unite("Transaction", everything(), sep = ",", remove = TRUE)

top_transactions <- as(split(unlist(strsplit(top_pizza_transactions$Transaction, ",")), 
                             rep(1:nrow(top_pizza_transactions), lengths(strsplit(top_pizza_transactions$Transaction, ",")))), "transactions")

top_rules <- apriori(
  top_transactions,
  parameter = list(supp = 0.01, conf = 0.5, minlen = 2)
)

inspect(sort(top_rules, by = "lift")[1:10])

ggplot(top_10_rules, aes(x = reorder(lhs, lift), y = lift, fill = rhs)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Association Rules by Top Pizzas",
    x = "Rule Antecedent (lhs)",
    y = "Lift",
    fill = "Rule Consequent (rhs)"
    
#Decision Tree

library (rpart)
library(rpart.plot)

profit_tree <- rpart(
  Profit ~ Profit_Margin + Popularity_Score + Sales_Contribution + Avg_Profit_Per_Unit + Favorite_Count,
  data = ingredient_profitability,
  method = "anova"
)

rpart.plot(profit_tree, main = "Decision Tree for Ingredient Profitability")


# Output Analysis

# Logistic Regression 1

# True labels and predicted probabilities
y_true <- ingredient_profitability$Is_Favorite
y_pred_prob <- ingredient_profitability$Predicted_Probabilities_Favorites
y_pred_class <- ingredient_profitability$Predicted_Favorites

# Accuracy
accuracy <- mean(y_pred_class == y_true)
print(paste("Accuracy:", accuracy))

# Precision, Recall, F1 Score
library(caret)
confusion_favorites <- confusionMatrix(factor(y_pred_class), factor(y_true))
precision <- confusion_favorites$byClass["Precision"]
recall <- confusion_favorites$byClass["Recall"]
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))

# ROC AUC
library(pROC)
roc_curve <- roc(y_true, y_pred_prob)
roc_auc <- as.numeric(auc(roc_curve))

print(paste("ROC AUC:", roc_auc))

# PR AUC
library(PRROC)
pr_curve <- pr.curve(scores.class0 = y_pred_prob, weights.class0 = y_true)
pr_auc <- pr_curve$auc.integral
print(paste("PR AUC:", pr_auc))

# Store metrics
results <- data.frame(
  Model = "Logistic Regression",
  Accuracy = accuracy,
  Precision = precision,
  Recall = recall,
  F1_Score = f1_score,
  ROC_AUC = roc_auc,
  PR_AUC = pr_auc
)

print(results)

#Logistic Regression 2

# True labels and predictions
y_true <- ingredient_profitability$High_Profit
y_pred_prob <- ingredient_profitability$Predicted_Probabilities_High_Profit
y_pred_class <- ingredient_profitability$Predicted_High_Profit

# Accuracy
accuracy <- mean(y_pred_class == y_true)
print(paste("Accuracy:", accuracy))

# Precision, Recall, F1 Score
library(caret)
confusion_high_profit <- confusionMatrix(factor(y_pred_class), factor(y_true))
precision <- confusion_high_profit$byClass["Precision"]
recall <- confusion_high_profit$byClass["Recall"]
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))

# ROC AUC
library(pROC)
roc_curve <- roc(y_true, y_pred_prob)
roc_auc <- as.numeric(auc(roc_curve))
print(paste("ROC AUC:", roc_auc))

# PR AUC
library(PRROC)
pr_curve <- pr.curve(scores.class0 = y_pred_prob, weights.class0 = y_true)
pr_auc <- pr_curve$auc.integral
print(paste("PR AUC:", pr_auc))

# Store results
results_high_profit <- data.frame(
  Model = "High vs. Low Profitability Logistic Regression",
  Accuracy = accuracy,
  Precision = precision,
  Recall = recall,
  F1_Score = f1_score,
  ROC_AUC = roc_auc,
  PR_AUC = pr_auc
)

print(results_high_profit)

# Visualization
results_long <- tidyr::pivot_longer(results_high_profit, cols = -Model, names_to = "Metric", values_to = "Value")

ggplot(results_long, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "High vs. Low Profitability Logistic Regression Metrics", x = "Metric", y = "Value")






# ChatGPTed Confusion Matrix Code
# Load library for confusion matrix
library(caret)

# Split data for Logistic Regression: High vs. Low Profitability
logistic_pred <- predict(logistic_model, type = "response")
logistic_pred_class <- ifelse(logistic_pred > 0.5, 1, 0)

confusion_profit <- confusionMatrix(
  factor(logistic_pred_class, levels = c(0, 1)),
  factor(ingredient_profitability$High_Profit, levels = c(0, 1))
)

print("Confusion Matrix: High vs. Low Profitability")
print(confusion_profit)

# Split data for Logistic Regression: Customer Favorites
favorite_pred <- predict(favorite_model, type = "response")
favorite_pred_class <- ifelse(favorite_pred > 0.5, 1, 0)

confusion_favorite <- confusionMatrix(
  factor(favorite_pred_class, levels = c(0, 1)),
  factor(ingredient_profitability$Is_Favorite, levels = c(0, 1))
)

print("Confusion Matrix: Customer Favorites")
print(confusion_favorite)


# Linear Regression 

# Predicting Profit
# Predictions
predicted_profit <- predict(linear_model, newdata = ingredient_profitability)

# True values
true_profit <- ingredient_profitability$Profit

# R-squared
r_squared <- summary(linear_model)$r.squared

# MAE, MSE, RMSE
mae <- mean(abs(predicted_profit - true_profit))
mse <- mean((predicted_profit - true_profit)^2)
rmse <- sqrt(mse)

# Print metrics
cat("Metrics for Predicting Profit:\n")
cat("R-squared:", r_squared, "\n")
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")

# Store Results
results_profit <- data.frame(
  Model = "Predicting Profit (Linear Regression)",
  R_squared = r_squared,
  MAE = mae,
  MSE = mse,
  RMSE = rmse
)

print(results_profit)

# Visualization: Predicted vs. Actual
library(ggplot2)

ggplot(ingredient_profitability, aes(x = true_profit, y = predicted_profit)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Predicted vs. Actual Profit",
    x = "Actual Profit",
    y = "Predicted Profit"
  )

# Linear Regression 2

# Predicting Ingredient Demand
# Predictions
predicted_demand <- predict(demand_model, newdata = ingredient_profitability)

# True values
true_demand <- ingredient_profitability$Usage_Count

# R-squared
r_squared_demand <- summary(demand_model)$r.squared

# MAE, MSE, RMSE
mae_demand <- mean(abs(predicted_demand - true_demand))
mse_demand <- mean((predicted_demand - true_demand)^2)
rmse_demand <- sqrt(mse_demand)

# Print metrics
cat("Metrics for Predicting Ingredient Demand:\n")
cat("R-squared:", r_squared_demand, "\n")
cat("MAE:", mae_demand, "\n")
cat("MSE:", mse_demand, "\n")
cat("RMSE:", rmse_demand, "\n")

# Store Results
results_demand <- data.frame(
  Model = "Predicting Ingredient Demand (Linear Regression)",
  R_squared = r_squared_demand,
  MAE = mae_demand,
  MSE = mse_demand,
  RMSE = rmse_demand
)

print(results_demand)

# Visualization: Predicted vs. Actual
ggplot(ingredient_profitability, aes(x = true_demand, y = predicted_demand)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Predicted vs. Actual Ingredient Demand",
    x = "Actual Demand",
    y = "Predicted Demand"
  )

# Kmeans 1

# Load required libraries
library(caret)
library(pROC)
library(cluster)
library(dplyr)

# Check if True_Cluster (ground truth labels) exists in the dataset
if ("True_Cluster" %in% colnames(ingredient_profitability)) {
  # Ground Truth Labels Available
  cat("Using Ground Truth Labels for Cluster Evaluation\n")
  
  # Confusion Matrix
  conf_matrix <- confusionMatrix(
    factor(ingredient_profitability$Cluster),
    factor(ingredient_profitability$True_Cluster)
  )
  
  # Precision, Recall, F1 Score
  precision <- conf_matrix$byClass["Precision"]
  recall <- conf_matrix$byClass["Recall"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # ROC AUC (One-vs-Rest)
  roc_curve <- multiclass.roc(ingredient_profitability$True_Cluster, ingredient_profitability$Cluster)
  roc_auc <- auc(roc_curve)
  
  # Print Metrics
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1 Score:", f1_score, "\n")
  cat("ROC AUC:", roc_auc, "\n")
  
} else {
  # No Ground Truth Labels
  cat("Using Internal Metrics for Cluster Evaluation\n")
  
  # Prepare clustering data
  clustering_data <- ingredient_profitability %>%
    select(Profit_Margin, Avg_Profit_Per_Unit, Sales_Contribution) %>%
    na.omit()
  
  # Silhouette Score
  silhouette_scores <- silhouette(ingredient_profitability$Cluster, dist(clustering_data))
  avg_silhouette <- mean(silhouette_scores[, 3])
  
  # Within-Cluster Sum of Squares (WCSS)
  wcss <- sum(kmeans(clustering_data, centers = max(ingredient_profitability$Cluster))$withinss)
  
  # Print Metrics
  cat("Average Silhouette Score:", avg_silhouette, "\n")
  cat("Within-Cluster Sum of Squares (WCSS):", wcss, "\n")
}

# kmeans 2
# Load required libraries
library(caret)
library(cluster)
library(ggplot2)
library(dplyr)

# Check if ground truth labels (e.g., True_Cluster) exist
if ("True_Cluster" %in% colnames(ingredient_profitability)) {
  # Ground Truth Labels Available
  cat("Using Ground Truth Labels for Cluster Evaluation\n")
  
  # Confusion Matrix
  conf_matrix <- confusionMatrix(
    factor(ingredient_profitability$Preference_Cluster),
    factor(ingredient_profitability$True_Cluster)
  )
  
  # Precision, Recall, F1 Score
  precision <- conf_matrix$byClass["Precision"]
  recall <- conf_matrix$byClass["Recall"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # ROC AUC (One-vs-Rest)
  library(pROC)
  roc_curve <- multiclass.roc(ingredient_profitability$True_Cluster, ingredient_profitability$Preference_Cluster)
  roc_auc <- auc(roc_curve)
  
  # Print Metrics
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1 Score:", f1_score, "\n")
  cat("ROC AUC:", roc_auc, "\n")
  
} else {
  # No Ground Truth Labels
  cat("Using Internal Metrics for Cluster Evaluation\n")
  
  # Prepare clustering data
  clustering_data <- ingredient_profitability %>%
    select(Favorite_Count, Profit_Margin) %>%
    na.omit()
  
  # Silhouette Score
  silhouette_scores <- silhouette(ingredient_profitability$Preference_Cluster, dist(clustering_data))
  avg_silhouette <- mean(silhouette_scores[, 3])
  
  # Within-Cluster Sum of Squares (WCSS)
  wcss <- sum(preference_kmeans$withinss)
  
  # Print Metrics
  cat("Average Silhouette Score:", avg_silhouette, "\n")
  cat("Within-Cluster Sum of Squares (WCSS):", wcss, "\n")
}

# Cluster Summary: Preference Clusters
preference_cluster_summary <- ingredient_profitability %>%
  group_by(Preference_Cluster) %>%
  summarise(
    Avg_Favorite_Count = mean(Favorite_Count, na.rm = TRUE),
    Avg_Profit_Margin = mean(Profit_Margin, na.rm = TRUE),
    Ingredient_Count = n()
  )

cat("Preference Cluster Summary:\n")
print(preference_cluster_summary)

# Ingredients by Preference Cluster
ingredients_in_clusters <- ingredient_profitability %>%
  select(Ingredient, Preference_Cluster) %>%
  arrange(Preference_Cluster)

cat("Ingredients by Preference Cluster:\n")
print(ingredients_in_clusters)


# market basket analysis

# Market basket 1
library(arules)
library(ggplot2)

# Prepare transactions
ingredient_transactions <- cleaned_data %>%
  select(starts_with("Ingrediant")) %>%
  mutate_all(as.character) %>%  # Ensure all ingredient values are character
  unite("Transaction", everything(), sep = ",", remove = TRUE) %>%  # Combine into a single column
  filter(Transaction != "")  # Remove empty transactions if any

transaction_list <- strsplit(ingredient_transactions$Transaction, ",")
transactions <- as(transaction_list, "transactions")

# Generate rules
rules <- apriori(
  transactions,
  parameter = list(supp = 0.005, conf = 0.4, minlen = 2)
)

# Stop if no rules generated
if (length(rules) == 0) {
  stop("No rules generated. Consider lowering support or confidence thresholds.")
}

# Convert rules to a dataframe
rules_df <- as(rules, "data.frame")
rules_df$lhs <- as.character(labels(lhs(rules)))
rules_df$rhs <- as.character(labels(rhs(rules)))

# Extract top rules by Lift
top_rules_df <- rules_df %>%
  arrange(desc(lift)) %>%
  slice(1:10)

# Visualization of Top Rules by Lift
ggplot(top_rules_df, aes(x = reorder(lhs, lift), y = lift, fill = rhs)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Association Rules by Lift",
    x = "Rule Antecedent (lhs)",
    y = "Lift",
    fill = "Rule Consequent (rhs)"
  )

# Print metrics for top rules
print("Top 10 Association Rules:")
print(top_rules_df[, c("lhs", "rhs", "support", "confidence", "lift")])


# Market Basket 2

# Prepare transactions for top pizzas
top_pizza_transactions <- cleaned_data %>%
  filter(pizza_name %in% top_pizzas$pizza_name) %>%
  select(starts_with("Ingrediant")) %>%
  mutate_all(as.character) %>%
  unite("Transaction", everything(), sep = ",", remove = TRUE)

# Create transaction list
top_transactions <- as(
  split(
    unlist(strsplit(top_pizza_transactions$Transaction, ",")),
    rep(1:nrow(top_pizza_transactions), lengths(strsplit(top_pizza_transactions$Transaction, ",")))
  ),
  "transactions"
)

# Generate rules for top pizzas
top_rules <- apriori(
  top_transactions,
  parameter = list(supp = 0.01, conf = 0.5, minlen = 2)
)

# Extract top rules by Lift
top_rules_df <- as(top_rules, "data.frame")
top_rules_df$lhs <- as.character(labels(lhs(top_rules)))
top_rules_df$rhs <- as.character(labels(rhs(top_rules)))

# Filter top 10 rules by Lift
top_10_rules <- top_rules_df %>%
  arrange(desc(lift)) %>%
  slice(1:10)

# Visualization of Top Rules for Top Pizzas
ggplot(top_10_rules, aes(x = reorder(lhs, lift), y = lift, fill = rhs)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Association Rules by Lift for Top Pizzas",
    x = "Rule Antecedent (lhs)",
    y = "Lift",
    fill = "Rule Consequent (rhs)"
  )

# Print metrics for top rules
print("Top 10 Association Rules for Top Pizzas:")
print(top_10_rules[, c("lhs", "rhs", "support", "confidence", "lift")])


# Decision Tree
# Load Required Libraries
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(PRROC)
library(ggplot2)
library(tidyr)

# Prepare the Target Variable: High vs. Low Profit
ingredient_profitability$High_Profit <- ifelse(
  ingredient_profitability$Profit > median(ingredient_profitability$Profit, na.rm = TRUE),
  1, 0
)

# Build Decision Tree for Classification
set.seed(123)
profit_tree <- rpart(
  High_Profit ~ Profit_Margin + Popularity_Score + Sales_Contribution + Avg_Profit_Per_Unit + Favorite_Count,
  data = ingredient_profitability,
  method = "class"
)

# Visualize the Decision Tree
rpart.plot(profit_tree, main = "Decision Tree for High vs. Low Profit")

# Predictions
ingredient_profitability$Predicted_Probabilities <- predict(profit_tree, type = "prob")[, 2]  # Probability of class 1
ingredient_profitability$Predicted_Classes <- ifelse(
  ingredient_profitability$Predicted_Probabilities > 0.5, 1, 0
)

# True Labels
y_true <- ingredient_profitability$High_Profit

# Evaluate Metrics

# Accuracy
accuracy <- mean(ingredient_profitability$Predicted_Classes == y_true)

# Confusion Matrix for Precision, Recall, F1-Score
conf_matrix <- confusionMatrix(
  factor(ingredient_profitability$Predicted_Classes),
  factor(y_true)
)
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- 2 * (precision * recall) / (precision + recall)

# ROC AUC
roc_curve <- roc(y_true, ingredient_profitability$Predicted_Probabilities)
roc_auc <- as.numeric(auc(roc_curve))  # Convert to numeric

# PR AUC
pr_curve <- pr.curve(
  scores.class0 = ingredient_profitability$Predicted_Probabilities,
  weights.class0 = y_true
)
pr_auc <- pr_curve$auc.integral

# Print Metrics
cat("Decision Tree Metrics:\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
cat("ROC AUC:", roc_auc, "\n")
cat("PR AUC:", pr_auc, "\n")

# Store Results in a Dataframe
results_tree <- data.frame(
  Model = "Decision Tree (High vs. Low Profit)",
  Accuracy = accuracy,
  Precision = precision,
  Recall = recall,
  F1_Score = f1_score,
  ROC_AUC = roc_auc,
  PR_AUC = pr_auc
)

print(results_tree)

# Visualize Results
results_long <- pivot_longer(results_tree, cols = -Model, names_to = "Metric", values_to = "Value")

ggplot(results_long, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Decision Tree Metrics", x = "Metric", y = "Value")
