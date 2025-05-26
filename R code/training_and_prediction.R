# Import of libraries
library(tidyr)
library(ggplot2)
library(dplyr)
library(caret)

############# Players model ###########################

############# Players model ###########################

# Load data (ensure strings stay character until you convert to factors)
players <- read.csv("data/players.csv", stringsAsFactors = FALSE)

# Drop unneeded cols
players <- players %>%
  select(-short_name, -player_positions, -shooting, -passing)

# Define “Big 6” (corrected spelling)
big6 <- c("England", "Spain", "France", "Portugal", "Brazil", "Argentina")
print(big6)

# Recode nationality into Big6 vs "Others"
players <- players %>%
  mutate(
    nat_group = if_else(nationality %in% big6, nationality, "Others")
  )
print("After recoding nationality into nat_group, head of players:")
print(head(players))
print("Counts per nat_group:")
print(table(players$nat_group))

# Turn that new factor into dummies (drop one to avoid multicollinearity)
dummies <- model.matrix(~ nat_group, data = players)
colnames(dummies) <- gsub("nat_group", "", colnames(dummies))

# Drop one dummy column (e.g., Argentina) to use as reference
# This prevents perfect multicollinearity
if("Argentina" %in% colnames(dummies)) {
  dummies <- dummies[, !colnames(dummies) %in% "Argentina"]
  print("Dropped 'Argentina' dummy - using as reference category")
}

print("Dummy columns created (after dropping reference):")
print(colnames(dummies))
print("Head of dummies:")
print(head(dummies))

# Bind back and drop the old cols
players_final <- bind_cols(
  players %>% select(-nationality, -nat_group),
  as.data.frame(dummies)
)
print("After binding and dropping, head of players_final:")
print(head(players_final))

###################### Train ###################
set.seed(123)

# Split into CV (80%) + Test (20%)
train_idx <- createDataPartition(players_final$value_eur, p = 0.8, list = FALSE)
cv_data   <- players_final[train_idx, ]
test_data <- players_final[-train_idx, ]

print(paste("CV data dimensions:", nrow(cv_data), "x", ncol(cv_data)))
print(paste("Test data dimensions:", nrow(test_data), "x", ncol(test_data)))

# Set up 10-fold CV
cv_ctrl <- trainControl(
  method          = "cv",
  number          = 10,
  verboseIter     = TRUE,          # Shows progress per fold
  savePredictions = "final",
  summaryFunction = defaultSummary
)

# Train the model (ONLY ONCE)
cat("\nStarting 10-fold CV training...\n")
time_taken <- system.time({
  model_lm <- train(
    value_eur ~ .,
    data      = cv_data,
    method    = "lm",
    metric    = "RMSE",
    trControl = cv_ctrl,
    preProcess = c("center", "scale")
  )
})
cat(sprintf("\nTraining completed in %.2f seconds.\n", time_taken["elapsed"]))

# Show CV results
print(model_lm)

# RMSE per fold & visualization
rmse_df <- model_lm$resample %>%
  mutate(Fold = factor(Resample, levels = paste0("Fold", sprintf("%02d", 1:10))))

# Plot RMSE by fold
p1 <- ggplot(rmse_df, aes(x = Fold, y = RMSE)) +
  geom_point(size = 2) +
  geom_line(aes(group = 1), linetype = "dashed") +
  labs(title = "RMSE por fold en Cross Validation",
       x = "Fold",
       y = "RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

# Boxplot of CV RMSE
p2 <- ggplot(rmse_df, aes(y = RMSE)) +
  geom_boxplot() +
  labs(title = "Distribución del RMSE en la Cross Validation",
       y = "RMSE") +
  theme_minimal()

print(p2)

# Final test-set evaluation
cat("\nEvaluating on test set...\n")
pred_test <- predict(model_lm, newdata = test_data)

# Compute test RMSE
test_rmse <- RMSE(pred_test, test_data$value_eur)
cat(sprintf("Test RMSE: %.2f\n", test_rmse))

# Plot Observed vs Predicted on test
p3 <- ggplot(data.frame(observed = test_data$value_eur, predicted = pred_test), 
             aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "maroon", linetype = "dotted") +
  labs(title = "Observados vs. Predicciones: Prueba de Validación",
       x = "obs value_eur",
       y = "pred value_eur") +
  theme_minimal()

print(p3)

# Optional: Check for any remaining issues
cat("\nModel summary:\n")
print(summary(model_lm$finalModel))

# Check for high correlations between predictors
cat("\nChecking for multicollinearity issues...\n")
if(require(car, quietly = TRUE)) {
  vif_values <- vif(model_lm$finalModel)
  high_vif <- vif_values[vif_values > 10]
  if(length(high_vif) > 0) {
    cat("Variables with high VIF (>10):\n")
    print(high_vif)
  } else {
    cat("No severe multicollinearity detected (all VIF < 10)\n")
  }
}