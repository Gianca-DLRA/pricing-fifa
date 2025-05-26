# Fit the GLM with a Gamma distribution and log link
# Getting rid of zeros in value_eur for the Gamma model
players_gamma <- players_final[players_final$value_eur > 0, ]

# Fit the GLM with Gamma
glm_gamma_model <- glm(
  value_eur ~ .,
  data = players_gamma,
  family = Gamma(link = "log")
)

print("-------------------- GLM: Gamma distribution--------------------------")
# Summary of the GLM
summary(glm_gamma_model)

print("-------------------- Gaussian OLS--------------------------")
ols_model <- lm(value_eur ~ ., data = players_gamma) #To fit with the same no. of obs.
summary(ols_model)


# Compare AIC
aic_comparison <- AIC(ols_model, glm_gamma_model)
print(aic_comparison)


# Predictions (in-sample)
ols_preds <- predict(ols_model)
glm_preds <- predict(glm_gamma_model, type = "response")

# Match actuals for GLM subset
actual_gamma <- players_gamma$value_eur

# Plot: Observed vs Predicted
par(mfrow = c(1, 2))  # 2 plots side-by-side

# OLS Plot
plot(players_gamma$value_eur, ols_preds,
     xlab = "obs value_eur", ylab = "pred value_eur",
     main = "OLS: Observaciones vs Predicciones", pch = 16, col = "steelblue")
abline(0, 1, col = "darkgray", lty = 2)

# GLM (Gamma) Plot
plot(actual_gamma, glm_preds,
     xlab = "obs value_eur", ylab = "pred value_eur",
     main = "GLM (Gamma): Observed vs Predicted", pch = 16, col = "darkolivegreen4")
abline(0, 1, col = "darkgray", lty = 2)

# Reset plotting layout
par(mfrow = c(1, 1))