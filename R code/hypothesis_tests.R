################################## HYPOTHESIS TESTS ##################################
head(players_final)

############### First test: ANOVA Test for the nationalities ########################

library(dplyr)
# Full model: Includes player metrics + nationality dummies
full_model <- lm(value_eur ~ overall + pace + dribbling + defending + physic +
                   Brazil + England + France + Others + Portugal + Spain,
                 data = players_final)

# Reduced model: Includes only player metrics
reduced_model <- lm(value_eur ~ overall + pace + dribbling + defending + physic,
                    data = players_final)

# Perform the F-test (nested model comparison)
anova_result <- anova(reduced_model, full_model)

# Print the result
print(anova_result)

