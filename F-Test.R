#F-test:
F_test_model_small_V22 <- lm(F_test_data_V22$scoreDiff ~ 1, data = F_test_data_V22)
F_test_model_large_V22 <- lm(F_test_data_V22$scoreDiff ~ factor(F_test_data_V22$kommisjon), data = F_test_data_V22)

F_test_model_small_V23 <- lm(F_test_data_V23$scoreDiff ~ 1, data = F_test_data_V23)
F_test_model_large_V23 <- lm(F_test_data_V23$scoreDiff ~ factor(F_test_data_V23$kommisjon), data = F_test_data_V23)

summary(F_test_model_large_V22)
summary(F_test_model_large_V23)

anova(F_test_model_small_V22, F_test_model_large_V22)
anova(F_test_model_small_V23, F_test_model_large_V23)

#Presentere resultater veiledningstime 26.09.2024:
summary(F_test_model_large_V22)
summary(F_test_model_large_V23)