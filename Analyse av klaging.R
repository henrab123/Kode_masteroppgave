# Load libraries
library(ggplot2)
library(dplyr)

# Adding the estimates of gamma to the data.frames:
KLAGE_V22$gamma <- estimated_parameters_V22_m1$gamma[,"Estimate"]
KLAGE_V23$gamma <- estimated_parameters_V23_m1$gamma[,"Estimate"]

KLAGE_V22$s <- estimated_parameters_V22_m1$s[,"Estimate"][as.numeric(factor(KLAGE_V22$kommisjon))]
KLAGE_V23$s <- estimated_parameters_V23_m1$s[,"Estimate"][as.numeric(factor(KLAGE_V23$kommisjon))]

KLAGE_V22$Year <- char(2022)
KLAGE_V23$Year <- char(2023)

KLAGE_Kombinert <- bind_rows(KLAGE_V22, KLAGE_V23)
KLAGE_Kombinert_UtenA$Year <- as.factor(KLAGE_Kombinert_UtenA$Year)

KLAGE_V22_UtenA <- KLAGE_V22 %>% filter(finalGradeLabel != "A")
KLAGE_V23_UtenA <- KLAGE_V23 %>% filter(finalGradeLabel != "A")
KLAGE_Kombinert_UtenA <- KLAGE_Kombinert %>% filter(finalGradeLabel != "A")

#####################
### Visualization ###
#####################


ggplot(KLAGE_Kombinert_UtenA, aes(x = gamma, y = totalScore, color = Year)) +
  geom_point(size = 4, alpha = 0.8, shape = 16) +
  scale_color_manual(
    values = c("2022" = "#1f77b4", "2023" = "#ff7f0e"),
    labels = c("\u00c5r 2022", "\u00c5r 2023")
  ) +
  ggtitle((expression("Sammenheng mellom " ~ gamma ~ " og " ~ "total poengsum"))) + 
  labs(
    x = expression(gamma),
    y = "Total poengsum",
    color = "\u00c5rstall"
  ) +
  theme_minimal(base_size = 26) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.05, 0.95),
    legend.box.background = element_rect(color = "grey80"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 40)
  )

######################################
### TESTING COVARIATE SIGNIFICANCE ###
######################################

datasets <- list(KLAGE_V22_UtenA, KLAGE_V23_UtenA, KLAGE_Kombinert_UtenA)
dataset_names <- c("KLAGE_V22_UtenA", "KLAGE_V23_UtenA", "KLAGE_Kombinert_UtenA")
vars <- c("s", "gamma", "finalGradeLabel", "totalScore")

#Loopiong over data
for (j in seq_along(datasets)) {
  df <- datasets[[j]]
  cat("\nResultater for datasett:", dataset_names[j], "\n")
  
  p_values <- numeric(length(vars))
  
  for (i in seq_along(vars)) {
    formula_full <- as.formula(paste("klagde ~", vars[i]))
    formula_null <- as.formula("klagde ~ 1")
    
    model_full <- glm(formula_full, data = df, family = binomial)
    model_null <- glm(formula_null, data = df, family = binomial)
    
    #LRT
    lr_test <- anova(model_null, model_full, test = "Chisq")
    
    #p_value
    p_values[i] <- lr_test$`Pr(>Chi)`[2]
  }
  
  #Printing
  result <- data.frame(Variable = vars, P_value = p_values)
  print(result)
}

#Year as well:
model_full <- glm(klagde ~ Year, data = KLAGE_Kombinert_UtenA, family = binomial)
model_null <- glm(klagde ~1, data = KLAGE_Kombinert_UtenA, family = binomial)

#LRT
lr_test <- anova(model_null, model_full, test = "Chisq")
lr_test$`Pr(>Chi)`[2]  

#s to test as well:
model_full <- glm(klagde ~ s, data = KLAGE_Kombinert_UtenA, family = binomial)
model_null <- glm(klagde ~1, data = KLAGE_Kombinert_UtenA, family = binomial)

#LRT
lr_test <- anova(model_null, model_full, test = "Chisq")
lr_test$`Pr(>Chi)`[2]  
#############################
### V22 - MODEL SELECTION ###
#############################

# Full model:
full_model_V22 <- glm(klagde ~ (gamma + finalGradeLabel + totalScore)^2, data = KLAGE_V22_UtenA, family = binomial)

# Model selection AIC:
best_model_AIC_V22 <- step(full_model_V22, direction = "both")

# Model selection BIC:
best_model_BIC_V22 <- step(full_model_V22, direction = "both", k = log(nrow(KLAGE_V22_UtenA)))

# Best Models by BIC: 
model_V22_1 <- glm(klagde ~ gamma + finalGradeLabel + totalScore, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_1)

model_V22_2 <- glm(klagde ~ finalGradeLabel + totalScore, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_2)

model_V22_3 <- glm(klagde ~ gamma + finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_3)

#############################
### V23 - MODEL SELECTION ###
#############################

# Full model:
full_model_V23 <- glm(klagde ~ (gamma + finalGradeLabel + totalScore)^2, data = KLAGE_V23_UtenA, family = binomial)

# Model selection AIC:
best_model_AIC_V23 <- step(full_model_V23, direction = "both")

# Model selection BIC:
best_model_BIC_V23 <- step(full_model_V23, direction = "both", k = log(nrow(KLAGE_V23_UtenA)))

# Best Models by BIC: 
model_V23_1 <- glm(klagde ~ totalScore + finalGradeLabel, data = KLAGE_V23_UtenA, family = binomial)
summary(model_V23_1)

model_V23_2 <- glm(klagde ~ gamma + finalGradeLabel, data = KLAGE_V23_UtenA, family = binomial)
summary(model_V23_2)

##############################
### VCMB - MODEL SELECTION ###
##############################

# Full model:
full_model_Kombinert <- glm(klagde ~ (gamma + finalGradeLabel + totalScore + Year)^2, data = KLAGE_Kombinert_UtenA, family = binomial)

# Model selection AIC:
best_model_AIC_Kombinert <- step(full_model_Kombinert, direction = "both")

# Model selection BIC:
best_model_BIC_Kombinert <- step(full_model_Kombinert, direction = "both", k = log(nrow(KLAGE_Kombinert_UtenA)))

# Best Models by BIC: 
model_VCMB_1 <- glm(klagde ~ totalScore + finalGradeLabel + Year, data = KLAGE_Kombinert_UtenA, family = binomial)
step(model_VCMB_1)

model_VCMB_2 <- glm(klagde ~ gamma + finalGradeLabel + Year, data = KLAGE_Kombinert_UtenA, family = binomial)
step(model_VCMB_2)






model_with_interaction <- glm(klagde ~ gamma + finalGradeLabel + Year + gamma:finalGradeLabel, data = KLAGE_Kombinert_UtenA, family = binomial)

model_no_interaction <- glm(klagde ~ gamma + finalGradeLabel + Year, data = KLAGE_Kombinert_UtenA, family = binomial)

anova(model_no_interaction, model_with_interaction, test = "Chisq")



modelVCMB_2 <- glm(klagde ~ totalScore + finalGradeLabel + Year, data = KLAGE_Kombinert_UtenA, family = binomial)
summary(modelVCMB_2)

































model_V22 <- glm(klagde ~ gamma + finalGradeLabel, data = KLAGE_V22, family = binomial)
summary(model_V22)

model_V22_utenA <- glm(klagde ~ gamma + finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_utenA)

model_V22_utenA_medstuff <- glm(klagde ~ gamma + finalGradeLabel + gamma*finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_utenA_medstuff)


#####################################
### LOGISTISK REG ~ s, FinalGrade ###
#####################################

model_V22_utenA_Sbare <- glm(klagde ~ s, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_utenA_Sbare)

model_V22_utenA_S <- glm(klagde ~ s + finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_utenA_S)

model_V22_utenA_medstuff_S <- glm(klagde ~ s + finalGradeLabel + s*finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_utenA_medstuff_S)

#####################################
### LOGISTISK REG ~ s, FinalGrade ###
#####################################

# Full model
full_model <- glm(klagde ~ (s + gamma + finalGradeLabel + totalScore)^2, data = KLAGE_V22_UtenA, family = binomial)

# Stepwise selection
best_model <- step(full_model)

Nesten_optimal_modell <- glm(klagde ~  s + gamma + finalGradeLabel + totalScore, data = KLAGE_V22_UtenA, family = binomial)
summary(Nesten_optimal_modell)

Optimal_modell <- glm(klagde ~ gamma + finalGradeLabel + totalScore, data = KLAGE_V22_UtenA, family = binomial)
summary(Optimal_modell)

Optimal_modellA <- glm(klagde ~ gamma + finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(Optimal_modellA)

Optimal_modellB <- glm(klagde ~ finalGradeLabel + totalScore, data = KLAGE_V22_UtenA, family = binomial)
summary(Optimal_modellB)

Null_modellB <- glm(klagde ~ 1, data = KLAGE_V22_UtenA, family = binomial)
summary(Null_modellB)

anova(Null_modellB, Optimal_modellB, test = "Chisq")

####################
### LARGER MODEL ###
####################

# Full model
full_model <- glm(klagde ~ (s + gamma + finalGradeLabel + totalScore)^2, data = KLAGE_V22_UtenA, family = binomial)

# Stepwise selection
best_model <- step(full_model)

####################
### ............ ###
####################


model_V22_utenA <- glm(klagde ~ gamma + finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_utenA)

# Plot logistic regression curve
ggplot(KLAGE_V22_UtenA, aes(x = gamma, y = klagde)) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.5) +  # Scatter points
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +  # Logistic curve
  labs(title = "Probability of Klage as a function of Gamma",
       x = "Gamma", y = "Probability of Klage") +
  theme_minimal()


KLAGE_V22_A <- KLAGE_V22 %>% filter(finalGradeLabel == "A")
KLAGE_V22_B <- KLAGE_V22 %>% filter(finalGradeLabel == "B")
KLAGE_V22_C <- KLAGE_V22 %>% filter(finalGradeLabel == "C")
KLAGE_V22_D <- KLAGE_V22 %>% filter(finalGradeLabel == "D")
KLAGE_V22_E <- KLAGE_V22 %>% filter(finalGradeLabel == "E")
KLAGE_V22_F <- KLAGE_V22 %>% filter(finalGradeLabel == "F")

model_V22_A <- glm(klagde ~ gamma, data = KLAGE_V22_A, family = binomial)
model_V22_B <- glm(klagde ~ gamma, data = KLAGE_V22_B, family = binomial)
model_V22_C <- glm(klagde ~ gamma, data = KLAGE_V22_C, family = binomial)
model_V22_D <- glm(klagde ~ gamma, data = KLAGE_V22_D, family = binomial)
model_V22_E <- glm(klagde ~ gamma, data = KLAGE_V22_E, family = binomial)
model_V22_F <- glm(klagde ~ gamma, data = KLAGE_V22_F, family = binomial)

summary(model_V22_A)
summary(model_V22_B)
summary(model_V22_C)
summary(model_V22_D)
summary(model_V22_E)
summary(model_V22_F)


KLAGE_V23_A <- KLAGE_V23 %>% filter(finalGradeLabel == "A")
KLAGE_V23_B <- KLAGE_V23 %>% filter(finalGradeLabel == "B")
KLAGE_V23_C <- KLAGE_V23 %>% filter(finalGradeLabel == "C")
KLAGE_V23_D <- KLAGE_V23 %>% filter(finalGradeLabel == "D")
KLAGE_V23_E <- KLAGE_V23 %>% filter(finalGradeLabel == "E")
KLAGE_V23_F <- KLAGE_V23 %>% filter(finalGradeLabel == "F")

model_V23_A <- glm(klagde ~ gamma, data = KLAGE_V23_A, family = binomial)
model_V23_B <- glm(klagde ~ gamma, data = KLAGE_V23_B, family = binomial)
model_V23_C <- glm(klagde ~ gamma, data = KLAGE_V23_C, family = binomial)
model_V23_D <- glm(klagde ~ gamma, data = KLAGE_V23_D, family = binomial)
model_V23_E <- glm(klagde ~ gamma, data = KLAGE_V23_E, family = binomial)
model_V23_F <- glm(klagde ~ gamma, data = KLAGE_V23_F, family = binomial)

summary(model_V23_A)
summary(model_V23_B)
summary(model_V23_C)
summary(model_V23_D)
summary(model_V23_E)
summary(model_V23_F)




# Fit logistic regression model for each grade
models_by_grade <- KLAGE_V22 %>%
  group_by(finalGradeLabel) %>%
  do(model = glm(klagde ~ gamma, data = ., family = binomial)) 

# View the models (coefficients, etc.)
models_by_grade$model

# Plot logistic regression curve for each grade
ggplot(KLAGE_V22, aes(x = gamma, y = klagde, color = finalGradeLabel)) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.5) +  # Scatter points
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Logistic curve
  facet_wrap(~ finalGradeLabel) +  # Facet by grade
  labs(title = "Probability of Klage as a function of gamma for Each Grade",
       x = "gamma", y = "Probability of Klage") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend as it's redundant here




#############
### GAMMA ###
#############

ggplot(KLAGE_V22, aes(x = gamma, y = as.factor(klagde), color = finalGradeLabel)) + 
  geom_histogram(aes(x = gamma, y = ..density..), bins = 30, fill = "gray", alpha = 0.2, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of Gamma", 
       x = "Gamma", y = "Klagde (0 = No, 1 = Yes)", color = "Grade") +
  theme_classic() +  # High contrast without gridlines
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

ggplot(KLAGE_V22, aes(x = gamma, y = as.factor(klagde), color = as.factor(kommisjon))) + 
  geom_histogram(aes(x = gamma, y = ..density..), bins = 30, fill = "gray", alpha = 0.2, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of Gamma", 
       x = "Gamma", y = "Klagde (0 = No, 1 = Yes)", color = "Kommisjon") +
  theme_classic() +  # High contrast without gridlines
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

#############
###   S   ###
#############

ggplot(KLAGE_V22, aes(x = s, y = as.factor(klagde), color = finalGradeLabel)) + 
  # Histogram of s (scaled density if needed)
  geom_histogram(aes(x = s, y = after_stat(density) * 0.01), bins = 30, fill = "gray", alpha = 0.2, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of s", 
       x = "s", y = "Klagde (0 = No, 1 = Yes)", color = "Grade") +
  theme_classic() +  # High contrast without gridlines
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

################
### TOTSCORE ###
################

ggplot(KLAGE_V22, aes(x = totalScore, y = as.factor(klagde), color = finalGradeLabel)) + 
  # Transparent histogram of gamma
  geom_histogram(aes(x = totalScore, y = ..density..*15), bins = 30, fill = "gray", alpha = 0.3, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of TotalScore", 
       x = "TotalScore", y = "Klagde (0 = No, 1 = Yes)", color = "Grade") +
  theme_classic() +  
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )


KLAGE_V22_utenF <- KLAGE_V22 %>% filter(finalGradeLabel != "F")
KLAGE_V23_utenF <- KLAGE_V23 %>% filter(finalGradeLabel != "F")

#############
### GAMMA ###
#############

ggplot(KLAGE_V22, aes(x = gamma, y = as.factor(klagde), color = finalGradeLabel)) + 
  geom_histogram(aes(x = gamma, y = ..density..), bins = 30, fill = "gray", alpha = 0.2, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of Gamma NOF", 
       x = "Gamma", y = "Klagde (0 = No, 1 = Yes)", color = "Grade") +
  theme_classic() +  # High contrast without gridlines
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

ggplot(KLAGE_V22_utenF, aes(x = gamma, y = as.factor(klagde), color = as.factor(kommisjon))) + 
  geom_histogram(aes(x = gamma, y = ..density..), bins = 30, fill = "gray", alpha = 0.2, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of Gamma NOF", 
       x = "Gamma", y = "Klagde (0 = No, 1 = Yes)", color = "Kommisjon") +
  theme_classic() +  # High contrast without gridlines
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

#############
###   S   ###
#############

ggplot(KLAGE_V22_utenF, aes(x = s, y = as.factor(klagde), color = finalGradeLabel)) + 
  # Histogram of s (scaled density if needed)
  geom_histogram(aes(x = s, y = after_stat(density) * 0.01), bins = 30, fill = "gray", alpha = 0.2, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of s NOF", 
       x = "s", y = "Klagde (0 = No, 1 = Yes)", color = "Grade") +
  theme_classic() +  # High contrast without gridlines
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

################
### TOTSCORE ###
################

ggplot(KLAGE_V22_utenF, aes(x = totalScore, y = as.factor(klagde), color = finalGradeLabel)) + 
  # Transparent histogram of gamma
  geom_histogram(aes(x = totalScore, y = ..density..*15), bins = 30, fill = "gray", alpha = 0.3, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of TotalScore NOF", 
       x = "TotalScore", y = "Klagde (0 = No, 1 = Yes)", color = "Grade") +
  theme_classic() +  
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )