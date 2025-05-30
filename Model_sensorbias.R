library(RTMB)
set.seed(1)

################################
## DEFINING GLOBAL PARAMETERS ##
################################

RTMB_V22
RTMB_V23

#################################
## CREATING LIST OF PARAMETERS ##
#################################

# Calculating the thresholds
alpha_true_values_V22 <- calculating_true_thresholds(RTMB_V22)
alpha_true_values_V23 <- calculating_true_thresholds(RTMB_V23)

### V22 ###
par_V22 <- list(
  # Random effects
  s     = rep(0, length(unique(RTMB_V22$kommisjon))),  # Examiner random effects
  gamma = rep(0, length(RTMB_V22$kommisjon)),          # Student random effects
  
  # Standard deviation
  log_std_s     = log(1),  # Std deviation for examiners
  log_std_gamma  = log(1), # Std deviation for students
  
  # Parameter to model the IRT
  lambda = rep(1, length(RTMB_V22$Y[1,])),
  kappa  = rep(1, sum(calculating_num_of_alphas(RTMB_V22$Y)!=1))
)

# Thresholds par_V22
for (i in 1:ncol(RTMB_V22$Y)) {
  par_V22[[paste0("alpha", i)]] <- alpha_true_values_V22[[paste0("Q",i)]]
}

### V23 ###
par_V23 <- list(
  # Random effects
  s       = rep(0, length(unique(RTMB_V23$kommisjon))),       # Examiner random effects
  gamma   = rep(0, length(RTMB_V23$kommisjon)),               # Student random effects
  
  # Standard deviation
  log_std_s     = log(1),   # Std deviation for examiners
  log_std_gamma  = log(1),   # Std deviation for students
  
  # Parameter to model the IRT
  lambda = rep(1, length(RTMB_V23$Y[1,])),
  kappa  = rep(1, 2) #Bytte til noe som gj??r dette uavhengig??
)

# Thresholds par_V23
for (i in 1:ncol(RTMB_V23$Y)) {
  par_V23[[paste0("alpha", i)]] <- alpha_true_values_V23[[paste0("Q",i)]]
}

############################################
## CREATING THE FUNCTION TO CALCULATE NLL ##
############################################

f <- function(parms){
  
  # Making the variable local
  getAll(data, parms, warn = FALSE)
  
  #Initializing the nll:
  nll <- 0
  
  #Counting numer of automaticly questions:
  q_count <- 0
  
  # Loop over all the questions
  for (q in 1:ncol(Y)){
    
    # Extracting the thresholds for question q:
    alpha_q <- parms[[paste0("alpha",q)]]
    
    # Transforming from alphas to thetas
    for (j in 1:length(alpha_q)){
      if (j == 1){
        theta <- alpha_q[1]
      } else {
        theta <- c(theta, theta[j-1] + exp(alpha_q[j]))
      }
    }
    
    # Loop over each student:
    for (i in 1:nrow(Y)){
      
      # Checking for automatic or manual q
      if (length(theta) == 1){
        eta = lambda[q]*gamma[i]                                        # Automatic corrected q
        if (i == 1){
          q_count <- q_count + 1
        }
      } else {
        eta = lambda[q]*gamma[i] + kappa[q-q_count]*s[kommisjon[i]]           # Manually  corrected q
      } 
      
      # Updating the nll:
      if (Y[i,q] == 0) { 
        nll  <- nll - log(pnorm(-eta + theta[Y[i,q]+1]) - 0)                          # Response in the first category
      } else if (Y[i,q] == length(theta)) {
        nll  <- nll - log(1 - pnorm(-eta + theta[Y[i,q]]))                            # Response in the last category
      } else {
        nll  <- nll - log(pnorm(-eta + theta[Y[i,q]+1]) - pnorm(-eta + theta[Y[i,q]])) # Response in the Y_iq-1 category
      }
    }
  }
  
  # Adding random effects 
  nll <- nll - sum(dnorm(gamma, mean = 0, sd = exp(log_std_gamma), log = TRUE))
  nll <- nll - sum(dnorm(s,     mean = 0, sd = exp(log_std_s)    , log = TRUE))
  
  #returning nll:
  return(nll)
}

###############################################
## CREATE THE OBJECTIVE FUNCTION AND FITTING ##
###############################################

creating_obj <- function(f, data, parameter, random_effects, mapping=NULL){
  obj <- MakeADFun(func = f, parameters = parameter, random = random_effects, map = mapping)
  fit <- nlminb(start = obj$par, objective = obj$fn, gradient = obj$gr)
  return(obj)
}

# Calculating V22 - Model 1 - Without question specific variance:
data <- RTMB_V22
obj_V22_m1 <- creating_obj(f, RTMB_V22, par_V22, c("gamma", "s"), mapping=list(lambda = factor(rep(NA,20)), kappa=factor(rep(NA, 2))))
sdr_V22_m1 <- sdreport(obj_V22_m1)
summary(sdr_V22_m1)

# Calculating V22 - Model 2 - With question specific variance:
data <- RTMB_V22
obj_V22_m2 <- creating_obj(f, RTMB_V22, par_V22, c("gamma", "s"), mapping=list(log_std_gamma = factor(NA), log_std_s=factor(NA)))
sdr_V22_m2 <- sdreport(obj_V22_m2)
summary(sdr_V22_m2)

# Calculating V23 - Model 1 - Without question specific variance:
data <- RTMB_V23
obj_V23_m1 <- creating_obj(f, RTMB_V23, par_V23, c("gamma", "s"), mapping=list(lambda = factor(rep(NA,17)), kappa=factor(rep(NA, 2))))
sdr_V23_m1 <- sdreport(obj_V23_m1)
summary(sdr_V23_m1)

# Calculating V23 - Model 2 - With question specific variance:
data <- RTMB_V23
obj_V23_m2 <- creating_obj(f, RTMB_V23, par_V23, c("gamma", "s"), mapping=list(log_std_gamma = factor(NA), log_std_s=factor(NA)))
sdr_V23_m2 <- sdreport(obj_V23_m2)
summary(sdr_V23_m2)

# Summary
summary(sdr_V22_m1)
summary(sdr_V22_m2)
summary(sdr_V23_m1)
summary(sdr_V23_m2)

###########################
## LIKELIHOOD RATIO TEST ##
###########################

# Function to calculate it:
LRT <- function(nll0, nll1){
  lambda  <- 2*(nll0-nll1)
  p_value <- 0.5*pchisq(lambda, 7, lower.tail = FALSE) + 0.5*pchisq(lambda, 8, lower.tail = FALSE)
  return(p_value)
}

nll_null <- 56
nll_alt  <- 48.7

p_val <- LRT(nll_null, nll_alt)
print(p_val)

#####################################################
## CREATING THE FUNCTION TO CALCULATE NLL UNDER H0 ##
#####################################################

f_H0 <- function(parms){
  
  # Making the variable local
  getAll(data, parms, warn = FALSE)
  
  #Initializing the nll:
  nll <- 0
  
  #Counting numer of automaticly questions:
  q_count <- 0
  
  # Loop over all the questions
  for (q in 1:ncol(Y)){
    
    # Extracting the thresholds for question q:
    alpha_q <- parms[[paste0("alpha",q)]]
    
    # Transforming from alphas to thetas
    for (j in 1:length(alpha_q)){
      if (j == 1){
        theta <- alpha_q[1]
      } else {
        theta <- c(theta, theta[j-1] + exp(alpha_q[j]))
      }
    }
    
    # Loop over each student:
    for (i in 1:nrow(Y)){
      
      # Checking for automatic or manual q
      if (length(theta) == 1){
        eta = lambda[q]*gamma[i]                                        # Automatic corrected q
        if (i == 1){
          q_count <- q_count + 1
        }
      } else {
        eta = lambda[q]*gamma[i]         # Manually  corrected q
      } 
      
      # Updating the nll:
      if (Y[i,q] == 0) { 
        nll  <- nll - log(pnorm(-eta + theta[Y[i,q]+1]) - 0)                          # Response in the first category
      } else if (Y[i,q] == length(theta)) {
        nll  <- nll - log(1 - pnorm(-eta + theta[Y[i,q]]))                            # Response in the last category
      } else {
        nll  <- nll - log(pnorm(-eta + theta[Y[i,q]+1]) - pnorm(-eta + theta[Y[i,q]])) # Response in the Y_iq-1 category
      }
    }
  }
  
  # Adding random effects 
  nll <- nll - sum(dnorm(gamma, mean = 0, sd = exp(log_std_gamma), log = TRUE))
  
  #returning nll:
  return(nll)
}

##########################
## PERFORMING THE TEST ###
##########################

# Creating parameter lists under H0:
par_V22_H0 <- par_V22
par_V22_H0$log_std_s <- log(1)

data <- RTMB_V22
obj_V22_m1_H0 <- creating_obj(f_H0, RTMB_V22, par_V22_H0, c("gamma"), mapping=list(s=factor(rep(NA,6)), lambda = factor(rep(NA,20)), kappa=factor(rep(NA, 2)), log_std_s=factor(NA)))
sdr_V22_m1_H0 <- sdreport(obj_V22_m1_H0)
summary(sdr_V22_m1_H0)

test <- LRT(obj_V22_m1_H0$fn(obj_V22_m1_H0$par), obj_V22_m1$fn(obj_V22_m1$par))

obj_V22_m1_H0$fn(obj_V22_m1_H0$par)
obj_V22_m1$fn(obj_V22_m1$par)

h <- AIC(1000,obj_V22_m1$fn(obj_V22_m1$par))

1-test












par_V23_H0 <- par_V22
par_V23_H0$log_std_s <- -Inf

# Calculating V22 - Model 1 - Under the 0 hypothesis:
data <- RTMB_V22
obj_V22_m1 <- creating_obj(f, RTMB_V22, par_V22, c("gamma", "s"), mapping=list(lambda = factor(rep(NA,20)), kappa=factor(rep(NA, 2))))
sdr_V22_m1 <- sdreport(obj_V22_m1)
summary(sdr_V22_m1)



data <- RTMB_V22
obj_V22_m1_H0 <- creating_obj(f, RTMB_V22, par_V22, c("gamma", "s"), mapping=list(lambda = factor(rep(NA,20)), kappa=factor(rep(NA, 2)), s=factor(rep(NA,6))))
sdr_V22_m1_H0 <- sdreport(obj_V22_m1_H0)
summary(sdr_V22_m1_H0)

L_V22_M1    <- obj_V22_m1$fn(obj_V22_m1$par)
L_V22_M1_H0 <- obj_V22_m1_H0$fn(obj_V22_m1_H0$par)

lambda_V22_M1  <- 2*(L_V22_M1-L_V22_M1_H0)
p_value_V22_M1 <- 0.5*pchisq(lambda_V22_M1, 1, lower.tail = FALSE) + 0.5*pchisq(lambda_V22_M1, 2, lower.tail = FALSE)
p_value_V22_M1













# Calculating V22 - Model 2 - Under the 0 hypothesis:
data <- RTMB_V22
obj_V22_m2_H0 <- creating_obj(f, RTMB_V22, par_V22, c("gamma"), mapping=list(log_std_gamma = factor(NA), log_std_s=factor(NA), s=factor(rep(NA,6))))
sdr_V22_m2_H0 <- sdreport(obj_V22_m2_H0)
summary(sdr_V22_m2_H0)

# Calculating V23 - Model 1 - Under the 0 hypothesis:

# Calculating V23 - Model 2 - Under the 0 hypothesis:

#Calculating the p-value for - Model 1 - V22:
L_V22_M1    <- obj_V22_m1$fn(obj_V22_m1$par)
L_V22_M1_H0 <- obj_V22_m1_H0$fn(obj_V22_m1_H0$par)

lambda_V22_M1  <- 2*(L_V22_M1-L_V22_M1_H0)
p_value_V22_M1 <- 0.5*pchisq(lambda_V22_M1, 1, lower.tail = FALSE) + 0.5*pchisq(lambda_V22_M1, 2, lower.tail = FALSE)
p_value_V22_M1

#Calculating the p-value for - Model 2 - V22:
L_V22_M2    <- obj_V22_m2$fn(obj_V22_m2$par)
L_V22_M2_H0 <- obj_V22_m2_H0$fn(obj_V22_m2_H0$par)

lambda_V22_M2  <- 2*(L_V22_M2-L_V22_M2_H0)
p_value_V22_M2 <- 0.5*pchisq(lambda_V22_M2, 1, lower.tail = FALSE) + 0.5*pchisq(lambda_V22_M2, 2, lower.tail = FALSE)

#Calculating the p-value for - Model 1 - V23:

#Calculating the p-value for - Model 2 - V23:

L1V22 <-obj_V22$fn(obj_V22$par)
L1V23 <-obj_V23$fn(obj_V23$par)
L0V22 #<-obj_V22$fn(obj_V22$par)
L0V23 #<-obj_V23$fn(obj_V23$par)

df1=1
df2=2

lambda_V22 <- -2*(L1V22-L0V22)
lambda_V23 <- 2*(L1V23-L0V23)

p_value_V22 <- 0.5*pchisq(lambda_V22, 1, lower.tail = FALSE) + 0.5*pchisq(lambda_V22, 2, lower.tail = FALSE)
p_value_V23 <- 0.5*pchisq(lambda_V23, 1, lower.tail = FALSE) + 0.5*pchisq(lambda_V23, 2, lower.tail = FALSE)

p_value_V22
p_value_V23

###########
### AIC ###
###########

AIC <- function(k, nll){
  return(2*k + 2*nll)
}



###########
### BIC ###
###########

BIC <- function(n, k, nll){
  return(log(n)*k + 2*nll)
}