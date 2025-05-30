# Setting seed:
set.seed(2)

#Defining function:
simulate_data <- function(N=100, 
                          Kommisjon=2, 
                          K_Nr=NULL, 
                          s=redp(1,K), 
                          gamma=rep(0,N), 
                          QA=2, AT=c(-0.5, 1), 
                          QM=2, MT=matrix(c(-1,-0.5,3,4,5,6), nrow=2, ncol=3, byrow=TRUE),
                          lambda=rep(1,QA+QM), kappa=rep(1,QM)){
  
 "DESCRIPTION OF THE FUNCTION:
  Input variables:
    N: Number of students
    K: Number of commissions [N must be divisible by K]
    K_f: The partition of the comissions [Must be divisible by N and K]
    s: The mean of each of the comissions
    QA: Number of automatic corrected questions
    AT: Threshold for the Automatic questions
    QM: Number of manually questions
    model: The type of model used
  
  This function simulates results from exams based on the model we want to evaluate. 
  "

  if (length(AT) != QA) {
    warning("Dimensions of AT do not fit the value of QA!")
  }

  
  #Implementation:
  simulated_data <- NULL
  
  #Commission
  simulated_data$kommisjon <- Kommisjon


  #Candidate number:
  if (is.null(K_Nr)){
    simulated_data$kandidatnummer <- seq(1, N)
  } else {
    simulated_data$kandidatnummer <- K_Nr
  }
  
  #Data:
  simulated_data$Y <- matrix(0, nrow = N, ncol = (QA + QM))
  
  #Defining models:
  eta_a <- function(q,i) lambda[q]*gamma[i]
  eta_m <- function(q,i) lambda[q+QA]*gamma[i] + kappa(q)*s[simulated_data$kommisjon[i]]
  
  for (q in 1:QA){
    for (i in 1:N){
      eta_pred <- eta_a(q,i) + rnorm(1,0,1)
      if (eta_pred > AT[q]){
        simulated_data$Y[i,q] <- 1
      }
    }
  }
  
  for (q in 1:QM){
    for (i in 1:N){
      eta_pred <- eta_m(q,i) + rnorm(1,0,1)
      for (j in 1:length(MT[1,])){
        if(eta_pred > MT[q,j]){
          simulated_data$Y[i,q+QA] <- j
        }
      }
    }
  }

  return(data.frame(kommisjon = simulated_data$kommisjon,
                    kandidatnummer = simulated_data$kandidatnummer,
                    ferdigheter = gamma,
                    Y = I(simulated_data$Y)))
}



make_parameter_list <- function(N, K, log_std_gamma = log(1), log_std_s = log(1), QA, QM, Theta_a, Theta_m){
  
  par <- list(
    # Random effects
    s     = rep(0,K),          # Examiner random effects
    gamma = rep(0,N),          # Student random effects
    
    # Standard deviation
    log_std_s      = log_std_s,   # Std deviation for examiners
    log_std_gamma  = log_std_gamma,   # Std deviation for students
    
    # Parameter to model the IRT
    lambda = rep(1, (QA+QM)),
    kappa  = rep(1, QM) #Bytte til noe som gj??r dette uavhengig??
  )
  
  #Generating the alphas form the Thresholds:
  alphas <- create_true_thresholds(Theta_a, Theta_m)
  
  for (i in 1:(QA+QM)) {
    par[[paste0("alpha", i)]] <- alphas[[paste0("Q",i)]]
  }
  
  return(par)
}

#################
## SIMULATIONS ##
#################

#Initializing parameters:
N = 1000
K = 5 
K_f = FALSE

mu_gamma = 0
mu_s     = 0

log_std_gamma = log(1)
log_std_s     = log(1)

QA = 20
QM = 6 
Lvl = 10

lambda = rep(1, (QA+QM))
kappa  = rep(1, QM)

Theta_a <- c(-1.5, seq(-1, 1, ((1 - -1)/(QA - 3))), 1.5)
Theta_m <- t(apply(matrix(runif(QM*Lvl, -3, 3), QM, Lvl), 1 , sort))

N_S <- 1

#true_values$alpha
#estimated_parameters$alpha[,1]

#Performing the simulations and error estimation:
simulation <- function(N_S, modelchoice, initialconditions="Zero"){

  error_gamma <- rep(0,N)
  error_s     <- rep(0,K)
  error_std_gamma <- 0
  error_std_s     <- 0
  error_lambda = rep(0, (QA+QM))
  error_kappa  = rep(0, QM)
  
  for (i in 1:N_S) {
    set.seed(i)
    
    #Generate gamma and s:
    gamma_true <- rnorm(N, mu_gamma, exp(log_std_gamma))
    s_true     <- rnorm(K, mu_s    , exp(log_std_s    ))
    
    #Generate lambda and kappa:
    lambda_true <- rnorm((QA+QM), 1, 0.25)
    kappa_true  <- rnorm(QM,      1, 0.25)
    
    #Simulating the data:
    simulated_data <- simulate_data(N=N, K=K, K_f=K_f, s=s_true, gamma=gamma_true, QA=QA, AT=Theta_a, QM=QM, MT=Theta_m, kappa=kappa_true, lambda=lambda_true, model=modelchoice)
    
    #Creating list of parameters
    params <- make_parameter_list(N, K, log_std_gamma, log_std_s, QA, QM, Theta_a, Theta_m)
    
    if (initialconditions == "True"){
      params$gamma  <- gamma_true
      params$s      <- s_true
      params$lambda <- lambda_true
      params$kappa  <- kappa_true
    }
    
    #Estimating parameters:
    data <<- simulated_data
    
    #To access all the variables:
    getAll(data, params)
    
    if (modelchoice == 1){
      obj  <- MakeADFun(func = f, parameters = params, random=c("gamma", "s"), map = list(lambda = factor(rep(NA, QA + QM)), kappa = factor(rep(NA, QM))))
    } 
    
    if (modelchoice == 2){
      obj  <- MakeADFun(func = f, parameters = params, random=c("gamma", "s"), map = list(log_std_gamma = factor(NA), log_std_s=factor(NA)))
    }
    
    fit  <- nlminb(start = obj$par, objective = obj$fn, gradient = obj$gr)
    sdr  <- sdreport(obj)
    smry <- summary(sdr, select = c("all", "fixed", "random", "report"), p.value = TRUE)

    #Ordering the estimated values
    estimated_parameters <- ordering_estimators(smry, data, QA, QM)
    
    #True parameters used:
    true_values <- list(gamma=gamma_true, s=s_true, log_std_gamma=log_std_gamma, log_std_s=log_std_s, Theta_a=Theta_a, Theta_m=Theta_m, alphas=create_true_thresholds(Theta_a, Theta_m), kappa=kappa_true, lambda=lambda_true)
    
    #Calculating errors:
    difference_std_gamma <- exp(estimated_parameters$log_std_gamma[1])-exp(true_values$log_std_gamma)
    difference_std_s     <- exp(estimated_parameters$log_std_s[1])    -exp(true_values$log_std_s)
    
    if (modelchoice == 2){
      difference_std_gamma <- exp(estimated_parameters$log_std_gamma[1])-exp(true_values$log_std_gamma)
      difference_std_s     <- exp(estimated_parameters$log_std_s[1])    -exp(true_values$log_std_s)
    }
    
    difference_s     <- estimated_parameters$s[,1]    -true_values$s
    difference_gamma <- estimated_parameters$gamma[,1]-true_values$gamma
    
    difference_kappa  <- estimated_parameters$kappa[,1] -true_values$kappa
    difference_lambda <- estimated_parameters$lambda[,1]-true_values$lambda
    
    #Updating average error:
    error_std_gamma <- (error_std_gamma*(i-1) + abs(difference_std_gamma))/i
    error_std_s     <- (error_std_s*(i-1)     + abs(difference_std_s    ))/i 
    
    error_gamma <- (error_gamma*(i-1) + abs(difference_gamma))/i
    error_s     <- (error_s*(i-1)     + abs(difference_s    ))/i
    
    error_kappa  <- (error_kappa *(i-1) + abs(difference_kappa ))/i
    error_lambda <- (error_lambda*(i-1) + abs(difference_lambda))/i 
    
    #plot(seq(length(difference_gamma)), abs(difference_gamma))
  }
  
  return(list(std_gamma=error_std_gamma, 
              std_s=error_std_s, 
              gamma=error_gamma, 
              s=error_s, 
              kappa=error_kappa, 
              lambda=error_lambda))
}

#debugonce(simulation)


#TESTING MODEL 1 WITH I.C.
error <- simulation(3, 1, "True")

plot(seq(length(error$gamma)), abs(error$gamma), main = "True I.C - Model 1")
abline(h = mean(error$gamma), col = "red", lwd = 2, lty = 2)

error <- simulation(3, 1)

plot(seq(length(error$gamma)), abs(error$gamma), main = "Random I.C - Model 1")
abline(h = mean(error$gamma), col = "red", lwd = 2, lty = 2)

#TESTING MODEL 2 WITH I.C.
error <- simulation(3, 2, "True")

plot(seq(length(error$gamma)), abs(error$gamma), main = "True I.C - Model 2")
abline(h = mean(error$gamma), col = "red", lwd = 2, lty = 2)

error <- simulation(3, 2)

plot(seq(length(error$gamma)), abs(error$gamma), main = "Random I.C - Model 2")
abline(h = mean(error$gamma), col = "red", lwd = 2, lty = 2)

#TESTING FOR N:

#Initializing parameters:
Ns = c(300,400,500,600,700,800,900,1000)
error_bar_gamma <- rep(0, length(Ns))
error_bar_s     <- rep(0, length(Ns))

for (i in 1:length(Ns)){
  N <- Ns[i]
  error <- simulation(5, 1)
  error_bar_gamma[i] <- mean(error$gamma)
  error_bar_s[i]     <- mean(error$s)
}

plot(Ns, error_bar_s, main = "Error of s of N")
abline(h = mean(error_bar_s), col = "red", lwd = 2, lty = 2)


#TESTING FOR QA:
#Initializing parameters:
QAs = c(10,15,20,25,30,35,40,45,50)

error_bar_gamma <- rep(0, length(Ns))
error_bar_s     <- rep(0, length(Ns))

for (i in 1:length(Ns)){
  QA <- QAs[i]
  lambda = rep(1, (QA+QM))
  Theta_a <- c(-1.5, seq(-1, 1, ((1 - -1)/(QA - 3))), 1.5)
  error <- simulation(5, 1)
  error_bar_gamma[i] <- mean(error$gamma)
  error_bar_s[i]     <- mean(error$s)
}
QAs = c(10,15,20,25,30,35,40,45)
plot(QAs, error_bar_gamma, main = "Error of gamma of QA")
abline(h = mean(error_bar_gamma), col = "red", lwd = 2, lty = 2)
error_bar_gamma



error <- simulation(5, 1)

plot(seq(length(error$gamma)), abs(error$gamma))
abline(h = mean(error$gamma), col = "red", lwd = 2, lty = 2)

error <- simulation(5, 1)

plot(seq(length(error$gamma)), abs(rnorm(N,0,1)-rnorm(N,0,1)), , main = "Random I.C.")
abline(h = mean(error$gamma), col = "red", lwd = 2, lty = 2)


error <- simulation(50, 1)

plot(seq(length(error$gamma)), abs(error$gamma))
abline(h = mean(error$gamma), col = "red", lwd = 2, lty = 2)




plot(seq(length(error$gamma)), abs(rnorm(N,0,1)-rnorm(N,0,1)))
abline(h = mean(error$gamma), col = "red", lwd = 2, lty = 2)

plot(seq(length(error$s)), abs(error$s))
abline(h = mean(error$s), col = "red", lwd = 2, lty = 2)

plot(seq(length(error$lambda)), abs(error$lambda))
abline(h = mean(error$lambda), col = "red", lwd = 2, lty = 2)

plot(seq(length(error$kappa)), abs(error$kappa))
abline(h = mean(error$kappa), col = "red", lwd = 2, lty = 2)

plot(seq(length(error$std_gamma)), abs(error$std_gamma))

plot(seq(length(error$std_s)), abs(error$std_s))

################
### PLOTTING ###
################

library(ggplot2)
y = cumsum(rnorm(100))
#Simulerer noen data
set.seed(123)
df <- data.frame(
  x = 1:100,
  y = y,   # En tilfeldig trend
  ymin = y - 5,  # Nedre b??nd
  ymax = y + 5   # ??vre b??nd
)

# Plot med konfidensb??nd
ggplot(df, aes(x=x, y=y)) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="steelblue", alpha=0.3) +
  geom_line(color="steelblue", size=1.2) +
  theme_minimal() +
  labs(title="Kul graf med usikkerhet", x="Tid", y="Verdi") +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5))

set.seed(1)