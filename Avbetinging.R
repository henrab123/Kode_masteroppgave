###################
### AVBETINGING ###
###################

# Helping funcion:
I <- function(th_0,th_1){
  return(pnorm(th_1)-pnorm(th_0))
}

#calulating the thresholdsvalues:
TH <- function(gamma, s, lambda, kappa, theta_0, theta_1){
  TH_01 <- c(theta_0-lambda*gamma-kappa*s,theta_1-lambda*gamma-kappa*s)
  return(TH_01)
}

# Log-likelihood of one question:
f <- function(th_0, th_1){
  return(log(I(th_0,th_1)))
}

# Derivative of the log-likelihood of one question:
f_d <- function(th_0,th_1){
  return((-(dnorm(th_1)-dnorm(th_0))/I(th_0,th_1)))
}

# Second order derivative of the log-likelihood of one question:
f_dd <- function(th_0,th_1){
  I_val <- I(th_0,th_1)
  numerator <- I_val * (th_0*dnorm(th_0) - th_1*dnorm(th_1)) - (dnorm(th_0) - dnorm(th_1))**2
  denominator <- I(th_0,th_1)**2
  return(numerator/denominator)
}

# Creating a polynomial:
poly_gamma <- function(th_0,th_1,gamma){
  a <- f_dd(th_0,th_1)/2
  b <- f_d(th_0,th_1) -2*a*gamma
  c <- f(th_0,th_1)-a*gamma**2-b*gamma
  coefficients <- c(a,b,c)
  return(coefficients)
}

f(-0.5,0.5)
f_d(-0.5,0.5)
-1/f_dd(-1.5,1.5)
a <-poly_gamma(0,1,0)
a

#########################
### TESTE ?? AVBETINGE ###
#########################

avb_data <- function(estimated_par, data, n_alpha){
  
  #Creating the maticies:
  tau         <- matrix(0, ncol=ncol(data$Y), nrow=nrow(data$Y))
  gamma_j     <- matrix(0, ncol=ncol(data$Y), nrow=nrow(data$Y))
  
  # Creating the matricies:
  gamma_minus_j <- matrix(0, ncol=ncol(data$Y), nrow=nrow(data$Y))
  
  #Creating the predictors:
  eta           <- rep(0, length(data$kandidatnummer))
  eta_without_s <- rep(0, length(data$kandidatnummer))
  
  #n_alpha       <- calculating_num_of_alphas(data$Y)
  
  for (q in 1:ncol(data$Y)){
    
    # Each unique possible score on question q:
    points_q <- sort(unique(data$Y_m[,q]))
    
    #Finding the thresholds:
    theta_q <- c(-1e15, alpha_to_theta(estimated_par$alpha[grep(paste0("^", paste0("alpha", q), "($|\\.)"), rownames(estimated_par$alpha)), 1]), 1e15)
    
    # Automatically corrected question q
    if (n_alpha[q] == 1){
      
      # Automatically corrected predictor:
      for (i in 1:length(data$kandidatnummer)){
        eta[i] <- -estimated_par$gamma[i,1]*estimated_par$lambda[q,1] 
      }
      
    } else {
        
      # Manually with sensor bias:
      for (i in 1:length(data$kandidatnummer)){
        eta[i]  <- -estimated_par$gamma[i,1]*estimated_par$lambda[q,1] - estimated_par$s[data$kommisjon[i],1]*estimated_par$kappa[q-(length(estimated_par$lambda[,1])-length(estimated_par$kappa[,1])),1]
      }
      
    }
      
    for (i in 1:nrow(data$Y)){
        
      ################# REGNE UT ###############
      y_iq      <- data$Y[i,q]
        
      theta_iq0 <- theta_q[y_iq+1]-eta[i]
      theta_iq1 <- theta_q[y_iq+2]-eta[i]
        
      tau[i,q]      <- -f_dd(theta_iq0,theta_iq1)
      gamma_j[i,q]  <- estimated_par$gamma[i,1] - f_d(theta_iq0,theta_iq1)/(f_dd(theta_iq0,theta_iq1))
      
      # To fix numerical problems:
      if (is.nan(gamma_j[i,q])){
        gamma_j[i,q] <- estimated_par$gamma[i,1]
        tau[i,q]     <- 1
      }
    }
  }
  
  #Creating gamma-j:
  for (q in 1:ncol(data$Y)){
    for (i in 1:nrow(data$Y)){
      ################# REGNE UT ###############
      
      tau_q <- sum(tau[i,])
      gamma_minus_j[i,q] <-  (tau_q*estimated_par$gamma[i,1] - tau[i,q]*gamma_j[i,q]) / (tau_q-tau[i,q])
    }
    
  }

  return(gamma_minus_j)
  
}


#Prepearing data:
#V22 - model 1
AVB_DATA_V22_m1       <- RTMB_V22
AVB_DATA_V22_m1$gamma <- estimated_parameters_V22_m1$gamma[,1]
AVB_DATA_V22_m1$s     <- estimated_parameters_V22_m1$s[AVB_DATA_V22_m1$kommisjon,1]

#V22 - model 2
AVB_DATA_V22_m2       <- RTMB_V22
AVB_DATA_V22_m2$gamma <- estimated_parameters_V22_m2$gamma[,1]
AVB_DATA_V22_m2$s     <- estimated_parameters_V22_m2$s[AVB_DATA_V22_m2$kommisjon,1]

#V23 - model 1
AVB_DATA_V23_m1       <- RTMB_V23
AVB_DATA_V23_m1$gamma <- estimated_parameters_V23_m1$gamma[,1]
AVB_DATA_V23_m1$s     <- estimated_parameters_V23_m1$s[AVB_DATA_V23_m1$kommisjon,1]

#V23 - model 2
AVB_DATA_V23_m2       <- RTMB_V23
AVB_DATA_V23_m2$gamma <- estimated_parameters_V23_m2$gamma[,1]
AVB_DATA_V23_m2$s     <- estimated_parameters_V23_m2$s[AVB_DATA_V23_m2$kommisjon,1]


# Avbetinging:
gamma_minus_j_V22_m1 <- avb_data(estimated_parameters_V22_m1, AVB_DATA_V22_m1, calculating_num_of_alphas(AVB_DATA_V22_m1$Y))
gamma_minus_j_V22_m2 <- avb_data(estimated_parameters_V22_m2, AVB_DATA_V22_m2, calculating_num_of_alphas(AVB_DATA_V22_m2$Y))
gamma_minus_j_V23_m1 <- avb_data(estimated_parameters_V23_m1, AVB_DATA_V23_m1, calculating_num_of_alphas(AVB_DATA_V23_m1$Y))
gamma_minus_j_V23_m2 <- avb_data(estimated_parameters_V23_m2, AVB_DATA_V23_m2, calculating_num_of_alphas(AVB_DATA_V23_m2$Y))





