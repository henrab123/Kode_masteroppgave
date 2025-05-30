###########################
## BOOTSTEAP THING THANG ##
###########################

#Clearifiyng the names
RTMB_V22
estimated_parameters_V22_m1$


bootstrap_par <- function(estimated_par, data){
  # Calculating the N and K
  N <- (nrow(data$Y)) 
  K <- length(unique(data$kommisjon))
  Kommisjon <- (data$kommisjon)
  
  K_Nr <- data$kandidatnummer
  
  #Finding the s an gammas
  s <- estimated_par$s[,1]
  gamma <- estimated_par$gamma[,1]
  
  # Finding QA and QM:
  Nalphas <- rep(0, length(data$Y[1,]))
  
  for (q in 1:length(data$Y[1,])){
    Nalphas[q] <- length(estimated_par$alpha[grep(paste0("^", paste0("alpha", q), "($|\\.)"), rownames(estimated_par$alpha)), 1])
  }
  
  QA <- sum(Nalphas == 1)
  QM <- sum(Nalphas != 1)
  
  #Finding AT and MT
  AT <- estimated_par$alpha[1:QA,1]
  
  max_alphas <- max(Nalphas[Nalphas != 1])
  MT <- matrix(Inf, nrow = QM, ncol = max_alphas)
  ###Make it to thetas not aplphas:
  
  for (q in (QA+1):(QA+QM)){
    matched_values <- alpha_to_theta(estimated_par$alpha[grep(paste0("^", paste0("alpha", q), "($|\\.)"), rownames(estimated_par$alpha)), 1])
    MT[q - QA, 1:length(matched_values)] <- matched_values
  }
  
  #Finding lambda and kappa:
  lambda <- estimated_par$lambda[,1]
  kappa  <- estimated_par$kappa[,1]
  
  log_std_gamma <- estimated_par$log_std_gamma[,1]
  log_std_s     <- estimated_par$log_std_s[,1]
  
  #Returning included estimates:
  return(list(
    N = N,
    K = K,
    Kommisjon = Kommisjon,
    Kandidatnummer = K_Nr,
    s       = s,
    gamma   = gamma,
    lambda  = lambda,
    kappa   = kappa,
    QA = QA,
    QM = QM,
    AT = AT,
    MT = MT,
    log_std_gamma = log_std_gamma,
    log_std_s = log_std_s
  ))
}

run_datagen <- function(bootstrap_par){
  
  #Just because it is just crazy implementation...
  generated_data <- simulate_data(N=bootstrap_par$N, 
                    Kommisjon=bootstrap_par$Kommisjon, 
                    K_Nr=bootstrap_par$Kandidatnummer, 
                    s=bootstrap_par$s, 
                    gamma=bootstrap_par$gamma, 
                    QA=bootstrap_par$QA, 
                    AT=bootstrap_par$AT, 
                    QM=bootstrap_par$QM, 
                    MT=bootstrap_par$MT,
                    lambda=bootstrap_par$lambda, 
                    kappa =bootstrap_par$kappa)
  return(generated_data)
}

run_paragen <- function(bootstrap_par){
  
  #Just because it is just crazy implementation...
  generated_par <- make_parameter_list(N=bootstrap_par$N,
                                       K=bootstrap_par$K,
                                       log_std_gamma= bootstrap_par$log_std_gamma,
                                       log_std_s=bootstrap_par$log_std_s,
                                       QA=bootstrap_par$QA,
                                       QM=bootstrap_par$QM,
                                       Theta_a = bootstrap_par$AT,
                                       Theta_m = bootstrap_par$MT)
  return(generated_par)
}


#debugonce(bootstrap_par)
SIM_V22 <- bootstrap_par(estimated_parameters_V22_m1, RTMB_V22)
SIM_V22_DATA <- run_datagen(SIM_V22)
SIM_V22_par  <- run_paragen(SIM_V22)



# LET THE BOOTSTRAP BEGIN #

creating_obj <- function(f, data, parameter, random_effects, mapping=NULL){
  obj <- MakeADFun(func = f, parameters = parameter, random = random_effects, map = mapping)
  fit <- nlminb(start = obj$par, objective = obj$fn, gradient = obj$gr)
  return(obj)
}

# Calculating V22 - Model 1 - Without question specific variance:
data <- SIM_V22_DATA #M?? v??re med
SIM_obj_V22_m1 <- creating_obj(f, SIM_V22_DATA, SIM_V22_par, c("gamma", "s"), mapping=list(lambda = factor(rep(NA,20)), kappa=factor(rep(NA, 2))))
SIM_sdr_V22_m1 <- sdreport(SIM_obj_V22_m1)
summary(SIM_sdr_V22_m1)

SIM_V22_DATA_2 <- run_datagen(SIM_V22)
data <- SIM_V22_DATA_2 #M?? v??re med
SIM_obj_V22_m1_2 <- creating_obj(f, SIM_V22_DATA_2, SIM_V22_par, c("gamma", "s"), mapping=list(lambda = factor(rep(NA,20)), kappa=factor(rep(NA, 2))))
SIM_sdr_V22_m1_2 <- sdreport(SIM_obj_V22_m1_2)
summary(SIM_sdr_V22_m1_2)










###############################
# M?? adde mapping her ogs??!!##################
#######################
####################

Bootstrap_X5000 <- function(boot_par, B=1, name = "Test"){
  
  dir.create("C:/Users/andre/OneDrive/Skrivebord/Filer-Master/Simulering_Biaskorreksjon", showWarnings = FALSE)
  boot_par_combined <- flatten_selected_fields(boot_par)

  for (i in 1:B){
    set.seed(i)
    data <<- run_datagen(boot_par)
    par  <- run_paragen(boot_par)
    
    obj_i <- creating_obj(f, data, par, c("gamma", "s"), mapping=list(lambda = factor(rep(NA,20)), kappa=factor(rep(NA, 2))))
    sdr_i <- sdreport(obj_i)
    sry_i <- summary(sdr_i, select = c("all", "fixed", "random", "report"), p.value = TRUE)
    
    estimated_par <- ordering_estimators(sry_i, data)
    boot_par_i    <- bootstrap_par(estimated_par, data)
    boot_flatpar_i  <- flatten_selected_fields(boot_par_i)
    boot_par_combined <- rbind(boot_par_combined, boot_flatpar_i)
    if (i %% 100 == 0 || i == B) {
      saveRDS(boot_par_combined, file = file.path("C:/Users/andre/OneDrive/Skrivebord/Filer-Master/Simulering_Biaskorreksjon", paste0(name,"i", i, ".rds")))
    }
  }
    
  return(boot_par_combined)
}



######################
### BIAS CORECTION ###
######################

cat("gamma1: ",
    estimated_parameters_V22_m1$gamma[1, 1], " ",
    estimated_parameters_V22_m1$gamma[1, 2], " ",
    mean(V2022_m1_i1000$gamma_1), " ",
    sd(V2022_m1_i1000$gamma_1), "\n")

cat("gamma2: ",
    estimated_parameters_V22_m1$gamma[2, 1], " ",
    estimated_parameters_V22_m1$gamma[2, 2], " ",
    mean(V2022_m1_i1000$gamma_2), " ",
    sd(V2022_m1_i1000$gamma_2), "\n")

cat("s1: ",
    estimated_parameters_V22_m1$s[1, 1], " ",
    estimated_parameters_V22_m1$s[1, 2], " ",
    mean(V2022_m1_i1000$s_1), " ",
    sd(V2022_m1_i1000$s_1), "\n")

cat("s2: ",
    estimated_parameters_V22_m1$s[2, 1], " ",
    estimated_parameters_V22_m1$s[2, 2], " ",
    mean(V2022_m1_i1000$s_2), " ",
    sd(V2022_m1_i1000$s_2), "\n")

cat("s5: ",
    estimated_parameters_V22_m1$s[5, 1], " ",
    estimated_parameters_V22_m1$s[5, 2], " ",
    mean(V2022_m1_i1000$s_5), " ",
    sd(V2022_m1_i1000$s_5), "\n")

cat("log_std_gamam: ",
    estimated_parameters_V22_m1$log_std_gamma[1, 1], " ",
    estimated_parameters_V22_m1$log_std_gamma[1, 2], " ",
    mean(V2022_m1_i1000$log_std_gamma), " ",
    sd(V2022_m1_i1000$log_std_gamma), "\n")

cat("log_std_s: ",
    estimated_parameters_V22_m1$log_std_s[1, 1], " ",
    estimated_parameters_V22_m1$log_std_s[1, 2], " ",
    mean(V2022_m1_i1000$log_std_s), " ",
    sd(V2022_m1_i1000$log_std_s), "\n")
