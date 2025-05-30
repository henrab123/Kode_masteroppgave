###########################################
## CALCULATING THRESHOLDS FOR PARAMETERS ##
###########################################

"This function calculates the true thresholds
for the Real data provided. The function 
returns the alphas, that is the transformed 
version of the thresholds theta."


calculating_true_thresholds <- function(data){
  
  # List to store thresholds
  true_thresholds <- list()
  
  # Looping over each question
  for (q in 1:ncol(data$Y)) {
    
    # Counting the responses
    n_thresholds_q <- length(unique(data$Y[,q])) - 1
    
    # Vectors to store the thresholds
    alphas  <- rep(0, n_thresholds_q)
    thetas  <- rep(0, n_thresholds_q)
    
    # Cumulative counting
    y_cum <- 0
    
    for (k in 1:n_thresholds_q){
      
      # Updating the cumulative count
      y_cum <- y_cum + sum(data$Y[,q] == k-1)
      
      # Calculating the thetas
      thetas[k] <- qnorm(y_cum/nrow(data$Y))
      
      # Transforming the thetas to alphas
      if (k == 1){
        alphas[k] <- thetas[k]
      } else {
        alphas[k] <- log(thetas[k]-thetas[k-1])
      }  
    }
    
    #Adding the true values to the vector
    true_thresholds[[paste0("Q",q)]] <- alphas
  }
  
  return(true_thresholds)
}