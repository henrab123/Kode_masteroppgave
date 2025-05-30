######################################
## CALCULATING DIFFERENCE IN POINTS ##
######################################

difference_in_points <- function(pred_point_with, pred_point_without, VYY){
  
  poengsum_o      <- VYY$totalScore
  poengsforskjell <- pred_point_without$Y_sum_manu - pred_point_with$Y_sum_manu 
  poengsum_ny     <- poengsum_o + poengsforskjell
    
  df <- data.frame(
    kommisjon      = pred_point_with$kommisjon,
    kandidatnummer = pred_point_with$kandidatnummer,
    karakter       = VYY$ext_inspera_finalGrade,
    poengsum_o     = poengsum_o,
    poengsum_ny    = poengsum_ny,
    poengsforskjell= poengsforskjell
    )
  return(df)
}

average_difference <- function(DIP){
  stopifnot("poengsforskjell" %in% colnames(DIP))  # Ensure column exists
  
  DIP <- DIP %>%
    mutate(poengsforskjell = as.numeric(poengsforskjell))  # Ensure it's numeric
  
  avdif <- DIP %>%
    group_by(kommisjon) %>%
    summarise(mean_sum = mean(poengsforskjell, na.rm = TRUE), .groups = "drop")  # Return as DF
  
  return(avdif)
}

# Difference in poits:
#debugonce(difference_in_points)
DIP_V22_m1 <- difference_in_points(Predicted_points_V22_m1, Predicted_points_V22_m1_utens, V22)
DIP_V22_m2 <- difference_in_points(Predicted_points_V22_m2, Predicted_points_V22_m2_utens, V22)
DIP_V23_m1 <- difference_in_points(Predicted_points_V23_m1, Predicted_points_V23_m1_utens, V23)
DIP_V23_m2 <- difference_in_points(Predicted_points_V23_m2, Predicted_points_V23_m2_utens, V23)

# Average difference for each kommisjon:
AVDIP_V22_m1   <- average_difference(DIP_V22_m1)
AVDIP_V22_m2   <- average_difference(DIP_V22_m2)
AVDIP_V23_m1   <- average_difference(DIP_V23_m1)
AVDIP_V23_m2   <- average_difference(DIP_V23_m2)

########################
## CALCULATING GRADES ##
########################

karaktergrense_V22 <- c(-Inf,   38, 52.5, 64.5, 76.5, 88.5, 150)
karaktergrense_V23 <- c(-Inf, 36.5, 52.5, 64.5, 76.5, 88.5, 150)
karakterer         <- c("F","E","D","C","B","A")

points_to_grade <- function(data, grades, grade_threshold){
  
  grading <- rep(0, length(data))
  
  for (i in 1:length(data)) {
    for (g in 1:length(grade_threshold)) {
      if (data[i] < grade_threshold[g]){
        grading[i] <- grades[g-1]
        break
      }
    }
  }
  
  return(grading)
}
V23$totalScore
# New Grade:
DIP_V22_m1$karakter_ny <- points_to_grade(DIP_V22_m1$poengsum_ny, karakterer, karaktergrense_V22)
DIP_V22_m2$karakter_ny <- points_to_grade(DIP_V22_m2$poengsum_ny, karakterer, karaktergrense_V22)
DIP_V23_m1$karakter_ny <- points_to_grade(DIP_V23_m1$poengsum_ny, karakterer, karaktergrense_V23)
DIP_V23_m2$karakter_ny <- points_to_grade(DIP_V23_m2$poengsum_ny, karakterer, karaktergrense_V23) 

# Changed grades for each dataset
V22_m1_changed_grades <- DIP_V22_m1 %>%
                         filter(karakter != karakter_ny)

V22_m2_changed_grades <- DIP_V22_m2 %>%
                         filter(karakter != karakter_ny)

V23_m1_changed_grades <- DIP_V23_m1 %>%
                         filter(karakter != karakter_ny)

V23_m2_changed_grades <- DIP_V23_m2 %>%
                         filter(karakter != karakter_ny)

