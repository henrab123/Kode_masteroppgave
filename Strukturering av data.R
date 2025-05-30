#Importing libraries:
library(irt)
library(lavaan)

# Importing the data:
library(rjson)
All_V22 <- fromJSON(file = 'C:\\Users\\andre\\OneDrive\\Skrivebord\\Filer-Master\\result_export_22.json')
All_V23 <- fromJSON(file = 'C:\\Users\\andre\\OneDrive\\Skrivebord\\Filer-Master\\result_export_23.json')

# All_V21 <- fromJSON(file = 'C:\\Users\\andre\\OneDrive\\Skrivebord\\Filer-Master\\TMA4245_V21_Cleaned.json')
All_V22_SUP <- fromJSON(file = 'C:\\Users\\andre\\OneDrive\\Skrivebord\\Filer-Master\\TMA4245_V22_Cleaned.json')
All_V23_SUP <- fromJSON(file = 'C:\\Users\\andre\\OneDrive\\Skrivebord\\Filer-Master\\TMA4245_V23_Cleaned.json')

Sensor_V22 <- read.csv("C:\\Users\\andre\\OneDrive\\Skrivebord\\Filer-Master\\sensor_22.csv", sep=';')
Sensor_V22$kandidatnummer <- as.character(Sensor_V22$kandidatnummer)
Sensor_V23 <- read.csv("C:\\Users\\andre\\OneDrive\\Skrivebord\\Filer-Master\\sensor_23.csv", sep=';')
Sensor_V23$kandidatnummer <- as.character(Sensor_V23$kandidatnummer)

Kommisjon_V22 <- read.csv("C:\\Users\\andre\\OneDrive\\Skrivebord\\Filer-Master\\kommisjon_v22.csv", sep=';')
Kommisjon_V23 <- read.csv("C:\\Users\\andre\\OneDrive\\Skrivebord\\Filer-Master\\kommisjon_v23.csv", sep=';')

Grenser_V22 <- read.csv("C:\\Users\\andre\\OneDrive\\Skrivebord\\Filer-Master\\Grenser_automatisk_v22.csv", sep=';')
Grenser_V23 <- read.csv("C:\\Users\\andre\\OneDrive\\Skrivebord\\Filer-Master\\Grenser_automatisk_v23.csv", sep=';')

#Structuring the data
library(tibble)
library(tidyr)
library(dplyr)

# data_V21 <- tibble(place = All_V21$candidates)
data_V22 <- tibble(place = All_V22$ext_inspera_candidates)
data_V23 <- tibble(place = All_V23$ext_inspera_candidates)

data_V22_sup <- tibble(place = All_V22_SUP$candidates)
data_V23_sup <- tibble(place = All_V23_SUP$candidates)


V22_SUP <- data_V22_sup %>%
           unnest_wider(place) %>%
           select(candidateId, evaluationRounds) %>%
           unnest_wider(evaluationRounds, names_sep = "_") %>%
           unnest_wider(evaluationRounds_1) %>%
           unnest_wider(roundGrades, names_sep = "_") %>%
           unnest_wider(questions, names_sep = "_") %>%
           unnest_wider(questions_3, names_sep = "_") %>%
           unnest_wider(questions_6, names_sep = "_") %>%
  
           select(candidateId, finalGradeLabel, roundId, roundGrades_1, Question5 = questions_3_evaluations, Question6 = questions_6_evaluations) %>%
           
           unnest_wider(roundGrades_1) %>%

           unnest_wider(Question5, names_sep="_") %>%
           unnest_wider(Question5_2, names_sep="_") %>%
  
           unnest_wider(Question6, names_sep="_") %>%
           unnest_wider(Question6_2, names_sep="_") %>%
  
           select(candidateId, finalGradeLabel, roundId, sensorId = Question6_2_evaluatorId, roundId, Question5 = Question5_2_score, Question6 = Question6_2_score)

V23_SUP <- data_V23_sup %>%
           unnest_wider(place) %>%
           select(candidateId, evaluationRounds) %>%
           unnest_wider(evaluationRounds, names_sep = "_") %>%
           unnest_wider(evaluationRounds_1) %>%
           unnest_wider(roundGrades, names_sep = "_") %>%
           unnest_wider(questions, names_sep = "_") %>%
           unnest_wider(questions_4, names_sep = "_") %>%
           unnest_wider(questions_5, names_sep = "_") %>%
  
           select(candidateId, finalGradeLabel, roundId, roundGrades_1, Question4 = questions_4_evaluations, Question5 = questions_5_evaluations) %>%
  
           unnest_wider(roundGrades_1) %>%
  
           unnest_wider(Question4, names_sep="_") %>%
           unnest_wider(Question4_2, names_sep="_") %>%
  
           unnest_wider(Question5, names_sep="_") %>%
           unnest_wider(Question5_2, names_sep="_") %>%
  
           select(candidateId, finalGradeLabel, roundId, sensorId = Question5_2_evaluatorId, roundId, Question4 = Question4_2_score, Question5 = Question5_2_score)

#V2022:
V22 <- data_V22 %>% 
        unnest_wider(place) %>% 
        unnest_wider(result) %>% 
        select(ext_inspera_candidateId, ext_inspera_autoScore, ext_inspera_finalGrade, ext_inspera_totalScore, score, ext_inspera_questions) %>%
        unnest_wider(ext_inspera_questions, names_sep = "") %>%
  
        #Q1:
        hoist(ext_inspera_questions1, 
              ext_inspera_autoScore1 = "ext_inspera_autoScore",
              ext_inspera_maxQuestionScore1 = "ext_inspera_maxQuestionScore",
              ext_inspera_duration1 = "ext_inspera_durationSeconds",
              ext_inspera_candidateResponses1 = "ext_inspera_candidateResponses") %>%
              
        unnest_wider(ext_inspera_candidateResponses1, names_sep = "") %>% 
        unnest_longer(ext_inspera_candidateResponses11) %>%
        unnest_longer(ext_inspera_candidateResponses12) %>% 
        unnest_longer(ext_inspera_candidateResponses13) %>% 
        unnest_longer(ext_inspera_candidateResponses14) %>% 
  
        select(-ext_inspera_candidateResponses11_id,-ext_inspera_candidateResponses12_id,-ext_inspera_candidateResponses13_id,-ext_inspera_candidateResponses14_id) %>%
  
        #Q2:
        hoist(ext_inspera_questions2, 
              ext_inspera_autoScore2 = "ext_inspera_autoScore",
              ext_inspera_maxQuestionScore2 = "ext_inspera_maxQuestionScore",
              ext_inspera_duration2 = "ext_inspera_durationSeconds",
              ext_inspera_candidateResponses2 = "ext_inspera_candidateResponses") %>%
          
        unnest_wider(ext_inspera_candidateResponses2, names_sep = "") %>% 
        unnest_longer(ext_inspera_candidateResponses21) %>%
        unnest_longer(ext_inspera_candidateResponses22) %>% 
        unnest_longer(ext_inspera_candidateResponses23) %>% 
        unnest_longer(ext_inspera_candidateResponses24) %>%
        unnest_longer(ext_inspera_candidateResponses25) %>% 
        unnest_longer(ext_inspera_candidateResponses26) %>% 
          
        select(-ext_inspera_candidateResponses21_id,-ext_inspera_candidateResponses22_id,-ext_inspera_candidateResponses23_id,-ext_inspera_candidateResponses24_id,-ext_inspera_candidateResponses25_id,-ext_inspera_candidateResponses26_id) %>%

        #Q3:
        hoist(ext_inspera_questions3, 
              ext_inspera_autoScore3 = "ext_inspera_autoScore",
              ext_inspera_maxQuestionScore3 = "ext_inspera_maxQuestionScore",
              ext_inspera_duration3 = "ext_inspera_durationSeconds",
              ext_inspera_candidateResponses3 = "ext_inspera_candidateResponses") %>%
          
        unnest_wider(ext_inspera_candidateResponses3, names_sep = "") %>% 
        unnest_longer(ext_inspera_candidateResponses31) %>%
        unnest_longer(ext_inspera_candidateResponses32) %>% 
        unnest_longer(ext_inspera_candidateResponses33) %>% 
        unnest_longer(ext_inspera_candidateResponses34) %>% 
        unnest_longer(ext_inspera_candidateResponses35) %>% 
        select(-ext_inspera_candidateResponses31_id,-ext_inspera_candidateResponses32_id,-ext_inspera_candidateResponses33_id,-ext_inspera_candidateResponses34_id, - ext_inspera_candidateResponses35_id) %>%
  
        #Q4:
        hoist(ext_inspera_questions4, 
              ext_inspera_autoScore4 = "ext_inspera_autoScore",
              ext_inspera_maxQuestionScore4 = "ext_inspera_maxQuestionScore",
              ext_inspera_duration4 = "ext_inspera_durationSeconds",
              ext_inspera_candidateResponses4 = "ext_inspera_candidateResponses") %>%
        
        unnest_wider(ext_inspera_candidateResponses4, names_sep = "") %>% 
        unnest_longer(ext_inspera_candidateResponses41) %>%
        unnest_longer(ext_inspera_candidateResponses42) %>% 
        unnest_longer(ext_inspera_candidateResponses43) %>% 
  
        select(-ext_inspera_candidateResponses41_id,-ext_inspera_candidateResponses42_id,-ext_inspera_candidateResponses43_id) %>%
  
        #Q5:
        hoist(ext_inspera_questions5, 
              ext_inspera_manualScore5 = "ext_inspera_manualScores",
              ext_inspera_maxQuestionScore5 = "ext_inspera_maxQuestionScore",
              ext_inspera_duration5 = "ext_inspera_durationSeconds") %>%
        unnest_longer(ext_inspera_manualScore5) %>%
        hoist(ext_inspera_manualScore5, ext_inspera_manual_Score5 = "ext_inspera_manualScore", sensor1 = "ext_inspera_gradingTeacherName") %>%
        select(-ext_inspera_manualScore5) %>%  
        #Q6:
        hoist(ext_inspera_questions6, 
              ext_inspera_manualScore6 = "ext_inspera_manualScores",
              ext_inspera_maxQuestionScore6 = "ext_inspera_maxQuestionScore",
              ext_inspera_duration6 = "ext_inspera_durationSeconds") %>%
        unnest_longer(ext_inspera_manualScore6) %>%
        hoist(ext_inspera_manualScore6, ext_inspera_manual_Score6 = "ext_inspera_manualScore", sensor2 = "ext_inspera_gradingTeacherName") %>%
        select(-ext_inspera_manualScore6) %>%
        
        select(-ext_inspera_questions1, -ext_inspera_questions2, -ext_inspera_questions3, -ext_inspera_questions4, -ext_inspera_questions5, -ext_inspera_questions6) 


#Creating binary response for automatically corrected task V22:
for (i in (1:nrow(V22))){

  #Task 1.1
  if      (is.na(V22$ext_inspera_candidateResponses11[i]) || V22$ext_inspera_candidateResponses11[i] == ""){
           V22$ext_inspera_candidateResponses11[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses11[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[1])) && as.numeric(V22$ext_inspera_candidateResponses11[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[1]))){
           V22$ext_inspera_candidateResponses11[i] <- 1}
  else    {V22$ext_inspera_candidateResponses11[i] <- 0}
  
  #Task 1.2
  if      (is.na(V22$ext_inspera_candidateResponses12[i]) || V22$ext_inspera_candidateResponses12[i] == ""){
           V22$ext_inspera_candidateResponses12[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses12[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[2])) && as.numeric(V22$ext_inspera_candidateResponses12[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[2]))){
           V22$ext_inspera_candidateResponses12[i] <- 1}
  else    {V22$ext_inspera_candidateResponses12[i] <- 0}
  
  #Task 1.3
  if      (is.na(V22$ext_inspera_candidateResponses13[i]) || V22$ext_inspera_candidateResponses13[i] == ""){
           V22$ext_inspera_candidateResponses13[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses13[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[3])) && as.numeric(V22$ext_inspera_candidateResponses13[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[3]))){
           V22$ext_inspera_candidateResponses13[i] <- 1}
  else    {V22$ext_inspera_candidateResponses13[i] <- 0}
  
  #Task 1.4
  if      (is.na(V22$ext_inspera_candidateResponses14[i]) || V22$ext_inspera_candidateResponses14[i] == ""){
           V22$ext_inspera_candidateResponses14[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses14[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[4])) && as.numeric(V22$ext_inspera_candidateResponses14[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[4]))){
           V22$ext_inspera_candidateResponses14[i] <- 1}
  else    {V22$ext_inspera_candidateResponses14[i] <- 0}
  
  #Task 2.1
  if      (is.na(V22$ext_inspera_candidateResponses21[i]) || V22$ext_inspera_candidateResponses21[i] == ""){
           V22$ext_inspera_candidateResponses21[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses21[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[5])) && as.numeric(V22$ext_inspera_candidateResponses21[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[5]))){
           V22$ext_inspera_candidateResponses21[i] <- 1}
  else    {V22$ext_inspera_candidateResponses21[i] <- 0}
  
  #Task 2.2
  if      (is.na(V22$ext_inspera_candidateResponses22[i]) || V22$ext_inspera_candidateResponses22[i] == ""){
           V22$ext_inspera_candidateResponses22[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses22[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[6])) && as.numeric(V22$ext_inspera_candidateResponses22[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[6]))){
           V22$ext_inspera_candidateResponses22[i] <- 1}
  else    {V22$ext_inspera_candidateResponses22[i] <- 0}
  
  #Task 2.3
  if      (is.na(V22$ext_inspera_candidateResponses23[i]) || V22$ext_inspera_candidateResponses23[i] == ""){
           V22$ext_inspera_candidateResponses23[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses23[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[7])) && as.numeric(V22$ext_inspera_candidateResponses23[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[7]))){
           V22$ext_inspera_candidateResponses23[i] <- 1}
  else    {V22$ext_inspera_candidateResponses23[i] <- 0}
  
  #Task 2.4
  if      (is.na(V22$ext_inspera_candidateResponses24[i]) || V22$ext_inspera_candidateResponses24[i] == ""){
           V22$ext_inspera_candidateResponses24[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses24[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[8])) && as.numeric(V22$ext_inspera_candidateResponses24[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[8]))){
           V22$ext_inspera_candidateResponses24[i] <- 1}
  else    {V22$ext_inspera_candidateResponses24[i] <- 0}
  
  #Task 2.5
  if      (is.na(V22$ext_inspera_candidateResponses25[i]) || V22$ext_inspera_candidateResponses25[i] == ""){
           V22$ext_inspera_candidateResponses25[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses25[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[9])) && as.numeric(V22$ext_inspera_candidateResponses25[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[9]))){
           V22$ext_inspera_candidateResponses25[i] <- 1}
  else    {V22$ext_inspera_candidateResponses25[i] <- 0}

  #Task 2.6
  if      (is.na(V22$ext_inspera_candidateResponses26[i]) || V22$ext_inspera_candidateResponses26[i] == ""){
           V22$ext_inspera_candidateResponses26[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses26[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[10])) && as.numeric(V22$ext_inspera_candidateResponses26[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[10]))){
           V22$ext_inspera_candidateResponses26[i] <- 1}
  else    {V22$ext_inspera_candidateResponses26[i] <- 0}
  
  #Task 3.1
  if      (is.na(V22$ext_inspera_candidateResponses31[i]) || V22$ext_inspera_candidateResponses31[i] == ""){
           V22$ext_inspera_candidateResponses31[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses31[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[11])) && as.numeric(V22$ext_inspera_candidateResponses31[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[11]))){
           V22$ext_inspera_candidateResponses31[i] <- 1}
  else    {V22$ext_inspera_candidateResponses31[i] <- 0}
  
  #Task 3.2
  if      (is.na(V22$ext_inspera_candidateResponses32[i]) || V22$ext_inspera_candidateResponses32[i] == ""){
           V22$ext_inspera_candidateResponses32[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses32[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[12])) && as.numeric(V22$ext_inspera_candidateResponses32[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[12]))){
           V22$ext_inspera_candidateResponses32[i] <- 1}
  else    {V22$ext_inspera_candidateResponses32[i] <- 0}
  
  #Task 3.3
  if      (is.na(V22$ext_inspera_candidateResponses33[i]) || V22$ext_inspera_candidateResponses33[i] == ""){
           V22$ext_inspera_candidateResponses33[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses33[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[13])) && as.numeric(V22$ext_inspera_candidateResponses33[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[13]))){
           V22$ext_inspera_candidateResponses33[i] <- 1}
  else    {V22$ext_inspera_candidateResponses33[i] <- 0}
  
  #Task 3.4
  if      (is.na(V22$ext_inspera_candidateResponses34[i]) || V22$ext_inspera_candidateResponses34[i] == ""){
           V22$ext_inspera_candidateResponses34[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses34[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[14])) && as.numeric(V22$ext_inspera_candidateResponses34[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[14]))){
           V22$ext_inspera_candidateResponses34[i] <- 1}
  else    {V22$ext_inspera_candidateResponses34[i] <- 0}
  
  #Task 3.5
  if      (is.na(V22$ext_inspera_candidateResponses35[i]) || V22$ext_inspera_candidateResponses35[i] == ""){
           V22$ext_inspera_candidateResponses35[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses35[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[15])) && as.numeric(V22$ext_inspera_candidateResponses35[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[15]))){
           V22$ext_inspera_candidateResponses35[i] <- 1}
  else    {V22$ext_inspera_candidateResponses35[i] <- 0}
  
  #Task 4.1
  if      (is.na(V22$ext_inspera_candidateResponses41[i]) || V22$ext_inspera_candidateResponses41[i] == ""){
           V22$ext_inspera_candidateResponses41[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses41[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[16])) && as.numeric(V22$ext_inspera_candidateResponses41[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[16]))){
           V22$ext_inspera_candidateResponses41[i] <- 1}
  else    {V22$ext_inspera_candidateResponses41[i] <- 0}
  
  #Task 4.2
  if      (is.na(V22$ext_inspera_candidateResponses42[i]) || V22$ext_inspera_candidateResponses42[i] == ""){
           V22$ext_inspera_candidateResponses42[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses42[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[17])) && as.numeric(V22$ext_inspera_candidateResponses42[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[17]))){
           V22$ext_inspera_candidateResponses42[i] <- 1}
  else    {V22$ext_inspera_candidateResponses42[i] <- 0}
  
  #Task 4.3
  if      (is.na(V22$ext_inspera_candidateResponses43[i]) || V22$ext_inspera_candidateResponses43[i] == ""){
           V22$ext_inspera_candidateResponses43[i] <- 0}
  else if (as.numeric(V22$ext_inspera_candidateResponses43[i]) >= as.numeric(gsub(",",".",Grenser_V22$Min[18])) && as.numeric(V22$ext_inspera_candidateResponses43[i]) <= as.numeric(gsub(",",".",Grenser_V22$Max[18]))){
           V22$ext_inspera_candidateResponses43[i] <- 1}
  else    {V22$ext_inspera_candidateResponses43[i] <- 0}
  
}
  
  
#V2023:
V23 <-  data_V23 %>% 
        unnest_wider(place) %>% 
        unnest_wider(result) %>% 
        select(ext_inspera_candidateId, ext_inspera_finalGrade, ext_inspera_autoScore, ext_inspera_questions) %>%
        unnest_wider(ext_inspera_questions, names_sep = "") %>%
  
        #Q1:
        hoist(ext_inspera_questions1, 
              ext_inspera_autoScore1 = "ext_inspera_autoScore",
              ext_inspera_maxQuestionScore1 = "ext_inspera_maxQuestionScore",
              ext_inspera_duration1 = "ext_inspera_durationSeconds",
              ext_inspera_candidateResponses1 = "ext_inspera_candidateResponses") %>%
  
        unnest_wider(ext_inspera_candidateResponses1, names_sep = "") %>% 
        unnest_longer(ext_inspera_candidateResponses11) %>%
        unnest_longer(ext_inspera_candidateResponses12) %>% 
        unnest_longer(ext_inspera_candidateResponses13) %>% 
        unnest_longer(ext_inspera_candidateResponses14) %>%
        unnest_longer(ext_inspera_candidateResponses15) %>%
  
        select(-ext_inspera_candidateResponses11_id,-ext_inspera_candidateResponses12_id,-ext_inspera_candidateResponses13_id,-ext_inspera_candidateResponses14_id,-ext_inspera_candidateResponses15_id) %>%
        
        #Q2:
        hoist(ext_inspera_questions2, 
              ext_inspera_autoScore2 = "ext_inspera_autoScore",
              ext_inspera_maxQuestionScore2 = "ext_inspera_maxQuestionScore",
              ext_inspera_duration2 = "ext_inspera_durationSeconds",
              ext_inspera_candidateResponses2 = "ext_inspera_candidateResponses") %>%
        
        unnest_wider(ext_inspera_candidateResponses2, names_sep = "") %>% 
        unnest_longer(ext_inspera_candidateResponses21) %>%
        unnest_longer(ext_inspera_candidateResponses22) %>% 
        unnest_longer(ext_inspera_candidateResponses23) %>% 
        unnest_longer(ext_inspera_candidateResponses24) %>%
        unnest_longer(ext_inspera_candidateResponses25) %>%
        
        select(-ext_inspera_candidateResponses21_id,-ext_inspera_candidateResponses22_id,-ext_inspera_candidateResponses23_id,-ext_inspera_candidateResponses24_id,-ext_inspera_candidateResponses25_id) %>%
        
        #Q3:
        hoist(ext_inspera_questions3, 
              ext_inspera_autoScore3 = "ext_inspera_autoScore",
              ext_inspera_maxQuestionScore3 = "ext_inspera_maxQuestionScore",
              ext_inspera_duration3 = "ext_inspera_durationSeconds",
              ext_inspera_candidateResponses3 = "ext_inspera_candidateResponses") %>%
        
        unnest_wider(ext_inspera_candidateResponses3, names_sep = "") %>% 
        unnest_longer(ext_inspera_candidateResponses31) %>%
        unnest_longer(ext_inspera_candidateResponses32) %>% 
        unnest_longer(ext_inspera_candidateResponses33) %>% 
        unnest_longer(ext_inspera_candidateResponses34) %>%
        unnest_longer(ext_inspera_candidateResponses35) %>%
        
        select(-ext_inspera_candidateResponses31_id,-ext_inspera_candidateResponses32_id,-ext_inspera_candidateResponses33_id,-ext_inspera_candidateResponses34_id,-ext_inspera_candidateResponses35_id) %>%
  
        #Q4:
        hoist(ext_inspera_questions4, 
              ext_inspera_manualScore4 = "ext_inspera_manualScores",
              ext_inspera_maxQuestionScore4 = "ext_inspera_maxQuestionScore",
              ext_inspera_duration4 = "ext_inspera_durationSeconds") %>%
        unnest_longer(ext_inspera_manualScore4) %>%
        hoist(ext_inspera_manualScore4, 
              ext_inspera_manual_Score4 = "ext_inspera_manualScore",
              sensor1 = "ext_inspera_gradingTeacherName") %>%
        select(-ext_inspera_manualScore4) %>%
        
        #Q5:
        hoist(ext_inspera_questions5, 
              ext_inspera_manualScore5 = "ext_inspera_manualScores",
              ext_inspera_maxQuestionScore5 = "ext_inspera_maxQuestionScore",
              ext_inspera_duration5 = "ext_inspera_durationSeconds") %>%
        unnest_longer(ext_inspera_manualScore5) %>%
        hoist(ext_inspera_manualScore5, 
              ext_inspera_manual_Score5 = "ext_inspera_manualScore",
              sensor2 = "ext_inspera_gradingTeacherName") %>%
        select(-ext_inspera_manualScore5) %>%
      
        select(-ext_inspera_questions1, -ext_inspera_questions2, -ext_inspera_questions3, -ext_inspera_questions4, -ext_inspera_questions5)


#Creating binary response for automaticly corrected taskt V23:
for (i in (1:nrow(V23))){
  
  #Task 1.1
  if      (is.na(V23$ext_inspera_candidateResponses11[i]) || V23$ext_inspera_candidateResponses11[i] == ""){
           V23$ext_inspera_candidateResponses11[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses11[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[1])) && as.numeric(V23$ext_inspera_candidateResponses11[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[1]))){
           V23$ext_inspera_candidateResponses11[i] <- 1}
  else    {V23$ext_inspera_candidateResponses11[i] <- 0}
  
  #Task 1.2
  if      (is.na(V23$ext_inspera_candidateResponses12[i]) || V23$ext_inspera_candidateResponses12[i] == ""){
           V23$ext_inspera_candidateResponses12[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses12[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[2])) && as.numeric(V23$ext_inspera_candidateResponses12[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[2]))){
           V23$ext_inspera_candidateResponses12[i] <- 1}
  else    {V23$ext_inspera_candidateResponses12[i] <- 0}
  
  #Task 1.3
  if      (is.na(V23$ext_inspera_candidateResponses13[i]) || V23$ext_inspera_candidateResponses13[i] == ""){
           V23$ext_inspera_candidateResponses13[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses13[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[3])) && as.numeric(V23$ext_inspera_candidateResponses13[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[3]))){
           V23$ext_inspera_candidateResponses13[i] <- 1}
  else    {V23$ext_inspera_candidateResponses13[i] <- 0}
  
  #Task 1.4
  if      (is.na(V23$ext_inspera_candidateResponses14[i]) || V23$ext_inspera_candidateResponses14[i] == ""){
           V23$ext_inspera_candidateResponses14[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses14[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[4])) && as.numeric(V23$ext_inspera_candidateResponses14[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[4]))){
           V23$ext_inspera_candidateResponses14[i] <- 1}
  else    {V23$ext_inspera_candidateResponses14[i] <- 0}
  
  #Task 1.5
  if      (is.na(V23$ext_inspera_candidateResponses15[i]) || V23$ext_inspera_candidateResponses15[i] == ""){
           V23$ext_inspera_candidateResponses15[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses15[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[5])) && as.numeric(V23$ext_inspera_candidateResponses15[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[5]))){
           V23$ext_inspera_candidateResponses15[i] <- 1}
  else    {V23$ext_inspera_candidateResponses15[i] <- 0}
  
  #Task 2.1
  if      (is.na(V23$ext_inspera_candidateResponses21[i]) || V23$ext_inspera_candidateResponses21[i] == ""){
           V23$ext_inspera_candidateResponses21[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses21[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[6])) && as.numeric(V23$ext_inspera_candidateResponses21[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[6]))){
           V23$ext_inspera_candidateResponses21[i] <- 1}
  else    {V23$ext_inspera_candidateResponses21[i] <- 0}
  
  #Task 2.2
  if      (is.na(V23$ext_inspera_candidateResponses22[i]) || V23$ext_inspera_candidateResponses22[i] == ""){
           V23$ext_inspera_candidateResponses22[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses22[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[7])) && as.numeric(V23$ext_inspera_candidateResponses22[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[7]))){
           V23$ext_inspera_candidateResponses22[i] <- 1}
  else    {V23$ext_inspera_candidateResponses22[i] <- 0}
  
  #Task 2.3
  if      (is.na(V23$ext_inspera_candidateResponses23[i]) || V23$ext_inspera_candidateResponses23[i] == ""){
           V23$ext_inspera_candidateResponses23[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses23[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[8])) && as.numeric(V23$ext_inspera_candidateResponses23[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[8]))){
           V23$ext_inspera_candidateResponses23[i] <- 1}
  else    {V23$ext_inspera_candidateResponses23[i] <- 0}
  
  #Task 2.4
  if      (is.na(V23$ext_inspera_candidateResponses24[i]) || V23$ext_inspera_candidateResponses24[i] == ""){
           V23$ext_inspera_candidateResponses24[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses24[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[9])) && as.numeric(V23$ext_inspera_candidateResponses24[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[9]))){
           V23$ext_inspera_candidateResponses24[i] <- 1}
  else    {V23$ext_inspera_candidateResponses24[i] <- 0}
  
  #Task 2.5
  if      (is.na(V23$ext_inspera_candidateResponses25[i]) || V23$ext_inspera_candidateResponses25[i] == ""){
           V23$ext_inspera_candidateResponses25[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses25[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[10])) && as.numeric(V23$ext_inspera_candidateResponses25[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[10]))){
           V23$ext_inspera_candidateResponses25[i] <- 1}
  else    {V23$ext_inspera_candidateResponses25[i] <- 0}
  
  
  #Task 3.1
  if      (is.na(V23$ext_inspera_candidateResponses31[i]) || V23$ext_inspera_candidateResponses31[i] == ""){
           V23$ext_inspera_candidateResponses31[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses31[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[11])) && as.numeric(V23$ext_inspera_candidateResponses31[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[11]))){
           V23$ext_inspera_candidateResponses31[i] <- 1}
  else    {V23$ext_inspera_candidateResponses31[i] <- 0}
  
  #Task 3.2
  if      (is.na(V23$ext_inspera_candidateResponses32[i]) || V23$ext_inspera_candidateResponses32[i] == ""){
           V23$ext_inspera_candidateResponses32[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses32[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[12])) && as.numeric(V23$ext_inspera_candidateResponses32[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[12]))){
           V23$ext_inspera_candidateResponses32[i] <- 1}
  else    {V23$ext_inspera_candidateResponses32[i] <- 0}
  
  #Task 3.3
  if      (is.na(V23$ext_inspera_candidateResponses33[i]) || V23$ext_inspera_candidateResponses33[i] == ""){
           V23$ext_inspera_candidateResponses33[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses33[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[13])) && as.numeric(V23$ext_inspera_candidateResponses33[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[13]))){
           V23$ext_inspera_candidateResponses33[i] <- 1}
  else    {V23$ext_inspera_candidateResponses33[i] <- 0}
  
  #Task 3.4
  if      (is.na(V23$ext_inspera_candidateResponses34[i]) || V23$ext_inspera_candidateResponses34[i] == ""){
           V23$ext_inspera_candidateResponses34[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses34[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[14])) && as.numeric(V23$ext_inspera_candidateResponses34[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[14]))){
           V23$ext_inspera_candidateResponses34[i] <- 1}
  else    {V23$ext_inspera_candidateResponses34[i] <- 0}
  
  #Task 3.5
  if      (is.na(V23$ext_inspera_candidateResponses35[i]) || V23$ext_inspera_candidateResponses35[i] == ""){
           V23$ext_inspera_candidateResponses35[i] <- 0}
  else if (as.numeric(V23$ext_inspera_candidateResponses35[i]) >= as.numeric(gsub(",",".",Grenser_V23$Min[15])) && as.numeric(V23$ext_inspera_candidateResponses35[i]) <= as.numeric(gsub(",",".",Grenser_V23$Max[15]))){
           V23$ext_inspera_candidateResponses35[i] <- 1}
  else    {V23$ext_inspera_candidateResponses35[i] <- 0}
  
}

V22 <- right_join(Sensor_V22, V22, by = c("kandidatnummer" = "ext_inspera_candidateId"))
V23 <- right_join(Sensor_V23, V23, by = c("kandidatnummer" = "ext_inspera_candidateId"))

V22 <- left_join(V22, V22_SUP, by=c("kandidatnummer" = "candidateId"))
V23 <- left_join(V23, V23_SUP, by=c("kandidatnummer" = "candidateId"))

V22 <- V22 %>% mutate(ext_inspera_manual_Score5 = Question5, ext_inspera_manual_Score6 = Question6) %>%
               select(-sensor1, -sensor2) %>%
               mutate(totalScore = ext_inspera_autoScore + Question5 + Question6) %>%
               mutate(checkTotalScore = totalScore == ext_inspera_totalScore) %>%
               mutate(checkTotalScoreNA = !is.na(ext_inspera_totalScore)) %>%
               mutate(ext_inspera_totalScore = totalScore, ext_inspera_finalGrade = finalGradeLabel) %>%
               mutate(klagde = case_when(kommisjon == 7 ~ TRUE,
                                         kommisjon == 8 ~ TRUE))%>%
               distinct(kandidatnummer, .keep_all = TRUE)

V23 <- V23 %>% mutate(ext_inspera_manual_Score4 = Question4, ext_inspera_manual_Score5 = Question5) %>%
               select(-sensor1, -sensor2) %>%
               mutate(totalScore = ext_inspera_autoScore + Question4 + Question5) %>%
               mutate(klagde = case_when(kommisjon == 9  ~ TRUE,
                                         kommisjon == 10 ~ TRUE,
                                         kommisjon == 11 ~ TRUE)) %>%
               distinct(kandidatnummer, .keep_all = TRUE)

V22 <- V22 %>%   mutate(kommisjon = case_when(sensorId == 6731152 ~ 1,
                                              sensorId == 4576608 ~ 2,
                                              sensorId == 91385056 ~ 3,
                                              sensorId == 4578543 ~ 4,
                                              sensorId == 54222899 ~ 5,
                                              sensorId == 4588800 ~ 6))

V23 <- V23 %>%   mutate(kommisjon = case_when(sensorId == 4576608 ~ 1,
                                              sensorId == 6731152 ~ 2,
                                              sensorId == 91385056 ~ 3,
                                              sensorId == 4594128 ~ 4,
                                              sensorId == 120394339 ~ 5,
                                              sensorId == 89108842 ~ 6,
                                              sensorId == 54222899 ~ 7,
                                              sensorId == 4588800 ~ 8))

SensorIDs_V23 <- unique(V23$sensorId)
Kommisjonsnummer_V22 <- c()
SensorIDs_V23

V22$totalScore <- V22$ext_inspera_totalScore

#https://www.theuncertaintyproject.org/tools/noise-audit 

#Analysis:
library(RTMB)

#Preparing for paired T-test
PT_test_data_V22 <- select(V22, "kommisjon", "kandidatnummer", "ext_inspera_manual_Score5", "ext_inspera_manual_Score6", "ext_inspera_autoScore")

PT_test_data_V22$autoScore   <- PT_test_data_V22$ext_inspera_autoScore
PT_test_data_V22$manualScore <- PT_test_data_V22$ext_inspera_manual_Score5+
                                PT_test_data_V22$ext_inspera_manual_Score6

Average_a_V22 <- mean(PT_test_data_V22$autoScore)
Average_m_V22 <- mean(PT_test_data_V22$manualScore)
Max_a_V22 <- 45
Max_m_V22 <- 55

PT_test_data_V22$autoRel   <- (PT_test_data_V22$autoScore   - Average_a_V22)/Max_a_V22
PT_test_data_V22$manualRel <- (PT_test_data_V22$manualScore - Average_m_V22)/Max_m_V22


PT_test_data_V23 <- select(V23, "kommisjon", "kandidatnummer", "ext_inspera_manual_Score4", "ext_inspera_manual_Score5", "ext_inspera_autoScore")

PT_test_data_V23$autoScore   <- PT_test_data_V23$ext_inspera_autoScore
PT_test_data_V23$manualScore <- PT_test_data_V23$ext_inspera_manual_Score4+
                                PT_test_data_V23$ext_inspera_manual_Score5

Average_a_V23 <- mean(PT_test_data_V23$autoScore)
Average_m_V23 <- mean(PT_test_data_V23$manualScore)
Max_a_V23 <- 30
Max_m_V23 <- 70

PT_test_data_V23$autoRel   <- (PT_test_data_V23$autoScore   - Average_a_V23)/Max_a_V23
PT_test_data_V23$manualRel <- (PT_test_data_V23$manualScore - Average_m_V23)/Max_m_V23

PT_test_data_V22 <- select(PT_test_data_V22,"kommisjon", "kandidatnummer", "autoScore", "manualScore", "autoRel", "manualRel")
PT_test_data_V23 <- select(PT_test_data_V23,"kommisjon", "kandidatnummer", "autoScore", "manualScore", "autoRel", "manualRel")

#Paired T-test:
library(tidyverse)
for (i in 1:6) {
  test <- PT_test_data_V22 %>% filter(kommisjon == i) %>% select("kandidatnummer","autoRel", "manualRel")
  ttest_res <- t.test(test$autoRel,test$manualRel, mu=0, alt="two.sided", paired = T, conf.level = 0.95)
  print(paste("Number: ", i))
  print(paste("P-value:",ttest_res$p.value))
  print(paste("Mean-diff:",ttest_res$estimate))
  print(" ")
  boxplot(test$autoRel,test$manualRel)
}

for (i in 1:8) {
  test <- PT_test_data_V23 %>% filter(kommisjon == i) %>% select("kandidatnummer","autoRel", "manualRel")
  ttest_res <- t.test(test$autoRel,test$manualRel, mu=0, alt="two.sided", paired = T, conf.level = 0.95)
  print(paste("Number: ", i))
  print(paste("P-value:",ttest_res$p.value))
  print(paste("Mean-diff:",ttest_res$estimate))
  print(" ")
  boxplot(test$autoRel,test$manualRel)
}

#Preparing for paired F-test
F_test_data_V22 <- PT_test_data_V22 %>% select(kommisjon)
F_test_data_V22$scoreDiff <- PT_test_data_V22$manualRel - PT_test_data_V22$autoRel

F_test_data_V23 <- PT_test_data_V23 %>% select(kommisjon)
F_test_data_V23$scoreDiff <- PT_test_data_V23$manualRel - PT_test_data_V23$autoRel

#Preparing data fort RTMB:'

#cATEGORIZING THE MANUAL SCORED QUESTIONS FROM 1 TO N:
V22_uniq_sort_manual_score5 <- sort(unique(V22$ext_inspera_manual_Score5))
V22_uniq_sort_manual_score6 <- sort(unique(V22$ext_inspera_manual_Score6))

V22_ranked_manual_score5 <- match(V22$ext_inspera_manual_Score5, V22_uniq_sort_manual_score5)
V22_ranked_manual_score6 <- match(V22$ext_inspera_manual_Score6, V22_uniq_sort_manual_score6)

V22_ranked_manual_score5 <- V22_ranked_manual_score5 - 1 
V22_ranked_manual_score6 <- V22_ranked_manual_score6 - 1

# V22
V22$Y <- cbind(
  as.double(V22$ext_inspera_candidateResponses11),
  as.double(V22$ext_inspera_candidateResponses12),
  as.double(V22$ext_inspera_candidateResponses13),
  as.double(V22$ext_inspera_candidateResponses14),
  as.double(V22$ext_inspera_candidateResponses21),
  as.double(V22$ext_inspera_candidateResponses22),
  as.double(V22$ext_inspera_candidateResponses23),
  as.double(V22$ext_inspera_candidateResponses24),
  as.double(V22$ext_inspera_candidateResponses25),
  as.double(V22$ext_inspera_candidateResponses26),
  as.double(V22$ext_inspera_candidateResponses31),
  as.double(V22$ext_inspera_candidateResponses32),
  as.double(V22$ext_inspera_candidateResponses33),
  as.double(V22$ext_inspera_candidateResponses34),
  as.double(V22$ext_inspera_candidateResponses35),
  as.double(V22$ext_inspera_candidateResponses41),
  as.double(V22$ext_inspera_candidateResponses42),
  as.double(V22$ext_inspera_candidateResponses43),
  as.double(V22_ranked_manual_score5),
  as.double(V22_ranked_manual_score6)
)

V22$Y_m <- cbind(
  as.double(V22$ext_inspera_candidateResponses11),
  as.double(V22$ext_inspera_candidateResponses12),
  as.double(V22$ext_inspera_candidateResponses13),
  as.double(V22$ext_inspera_candidateResponses14),
  as.double(V22$ext_inspera_candidateResponses21),
  as.double(V22$ext_inspera_candidateResponses22),
  as.double(V22$ext_inspera_candidateResponses23),
  as.double(V22$ext_inspera_candidateResponses24),
  as.double(V22$ext_inspera_candidateResponses25),
  as.double(V22$ext_inspera_candidateResponses26),
  as.double(V22$ext_inspera_candidateResponses31),
  as.double(V22$ext_inspera_candidateResponses32),
  as.double(V22$ext_inspera_candidateResponses33),
  as.double(V22$ext_inspera_candidateResponses34),
  as.double(V22$ext_inspera_candidateResponses35),
  as.double(V22$ext_inspera_candidateResponses41),
  as.double(V22$ext_inspera_candidateResponses42),
  as.double(V22$ext_inspera_candidateResponses43),
  as.double(V22$ext_inspera_manual_Score5),
  as.double(V22$ext_inspera_manual_Score6)
)

RTMB_V22 <- V22 %>% select(kommisjon, kandidatnummer, Y, Y_m)


#cATEGORIZING THE MANUAL SCORED QUESTIONS FROM 1 TO N:
V23_uniq_sort_manual_score4 <- sort(unique(V23$ext_inspera_manual_Score4))
V23_uniq_sort_manual_score5 <- sort(unique(V23$ext_inspera_manual_Score5))

V23_ranked_manual_score4 <- match(V23$ext_inspera_manual_Score4, V23_uniq_sort_manual_score4)
V23_ranked_manual_score5 <- match(V23$ext_inspera_manual_Score5, V23_uniq_sort_manual_score5)

V23_ranked_manual_score4 <- V23_ranked_manual_score4-1
V23_ranked_manual_score5 <- V23_ranked_manual_score5-1

# V23
V23$Y <- cbind(
  as.double(V23$ext_inspera_candidateResponses11),
  as.double(V23$ext_inspera_candidateResponses12),
  as.double(V23$ext_inspera_candidateResponses13),
  as.double(V23$ext_inspera_candidateResponses14),
  as.double(V23$ext_inspera_candidateResponses15),
  as.double(V23$ext_inspera_candidateResponses21),
  as.double(V23$ext_inspera_candidateResponses22),
  as.double(V23$ext_inspera_candidateResponses23),
  as.double(V23$ext_inspera_candidateResponses24),
  as.double(V23$ext_inspera_candidateResponses25),
  as.double(V23$ext_inspera_candidateResponses31),
  as.double(V23$ext_inspera_candidateResponses32),
  as.double(V23$ext_inspera_candidateResponses33),
  as.double(V23$ext_inspera_candidateResponses34),
  as.double(V23$ext_inspera_candidateResponses35),
  as.double(V23_ranked_manual_score4),
  as.double(V23_ranked_manual_score5)
)

V23$Y_m <- cbind(
  as.double(V23$ext_inspera_candidateResponses11),
  as.double(V23$ext_inspera_candidateResponses12),
  as.double(V23$ext_inspera_candidateResponses13),
  as.double(V23$ext_inspera_candidateResponses14),
  as.double(V23$ext_inspera_candidateResponses15),
  as.double(V23$ext_inspera_candidateResponses21),
  as.double(V23$ext_inspera_candidateResponses22),
  as.double(V23$ext_inspera_candidateResponses23),
  as.double(V23$ext_inspera_candidateResponses24),
  as.double(V23$ext_inspera_candidateResponses25),
  as.double(V23$ext_inspera_candidateResponses31),
  as.double(V23$ext_inspera_candidateResponses32),
  as.double(V23$ext_inspera_candidateResponses33),
  as.double(V23$ext_inspera_candidateResponses34),
  as.double(V23$ext_inspera_candidateResponses35),
  as.double(V23$ext_inspera_manual_Score4),
  as.double(V23$ext_inspera_manual_Score5)
)

RTMB_V23 <- V23 %>% select(kommisjon, kandidatnummer, Y, Y_m)

#Preparing for klageanalyse:

KLAGE_V22 <- V22 %>% select(kommisjon, kandidatnummer, finalGradeLabel, totalScore, klagde) %>%
                     mutate(klagde = ifelse(!is.na(klagde), 1, 0))


KLAGE_V23 <- V23 %>% select(kommisjon, kandidatnummer, finalGradeLabel, totalScore, klagde)%>%
                     mutate(klagde = ifelse(!is.na(klagde), 1, 0))
                       
