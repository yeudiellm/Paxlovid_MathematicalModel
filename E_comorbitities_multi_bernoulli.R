#Librerias
rm(list = ls(all.names = TRUE))
gc()

library("rms")
library("dplyr")
library("tidyr")
library("readr")
library("mipfp")
library("corrplot")

#Get Matrix OddRatio Estimation 
oddFromDataSet <- function(data_set, var_names){
  n <- ncol(data_morb)
  OR_matrix <- matrix(data=Inf, nrow=n, ncol=n)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      props  <- table(data_morb[,i], data_morb[,j])
      #print(props)
      if (props[1,2]*props[2,1]!=0){
        odd_ratio <-(props[2,2]*props[1,1])/(props[1,2]*props[2,1])}
      else{
        odd_ratio <- 0
      }
      OR_matrix[i, j] <- odd_ratio 
      OR_matrix[j, i] <- odd_ratio
    }
  }
  colnames(OR_matrix)=var_names
  rownames(OR_matrix)=var_names
  return(OR_matrix)
}

#OPCIÓN MULTI-BERNOULLI Distribution 
SimMultBernoulli<-function(data_set, size, var_names){
  p <- colMeans(data_set)
  OR_matrix <-oddFromDataSet(data_set, var_names)
  p.joint  <- ObtainMultBinaryDist(odds=OR_matrix, marg.probs = p)
  y.sim    <- RMultBinary(n = size, mult.bin.dist = p.joint)$binary.sequences
  colnames(y.sim) <- var_names
  return(y.sim)
}

#Simulacion suponiendo Multinomial-Bernoulli 
pob_input <-read.csv("C:/Users/yeudi/Downloads/Pob_comorbidity/COVID_transform_2022_v8.csv")
pob_counts<-read.csv("C:/Users/yeudi/Downloads/Pob_comorbidity/SUMMARY_SINAVE_2022.csv")
pob_input$binary_patient_type <- as.integer(ifelse(pob_input$patient_type=="H", 1,0)) 

#Nombres
names(pob_input)
classification_vars <- c("age_group", "sex")
covars      <- c("covid", "binary_patient_type", "death")
morbidities <- c("obesity", "diabetes", "hyperten","ckd",
                 "cardio","copd", "asma", "immuno", "hiv")


#Summary DataFrame
columns_summary = c(classification_vars, "comorbidity", covars,"count", morbidities)
df_summary = data.frame(matrix(nrow = 0, ncol = length(columns_summary))) 
colnames(df_summary) = columns_summary
df_summary

#Check Odd_Ratios  
for (row in 1:nrow(pob_counts)){
  sex_type       = pob_counts[row, "sex"]  
  age_group_type = pob_counts[row, "age_group"]
  data_morb <- NULL
  SS        <- NULL
  data_morb <- pob_input %>% filter(sex == sex_type,
                                    age_group == age_group_type)
  data_morb <- data_morb[, c(covars, morbidities)]
  OR_matrix <- oddFromDataSet(data_morb, 
                              var_names=colnames(data_morb))
  cat(row, sex_type, age_group_type, "\n")
  print(min(OR_matrix))
  if (length(c(covars, morbidities))!=sum(OR_matrix==Inf)){
    #print(colMeans(data_morb))
    print(sum(OR_matrix==Inf))
    print(min(OR_matrix))
  }
}


#Simulacion
for (row in 1:nrow(pob_counts)){
  sex_type       = pob_counts[row, "sex"]  
  age_group_type = pob_counts[row, "age_group"]
  cat(row, sex_type,age_group_type, "\n")
  data_morb <- NULL
  SS        <- NULL
  data_morb <- pob_input %>% filter(sex == sex_type,
                                    age_group == age_group_type)
  data_morb <- data_morb[,c(covars, morbidities)]
  var_names <- names(data_morb)
  data_morb <- SimMultBernoulli(data_morb,
                       pob_counts[row, "sospechosos_nacional_2022"], 
                       var_names)
  data_morb <- as.data.frame(data_morb)
  data_morb$comorbidity  <- as.integer(apply(data_morb[,morbidities]==1, 1, any))
  SS <- data_morb %>% group_by(binary_patient_type, covid, comorbidity) %>%
    dplyr::summarize(count=n(), 
                     across(everything(), sum),
                     .groups ='drop')
  SS$age_group=age_group_type
  SS$sex      = sex_type
  SS <- SS[, columns_summary]
  df_summary <- rbind(df_summary,SS)
}

dim(df_summary)
View(df_summary)
df_summary$binary_patient_type <- ifelse(df_summary$binary_patient_type==1,"H","A")
head(df_summary$binary_patient_type)
head(df_summary)
dim(df_summary)

write.csv(df_summary, "C:/Users/yeudi/Downloads/Pob_comorbidity/Bernoulli_Try0_Simulation.csv",
          row.names=FALSE)

