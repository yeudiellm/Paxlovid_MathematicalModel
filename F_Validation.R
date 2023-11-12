rm(list = ls(all.names = TRUE))
gc()

library("rms")
library("dplyr")
library("tidyr")
library("readr")
#Datasets gold
df_pob <-read.csv("C:/Users/yeudi/Downloads/Pob_comorbidity/COVID_transform_2022_v8.csv")
df_counts<-read.csv("C:/Users/yeudi/Downloads/Pob_comorbidity/SUMMARY_SINAVE_2022.csv")
df_sim <- read.csv("C:/Users/yeudi/Downloads/Pob_comorbidity/Bernoulli_Try0_Simulation.csv")
morbidities <- c("obesity", "diabetes", "hyperten","ckd",
                 "cardio","copd", "asma", "immuno", "hiv")
covars      <- c("comorbidity", "binary_patient_type", "covid", "death")
classification_vars <- c("age_group_s","age", "sex")
df_pob$comorbidity <- as.integer(apply(df_pob[,morbidities]==1, 1, any))

#Validacion del dataset real
df_pob$binary_patient_type <- as.integer(ifelse(df_pob$patient_type=="H", 1,0)) 
df_pob$age_group_s    <- cut(df_pob$age, breaks=c(0, 18, 50, 65, Inf), right=FALSE)
#Conteo por sexo
df_pob2 <- df_pob[df_pob$age>=18, c(classification_vars,covars)]
df_pob2%>% group_by(sex) %>% dplyr::summarize(count=n())

df_pob2 <- df_pob2%>% group_by(sex,age_group_s) %>%
  dplyr::summarize( count = n(),
                    prob_comorbidity = mean(comorbidity), 
                    prob_covid       = mean(covid), 
                    prob_hospital    = mean(binary_patient_type),
                    prob_death       = mean(death),
                   .groups ='drop')

df_pob2
View(df_pob2)
write.csv(df_pob2, "C:/Users/yeudi/Downloads/Pob_comorbidity/val_sinave.csv")

#Validacion de la simulación
useful <- c("age_group", "avg_age")
temp_data <- df_counts[, c("age_group", "avg_age")]
temp_data <- temp_data %>% group_by(age_group) %>%
              dplyr::summarize(avg_age = mean(avg_age),.groups ='drop')

df_sim2 <- merge(x=df_sim,y=temp_data, 
             by="age_group", all.x=TRUE)
df_sim2$age_group_s    <- cut(df_sim2$avg_age, breaks=c(0, 18, 50, 65, Inf), right=FALSE)
df_sim2$binary_patient_type <- as.integer(ifelse(df_sim2$binary_patient_type=="H", 1,0))
covars      <- c("comorbidity", "binary_patient_type", "covid", "count", "death")
classification_vars <- c("age_group_s","avg_age", "sex")
df_sim2 <- df_sim2[,c(classification_vars, covars)]

df_sim2 %>% group_by(sex) %>% dplyr::summarize(count_ = sum(count))

df_sim_ <- df_sim2%>% group_by(sex, age_group_s) %>%
  dplyr::summarize( count_ = sum(count), 
                    prob_comorbidity = sum(count*comorbidity)/sum(count),
                    prob_covid       = sum(count*covid)/sum(count),
                    prob_hospital    = sum(count*binary_patient_type)/sum(count),
                    prob_death       = sum(death)/sum(count),
                    .groups ='drop')
df_sim_
write.csv(df_sim_, "C:/Users/yeudi/Downloads/Pob_comorbidity/val_multbern.csv")


#Prueba de bootstrap 
boot_inter <- read.csv("C:/Users/yeudi/Downloads/Pob_comorbidity/Bootstrap_0.csv", 
                       row.names = 1)
covars      <- c("binary_patient_type","covid", "sex")
#Covariables #No cayeron dentro del intervalo estable bias izq y derecho
sum(df_sim[df_sim$binary_patient_type=="H", "count"])/sum(df_sim$count)
sum(df_sim[df_sim$covid==1, "count"])/sum(df_sim$count)
sum(df_sim[df_sim$sex=="M", "count"])/sum(df_sim$count)
#Morbidities 
morbidities <- c("obesity", "diabetes", "hyperten","ckd",
                 "cardio","copd", "asma", "immuno", "hiv")
morb_percentage <- colSums(df_sim[, morbidities])
morb_percentage/sum(df_sim$count)






