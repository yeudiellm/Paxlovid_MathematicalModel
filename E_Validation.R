rm(list = ls(all.names = TRUE))
gc()

library("rms")
library("dplyr")
library("tidyr")
library("readr")
#Datasets gold
path = "D:/GitHub/Paxlovid_MathematicalModel"
df_pob <- read.csv(paste(path, "big_data/COVID_transform_2022_v9_p2.csv", sep = "/"))
df_counts<-read.csv(paste(path, "data/SUMMARY_SINAVE_2022.csv", sep="/"))
df_sim <- read.csv(paste(path, "data/Bernoulli_Full_Simulation.csv",sep="/"))
morbidities <- c("obesity", "diabetes", "hyperten","ckd",
                 "cardio","copd", "asthma", "immuno", "hiv")
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
write.csv(df_pob2, 
          paste(path, "data/val_sinave.csv", sep = "/"))

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
simulation_vars <-c("exec")
df_sim2 <- df_sim2[,c(simulation_vars, classification_vars, covars)]
View(df_sim2)
 
df_sim2 %>% group_by(exec, sex) %>% dplyr::summarize(count_ = sum(count),
                                                     .groups='drop')%>% 
            group_by(sex) %>% dplyr::summarize(count_ = mean(count_))

df_sim_means <- df_sim2%>% group_by(exec, sex, age_group_s) %>%
           dplyr::summarize( count_ = sum(count), 
                    prob_comorbidity = sum(count*comorbidity)/sum(count),
                    prob_covid       = sum(count*covid)/sum(count),
                    prob_hospital    = sum(count*binary_patient_type)/sum(count),
                    prob_death       = sum(death)/sum(count),
                    .groups ='drop')  %>% 
           group_by(sex, age_group_s) %>%
           dplyr::summarize( count_ = mean(count_),
                             prob_comorbidity = mean(prob_comorbidity),
                             prob_covid       = mean(prob_covid),
                             prob_hospital    = mean(prob_hospital),
                             prob_death       = mean(prob_death),
                             .groups= 'drop')

df_sim_stds <- df_sim2%>% group_by(exec, sex, age_group_s) %>%
           dplyr::summarize( count_ = sum(count), 
                            prob_comorbidity = sum(count*comorbidity)/sum(count),
                            prob_covid       = sum(count*covid)/sum(count),
                            prob_hospital    = sum(count*binary_patient_type)/sum(count),
                            prob_death       = sum(death)/sum(count),
                            .groups ='drop')  %>% 
                            group_by(sex, age_group_s) %>%
            dplyr::summarize( count_ = mean(count_),
                            prob_comorbidity = sd(prob_comorbidity),
                            prob_covid       = sd(prob_covid),
                            prob_hospital    = sd(prob_hospital),
                            prob_death       = sd(prob_death),
                            .groups= 'drop')
View(df_sim_stds)
View(df_sim_means)
write.csv(df_sim_means, 
           paste(path, "data/val_multbern_means.csv", sep = "/"))
write.csv(df_sim_stds, 
           paste(path, "data/val_multbern_stds.csv", sep="/"))


#Prueba de bootstrap #Hay que mejorarla 
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






