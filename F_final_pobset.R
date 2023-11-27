rm(list = ls(all.names = TRUE))
gc()
library("rms")
library("dplyr")
library("tidyr")
library("readr")  
library("plotly")
library("ggplot2")
path = "D:/GitHub/Paxlovid_MathematicalModel/data"
df_sim <- read.csv(paste(path, "Bernoulli_Full_Simulation.csv", sep = "/"))
df_sim_0 <- df_sim %>% filter(exec==1)
df_sim_0
names(df_sim_0)
str(df_sim_0)
df_sim_0$binary_patient_type <- as.integer(ifelse(df_sim_0$binary_patient_type=="H", 1,0))
referendum_vars <- c("age_group", "sex","comorbidity","binary_patient_type",  "count", "ckd",  "covid")
#Casos normales no CKD's includes 
df_sim2 <- df_sim_0[, referendum_vars]
df_sim2$countPriors <- df_sim2$count - df_sim2$ckd

head(df_sim2)
dim(df_sim2)
unique(df_sim2$age_group)

summaryze1 <-df_sim2%>% group_by(age_group, sex, comorbidity) %>% 
  dplyr::summarize( count_ = sum(countPriors), 
                    covid_ = sum(countPriors*covid),
                    non_covid_ = sum(countPriors)- sum(countPriors*covid),
                    hospitalary_ = sum(countPriors*binary_patient_type*covid),
                    ambulatory_  = sum(countPriors*covid) - sum(countPriors*binary_patient_type*covid),
                    prob_SH  = hospitalary_/count_,
                    prob_SA  = ambulatory_/count_,
                    prob_SR  = non_covid_/count_,
                    .groups = 'drop')
summaryze2 <- df_sim2[df_sim2$comorbidity==1, ]%>% group_by(age_group, sex, comorbidity) %>%
  dplyr::summarize( count_ = sum(ckd), 
                    covid_ = sum(ckd*covid),
                    non_covid_ = sum(ckd)- sum(ckd*covid),
                    hospitalary_ = sum(ckd*binary_patient_type*covid),
                    ambulatory_  = sum(ckd*covid) - sum(ckd*binary_patient_type*covid),
                    prob_SH = hospitalary_/count_,
                    prob_SA  = ambulatory_/count_,
                    prob_SR   = non_covid_/count_, 
                    .groups = 'drop')
summaryze1$ckd_group=0
summaryze2$ckd_group=1
useful_vars <- c("age_group", "sex","comorbidity", "ckd_group", "count_",
                 "prob_SA", "prob_SH", "prob_SR")
summaryze1_vis <- summaryze1[, useful_vars]
summaryze2_vis <- summaryze2[, useful_vars]
summaryzeALL <- rbind(summaryze1_vis, summaryze2_vis)
View(summaryze2)
#Epsilon decimal correction
summaryzeALL$prob_SR <- round(summaryzeALL$prob_SR,8)
summaryzeALL$prob_SA <- round(summaryzeALL$prob_SA,8)
summaryzeALL$prob_SH <- 1- (summaryzeALL$prob_SR+ summaryzeALL$prob_SA)

View(summaryzeALL)
summaryzeALL$prob_SH+summaryzeALL$prob_SA + summaryzeALL$prob_SR

write.csv(summaryzeALL, paste(path, "Final_PobSet.csv", sep = "/"), 
          row.names=FALSE)
View(summaryzeALL)


