rm(list = ls(all.names = TRUE))
gc()
library("rms")
library("dplyr")
library("tidyr")
library("readr")  
library("plotly")
library("ggplot2")

df_sim <-read.csv("C:/Users/yeudi/Downloads/Pob_comorbidity/Bernoulli_Try0_Simulation.csv")
names(df_sim)
str(df_sim)
df_sim$binary_patient_type <- as.integer(ifelse(df_sim$binary_patient_type=="H", 1,0))
referendum_vars <- c("age_group", "sex","comorbidity","binary_patient_type",  "count", "ckd",  "covid")
#Casos normales no CKD's includes 
df_sim2 <- df_sim[, referendum_vars]
df_sim2$countPriors <- df_sim2$count - df_sim2$ckd

head(df_sim2)

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

#Epsilon decimal correction
summaryzeALL$prob_SR <- round(summaryzeALL$prob_SR,8)
summaryzeALL$prob_SA <- round(summaryzeALL$prob_SA,8)
summaryzeALL$prob_SH <- 1- (summaryzeALL$prob_SR+ summaryzeALL$prob_SA)

View(summaryzeALL)
summaryzeALL$prob_SH+summaryzeALL$prob_SA + summaryzeALL$prob_SR


summaryzeALL$prob_AR  <- NaN
summaryzeALL$prob_HR  <- NaN 
summaryzeALL$prob_IR  <- NaN
summaryzeALL$prob_AD  <- NaN
summaryzeALL$prob_HD  <- NaN 
summaryzeALL$prob_ID  <- NaN

summaryzeALL$prob_AH_pxlvd <- NaN 
summaryzeALL$prob_AH_NO_pxlvd <- NaN 
summaryzeALL$prob_HI_rmsvr <- NaN
summaryzeALL$prob_HI_NO_rmsvr <-NaN 

summaryzeALL$cost_A_pxlvd_treatment    <-NaN
summaryzeALL$cost_A_NO_pxlvd_treatment <-NaN 
summaryzeALL$cost_A_healthcare         <-NaN
summaryzeALL$cost_A_healthcare_DayOne  <-NaN

summaryzeALL$cost_H_rmsvr_treatment    <-NaN
summaryzeALL$cost_H_NO_rmsvr_treatment <-NaN 
summaryzeALL$cost_H_healthcare         <-NaN
summaryzeALL$cost_H_healthcare_DayOne  <-NaN

summaryzeALL$cost_I_medicine_treatment    <-NaN
summaryzeALL$cost_I_NO_medicine_treatment <-NaN 
summaryzeALL$cost_I_healthcare         <-NaN
summaryzeALL$cost_I_healthcare_DayOne  <-NaN

write.csv(summaryzeALL, "C:/Users/yeudi/Downloads/Pob_comorbidity/Final_PobSet.csv", 
          row.names=FALSE)
View(summaryzeALL)


