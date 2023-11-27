rm(list = ls(all.names = TRUE))
gc()
library(diagram)
library(heemod)
library(ggplot2)
path = "D:/GitHub/Paxlovid_MathematicalModel/data"
general_params     <- read.csv(paste(path, "Gen_params.csv", sep="/"), 
                      header=FALSE, row.names=1)
general_params     <- as.data.frame(t(general_params))
markov_params      <- read.csv(paste(path, "Final_PobSet.csv", sep = "/"))
survival_nomed     <- read.csv(paste(path, "Prob_Nomed_30days.csv", sep="/"))
survival_paxlovid  <- read.csv(paste(path, "Prob_Paxlovid_30days.csv", sep="/"))
survival_redemsivir<- read.csv(paste(path, "Prob_Redemsivir_30days.csv", sep="/"))


survival_nomed$patient_type <- as.integer(
                            ifelse(survival_nomed$patient_type=="H", 1,0))
survival_paxlovid$patient_type <- as.integer(
                            ifelse(survival_paxlovid$patient_type=="H", 1,0))
survival_redemsivir$patient_type <- as.integer(
                          ifelse(survival_redemsivir$patient_type=="H", 1,0))

#View(survival_nomed)
#View(markov_params)
#Convertir survival prob días a vectores
survival_data_to_prob_death <- function(row, sdf, bin_patient_type){
  keys = paste('day_', rep(1:30), sep="")
  filter_surv =sdf %>% dplyr::filter(age_group==row$age_group, 
                                     sex==row$sex, 
                                     comorbidity==row$comorbidity,
                                     patient_type==bin_patient_type)
  filter_surv = filter_surv[,keys]
  death_prob  = 1-filter_surv
  return(as.vector(death_prob))
}
#Limite inferior de intervalores
interval_to_num <- function(x){
  lim_inf = strtoi(substring(x, 3,4))
  #lim_sup = strtoi(substring(z, 8,9))
  return(lim_inf)}
markov_params$age_inf = lapply(markov_params$age_group,interval_to_num)
#names(markov_params)
group_vars  <- c("age_group", "sex", "comorbidity", "ckd_group")
probs_vars  <- c("prob_SA","prob_SH","prob_SR","prob_AR","prob_HR","prob_IR",
                 "prob_ID","prob_AH_pxlvd","prob_AH_NO_pxlvd",
                 "prob_HI_rmsvr","prob_HI_NO_rmsvr")
costs_vars  <- c("cost_A_pxlvd_treatment","cost_A_NO_pxlvd_treatment",
                 "cost_A_healthcare","cost_A_healthcare_DayOne",
                 "cost_H_rmsvr_treatment","cost_H_NO_rmsvr_treatment",
                 "cost_H_healthcare","cost_H_healthcare_DayOne",
                 "cost_I_medicine_treatment",
                 "cost_I_healthcare","cost_I_healthcare_DayOne")

sum(is.na(markov_params))
#names(markov_params)
#head(markov_params)
names(general_params)
#GLOBAL VARIABLES 
vec_prob_AD <- rep(0.01, 30)  
vec_prob_HD <- rep(0.01, 30)
#Defining parameters
define_parameters_with_row <- function(row,general_params){
  params <- define_parameters(
    #Suspicios to Sickness state
    #initial_probs_sum = row$prob_SA+row$prob_SH+row$prob_SR,
    prob_SA =row$prob_SA,
    prob_SH =row$prob_SH,
    prob_SR =row$prob_SR,
    #Sickness state to Recovery
    prob_AR =general_params$prob_AR,
    prob_HR =general_params$prob_HR, 
    prob_IR =general_params$prob_IR, 
    #Sickness state to Death. 
    #vec_prob_AD = vec_prob_AD, 
    #vec_prob_HD = vec_prob_HD,
    prob_AD =vec_prob_AD[model_time], 
    prob_HD =vec_prob_HD[model_time], 
    prob_ID =general_params$prob_ID, 
    #Transition sickness 
    prob_AH_pxlvd    =general_params$prob_AH_pxlvd, 
    prob_AH_NO_pxlvd =general_params$prob_AH_NO_pxlvd, 
    prob_HI_rmsvr    =general_params$prob_HI_rmsvr, 
    prob_HI_NO_rmsvr =general_params$prob_HI_NO_rmsvr, 
    #Costs
    #Ambulatory
    cost_A_pxlvd_treatment       = general_params$cost_A_pxlvd_treatment, 
    cost_A_NO_pxlvd_treatment    = general_params$cost_A_NO_pxlvd_treatment, 
    cost_A_healthcare            = general_params$cost_A_healthcare, 
    cost_A_healthcare_DayOne     = general_params$cost_A_healthcare_DayOne, 
    #Hospitalary
    cost_H_rmsvr_treatment       = general_params$cost_H_rmsvr_treatment,
    cost_H_NO_rmsvr_treatment    = general_params$cost_H_NO_rmsvr_treatment, 
    cost_H_healthcare            = general_params$cost_H_healthcare, 
    cost_H_healthcare_DayOne     = general_params$cost_H_healthcare_DayOne, 
    #ICU
    cost_I_medicine_treatment    = general_params$cost_I_medicine_treatment, 
    #cost_I_NO_medicine_treatment = row$cost_I_NO_medicine_treatment, 
    cost_I_healthcare            = general_params$cost_I_healthcare, 
    cost_I_healthcare_DayOne     = general_params$cost_I_healthcare_DayOne)
  return(params)
}

mat_pxlvd <- define_transition(
  state_names = c("S","A", "H", "I", "R", "D"),
  C   ,prob_SA,prob_SH       ,0.00            ,prob_SR ,0.00,    #S
  0.00,C      ,prob_AH_pxlvd ,0.00            ,prob_AR,prob_AD,  #A
  0.00,0.00   ,C             ,prob_HI_NO_rmsvr,prob_HR,prob_HD,  #H   
  0.00,0.00   ,0.00          ,C               ,prob_IR,prob_AD,  #I 
  0.00,0.00   ,0.00          ,0.00            ,1.00   ,0.00   ,  #R 
  0.00,0.00   ,0.00          ,0.00            ,0.00   ,1.00    ) #D
mat_rmsvr <- define_transition(
  state_names = c("S","A", "H", "I", "R", "D"),
  C   ,prob_SA,prob_SH         ,0.00         ,prob_SR ,0.00,    #S
  0.00,C      ,prob_AH_NO_pxlvd,0.00         ,prob_AR,prob_AD,  #A
  0.00,0.00   ,C               ,prob_HI_rmsvr,prob_HR,prob_HD,  #H   
  0.00,0.00   ,0.00            ,C            ,prob_IR,prob_AD,  #I 
  0.00,0.00   ,0.00            ,0.00         ,1.00   ,0.00   ,  #R 
  0.00,0.00   ,0.00            ,0.00         ,0.00   ,1.00    ) #D
mat_nomed <- define_transition(
  state_names = c("S","A", "H", "I", "R", "D"),
  0.00,prob_SA,prob_SH         ,0.00            ,prob_SR ,0.00,    #S
  0.00,C      ,prob_AH_NO_pxlvd,0.00            ,prob_AR,prob_AD,  #A
  0.00,0.00   ,C               ,prob_NO_HI_rmsvr,prob_HR,prob_HD,  #H   
  0.00,0.00   ,0.00            ,C               ,prob_IR,prob_AD,  #I 
  0.00,0.00   ,0.00            ,0.00            ,1.00   ,0.00   ,  #R 
  0.00,0.00   ,0.00            ,0.00            ,0.00   ,1.00    ) #D
#Control States
state_Suspicious <- define_state(
  cost_drugs = 0, 
  cost_healthcare=0,
  cost_total = 0,
  count_drug_dosis_dia =0)
state_Recovery   <- define_state(
  cost_drugs =0, 
  cost_healthcare =0, 
  cost_total =0, 
  count_drug_dosis_dia =0)
state_Death      <- define_state(
  cost_drugs =0, 
  cost_healthcare=0, 
  cost_total =0, 
  count_drug_dosis_dia=0)
#Sickness states
state_ICU <- define_state(
  cost_drugs = as.numeric(ifelse(state_time<=5, cost_I_medicine_treatment, 0)),
  cost_healthcare = as.numeric(ifelse(state_time==1, cost_I_healthcare_DayOne, 
                                      cost_I_healthcare)), 
  cost_total = cost_drugs + cost_healthcare, 
  count_drug_dosis_dia=0)

state_Ambulatory_pxlvd  <- define_state(
  cost_drugs = as.numeric(ifelse(state_time<=5, cost_A_pxlvd_treatment,
                                 cost_A_NO_pxlvd_treatment)),
  cost_healthcare = as.numeric(ifelse(state_time==1, cost_A_healthcare_DayOne, 
                                      cost_A_healthcare)), 
  cost_total = cost_drugs + cost_healthcare, 
  count_drug_dosis_dia=1)
state_Ambulatory_NO_pxlvd  <- define_state(
  cost_drugs = cost_A_NO_pxlvd_treatment,
  cost_healthcare = as.numeric(ifelse(state_time==1, cost_A_healthcare_DayOne, 
                                      cost_A_healthcare)), 
  cost_total = cost_drugs + cost_healthcare,
  count_drug_dosis_dia=0)

state_Hospitalary_rmsvr <- define_state(
  cost_drugs = as.numeric(ifelse(state_time<=3, cost_H_rmsvr_treatment,
                                 cost_H_NO_rmsvr_treatment)),
  cost_healthcare = as.numeric(ifelse(state_time==1, cost_H_healthcare_DayOne, 
                                      cost_H_healthcare)), 
  cost_total = cost_drugs + cost_healthcare, 
  count_drug_dosis_dia=1)
state_Hospitalary_NO_rmsvr <- define_state(
  cost_drugs = cost_H_NO_rmsvr_treatment,
  cost_healthcare = as.numeric(ifelse(state_time==1, cost_H_healthcare_DayOne, 
                                      cost_H_healthcare)), 
  cost_total = cost_drugs + cost_healthcare, 
   
  count_drug_dosis_dia=0)

#Medical Strategies
strat_pxlvd <- define_strategy(
  transition = mat_pxlvd,
  S = state_Suspicious, 
  A = state_Ambulatory_pxlvd, 
  H = state_Hospitalary_NO_rmsvr, 
  I = state_ICU,
  R = state_Recovery, 
  D = state_Death)
strat_rmsvr <- define_strategy(
  transition = mat_rmsvr,
  S = state_Suspicious, 
  A = state_Ambulatory_NO_pxlvd, 
  H = state_Hospitalary_rmsvr, 
  I = state_ICU, 
  R = state_Recovery,
  D = state_Death)
strat_nomed <- define_strategy(
  transition = mat_rmsvr,
  S = state_Suspicious, 
  A = state_Ambulatory_NO_pxlvd, 
  H = state_Hospitalary_NO_rmsvr, 
  I = state_ICU, 
  R = state_Recovery,
  D = state_Death)

decision_strategy1 <- function(row){
  if(row$ckd_group==1){
    return("NOMED")}
  if(row$comorbidity==1 | row$age_inf>=50){
    return("PXLVD")
  }
  else{
    return("RMSVR")
  }
}
decision_strategy2 <-function(row){
  if(row$ckd_group==1){
    return("NOMED")
  }
  return("PXLVD")
}

run_model_2<- function(strat,param, count_pob){
    res_mod <- run_model(
      strategy = strat,
      init  = c(count_pob, 0, 0, 0, 0, 0),
      parameters = param,
      cycles = 28,
      method = "end",
      cost = cost_total,
      effect = count_drug_dosis_dia
    ) 
    return(res_mod)
}
#Final Params Sequences
opt_strategies <- c("PXLVD", "RMSVR", "NOMED")
opt_survivals  <- list(survival_paxlovid, survival_redemsivir, survival_nomed)
opt_counts     <- c(0, 0, 0 )
#Strategies
for ( irow in 1:nrow(markov_params)){
  print(markov_params[irow, group_vars])
  row = markov_params[irow, ]
  count_pob   = markov_params[irow, "count_"]
  medical_strategy = decision_strategy1(markov_params[irow,])
  for (i in 1:3){
      if(medical_strategy == opt_strategies[i]){
        vec_prob_AD <<- survival_data_to_prob_death(row, 
                                          as.data.frame(opt_survivals[i]), 0)
        vec_prob_HD <<- survival_data_to_prob_death(row,
                                          as.data.frame(opt_survivals[i]), 1)
        param <- define_parameters_with_row(markov_params[irow,],general_params)
        opt_counts[i] = opt_counts[i]+1
      }
  }
  if(medical_strategy=="PXLVD"){
    res_mod <- run_model_2(strat_pxlvd, param, count_pob)
  }
  if(medical_strategy=="RMSVR"){
    res_mod <- run_model_2(strat_rmsvr, param, count_pob)
  }
  if(medical_strategy=="NOMED"){
    res_mod <- run_model_2(strat_nomed, param, count_pob)
  }
  summary_count = res_mod$eval_strategy_list$strategy$counts
  summary_cost = res_mod$eval_strategy_list$strategy$values
  write.csv(summary_count,paste(path,"/strat1_counts_", irow, ".csv", sep=""),
            row.names=FALSE)
  write.csv(summary_cost,paste(path,"/strat1_cost_", irow, ".csv", sep=""),
            row.names=FALSE)
}

opt_counts

opt_counts     <- c(0, 0, 0 )

#Running strategy 2 
for ( irow in 1:nrow(markov_params)){
  print(markov_params[irow, group_vars])
  row = markov_params[irow, ]
  count_pob   = markov_params[irow, "count_"]
  medical_strategy = decision_strategy2(markov_params[irow,])
  for (i in 1:3){
    if(medical_strategy == opt_strategies[i]){
      vec_prob_AD <<- survival_data_to_prob_death(row, 
                                                  as.data.frame(opt_survivals[i]), 0)
      vec_prob_HD <<- survival_data_to_prob_death(row,
                                                  as.data.frame(opt_survivals[i]), 1)
      param <- define_parameters_with_row(markov_params[irow,],general_params)
      opt_counts[i] = opt_counts[i]+1
    }
  }
  if(medical_strategy=="PXLVD"){
    res_mod <- run_model_2(strat_pxlvd, param, count_pob)
  }
  if(medical_strategy=="NOMED"){
    res_mod <- run_model_2(strat_nomed, param, count_pob)
  }
  summary_count = res_mod$eval_strategy_list$strategy$counts
  summary_cost = res_mod$eval_strategy_list$strategy$values
  write.csv(summary_count,paste(path,"/strat2_counts_", irow, ".csv", sep=""),
            row.names=FALSE)
  write.csv(summary_cost,paste(path,"/strat2_cost_", irow, ".csv", sep=""),
            row.names=FALSE)
}

opt_counts
  

  
  