rm(list = ls(all.names = TRUE))
gc()
library(diagram)
library(heemod)
library(ggplot2)

markov_params <-read.csv("C:/Users/yeudi/Downloads/Pob_comorbidity/Final_PobSet.csv")
interval_to_num <- function(x){
  lim_inf = strtoi(substring(x, 3,4))
  #lim_sup = strtoi(substring(z, 8,9))
  return(lim_inf)}
markov_params$age_inf = lapply(markov_params$age_group,interval_to_num)
group_vars  <- c("age_group", "sex", "comorbidity", "ckd_group")
probs_vars  <- c("prob_SA","prob_SH","prob_SR","prob_AR","prob_HR","prob_IR",
                 "prob_AD","prob_HD","prob_ID","prob_AH_pxlvd","prob_AH_NO_pxlvd",
                 "prob_HI_rmsvr","prob_HI_NO_rmsvr")
costs_vars  <- c("cost_A_pxlvd_treatment","cost_A_NO_pxlvd_treatment",
                 "cost_A_healthcare","cost_A_healthcare_DayOne",
                 "cost_H_rmsvr_treatment","cost_H_NO_rmsvr_treatment",
                 "cost_H_healthcare","cost_H_healthcare_DayOne",
                 "cost_I_medicine_treatment","cost_I_NO_medicine_treatment",
                 "cost_I_healthcare","cost_I_healthcare_DayOne")
markov_params[, probs_vars][is.na(markov_params[,probs_vars])] <- 0.1
markov_params[, costs_vars][is.na(markov_params[,costs_vars])] <- 100.0
sum(is.na(markov_params))

names(markov_params)
head(markov_params)

define_parameters_with_row <- function(row){
  params <- define_parameters(
    #Suspicios to Sickness state
    #initial_probs_sum = row$prob_SA+row$prob_SH+row$prob_SR,
    prob_SA =row$prob_SA,
    prob_SH =row$prob_SH,
    prob_SR =row$prob_SR,
    #Sickness state to Recovery
    prob_AR =row$prob_AR,
    prob_HR =row$prob_HR, 
    prob_IR =row$prob_IR, 
    #Sickness state to Death
    prob_AD =row$prob_AD, 
    prob_HD =row$prob_HD, 
    prob_ID =row$prob_ID, 
    #Transition sickness 
    prob_AH_pxlvd    =row$prob_AH_pxlvd, 
    prob_AH_NO_pxlvd =row$prob_AH_NO_pxlvd, 
    prob_HI_rmsvr    =row$prob_HI_rmsvr, 
    prob_HI_NO_rmsvr =row$prob_HI_NO_rmsvr, 
    #Costs
    #Ambulatory
    cost_A_pxlvd_treatment       = row$cost_A_pxlvd_treatment, 
    cost_A_NO_pxlvd_treatment    = row$cost_A_NO_pxlvd_treatment, 
    cost_A_healthcare            = row$cost_A_healthcare, 
    cost_A_healthcare_DayOne     = row$cost_A_healthcare_DayOne, 
    #Hospitalary
    cost_H_rmsvr_treatment       = row$cost_H_rmsvr_treatment,
    cost_H_NO_rmsvr_treatment    = row$cost_H_NO_rmsvr_treatment, 
    cost_H_healthcare            = row$cost_H_healthcare, 
    cost_H_healthcare_DayOne     = row$cost_H_healthcare_DayOne, 
    #ICU
    cost_I_medicine_treatment    = row$cost_I_medicine_treatment, 
    #cost_I_NO_medicine_treatment = row$cost_I_NO_medicine_treatment, 
    cost_I_healthcare            = row$cost_I_healthcare, 
    cost_I_healthcare_DayOne     = row$cost_I_healthcare_DayOne)
  return(params)
}
#3 estados posibles 
#Los primeros 3 días de modelo se da redemsivir. 
#Los siguientes k días se deja de dar redemsivir. 
#Los siguientes 3+k días ... sigo sin saber
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
  cost_drugs = cost_I_medicine_treatment,
  cost_healthcare = as.numeric(ifelse(state_time==1, cost_I_healthcare_DayOne, 
                                      cost_I_healthcare)), 
  cost_total = cost_drugs + cost_healthcare, 
  count_drug_dosis_dia=0)

state_Ambulatory_pxlvd  <- define_state(
  cost_drugs = as.numeric(ifelse(model_time<=5, cost_A_pxlvd_treatment,
                                 cost_A_pxlvd_NO_treatment)),
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
  cost_drugs = as.numeric(ifelse(model_time<=3, cost_H_rmsvr_treatment,
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



#Running strategy 1 
for ( irow in 1:nrow(markov_params)){
  print(markov_params[irow, group_vars])
  row = markov_params[irow, ]
  count_pxlvd =0
  count_rmsvr =0
  count_nomed =0
  count_pob   = markov_params[irow, "count_"]
  param <- define_parameters_with_row(markov_params[irow,])
  medical_strategy = decision_strategy1(markov_params[irow,])
  if (medical_strategy == "PXLVD"){
    count_pxlvd =count_pxlvd+1 
    res_mod <- run_model(
      pxlvd = strat_pxlvd,
      init  = c(100, 0, 0, 0, 0, 0),
      parameters = param,
      cycles = 30,
      method = "end",
      cost = cost_total,
      effect = count_drug_dosis_dia
    )
    print(summary(res_mod))
  }
  if (medical_strategy  == "RMSVR"){
    count_rmsvr =count_rmsvr+1
    res_mod <- run_model(
      pxlvd = strat_rmsvr,
      init  = c(100, 0, 0, 0, 0, 0),
      parameters = param,
      cycles = 30,
      method = "end",
      cost = cost_total,
      effect = count_drug_dosis_dia
    )
    print(summary(res_mod))
  }
  if (medical_strategy == "NOMED"){
    count_nomed =count_nomed+1
    res_mod <- run_model(
      pxlvd = strat_nomed,
      init  = c(100, 0, 0, 0, 0, 0),
      parameters = param,
      cycles = 30,
      method = "end",
      cost = cost_total,
      effect = count_drug_dosis_dia
    )
    print(summary(res_mod))
  }
}
print(res_mod$eval_strategy_list$pxlvd$counts, n=31)
unique(markov_params$age_group)
count_pxlvd
count_rmsvr
count_nomed
#Running strategy 2 
for ( irow in 1:nrow(markov_params)){
  print(markov_params[irow, group_vars])
  row = markov_params[irow, ]
  count_pxlvd =0
  count_rmsvr =0
  count_nomed =0
  count_pob   = markov_params[irow, "count_"]
  param <- define_parameters_with_row(markov_params[irow,])
  medical_strategy = decision_strategy1(markov_params[irow,])
  if (medical_strategy == "PXLVD"){
    count_pxlvd =count_pxlvd+1 
    res_mod <- run_model(
      pxlvd = strat_pxlvd,
      init  = c(count_pob, 0, 0, 0, 0, 0),
      parameters = param,
      cycles = 30,
      method = "end",
      cost = cost_total,
      effect = count_drug_dosis_dia
    )
  }
  if (medical_strategy == "NOMED"){
    count_nomed =count_nomed+1
    res_mod <- run_model(
      pxlvd = strat_nomed,
      init  = c(count_pob, 0, 0, 0, 0, 0),
      parameters = param,
      cycles = 30,
      method = "end",
      cost = cost_total,
      effect = count_drug_dosis_dia
    )
  }
}

  

  
  