rm(list = ls(all.names = TRUE))
gc()
library(diagram)
library(heemod)
library(ggplot2)
path = "D:/GitHub/Paxlovid_MathematicalModel/05 Markov Model/data"
general_params<- read.csv(paste(path, "Gen_params.csv", sep="/"), 
                      header=FALSE, row.names=1)

general_params<- as.data.frame(t(general_params))
markov_params <- read.csv(paste(path, "Final_PobSet.csv", sep = "/"))
survival_syntc<- read.csv(paste(path, "COX_SurvProb_30days.csv", sep="/"))
survival_syntc$patient_type <- as.integer(
                            ifelse(survival_syntc$patient_type=="H", 1,0))
#Convertir survival prob días a vectores
survival_to_death <- function(row, sdf, bin_patient_type){
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

#GLOBAL VARIABLES 
vec_prob_AD <- rep(0.01, 30)  #Change according to AgeGroup
vec_prob_HD <- rep(0.01, 30)  #Change according to AgeGroup

#Defining parameters
define_parameters_with_row <- function(row,general_params){
  params <- define_parameters(
    #reduction rates
    rrr_rmsvr = general_params$RRR_redemsivir,  #si va a ejecutar
    rrr_pxlvd = general_params$RRR_paxlovid,    #si va a ejecutar
    #Suspicios to Sickness state
    #initial_probs_sum = row$prob_SA+row$prob_SH+row$prob_SR,
    prob_SA =row$prob_SA,
    prob_SH =row$prob_SH,
    prob_SR =row$prob_SR,
    #Sickness state to Death. 
    prob_AD =as.numeric(vec_prob_AD[model_time]), 
    prob_HD =as.numeric(vec_prob_HD[model_time]), 
    prob_ID =general_params$prob_ID,
    #Sickness state to Death 
    prob_AD_pxlvd = max(prob_AD - prob_AD*rrr_pxlvd, 0.00),
    prob_HD_pxlvd = max(prob_HD - prob_HD*rrr_pxlvd, 0.00),
    prob_ID_pxlvd = max(prob_ID - prob_ID*rrr_pxlvd, 0.00),
    prob_AD_rmsvr = max(prob_AD - prob_AD*rrr_rmsvr, 0.00),
    prob_HD_rmsvr = max(prob_HD - prob_HD*rrr_rmsvr, 0.00),
    prob_ID_rmsvr = max(prob_ID - prob_ID*rrr_rmsvr, 0.00),
    #Transition sickness 
    prob_AH_pxlvd =general_params$prob_AH_pxlvd, 
    prob_AH_syntc =general_params$prob_AH_syntc, 
    prob_HI_rmsvr =general_params$prob_HI_rmsvr, 
    prob_HI_syntc =general_params$prob_HI_syntc, 
    #Sickness state to Recovery
    prob_AR =general_params$prob_AR,
    prob_HR =general_params$prob_HR, 
    prob_IR =general_params$prob_IR,
    #Costs
    #Ambulatory
    cost_A_pxlvd_drugs    = general_params$cost_A_pxlvd_drugs, #Va a iterar 
    cost_A_pxlvd_DO_items = general_params$cost_A_pxlvd_DO_items,
    cost_A_pxlvd_items    = general_params$cost_A_pxlvd_items,
    cost_A_syntc_drugs    = general_params$cost_A_syntc_drugs, 
    cost_A_syntc_DO_items = general_params$cost_A_syntc_DO_items, 
    cost_A_syntc_items    = general_params$cost_A_syntc_items,
    #Hospitalary
    cost_H_rmsvr_drugs    = general_params$cost_H_rmsvr_drugs, 
    cost_H_rmsvr_DO_items = general_params$cost_H_rmsvr_DO_items,
    cost_H_rmsvr_items    = general_params$cost_H_rmsvr_items,
    cost_H_syntc_drugs    = general_params$cost_H_syntc_drugs, 
    cost_H_syntc_DO_items = general_params$cost_H_syntc_DO_items, 
    cost_H_syntc_items    = general_params$cost_H_syntc_items,
    #ICU
    cost_I_syntc_DO_drugs = general_params$cost_I_syntc_DO_drugs, 
    cost_I_syntc_drugs    = general_params$cost_I_syntc_drugs, 
    cost_I_syntc_DO_items = general_params$cost_I_syntc_DO_items, 
    cost_I_syntc_items    = general_params$cost_I_syntc_items, 
    )
  return(params)
}

# Define DSA
se_DSA <- define_dsa(
  rrr_pxlvd, 0.7213, 0.9556,
  rrr_rmsvr, 0.4141, 0.9688,
  cost_A_pxlvd_drugs, 51.6, 262.6,
  prob_AH_pxlvd, 0.75*general_params$prob_AH_pxlvd, 
                 1.25*general_params$prob_AH_pxlvd, 
  prob_AH_syntc, 0.75*general_params$prob_AH_syntc,
                 1.25*general_params$prob_AH_syntc)

# Define PSA
se_PSA <- define_psa(
  rrr_pxlvd ~ lognormal(mean=general_params$RRR_paxlovid,
                        sdlog=0.07175619284),
  rrr_rmsvr ~ lognormal(mean=general_params$RRR_redemsivir,
                        sdlog=0.2168241586),
  cost_A_pxlvd_drugs~ gamma(mean=general_params$cost_A_pxlvd_drugs, 
                            sd=sqrt(general_params$cost_A_pxlvd_drugs)),
  prob_AH_pxlvd ~ binomial(prob=general_params$prob_AH_pxlvd, 
                           size=1000),
  prob_AH_syntc ~ binomial(prob=general_params$prob_AH_syntc,
                           size=1000)
  )

# Matrix
mat_pxlvd <- define_transition(
  state_names = c("S","A", "H", "I", "R", "D"),
  C,prob_SA,prob_SH,0.00,prob_SR,0.00,   #S
  0.00,C,prob_AH_pxlvd,0.00,min(prob_AR,1-prob_AH_pxlvd-prob_AD_pxlvd),prob_AD_pxlvd, #A
  0.00,0.00,C,prob_HI_syntc,min(prob_HR,1-prob_HI_syntc-prob_HD_pxlvd),prob_HD_pxlvd,#H   
  0.00,0.00,0.00,C,prob_IR,prob_ID_pxlvd,#I 
  0.00,0.00,0.00,0.00,1.00,0.00,         #R 
  0.00,0.00,0.00,0.00,0.00,1.00)         #D

mat_rmsvr <- define_transition(
  state_names = c("S","A", "H", "I", "R", "D"),
  C,prob_SA,prob_SH,0.00,prob_SR,0.00,    #S
  0.00,C,prob_AH_syntc,0.00,min(prob_AR,1-prob_AH_syntc-prob_AD_rmsvr),prob_AD_rmsvr, #A
  0.00,0.00,C,prob_HI_rmsvr,min(prob_HR,1-prob_HI_rmsvr-prob_HD_rmsvr),prob_HD_rmsvr, #H   
  0.00,0.00,0.00,C,prob_IR,prob_ID_rmsvr, #I 
  0.00,0.00,0.00,0.00,1.00,0.00,          #R 
  0.00,0.00,0.00,0.00,0.00,1.00)          #D

mat_syntc <- define_transition(
  state_names = c("S","A", "H", "I", "R", "D"),
  0.00,prob_SA,prob_SH,0.00,prob_SR,0.00, #S
  0.00,C,prob_AH_syntc,0.00,min(prob_AR, 1-prob_AH_syntc-prob_AD),prob_AD,            #A
  0.00,0.00,C,prob_HI_syntc,min(prob_HR, 1-prob_HI_syntc-prob_HD),prob_HD,            #H   
  0.00,0.00,0.00,C,prob_IR,prob_ID,       #I 
  0.00,0.00,0.00,0.00,1.00,0.00,          #R 
  0.00,0.00,0.00,0.00,0.00,1.00)          #D


#Control States
state_Suspicious <- define_state(
  cost_drugs = 0, 
  cost_items = 0,
  cost_total = 0,
  count_deaths= 0)
state_Recovery   <- define_state(
  cost_drugs = 0, 
  cost_items = 0, 
  cost_total = 0, 
  count_deaths= 0)
state_Death      <- define_state(
  cost_drugs = 0, 
  cost_items = 0, 
  cost_total = 0, 
  count_deaths= as.numeric(ifelse(model_time==28, 1, 0)))
#Sickness states
state_ICU        <- define_state(
  cost_drugs = as.numeric(ifelse(state_time<=1, cost_I_syntc_DO_drugs,
                                                cost_I_syntc_drugs)),
  cost_items = as.numeric(ifelse(state_time<=1, cost_I_syntc_DO_items, 
                                                cost_I_syntc_items)), 
  cost_total = cost_drugs + cost_items, 
  count_deaths= 0)

state_A_pxlvd  <- define_state(
  cost_drugs = as.numeric(ifelse(state_time<=5, cost_A_pxlvd_drugs,
                                                cost_A_syntc_drugs)),
  cost_items = as.numeric(ifelse(state_time<=1, cost_A_pxlvd_DO_items, 
                                                cost_A_pxlvd_items)), 
  cost_total = cost_drugs + cost_items, 
  count_deaths= 0)

state_A_syntc  <- define_state(
  cost_drugs = cost_A_syntc_drugs,
  cost_items = as.numeric(ifelse(state_time<=1, cost_A_syntc_DO_items, 
                                                cost_A_syntc_items)), 
  cost_total = cost_drugs + cost_items,
  count_deaths= 0)

state_H_rmsvr <- define_state(
  cost_drugs = as.numeric(ifelse(state_time<=3, cost_H_rmsvr_drugs,
                                 cost_H_syntc_drugs)),
  cost_items = as.numeric(ifelse(state_time<=1, cost_H_rmsvr_DO_items, 
                                 cost_H_rmsvr_items)), 
  cost_total = cost_drugs + cost_items, 
  count_deaths= 0)

state_H_syntc <- define_state(
  cost_drugs = cost_H_syntc_drugs,
  cost_items = as.numeric(ifelse(state_time<=1, cost_H_syntc_DO_items, 
                                 cost_H_syntc_items)), 
  cost_total = cost_drugs + cost_items, 
  count_deaths=0)

#Medical Strategies
strat_pxlvd <- define_strategy(
  transition = mat_pxlvd,
  S = state_Suspicious, 
  A = state_A_pxlvd, 
  H = state_H_syntc, 
  I = state_ICU,
  R = state_Recovery, 
  D = state_Death)
strat_rmsvr <- define_strategy(
  transition = mat_rmsvr,
  S = state_Suspicious, 
  A = state_A_syntc, 
  H = state_H_rmsvr, 
  I = state_ICU, 
  R = state_Recovery,
  D = state_Death)
strat_syntc <- define_strategy(
  transition = mat_syntc,
  S = state_Suspicious, 
  A = state_A_syntc, 
  H = state_H_syntc, 
  I = state_ICU, 
  R = state_Recovery,
  D = state_Death)

decision_strategy1 <- function(row){
  if(row$ckd_group==1){
    return("SYNTC")}
  if(row$comorbidity==1 | row$age_inf>=50){
    return("PXLVD")
  }
  else{
    return("RMSVR")
  }
}
decision_strategy2 <-function(row){
  if(row$ckd_group==1){
    return("SYNTC")
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
      effect = count_deaths
    ) 
    return(res_mod)
}

################################################################################
######################### RUNNING MODELS #######################################
#Strategies
for ( irow in 1:nrow(markov_params)){
  print(markov_params[irow, group_vars])
  row = markov_params[irow, ]
  count_pob   = as.integer(markov_params[irow, "count_"])
  medical_strategy = decision_strategy1(markov_params[irow,])
  print(medical_strategy)
  vec_prob_AD <<- survival_to_death(row, survival_syntc, 0)
  vec_prob_HD <<- survival_to_death(row, survival_syntc, 1)
  param <- define_parameters_with_row(markov_params[irow,],general_params)
  if(medical_strategy=="PXLVD"){
    res_mod <- run_model_2(strat_pxlvd, param, count_pob)
  }
  if(medical_strategy=="RMSVR"){
    res_mod <- run_model_2(strat_rmsvr, param, count_pob)
  }
  if(medical_strategy=="SYNTC"){
    res_mod <- run_model_2(strat_syntc, param, count_pob)
  }
  print('run dsa')
  res_dsa <- run_dsa(model = res_mod,dsa = se_DSA)
  print('run psa')
  res_psa <- run_psa(model = res_mod,psa = se_PSA,N = 100)
  summary_count = res_mod$eval_strategy_list$strategy$counts
  summary_cost  = res_mod$eval_strategy_list$strategy$values
  summary_dsa  = res_dsa$dsa
  summary_psa   = res_psa$psa
  write.csv(summary_count,paste(path,"/strat1/count/", irow, ".csv", sep=""),
            row.names=FALSE)
  write.csv(summary_cost,paste(path, "/strat1/cost/", irow, ".csv", sep=""),
            row.names=FALSE)
  write.csv(summary_dsa,paste(path, "/strat1/dsa/", irow, ".csv", sep=""),
            row.names=FALSE)
  write.csv(summary_psa,paste(path, "/strat1/psa/", irow, ".csv", sep=""),
            row.names=FALSE)
}

#Running strategy 2 
for ( irow in 1:nrow(markov_params)){
  print(markov_params[irow, group_vars])
  row = markov_params[irow, ]
  count_pob   = as.integer(markov_params[irow, "count_"])
  medical_strategy = decision_strategy2(markov_params[irow,])
  print(medical_strategy)
  vec_prob_AD <<- survival_to_death(row, survival_syntc, 0)
  vec_prob_HD <<- survival_to_death(row, survival_syntc, 1)
  param <- define_parameters_with_row(markov_params[irow,],general_params)
  if(medical_strategy=="PXLVD"){
    res_mod <- run_model_2(strat_pxlvd, param, count_pob)
  }
  if(medical_strategy=="SYNTC"){
    res_mod <- run_model_2(strat_syntc, param, count_pob)
  }
  print('run dsa')
  res_dsa <- run_dsa(model = res_mod,dsa = se_DSA)
  print('run psa')
  res_psa <- run_psa(model = res_mod,psa = se_PSA,N = 100)
  summary_count = res_mod$eval_strategy_list$strategy$counts
  summary_cost  = res_mod$eval_strategy_list$strategy$values
  summary_dsa  = res_dsa$dsa
  summary_psa   = res_psa$psa
  write.csv(summary_count,paste(path,"/strat2/count/", irow, ".csv", sep=""),
            row.names=FALSE)
  write.csv(summary_cost,paste(path, "/strat2/cost/", irow, ".csv", sep=""),
            row.names=FALSE)
  write.csv(summary_dsa,paste(path, "/strat2/dsa/", irow, ".csv", sep=""),
            row.names=FALSE)
  write.csv(summary_psa,paste(path, "/strat2/psa/", irow, ".csv", sep=""),
            row.names=FALSE)
}


  
  