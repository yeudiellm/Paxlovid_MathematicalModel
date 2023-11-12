rm(list = ls(all.names = TRUE))
gc()
library(diagram)
library(heemod)
library(ggplot2)
#STATES :  SAHIRM -Model cost-effectiveness
#(S) Suspicious, 
#(A) Ambulatory, 
#(H) Hospitalary, 
#(I) ICU, 
#(R) Recovery
#(D) Death
param <- define_parameters(
  #Initital probabilities 
  prob_A = 0.341342975, 
  prob_H = 0.002435418, 
  prob_R = 0.656221607, 
  #Out of model probabilities
  prob_AR = 0.1, 
  prob_HR = 0.1, 
  prob_IR = 0.1, 
  prob_AD = 0.1,  
  prob_HD = 0.1, 
  prob_ID = 0.1,  
  #Patience's state probability
  prob_AH_pxlvd=0.2, 
  prob_AH_rmsvr=0.2, 
  prob_HI_pxlvd=0.2, 
  prob_HI_rmsvr=0.2,
  
  #Model Costs 
  cost_S = 100,  
  cost_A_pxlvd=300, 
  cost_H_pxlvd=300, 
  cost_I_pxlvd=300, 
  cost_A_rmsvr=400, 
  cost_H_rmsvr=400,
  cost_I_rmsvr=400)

mat_pxlvd <- define_transition(
  state_names = c("S","A", "H", "I", "R", "D"),
  C   ,prob_A,prob_H       ,0.00         ,prob_R ,0.00,     #S
  0.00,C     ,prob_AH_pxlvd,0.00         ,prob_AR,prob_AD,  #A
  0.00,0.00  ,C            ,prob_HI_pxlvd,prob_HR,prob_HD,  #H   
  0.00,0.00  ,0.00         ,C            ,prob_IR,prob_AD,  #I 
  0.00,0.00  ,0.00         ,0.00         ,1.00   ,0.00   ,  #R 
  0.00,0.00  ,0.00         ,0.00         ,0.00   ,1.00    ) #D
mat_rmsvr <- define_transition(
  state_names = c("S","A", "H", "I", "R", "D"),
  C   ,prob_A,prob_H       ,0.00         ,prob_R ,0.00,     #S
  0.00,C     ,prob_AH_rmsvr,0.00         ,prob_AR,prob_AD,  #A
  0.00,0.00  ,C            ,prob_HI_rmsvr,prob_HR,prob_HD,  #H   
  0.00,0.00  ,0.00         ,C            ,prob_IR,prob_AD,  #I 
  0.00,0.00  ,0.00         ,0.00         ,1.00   ,0.00   ,  #R 
  0.00,0.00  ,0.00         ,0.00         ,0.00   ,1.00    ) #D

state_Suspicious <- define_state(
  cost_total = 0,
  utility =0)
state_Recovery   <- define_state(
  cost_total=0,
  utility=0)
state_Death      <- define_state(
  cost_total=0, 
  utility=0)
state_Ambulatory_pxlvd  <- define_state(
  cost_total= as.integer(ifelse(state_time==1,10000,  cost_A_pxlvd)), 
  utility     =0)
state_Hospitalary_pxlvd <- define_state(
  cost_total  = as.integer(ifelse(state_time==1, 6000, cost_H_pxlvd)), 
  utility=0)
state_ICU_pxlvd <- define_state( 
  cost_total  = as.integer(ifelse(state_time==1, 100000, cost_I_pxlvd)),
  utility=0)
state_Ambulatory_rmsvr<- define_state(
  cost_total= cost_A_rmsvr, 
  utility     =0)
state_Hospitalary_rmsvr <- define_state(
  cost_total  = cost_H_rmsvr, 
  utility=0)
state_ICU_rmsvr <- define_state( 
  cost_total  = cost_I_rmsvr, 
  utility=0)

strat_pxlvd <- define_strategy(
  transition = mat_pxlvd,
  S = state_Suspicious, 
  A = state_Ambulatory_pxlvd, 
  H = state_Hospitalary_pxlvd, 
  I = state_ICU_pxlvd,
  R = state_Recovery, 
  D = state_Death)
strat_rmsvr <- define_strategy(
  transition = mat_rmsvr,
  S = state_Suspicious, 
  A = state_Ambulatory_rmsvr, 
  H = state_Hospitalary_rmsvr, 
  I = state_ICU_rmsvr, 
  R = state_Recovery,
  D = state_Death)

res_mod <- run_model(
  pxlvd = strat_pxlvd,
  init  = c(500000, 0, 0, 0, 0, 0),
  parameters = param,
  cycles = 30,
  method = "end",
  cost = cost_total,
  effect = utility
)
res_mod
s <- summary(res_mod)
s


plot(res_mod, type = "counts", panel = "by_strategy") +
  xlab("Time") +
  theme_bw() 

plot(res_mod, type = "values", panel = "by_value",
     free_y = TRUE) +
  xlab("Time") +
  theme_bw() +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  )

summary(res_mod)
res_mod$eval_strategy_list$pxlvd$
  
  
print(res_mod$eval_strategy_list$pxlvd$counts, n=31)

data.frame(res_mod$eval_strategy_list$pxlvd$counts)



