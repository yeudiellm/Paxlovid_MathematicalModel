rm(list = ls(all.names = TRUE))
gc()
library(diagram)
library(heemod)
library(ggplot2)
#Dudas 

#STATES :  SAHID-Model cost
#(S) Suspicious, 
#(A) Ambulatory, 
#(H) Hospitalary, 
#(I) ICU, 
#(R) Muerte
#(M) Recuperados

#(O) Out of cost
#(Muerte)
#(Recuperados)
#Ciclo for 
param <- define_parameters(
  cost_pxlvd = ifelse( model_time<=5, 500, 0),
  cost_rmsvr = ifelse( model_time<=3, 300, 0),
  #Proporciones iniciales 
  init_SA = 0.5, 
  init_SH = 0.25, 
  init_SD = 0.25,
  #Proporciones  recuperacion/muerte 
  out_AD_p =  0.2, #Como manejar los recuperados 
  out_HD_p =  0.2,
  out_ID_p =  0.2,
  out_AD_r =  0.1,  
  out_HD_r =  0.1, 
  out_ID_r =  0.1,
  #Proporciones de estado paciente 
  trans_AH_p = 0.01, #Revisar el paper de Ivana 
  trans_HI_p = 0.01,   #Esta falta 
  trans_AH_r = 0.01,   #Esta falta
  trans_HI_r = 0.01   #Esta falta 
)

mat_pxlvd <- define_transition(
  state_names = c("S","A", "H", "I", "D"),
  0.00, init_SA, init_SH , 0.00     , init_SD  , 
  0.00, C      , trans_AH_p, 0.00     , out_AD_p , 
  0.00, 0.00   , C       , trans_HI_p , out_HD_p , 
  0.00, 0.00   , 0.00    , C        , out_ID_p , 
  0.00, 0.00   , 0.00    , 0.00     , C      )
mat_rmsvr <- define_transition(
  state_names = c("S","A", "H", "I", "D"),
  0.00, init_SA, init_SH , 0.00     , init_SD  , 
  0.00, C      , trans_AH_r, 0.00     , out_AD_r , 
  0.00, 0.00   , c       , transHI_r  , out_HD_r ,  
  0.00, 0.00   , 0.00    , C        , out_ID_r , 
  0.00, 0.00   , 0.00    , 0.00     , C        )

state_Suspicious <- define_state(
    cost_health = 100, #Prueba de covid
    cost_drugs = 0 , 
    #cost_drugs  = dispatch_strategy(
    #              pxlvd = cost_pxlvd, 
    #              rmsvr = cost_rmsvr), 
    cost_total = cost_health + cost_drugs)
state_Ambulatory <- define_state(
    cost_health = 500, 
    cost_drugs  = dispatch_strategy(
                  pxlvd = cost_pxlvd, 
                  rmsvr = cost_rmsvr), 
    cost_total = cost_health + cost_drugs)
state_Hospitalary <- define_state(
    cost_health = 1500,
    cost_drugs  = dispatch_strategy(
                  pxlvd = 500,  #Redemsivir intravenoso
                  rmsvr = 500),  #Redemsivir intravenoso 
    cost_total = cost_health + cost_drugs)
state_ICU         <- define_state(
    cost_health = 4500,
    cost_drugs  = dispatch_strategy(
      pxlvd = 500,  #Redemsivir intravenoso
      rmsvr = 500),  #Redemsivir intravenoso 
    cost_total = cost_health + cost_drugs)
state_OutOfCost   <- define_state(
    cost_health = 0,
    cost_drugs  = 0, 
    cost_total = cost_health + cost_drugs)

strat_pxlvd <- define_strategy(
  transition = mat_pxlvd,
  S = state_Suspicious, 
  A = state_Ambulatory, 
  H = state_Hospitalary, 
  I = state_ICU,
  D = state_OutOfCost)
strat_rmsvr <- define_strategy(
  transition = mat_rmsvr,
  S = state_Suspicious, 
  A = state_Ambulatory, 
  H = state_Hospitalary, 
  I = state_ICU, 
  D = state_OutOfCost)

res_mod <- run_model(
  pxlvd = strat_pxlvd,
  rmsvr = strat_rmsvr,
  parameters = param,
  cycles = 30,
  cost = cost_total,
  effect = cost_drugs
)
summary(res_mod)

help("define_parameters")
 






