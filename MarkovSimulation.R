rm(list = ls(all.names = TRUE))
gc()
#STATES : Suspicious (S), Ambulatory (A), Hospitalary (H), ICU (I), Death or Health (D)
library(diagram)
library(heemod)
library(ggplot2)

mat_pxlvd <- define_transition(
  0.00 , 0.50 , 0.25 , 0.00 , 0.25, 
  0.00 , 0.80 , 0.10 , 0.05 , 0.05, 
  0.00 , 0.00 , 0.30 , 0.10 , 0.60, 
  0.00 , 0.00 , 0.00 , 0.15 , 0.85, 
  0.00 , 0.00 , 0.00 , 0.00 , 1.00, 
  state_names = c("Suspicious", "Ambulatory", "Hospitalary", "ICU",
                  "Cost0")
)

mat_nmsvr <- define_transition(
  0.00 , 0.50 , 0.00 , 0.00 , 0.50, 
  0.00 , 0.50 , 0.30 , 0.10 , 0.10, 
  0.00 , 0.00 , 0.50 , 0.25 , 0.25, 
  0.00 , 0.00 , 0.00 , 0.55 , 0.45, 
  0.00 , 0.00 , 0.00 , 0.00 , 1.00, 
  state_names = c("Suspicious", "Ambulatory", "Hospitalary", "ICU",
                  "Cost0")
)

plot(mat_pxlvd)
cost_pxlvd<- 500
cost_nmsvr<- 600

strat_pxlvd <- define_strategy(
  transition = mat_pxlvd,
  Suspicious = define_state(
    cost_health = 130,   #Prueba de Covid
    cost_drugs  = dispatch_strategy(
      pxlvd = cost_pxlvd, 
      nmsvr = cost_nmsvr
    ), 
    cost_total = cost_health + cost_drugs
  ), 
  Ambulatory = define_state(
    cost_health = 500,
    cost_drugs = dispatch_strategy(
      pxlvd = cost_pxlvd,
      nmsvr = cost_nmsvr,
    ),
    cost_total = cost_health + cost_drugs,
  ), 
  Hospitalary  = define_state(
    cost_health = 1500,
    cost_drugs = dispatch_strategy(
      pxlvd = cost_pxlvd,
      nmsvr = cost_nmsvr,
    ),
    cost_total = cost_health + cost_drugs,
  ), 
  ICU = define_state(
    cost_health = 4500,
    cost_drugs = dispatch_strategy(
      pxlvd = cost_pxlvd,
      nmsvr = cost_nmsvr,
    ),
    cost_total = cost_health + cost_drugs,
  ), 
  Cost0 = define_state(
    cost_health = 0,
    cost_drugs = dispatch_strategy(
      pxlvd = cost_pxlvd,
      nmsvr = cost_nmsvr,
    ),
    cost_total = cost_health + cost_drugs,
  )
)
strat_nmsvr <- define_strategy(
  transition = mat_nmsvr,
  Suspicious = define_state(
    cost_health = 130,   #Prueba de Covid
    cost_drugs  = dispatch_strategy(
      pxlvd = cost_pxlvd, 
      nmsvr = cost_nmsvr
    ), 
    cost_total = cost_health + cost_drugs
  ), 
  Ambulatory = define_state(
    cost_health = 500,
    cost_drugs = dispatch_strategy(
      pxlvd = cost_pxlvd,
      nmsvr = cost_nmsvr,
    ),
    cost_total = cost_health + cost_drugs,
  ), 
  Hospitalary  = define_state(
    cost_health = 1500,
    cost_drugs = dispatch_strategy(
      pxlvd = cost_pxlvd,
      nmsvr = cost_nmsvr,
    ),
    cost_total = cost_health + cost_drugs,
  ), 
  ICU = define_state(
    cost_health = 4500,
    cost_drugs = dispatch_strategy(
      pxlvd = cost_pxlvd,
      nmsvr = cost_nmsvr,
    ),
    cost_total = cost_health + cost_drugs,
  ), 
  Cost0 = define_state(
    cost_health = 0,
    cost_drugs = dispatch_strategy(
      pxlvd = cost_pxlvd,
      nmsvr = cost_nmsvr,
    ),
    cost_total = cost_health + cost_drugs,
  )
)

res_mod <- run_model(
  pxlvd = strat_pxlvd,
  nmsvr = strat_nmsvr,
  cycles = 30,
  cost = cost_total,
  effect = cost_drugs
)
summary(res_mod)
plot(res_mod, type = "counts", panel = "by_strategy") +
  xlab("Time") +
  theme_bw() +
  scale_color_brewer(
    name = "State",
    palette = "Set1"
  )
plot(res_mod, type = "counts", panel = "by_state", free_y=TRUE) +
  xlab("Time") +
  theme_bw() +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  )
plot(res_mod, type = "values", panel = "by_value",
     free_y = TRUE) +
  xlab("Time") +
  theme_bw() +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  )





