rm(list = ls(all.names = TRUE))
gc()
library("rms")
library("dplyr")
library("tidyr")
library("readr")
library("coxed")

avg_cases <- function(xs){
  # remuestra <- data[i]; mean(remuestra)
  mean(xs)
}

bootstrap <- function(xn, fun_, B){
  n <- length(xn)
  boot_sample <- rep(NA, B)
  for (t in 1:B){
    xn_sample <- sample(xn, size=n, replace=TRUE)
    boot_sample[t] <- fun_(xn_sample)
  }
  return(boot_sample)
}

#MORBIDITIES CONFIDENCE INTERVALS 
pob_input <-read.csv("C:/Users/yeudi/Downloads/Pob_comorbidity/COVID_transform_2022_v8.csv")
str(pob_input)
pob_input$binary_patient_type <- as.integer(ifelse(pob_input$patient_type=="H", 1,0)) 
pob_input$sex <- as.integer(ifelse(pob_input$sex=="M", 1, 0))

covars      <- c("binary_patient_type", "covid", "death", "sex")
morbidities <- c("obesity", "diabetes", "hyperten","ckd",
                 "cardio","copd", "asma", "immuno", "hiv")

rows_summary = c(covars, morbidities)
cols_summary = c("low", "est", "upp")
df_summary = data.frame(matrix(nrow = 0, ncol = length(cols_summary)))

B=1000
for(morb in rows_summary){
  vec_morb <- pob_input[, morb]
  boot_sim <- bootstrap(vec_morb, avg_cases, B)
  bca_ci   <- bca(boot_sim)
  result   <- c(bca_ci[1], avg_cases(vec_morb), bca_ci[2])
  print(result)
  df_summary <- rbind(df_summary, result)
  }

colnames(df_summary) <- cols_summary
rownames(df_summary) <- rows_summary

View(df_summary)

write.csv(df_summary, "C:/Users/yeudi/Downloads/Pob_comorbidity/Bootstrap_0.csv",
          row.names=TRUE)

df_summary*1000




