rm(list = ls(all.names = TRUE))
gc()
#Librerías 
library("rms")
library("dplyr")
library("tidyr")
library("readr")
library("tidyverse")
library("caret")
library("GGally")
#Set de datos 
pob_input <-read.csv("C:/Users/yeudi/Downloads/Pob_comorbidity/COVID_transform_2022_v8.csv")
unique(pob_input$age_group)
names(pob_input)
#Preprocesamiento 
#Obtener comorbilidad corregida
morbidities <- c("obesity", "diabetes", "hyperten","ckd",
                 "cardio","copd", "asma", "immuno", "hiv")
pob_input$comorbidity <- as.integer(apply(pob_input[,morbidities]==1, 1, any))
#Pacientes como variable binaria 
pob_input$binary_patient_type <- as.integer(ifelse(pob_input$patient_type=="H", 1,0))
#Filtrar por edad>=18 (grupo de estudio de interés)
expected_var         <-c("death")
predictors_vars      <- c("age_group", "sex", "covid", "binary_patient_type","comorbidity")
pob_input <- pob_input[pob_input$age>=18, c(expected_var, predictors_vars)]
names(pob_input)
unique(pob_input$age_group)
#Conversión de variables 
str(pob_input)
for (var in c(expected_var, predictors_vars)){
  pob_input[, var] <- as.factor(pob_input[,var])
}
str(pob_input)

#Balanceo de clases
prop.table(table(pob_input$death))
down_pob_input <- downSample(x = pob_input[, predictors_vars], 
                             y=as.factor(pob_input[,expected_var]), 
                             yname="death")
prop.table(table(down_pob_input$death))
#Descripción 
ggpairs(down_pob_input, aes(colour=death))
down_pob_input$age_group <- as.factor(down_pob_input$age_group)
down_pob_input$sex       <- as.factor(down_pob_input$sex)
str(down_pob_input)
help(factor)
logit_mod <- glm( death ~ age_group + sex + binary_patient_type + covid +
                          comorbidity,
                  data = down_pob_input, family=binomial(link="logit"))


summary(logit_mod)
anova(logit_mod, test = "Chisq")


#Estadísticos del resultado de la clasificación
pred<-predict(logit_mod,type="response")#Se realiza una predicción con el modelo que se evalue
print((max(pred)+min(pred))/(2)) #0.5004626
plot(pred)

ypred<- factor(ifelse(pred>((max(pred)+min(pred))/(2)),1,0), levels=c(0,1))#Se comparan los valores reales con las predicciones y se imprime la tabla de verdaderos y falsos
yreal <- down_pob_input$death
confusionMatrix(data=ypred, reference=yreal)
#Estadísticas cuando son covid-positivos
ypred_covid <- ypred[down_pob_input$covid==1]
yreal_covid <- yreal[down_pob_input$covid==1]
confusionMatrix(data=ypred_covid, reference = yreal_covid)

saveRDS(logit_mod, file="C:/Users/yeudi/Downloads/Pob_comorbidity/logit_death_model.rda")
logit_mod_2 =readRDS(file="C:/Users/yeudi/Downloads/Pob_comorbidity/logit_death_model.rda")

#Extremadamente determinado 
pob_counts <-read.csv("C:/Users/yeudi/Downloads/Pob_comorbidity/Bernoulli_Try0_Simulation.csv")

fake_data <- pob_counts[, predictors_vars]
fake_data$age_group <- as.factor(fake_data$age_group)
fake_data$sex       <- as.factor(fake_data$sex)
fake_data$binary_patient_type <- as.integer(ifelse(fake_data$binary_patient_type=="H", 1, 0))

pred_fake = predict(logit_mod, fake_data, type="response")
plot(pred_fake)
fake_data$death <- as.integer(ifelse(pred_fake>0.5004626, 1, 0))



