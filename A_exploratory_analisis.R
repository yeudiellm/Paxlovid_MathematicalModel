rm(list = ls(all.names = TRUE))
gc()
library("rms")
library("dplyr")
library("tidyr")
library("readr")  
library("plotly")
library("ggplot2")
path = "D:/GitHub/Paxlovid_MathematicalModel"
pob_input <- read.csv(paste(path, "big_data/COVID_transform_2022_v9.csv", sep = "/"))
pob_input$symptoms_date <-as.Date(pob_input$symptoms_date)
pob_input$admission_date<-as.Date(pob_input$admission_date)
pob_input$death_date    <-as.Date(pob_input$death_date)
pob_input$antiviral_date<-as.Date(pob_input$antiviral_date)

str(pob_input)
#GENERAL DATA 
pob_covid <- pob_input[pob_input$covid==1, ]
nrow(pob_covid)
prop.table(table(pob_input$covid))
#DISTRIBUTION AGE 
hist(pob_covid$age)
hist(pob_input$age)
summary(pob_input$age)
summary(pob_covid$age)
quantile(pob_covid$age, c(0.10, 0.90))
#DISTRIBUTIOON SEX 
prop.table(table(pob_input$sex))
prop.table(table(pob_covid$sex))
#SEPAS DE COVID 
round(100*prop.table(table(pob_covid$diagnostic_result)),2)

morbidities <- c("obesity", "diabetes", "hyperten","ckd",
                 "cardio","copd", "asthma", "immuno", "hiv")

100*sort(colMeans(pob_covid[,morbidities]))
100*sort(colMeans(pob_input[,morbidities]))

#Admission date ???
summary(as.numeric(pob_input$admission_date-pob_input$symptoms_date))
hist(as.numeric(pob_input$admission_date-pob_input$symptoms_date))
#Outliers 
pob_input[as.numeric(pob_input$admission_date-pob_input$symptoms_date)>300,]
pob_input[pob_input$antiviral_date>=as.Date("2023-01-01"), ]

#DISTRIBUTION PATIENT TYPE 
prop.table(table(pob_input$patient_type))
prop.table(table(pob_covid$patient_type))

#DEATH RATE 
100*prop.table(table(pob_covid$death))
summary(pob_covid$time[pob_covid$time!=999])
hist(pob_covid$time[pob_covid$time!=999])

death_per_month =round(100*prop.table(table(format(pob_covid$death_date,'%m'))),2)
cumsum(death_per_month)


