rm(list = ls(all.names = TRUE))
gc()

library("dplyr")
library("tidyr")
library("readr")

pob_input <-read.csv("C:/Users/yeudi/Downloads/Pob_comorbidity/COVID_transform_2022_v8.csv")
unique(pob_input$age_group)

#sum(pob_input$age>80)/nrow(pob_input)
str(pob_input)
unique(pob_input$diagnostic_result)
#hist(pob_input$age)
pob_input_count <- pob_input %>% group_by(age_group,sex)%>% 
                   dplyr::summarize(sospechosos_sinave_2022=n(),
                             confirmados_sinave_2022=sum(covid),
                             avg_age = mean(age),.groups = 'drop')

unique(pob_input_count$age_group)
length(unique(pob_input_count$age_group))
nrow(pob_input) #Casos sospechosos sinave
nrow(pob_input[pob_input$covid==1,]) #Casos confirmados sinave
head(pob_input_count) 

count_sospechosos_sinave_2022 =  2138724 #2138583
count_confirmados_sinave_2022 = 799511  #799419
count_sospechosos_nacional_2022 = 6498639 #5753435 Simulacion
count_confirmados_teo_nacional_2022 = 3219032

pob_input_count$sospechosos_nacional_2022 = count_sospechosos_nacional_2022*
                                            pob_input_count$sospechosos_sinave_2022/
                                            count_sospechosos_sinave_2022
pob_input_count$confirmados_teo_nacional_2022 = count_confirmados_teo_nacional_2022*
                                                pob_input_count$confirmados_sinave_2022/
                                                count_confirmados_sinave_2022
pob_input_count$sospechosos_nacional_2022 = as.integer(round(pob_input_count$
                                                         sospechosos_nacional_2022, 0))
pob_input_count$confirmados_teo_nacional_2022 = as.integer(round(pob_input_count$
                                                             confirmados_teo_nacional_2022,0))
#Quitar menores de edad
pob_input_count = pob_input_count[pob_input_count$avg_age>=18,]
head(pob_input_count)

pob_input_count<- pob_input_count[order(pob_input_count$sospechosos_nacional_2022),]
head(pob_input_count)
dim(pob_input_count)

write.csv(pob_input_count, "C:/Users/yeudi/Downloads/Pob_comorbidity/SUMMARY_SINAVE_2022.csv",
          row.names=FALSE)

head(pob_input_count)
dim(pob_input_count)

