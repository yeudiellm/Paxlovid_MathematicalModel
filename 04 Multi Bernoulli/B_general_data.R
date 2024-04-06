rm(list = ls(all.names = TRUE))
gc()

library("dplyr")
library("tidyr")
library("readr")
path = "D:/GitHub/Paxlovid_MathematicalModel"
pob_input_0 <- read.csv(paste(path, "big_data/COVID_transform_2022_v9.csv", sep = "/"))
dim(pob_input_0) #2138724      24
pob_input <- read.csv(paste(path, "big_data/COVID_transform_2022_v9_p2.csv", sep = "/")) 
dim(pob_input)
sum(pob_input[,'covid']==1) #799511
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
count_confirmados_sinave_2022 = 799511  #799511
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

#pob_input_count<- pob_input_count[order(pob_input_count$sospechosos_nacional_2022),]
View(pob_input_count)
dim(pob_input_count)
path = "D:/GitHub/Paxlovid_MathematicalModel"
pob_input_0 <- read.csv(paste(path, "big_data/COVID_transform_2022_v9.csv", sep = "/"))

write.csv(pob_input_count, paste(path,"data/SUMMARY_SINAVE_2022.csv", sep="/"),
          row.names=FALSE)

head(pob_input_count)
dim(pob_input_count)

