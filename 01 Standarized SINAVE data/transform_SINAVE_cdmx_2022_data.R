# Libraries
library("dplyr")
library("tidyr")
library("dplyr")

# ----------------------------------------------- FIRST PART -----------------------------------------------
# SINAVE COVID full dataset
base <-read.csv("/Users/ivanas.o/Downloads/Paxlovid/covid19_sisver_cdmx_completa.csv")

# Change dates format
base$fecha_inicio_sintomas <- substr(base$fecha_inicio_sintomas, 1, 10)
base$fecha_ingreso <- substr(base$fecha_ingreso, 1, 10)
base$fecha_defuncion <- substr(base$fecha_defuncion, 1, 10)
base$fecha_registro <- substr(base$fecha_registro, 1, 10)
base$fecinitxantivi <- substr(base$fecha_registro, 1, 10)
base$fecha_inicio_sintomas <- as.Date(base$fecha_inicio_sintomas,"%Y-%m-%d")
base$fecha_ingreso <- as.Date(base$fecha_ingreso,"%Y-%m-%d")
base$fecha_defuncion <- as.Date(base$fecha_defuncion,"%Y-%m-%d")
base$fecha_registro <- as.Date(base$fecha_registro,"%Y-%m-%d")
base$fecinitxantivi <- as.Date(base$fecinitxantivi, "%Y-%m-%d")

# Select columns of interest
ColumnasInteres <- c('fecha_registro', 'fecha_inicio_sintomas', 'fecha_ingreso', 'fecha_defuncion',
                     'fecinitxantivi', 'antivira', "txcrobia", "txantivi", "rectrata", 'antipireticos',
                     'resultado_definitivo', 'evolucion', 'sexo', 'edad', 'tipo_paciente', 'seringre',
                     'diabetes', 'inmusupr', 'hiperten', 'vihsida', 'enfcardi', 'obesidad', 'insrencr',
                     'epoc', 'asma', 'uci', 'intubado')

base_interes <- base[ ,ColumnasInteres]

covid_2022_todos <- base_interes  %>%  filter( (fecha_ingreso >= '2022-01-01') &
                                                 (fecha_ingreso <= '2022-12-31') & 
                                                 (fecha_inicio_sintomas >= '2022-01-01') &
                                                 (fecha_inicio_sintomas <= '2022-12-31') )

# Write COVID dataset of 2022 with columns of interest
write.csv(covid_2022_todos, "/Users/ivanas.o/Downloads/Paxlovid/COVID_full_2022_v2.csv", row.names=FALSE)

#--------------------------------------------------------SECOND PART---------------------------------------------------
library("dplyr")
library("tidyr")
library("dplyr")

# Read COVID 2022 data
covid_2022_b <-read.csv("/Users/ivanas.o/Downloads/Paxlovid/COVID_full_2022_v2.csv")

# Because we only consider as if we know until dec 2022, people who died in 2023 are input Nan values
def_2023 <- c("2023-01-27", "2022-12-31", "2023-01-05", "2023-01-16", "2023-01-14", "2023-01-09", "2023-01-06", "2023-01-20",
              "2023-01-30", "2023-01-02", "2023-01-03", "2023-01-12", "2023-01-08", "2023-01-07", "2023-01-13", "2023-01-25", 
              "2023-01-04", "2023-01-23", "2023-01-10", "2023-01-17", "2023-01-01", "2023-01-19", "2023-01-11", "2023-01-26", "2023-01-18", "2023-01-15", "2023-01-21")
ss <- covid_2022_b[covid_2022_b$fecha_defuncion %in% def_2023,]
ss %>% count(resultado_definitivo)
unique(covid_2022_b$fecha_defuncion)
covid_2022_b$fecha_defuncion[covid_2022_b$fecha_defuncion %in% def_2023] <- NA
unique(covid_2022_b$fecha_defuncion)

# Change date format
covid_2022_b$fecha_registro <- as.Date(covid_2022_b$fecha_registro, "%Y-%m-%d")
covid_2022_b$fecha_inicio_sintomas <- as.Date(covid_2022_b$fecha_inicio_sintomas,"%Y-%m-%d")
covid_2022_b$fecha_ingreso <- as.Date(covid_2022_b$fecha_ingreso,"%Y-%m-%d")
covid_2022_b$fecha_defuncion <- as.Date(covid_2022_b$fecha_defuncion,"%Y-%m-%d")
covid_2022_b$fecinitxantivi <- as.Date(covid_2022_b$fecinitxantivi, "%Y-%m-%d")

# Identify variable with NA values
summary(covid_2022_b)

# Fill NA of the variable 'evolucion'
covid_2022_b$evolucion[is.na(covid_2022_b$evolucion)] <- "SIN INFORMACION"
covid_2022_b %>% group_by(evolucion) %>% summarise(coount_evo = n(), .groups = 'drop')
unique(covid_2022_b$evolucion)

# Change values of evolution
trat_terminado <- c("SEGUIMIENTO TERMINADO","ALTA - MEJORIA", "ALTA - CURACION", "CASO NO GRAVE")
trat_sin_info <- c("EN TRATAMIENTO", "CASO GRAVE", "SEGUIMIENTO DOMICILIARIO", "SIN INFORMACION", 
                   "REFERENCIA", "ALTA - TRASLADO","ALTA - VOLUNTARIA")
trat_def <- c("DEFUNCION")  

covid_2022_b$evolucion[covid_2022_b$evolucion %in% trat_terminado] <- "ALTA"
covid_2022_b$evolucion[covid_2022_b$evolucion %in% trat_sin_info] <- "SIN INFORMACION"
covid_2022_b$evolucion<- with(covid_2022_b, ifelse(is.na(fecha_defuncion) == FALSE, "DEFUNCION", evolucion))

# Ambulatorios
ambu <- covid_2022_b[covid_2022_b["tipo_paciente"] != "HOSPITALIZADO",]
ambu %>% group_by(evolucion) %>% summarise(coount_evo = n(), .groups = 'drop')
names(covid_2022_b)

# Fill Na of variable 'antivira'
covid_2022_b$antivira[is.na(covid_2022_b$antivira)] <- "SIN INFORMACION"
covid_2022_b %>% group_by(antivira) %>% summarise(coount_ant = n(), .groups = 'drop')

# Fill NA of variable 'uci'
covid_2022_b$uci[is.na(covid_2022_b$uci)] <- "NO"
covid_2022_b %>% group_by(uci) %>% summarise(count_uci = n(), .groups = 'drop')

# Fill NA of variable 'intubado'
covid_2022_b$intubado[is.na(covid_2022_b$intubado)] <- "NO"
covid_2022_b %>% group_by(intubado) %>% summarise(count_int = n(), .groups = 'drop')

# Distinct values of each comorbidity
unique(covid_2022_b$obesidad)
unique(covid_2022_b$hiperten)
unique(covid_2022_b$diabetes)
unique(covid_2022_b$insrenc)
unique(covid_2022_b$enfcardi)
unique(covid_2022_b$epoc)
unique(covid_2022_b$asma)
unique(covid_2022_b$inmusupr)
unique(covid_2022_b$vih_sida)
unique(covid_2022_b$tipo_paciente)

# Change antivira values 
unique(covid_2022_b$antivira)

# Change specific values of antivirals 
covid_2022_b[1908965,'antivira'] <- 'Cefalexina  antifludes' # Change specific row

# Set values to lower case 
covid_2022_b$antivira <- tolower(covid_2022_b$antivira) # Set letter to lower case
list_antiviral <- unique(covid_2022_b$antivira) # list of antivirals
list_antiviral

# Change specific values of antivirals
sin_info_list <- c("----------------------------","------------------", "no especifica", "se igmora",
                   "se ignora","otro", "no especificado","desconocido", "no recuerda","se desconoce",
                   "antibiotico", "antibióticos" , "antibióticos y antigripal", 
                   "jarabe", "inyecciones para la tos", "inyecciones",
                   "antiviral", "antipereticos" , "antipireticos" , "antivirales",
                   "diabetes, hipertensión, obesidad", "antigripal","antibiótico", "sin informacion" )

covid_2022_b$antivira[covid_2022_b$antivira %in% sin_info_list ] <- "SIN INFORMACION" 

# New list of antivirals
list_antiviral <- unique(covid_2022_b$antivira) 
list_antiviral

# Remdesivir treatment
with_r <- grep("r", list_antiviral, value = TRUE) # list of antivirals with r
remdesivir_list <- c('remdesivir', "remdesivir + baricitinib", "rendesivir")
remdesivir <- covid_2022_b[covid_2022_b$antivira %in% remdesivir_list, ]
nrow(remdesivir)
remdesivir_list

# Paxlovid treatment
with_n <- grep("n", list_antiviral, value = TRUE)# no nirmaltrevir in the data ser
paxlovid_list <- append(c('ritonavir'), c("paxlovid")) # only ritonavir is available
paxlovid <- covid_2022_b[covid_2022_b$antivira %in% paxlovid_list, ]
nrow(paxlovid)
paxlovid_list

# Identify placebo treatment : Paracetamol
with_parace <- grep("parace", list_antiviral, value = TRUE)
paracetamol_list <- c(with_parace, c("parcetamol","parasetamol",
                                     "tempra","tempra, atifludes, azitromicina",
                                     "next","antifluides jr", "antfludes",
                                     "antifludes",
                                     "antiflu", "antiflu des", "antiflux dex",
                                     "antiflu y tesalon",
                                     "antifludes, tempra, acitromizina",
                                     "dualgos y antifludes",
                                     "antifudes", "rosel", "rosell","rozel",
                                     "dualgos", "amoxicilina, rosel",
                                     "roseel", "antiflu y tesalon","jarabe y tempra",
                                     "teraflu","teraflo y decenfrolies") )

# Identify placebo treatment : Ibuprofeno
with_ibup <- grep("ibup", list_antiviral, value = TRUE)
ibuprofeno_list <- c(with_ibup, c("uboprofeno", "iboprofeno","algidol ibruprofeno, jarabe brosol"))
# Placebo treatment
para_ibu_list <- c(paracetamol_list, ibuprofeno_list)
para_ibu <- covid_2022_b[covid_2022_b$antivira %in% para_ibu_list, ]
nrow(para_ibu)
ibuprofeno_list
list_antiviral_no_para_ibu <- c()
for ( i in list_antiviral){
  if (i %in% para_ibu_list == FALSE){
    list_antiviral_no_para_ibu <- append(list_antiviral_no_para_ibu, i)
  }
}


# Replace antivira
covid_2022_b$antivira[covid_2022_b$antivira %in% para_ibu_list] <- "SINTOMATICO"
covid_2022_b$antivira[covid_2022_b$antivira %in% paxlovid_list] <- "PAXLOVID"
covid_2022_b$antivira[covid_2022_b$antivira %in% remdesivir_list] <- "REMDESIVIR"
covid_2022_b$antivira <- with(covid_2022_b, 
                              ifelse(antivira %in% c("SINTOMATICO", "PAXLOVID", "REMDESIVIR", "SIN INFORMACION"), antivira, "OTROS"))


# ------------------------------------------------------- THIRD PART -------------------------------------------------
library("dplyr")
library("tidyr")
library("dplyr")

# Function: DataTransformation
#Input:  data - data base with relevant columns
#Output: data_m - modified data base

DataTransformation <- function(data, COVID_list){
  # Transform of date variables
  data$symptoms_date <- as.Date(data$fecha_inicio_sintomas)
  data$admission_date <- as.Date(data$fecha_ingreso)
  data$death_date <- as.Date(data$fecha_defuncion)
  data$antiviral_date <- as.Date(data$fecinitxantivi)
  
  # Transform of antiviral
  data$antivira <- as.factor(data$antivira)
  data$diagnostic_result <- as.factor(data$resultado_definitivo)
  data$age <- as.integer(data$edad)
  data$icu <- as.factor(with(data, ifelse(uci =="SI", 1, 0)))
  
  # Age group
  data$age_group <- data$age
  for(i in seq(0,120,5)){
    data$age_group[data$age_group %in% seq(i,i+4,1)] <-  paste("[", as.character(i), ",", as.character(i + 4), "]")}
  data$age_group <- as.factor(data$age_group)
  
  #Sex
  data$sex <- as.factor(with(data, ifelse(sexo == "MASCULINO", "M", "F")))
  #Obesity. 1 if the patient has obesity, 0 otherwise
  data$obesity <- as.factor(with(data, ifelse(obesidad == "SI", 1, 0)))
  # Diabetes. 1 if the patient has diabetes, 0 otherwise
  data$diabetes <- as.factor(with(data, ifelse(diabetes == "SI", 1, 0)))
  # Hyperthension. 1 if the patient has it, 0 otherwise
  data$hyperten <- as.factor(with(data, ifelse(hiperten == "SI", 1, 0)))
  # Chronic kidney disease. 1 if the patient has it, 0 otherwise
  data$ckd <- as.factor(with(data, ifelse(insrencr =="SI", 1, 0)))
  # Cardiovascular disease. 1 if the patient has it, 0 otherwise
  data$cardio <- as.factor(with(data, ifelse(enfcardi =="SI", 1, 0)))
  # Chronic obstructive pulmonary disease. 1 if the patient has it, 0 otherwise
  data$copd <- as.factor(with(data, ifelse(epoc == "SI", 1, 0)))
  # Asma. 1 if the patient has it, 0 otherwise
  data$asthma <- as.factor(with(data, ifelse(asma == "SI", 1, 0)))
  # Immunosuppressed. 1 if the patient has it, 0 otherwise
  data$immuno <- as.factor(with(data, ifelse(inmusupr == "SI", 1, 0)))
  # HIV. 1 if the patient has it, 0 otherwise
  data$hiv <- as.factor(with(data, ifelse(vihsida=="SI", 1, 0)))
  #Comorbidity
  # 1 if any of the listed comorbidities, 0 otherwise.
  data$comorbidity<-with(data, ifelse(obesity == 1| diabetes == 1| hyperten == 1| ckd == 1| cardio == 1| copd == 1| asthma == 1|
                                        immuno == 1| hiv == 1, 1, 0))
  
  data$comorbidity <- as.factor(data$comorbidity)
  
  #Covid or not 
  data$covid <- as.factor(with(data, ifelse(resultado_definitivo %in% COVID_list, 1, 0)))
  
  #Patient type
  data$patient_type <- as.factor(with(data, ifelse(tipo_paciente =="HOSPITALIZADO","H","A")))
  
  #Death: 1 if patient passed out, 0 otherwise
  data$death<-as.factor(with(data,ifelse(is.na(fecha_defuncion) == FALSE,1,0)))
  
  # Evolucion
  data$evolution <- as.factor(data$evolucion)
  
  #Time
  # Difference of days between the day of death and day of onset symptoms only if the patient died, otherwise is 999.
  data$time<-with(data,ifelse(death==1, as.numeric(as.Date(fecha_defuncion)) -as.numeric(as.Date(fecha_inicio_sintomas)),
                              999))
  data$time <- as.integer(data$time)
  
  #Select variables that will be used for future analysis
  data_m <- data[,
                 c("age","age_group", "sex", "obesity", "diabetes", "hyperten", "ckd", "cardio",
                   "copd", "asthma", "immuno", "hiv", "comorbidity", "covid", "patient_type","icu", "death", "time",
                   "symptoms_date", "admission_date", "death_date", "antiviral_date", "antivira", "diagnostic_result",
                   "evolucion")]
  
  
  return(data_m)
}

# List of COVID results
COVID_list <- c('SARS-CoV-2', 'SARS-CoV-2 - B.1.1.519','SARS-CoV-2 - MU','SARS-CoV-2 - OTRA',
                'SARS-CoV-2 - B.1.1.519','SARS-CoV-2 - OMICRON', 'SARS-CoV-2 - DELTA', 
                'SARS-CoV-2 - ALPHA', 'SARS-CoV-2 - GAMMA', 'SARS-CoV-2 - LAMBDA', 
                'SARS-CoV-2 - IOTA', "SARS-CoV-2 - OMICRON-BA.2") 

# Generate data frame from previous with the DataTransformation function
covid <- DataTransformation(covid_2022_b, COVID_list)

# Write transform data base
write.csv(covid, "/Users/ivanas.o/Downloads/Paxlovid/COVID_transform_2022_v10.csv", row.names=FALSE)

# ------------------------------------------------- FOURTH PART -------------------------------------------------
library("dplyr")
library("tidyr")
library("dplyr")

# Read COVID 2022 standarized database
covid_data <- read.csv("/Users/ivanas.o/Downloads/Paxlovid/COVID_transform_2022_v10.csv")

# Change group ages
covid_2 <- covid_data[covid_data$age >= 18, ]
covid_2$age_group[covid_2$age_group %in% c("[ 90 , 94 ]", "[ 95 , 99 ]","[ 100 , 104 ]","[ 105 , 109 ]",
                                           "[ 110 , 114 ]","[ 115 , 119 ]", "[ 120 , 124 ]")] <- "[ 90 , Older )"
covid_2$age_group[covid_2$age_group %in% c("[ 15 , 19 ]", "[ 20 , 24 ]")] <- "[ 18 , 24 ]"

covid_1 <- covid_data[covid_data$age < 18, ]
covid_1$age_group[covid_1$age_group %in% c("[ 15 , 19 ]")] <- "[ 15 , 17 ]"

covid_end <- rbind(covid_1, covid_2)

# Write new database
write.csv(covid_end, "/Users/ivanas.o/Downloads/Paxlovid/COVID_transform_2022_v10_p2.csv", row.names=FALSE)

