library("dplyr")
library("survival")
library("lattice")
library("ggplot2")
library("SparseM")
library("rms")
#library("Hmisc")
#library("tidyr")
#library("readr")

# ----------------------------------------- SINAVE data managing ----------------------------------------
data_SINAVE <-read.csv("/Users/ivanas.o/Downloads/Paxlovid/COVID_transform_2022_v9_p2.csv")
colSums(is.na(data_SINAVE))
# Only with COVID
data_SINAVE <- data_SINAVE %>% filter(covid == 1)
# Create an stratify variable from sex and patient type
data_SINAVE$sex_and_patient <- paste(data_SINAVE$sex, "_", data_SINAVE$patient_type)
data_SINAVE$sex_and_patient[data_SINAVE$sex_and_patient == "F _ A"] <- 0
data_SINAVE$sex_and_patient[data_SINAVE$sex_and_patient == "M _ A"] <- 1
data_SINAVE$sex_and_patient[data_SINAVE$sex_and_patient == "F _ H"] <- 2
data_SINAVE$sex_and_patient[data_SINAVE$sex_and_patient == "M _ H"] <- 3
# Censor people after a month
data_SINAVE$time[data_SINAVE$time > 30] <- 30
# Selection of columns
data_SINAVE <-data_SINAVE[,c("age", "age_group", "sex", "comorbidity", "patient_type", "sex_and_patient",
                             "obesity", "diabetes", "hyperten", "ckd", "cardio", "copd", "asthma", "immuno", "hiv",
                             "death", "time")]

# ProcessedData_ Function to select age range and convert variable to categorical variables
ProcessedData <- function(data_SINAVE, lower_age, upper_age){
  # People from lower_age to upper_age years old 
  if( upper_age == 999 ){
    data_SINAVE_COVID <- data_SINAVE %>% filter((age >= lower_age))
  }else{
  data_SINAVE_COVID <- data_SINAVE %>% filter((age >= lower_age) & (age <= upper_age))
  }
  # Transform variable to categorial variables
  data_SINAVE_COVID$age <- as.integer(data_SINAVE_COVID$age)
  data_SINAVE_COVID$age_group <- as.factor(data_SINAVE_COVID$age_group)
  data_SINAVE_COVID$sex <- as.factor(data_SINAVE_COVID$sex)
  data_SINAVE_COVID$comorbidity <- as.factor(data_SINAVE_COVID$comorbidity)
  data_SINAVE_COVID$patient_type <- as.factor(data_SINAVE_COVID$patient_type)
  data_SINAVE_COVID$sex_and_patient <- as.factor((data_SINAVE_COVID$sex_and_patient))
  data_SINAVE_COVID$obesity <- as.factor(data_SINAVE_COVID$obesity)
  data_SINAVE_COVID$diabetes <- as.factor(data_SINAVE_COVID$diabetes)
  data_SINAVE_COVID$hyperten <- as.factor(data_SINAVE_COVID$hyperten)
  data_SINAVE_COVID$ckd <- as.factor(data_SINAVE_COVID$ckd)
  data_SINAVE_COVID$cardio <- as.factor(data_SINAVE_COVID$cardio)
  data_SINAVE_COVID$copd <- as.factor(data_SINAVE_COVID$copd)
  data_SINAVE_COVID$asthma <- as.factor(data_SINAVE_COVID$asthma)
  data_SINAVE_COVID$immuno <- as.factor(data_SINAVE_COVID$immuno)
  data_SINAVE_COVID$hiv <- as.factor(data_SINAVE_COVID$hiv)
  data_SINAVE_COVID$time <- as.integer(data_SINAVE_COVID$time)
  
  return(data_SINAVE_COVID)
}

#data_SINAVE %>% count(age_group)
describe(data_SINAVE$age_group)

summary(data_SINAVE$age)
nrow(data_SINAVE[data_SINAVE$age >=18,] )/3

# OPCION CON 4 particiones
# Preprocessed data for range of ages
data_18_29 <- ProcessedData(data_SINAVE, 18, 29)
nrow(data_18_29)
data_30_44 <- ProcessedData(data_SINAVE, 30, 44)
nrow(data_30_44)
data_45_64 <- ProcessedData(data_SINAVE, 45, 64)
nrow(data_45_64)
data_65_older <- ProcessedData(data_SINAVE, 65, 999)
nrow(data_65_older)
nrow(data_18_29) + nrow(data_30_44) + nrow(data_45_64) + nrow(data_65_older) == nrow(data_SINAVE[data_SINAVE$age >=18,])

# OPCION CON3 particiones
# Preprocessed data for range of ages
data_18_34 <- ProcessedData(data_SINAVE, 18, 34)
nrow(data_18_34)
data_35_59 <- ProcessedData(data_SINAVE, 35, 59)
nrow(data_35_59)
data_60_ld <- ProcessedData(data_SINAVE, 60, 999)
nrow(data_60_ld)
nrow(data_18_34) + nrow(data_35_60) + nrow(data_60_ld)  == nrow(data_SINAVE[data_SINAVE$age >=18,])


# Summary and nans values
summary(data_SINAVE_COVID)
colSums(is.na(data_SINAVE_COVID))

# ------------------------ COX MODEL --------------------------------------
# CoxModel: function of the Cox Model
CoxModel <- function(data_processed){
  m1<-cph(Surv(time, death) ~ 
            age_group + strat(sex_and_patient) + obesity + diabetes  + hyperten + ckd + cardio + copd + asthma + immuno + hiv, 
          data_processed[,columns], x=TRUE, y=TRUE, surv=T)
  return(m1)
}

# Select columns
columns <- c("age_group" , "sex_and_patient",
             "obesity", "diabetes", "hyperten", "ckd", "cardio", "copd", "asthma", "immuno", "hiv", 
             "time", "death")

# Cox model for distinct range of ages 
# OPCION con 4 particiones
# From 18 to 29 years old
d <- datadist(data_18_29[,columns]); options(datadist = "d")
m_18_29 <- CoxModel(data_18_29)
cox.zph(m_18_29, transform="km", global=TRUE) #0.38
# From 30 to 44 years old
d <- datadist(data_30_44[,columns]); options(datadist = "d")
m_30_44 <- CoxModel(data_30_44)
cox.zph(m_30_44, transform="km", global=TRUE) #0.77
# From 45 to 64 years old
d <- datadist(data_45_64[,columns]); options(datadist = "d")
m_45_64 <- CoxModel(data_45_64)
cox.zph(m_45_64, transform="km", global=TRUE) # 0.6
# From 65 or older
d <- datadist(data_65_older[,columns]); options(datadist = "d")
m_65_older <- CoxModel(data_65_older)
cox.zph(m_65_older, transform="km", global=TRUE) # 0.101

# OPCION CON 3 particiones
# From 18 to 34
d <- datadist(data_18_34[,columns]); options(datadist = "d")
m_18_34 <- CoxModel(data_18_34)
cox.zph(m_18_34, transform="km", global=TRUE) 
# From 35 to 59
d <- datadist(data_35_59[,columns]); options(datadist = "d")
m_35_59 <- CoxModel(data_35_59)
cox.zph(m_35_59, transform="km", global=TRUE) 
# From 60 or older
d <- datadist(data_60_ld[,columns]); options(datadist = "d")
m_60_ld <- CoxModel(data_60_ld)
cox.zph(m_60_ld, transform="km", global=TRUE) 

# ---------------------------------- Survival probabilities --------------------------------------------
# SurvivalProb: function that return survival probabilities on a day for each age_group, sex_patient and 
# if it has a comorbidity or not
SurvivalProb <- function(cox_model, age_grp_lower_upper, cmbdts, sx_ptnt_v, day){
  # Survival probabilities in each variables combination
  p <- Predict(cox_model, age_group = age_grp_lower_upper, sex_and_patient = sx_ptnt_v, time = day,
               obesity = cmbdts, diabetes = cmbdts, hyperten = cmbdts,ckd = cmbdts, cardio = cmbdts, 
               copd = cmbdts, asthma = cmbdts, immuno = cmbdts, hiv = cmbdts) # nrow(p) # 4*(2**9)*6
  # Identify probailites of people with at least one comorbidity
  p$comorbidity <- as.numeric(p$obesity) + as.numeric(p$diabetes) + as.numeric(p$hyperten) + 
                  as.numeric(p$ckd) + as.numeric(p$cardio) + as.numeric(p$copd) + as.numeric(p$asthma) + 
                  as.numeric(p$immuno) + as.numeric(p$hiv) - 9
  p$comorbidity[p$comorbidity > 0] <- 1
  p$comorbidity <- as.factor(p$comorbidity)
  # Mean of survival probabilities  by age_group, sex, patient and  comorbidities
  new_columns <- c("age_group", "sex_and_patient", "comorbidity", "yhat")
  p_new <- p[,new_columns]
  p_new_mean <- p_new %>% 
                group_by(age_group, sex_and_patient, comorbidity) %>%
                summarise(m_yhat = mean(yhat)) %>% as.data.frame()
  # Rename of column depending on the survival day
  colnames(p_new_mean)[colnames(p_new_mean) == "m_yhat" ] <- paste("day_", as.character(day), sep="")
  
  return(p_new_mean)
}

cmbdts <- c("0", "1")
sx_ptnt_v <- c("0", "1", "2", "3")

# Survival for people from 18 to 49 years old
age_grp_18_29 <- c("[ 18 , 24 ]",  "[ 25 , 29 ]")
age_grp_30_44 <- c("[ 30 , 34 ]", "[ 35 , 39 ]",  "[ 40 , 44 ]") 
age_grp_45_64 <- c("[ 45 , 49 ]","[ 50 , 54 ]", "[ 55 , 59 ]", "[ 60 , 64 ]")
age_grp_65_ld <- c("[ 65 , 69 ]", "[ 70 , 74 ]", "[ 75 , 79 ]", "[ 80 , 84 ]", "[ 85 , 89 ]", "[ 90 , Older )")

# SurvivialProbDays: concatenate the survival prob of 30 days
SurvivalProbDays <- function(cox_model, p_day_1_lower_upper, age_grp_lower_upper){
  # Survival of day 2 to 30 and join of dataframes
  for (i in 2:30){
    p_day_i <- SurvivalProb(cox_model, age_grp_lower_upper, cmbdts, sx_ptnt_v, i)
    p_day_1_lower_upper <- merge(p_day_1_lower_upper, p_day_i, 
                                 by = c("age_group", "sex_and_patient", "comorbidity") )
  }
  return(p_day_1_lower_upper)
}

# People from 18 to 29
p_day_1_18_29 <- SurvivalProb(m_18_29, age_grp_18_29, cmbdts, sx_ptnt_v, 1)
p_18_29 <- SurvivalProbDays(m_18_29, p_day_1_18_29, age_grp_18_29)

# People from 30 to 44
p_day_1_30_44 <- SurvivalProb(m_30_44, age_grp_30_44, cmbdts, sx_ptnt_v, 1)
p_30_44 <- SurvivalProbDays(m_30_44, p_day_1_30_44, age_grp_30_44)

# People from 45 to 64
p_day_1_45_64 <- SurvivalProb(m_45_64, age_grp_45_64, cmbdts, sx_ptnt_v, 1)
p_45_64 <- SurvivalProbDays(m_45_64, p_day_1_45_64, age_grp_45_64)

# People from 45 to 64
p_day_1_65_ld <- SurvivalProb(m_65_older, age_grp_65_ld, cmbdts, sx_ptnt_v, 1)
p_65_ld <- SurvivalProbDays(m_65_older, p_day_1_65_ld, age_grp_65_ld)

p_18_ld <- do.call("rbind", list(p_18_29, p_30_44, p_45_64, p_65_ld))

p_18_ld$sex <- with(p_18_ld, 
                    ifelse(sex_and_patient %in% c("0", "2"), "F", "M"))

p_18_ld$patient_type <- with(p_18_ld, 
                             ifelse(sex_and_patient %in% c("0", "1"), "A", "H"))

names(p_18_ld)
p_18_ld_write <- p_18_ld[, c("age_group", "sex", "patient_type", "comorbidity", "day_1", "day_2", "day_3", "day_4", "day_5", "day_6", 
                          "day_7", "day_8", "day_9",  "day_10", "day_11", "day_12", "day_13", "day_14", "day_15", "day_16", "day_17", 
                          "day_18", "day_19", "day_20", "day_21", "day_22", "day_23", "day_24", "day_25", "day_26", "day_27", "day_28", 
                          "day_29", "day_30")]
p_18_ld_write
write.csv(p_18_ld_write, "/Users/ivanas.o/Downloads/Paxlovid/COX_SurvProb_30days.csv", row.names=FALSE)

# ---------------------------- EXTRA -------------------
# Op 1: no funciona
data_18_older <- ProcessedData(data_SINAVE, 18, 999) #0.003

# Op 2: 
data_18_29 <- ProcessedData(data_SINAVE, 18, 29) # 0.3
data_30_59 <- ProcessedData(data_SINAVE, 30, 59) # 0.2
data_60_older <- ProcessedData(data_SINAVE, 60, 999) # 0.1

# 35 a 59 * 

# Op 1
d <- datadist(data_18_older[,columns]); options(datadist = "d")
m_18_older <- CoxModel(data_18_older)
cox.zph(m_18_older, transform="km", global=TRUE)

# Op 2
d <- datadist(data_18_29[,columns]); options(datadist = "d")
m_18_29 <- CoxModel(data_18_29)
cox.zph(m_18_29, transform="km", global=TRUE)

d <- datadist(data_30_59[,columns]); options(datadist = "d")
m_30_59 <- CoxModel(data_30_59)
cox.zph(m_30_59, transform="km", global=TRUE)

d <- datadist(data_60_older[,columns]); options(datadist = "d")
m_60_older <- CoxModel(data_60_older)
cox.zph(m_60_older, transform="km", global=TRUE)

