library("dplyr")
library("tidyr")

#------------------------------------ CONACYT SUSPICIOUS CASES -----------------------------
# Count of national suspicious cases: positive, negative and unknown
nombre_estado <- c("Nacional", "DISTRITO FEDERAL")

# Positive cases
confirmados <-read.csv("/Users/ivanas.o/Downloads/Paxlovid/Sospechosos/Casos_Diarios_Estado_Nacional_Confirmados_20230620.csv")
confirmados_nacional <- confirmados %>% filter ( nombre %in% nombre_estado)
confirmados_nacional_2022 <- confirmados_nacional %>% select(contains("2022")) 
confirmados_nacional_2022 <- confirmados_nacional_2022 %>% mutate(confirms = rowSums(across(everything())))

# Negative cases
negativos <-read.csv("/Users/ivanas.o/Downloads/Paxlovid/Sospechosos/Casos_Diarios_Estado_Nacional_Negativos_20230620.csv")
negativos_nacional <- negativos %>% filter ( nombre %in% nombre_estado)
negativos_nacional_2022 <- negativos_nacional %>% select(contains("2022")) 
negativos_nacional_2022 <- negativos_nacional_2022 %>% mutate(negatives = rowSums(across(everything())))

# Unknown cases
sospechosos <-read.csv("/Users/ivanas.o/Downloads/Paxlovid/Sospechosos/Casos_Diarios_Estado_Nacional_Sospechosos_20230620.csv")
sospechosos_nacional <- sospechosos %>% filter ( nombre %in% nombre_estado)
sospechosos_nacional_2022 <- sospechosos_nacional %>% select(contains("2022")) 
sospechosos_nacional_2022 <- sospechosos_nacional_2022 %>% mutate(suspicious = rowSums(across(everything())))

nacional_2022 <- data.frame(cbind(rev(nombre_estado),confirmados_nacional_2022[, "confirms"],
                                    negativos_nacional_2022[, "negatives"],
                                    sospechosos_nacional_2022[, "suspicious"]))

names(nacional_2022) <- c("state","confirms", "negatives", "suspicious")
nacional_2022$total_cases <- as.integer(nacional_2022$confirms) + as.integer(nacional_2022$negatives) + as.integer(nacional_2022$suspicious)

nacional_2022$state[nacional_2022$state == "Nacional"] <- "National: All States"
nacional_2022$state[nacional_2022$state == "DISTRITO FEDERAL"] <- "Mexico City"

# Write data
write.csv(nacional_2022, "/Users/ivanas.o/Downloads/Paxlovid/suspicious_mexico_2022.csv", row.names=FALSE)







