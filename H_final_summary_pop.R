rm(list = ls(all.names = TRUE))
gc()
library(diagram)
library(heemod)
library(ggplot2)
path = "D:/GitHub/Paxlovid_MathematicalModel/data"
markov_params <- read.csv(paste(path, "Final_PobSet.csv", sep = "/"))

sum(markov_params$count_)
min(markov_params$count_)
min(markov_params$count_)
for(i in 1:90){
  a =read.csv(paste(path,"/strat1/count/", i, ".csv", sep=""))
  if(exists("s1_count")){
    s1_count= s1_count+a}
  else{
    s1_count= a
  }
}

for(i in 1:90){
  a =read.csv(paste(path,"/strat1/cost/", i, ".csv", sep=""))
  if(exists("s1_cost")){
    s1_cost= s1_cost+a}
  else{
    s1_cost= a
  }
}


for(i in 1:90){
  a =read.csv(paste(path,"/strat2/count/", i, ".csv", sep=""))
  if(exists("s2_count")){
    s2_count= s2_count+a}
  else{
    s2_count= a
  }
}

for(i in 1:90){
  a =read.csv(paste(path,"/strat2/cost/", i, ".csv", sep=""))
  if(exists("s2_cost")){
    s2_cost= s2_cost+a}
  else{
    s2_cost= a
  }
}


write.csv(round(s1_count, 0),paste(path,"strat1_counts.csv", sep="/"),
          row.names=FALSE)
write.csv(round(s2_count, 0),paste(path,"strat2_counts.csv", sep="/"),
          row.names=FALSE)

s1_cost$model_time = s1_cost$model_time/90
head(s1_cost)
s2_cost$model_time = s2_cost$model_time/90

write.csv(round(s1_cost, 3),paste(path,"strat1_cost.csv", sep="/"),
          row.names=FALSE)
write.csv(round(s2_cost, 3),paste(path,"strat2_cost.csv", sep="/"),
          row.names=FALSE)




