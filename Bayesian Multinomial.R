#Processing ternary alternations using Multinomial and Bayesian multinomial regression modeling 
#reading data
gen2<-read.csv("svsof.csv", sep=";", stringsAsFactors=TRUE)

summary(gen2)
#defining factors
gen2$genitive_type<-as.factor(gen2$genitive_type)
gen2$por_length_words<-as.numeric(gen2$por_length_words)
gen2$pum_length_words<-as.numeric(gen2$pum_length_words)
gen2$animacy<-as.factor(gen2$animacy)
gen2$alpha_persistence_S<-as.factor(gen2$alpha_persistence_S)
gen2$alpha_persistence_OF<-as.factor(gen2$alpha_persistence_OF)
gen2$alpha_persistence_NN<-as.factor(gen2$alpha_persistence_NN)
gen2$beta_persistence_S<-as.factor(gen2$beta_persistence_S)
gen2$beta_persistence_OF<-as.factor(gen2$beta_persistence_OF)
gen2$beta_persistence_NN<-as.factor(gen2$beta_persistence_NN)
gen2$TTR<-as.numeric(gen2$TTR)
gen2$por_thematicity_ptw<-as.numeric(gen2$por_thematicity_ptw)
gen2$pum_thematicity_ptw<-as.numeric(gen2$pum_thematicity_ptw)
gen2$final_sibilancy<-as.factor(gen2$final_sibilancy)
gen2$time<-as.numeric(gen2$time)
gen2$register<-as.factor(gen2$register)
gen2$period<-as.factor(gen2$period)

##testing multinomial regression with binary outcomes. Data: avsof.csv
install.packages("nnet")
library(nnet)
multinom_model2 <- multinom( 
  formula = genitive_type ~ por_length_words + pum_length_words +animacy + alpha_persistence_S +alpha_persistence_OF +beta_persistence_S + beta_persistence_OF + TTR + por_thematicity_ptw + pum_thematicity_ptw +final_sibilancy +time, data = gen2)
summary(multinom_model2)
##model evaluation


##testing Bayesian multinomial regression analysis with diachronic data. Data: threewaygen.csv

