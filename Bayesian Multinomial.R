#==================================================================================================
#Processing ternary alternations using Multinomial and Bayesian multinomial regression modeling 
#==================================================================================================
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

##defining formula
gen2_f1<- genitive_type ~ por_length_words + pum_length_words +animacy + 
  alpha_persistence_S + alpha_persistence_OF + beta_persistence_S + beta_persistence_OF + 
  TTR + por_thematicity_ptw + pum_thematicity_ptw + final_sibilancy + time

##=====================================================================================
##testing multinomial regression with binary outcomes. Data: avsof.csv
##=====================================================================================
install.packages("nnet")
library(nnet)
multinom_model2 <- multinom(gen2_f1, data = gen2)
summary(multinom_model2)
##In practice, multinomial model also processes binary responses but it reduces it to binomial logistic regression
##the specification over the formula of multinon() function: 
##The response should be a factor or a matrix with K columns, 
##which will be interpreted as counts for each of K classes. 
##A log-linear model is fitted, with coefficients zero for the first class. 
##An offset can be included: it should be a numeric matrix with K columns if the response is either a matrix with K columns 
##or a factor with K >= 2 classes, or a numeric vector for a response factor with 2 levels.
### Multinom () is efficient in processing binary alternations, but it does not compute the Wald z-values, P-values and CI.
### This is due to the fact that the authors discourage Wald test and the model deliberately avoids the computation of the second-derivative Hessian matrix, H.
### I guess it is to reduce the computation cost. 

### computing P-values manually
s <- summary(multinom_model2)
z <- s$coefficients / s$standard.errors
p <- 2 * (1 - pnorm(abs(z)))
p
##model evaluation: C-value
library(pROC)
###converting probabilistis to log-odds for the non-reference outcome
probs<-predict(multinom_model2, type="probs")
###identifying the reference level
ref<-levels (gen2$genitive_type) [1]
###calling the non-reference level
cat1<-levels(gen2$genitive_type) [2]
###creating a binary outcome
y_bin<-ifelse(gen2$genitive_type==cats, 1, 0)
###computing ROC and AUC
roc_obj<-roc(y_bin, probs)
c_value<-auc(roc_obj)
###C-value=0.8767

###or
library(Hmisc)
Dxy <- somers2(probs, y_bin)["Dxy"]
C_value <- (Dxy + 1) / 2
C_value
###C-value=0.876659

## comparing multinom () with glm () function
library(base)
glm_model2<-glm(gen2_f1, family= binomial, data=gen2)
summary(glm_model2)
###C-value using somers2() function
library(Hmisc)
somers2(binomial()$linkinv(fitted(glm_model2)), as.numeric(gen2$genitive_type) -1)
###C-value=0.876687

## In conclusion, the two modeling perform almost similar. For the c-value calculation, the roc() function is recommended. 


##=================================================================================================
##testing Bayesian multinomial regression analysis with diachronic data. Data: threewaygen.csv
##=================================================================================================
## Bayesian binary logistic regression modeling 
install.packages("brms")
library(brms)
library(bayesplot)
brm2<-brm(gen2_f1,
          data=gen2,
          family=bernoulli(link="logit"),
          warmup=500,
          iter=2000,
          chains=2,
          cores=2,
          seed=123)
summary(brm2)
prior_summary(brm2)
plot(brm2)
