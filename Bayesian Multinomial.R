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
gen2_f1
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
          family=bernoulli(link="logit"), ## specifying the binary logistic regression modeling
          warmup=500, ## the warmup iterations per chain, which is not used for inference 
          iter=2000, ## real number of iterations per chain, including burn-in
          chains=2, ## the number of independent MCMC chains
          cores=2, ## the number of CPU cores used in parallel, usually the same as the number of chains
          seed=123) ## the random seeds for reproducibility
summary(brm2)
prior_summary(brm2)
plot(brm2)

## About prior
## checking priors
get_prior(gen2_f1, data=gen2, family=bernoulli())
##prior predictive checking
pp_check(brm2, type="hist", ndraws=50)
##specifying priors (default setting)
priors<-c(
  set_prior("normal(0, 5)", class="Intercept"),
  set_prior("normal(0, 2.5)", class="b"))

##modeling fitting
brm2_p<-brm(gen2_f1,
             family=bernoulli(link="logit"),
             prior=priors,
             data=gen2,
             warmup=500,
             iter=2000,
             chains=2,
             cores=2,
             seed=123)
summary(brm2_p)
summary(brm2)
## If I don't specify priors, the model useing the default prior. 
prior_summary(brm2_p)
## the only difference is that the specified defualt prior (0, 2.5 / 0, 5) would be demonstrated if i do a summary


#==================================================================================================================
#Multinomial & Bayesian multinomial logistic regression
#==================================================================================================================
gen3<-read.csv("threewaygen.csv", sep=";", stringsAsFactors=TRUE)
summary(gen3)
##specifying formula
gen3$genitive_type<-as.factor(gen3$genitive_type)
gen3$por_length_words<-as.numeric(gen3$por_length_words)
gen3$pum_length_words<-as.numeric(gen3$pum_length_words)
gen3$animacy<-as.factor(gen3$animacy)
gen3$alpha_persistence_S<-as.factor(gen3$alpha_persistence_S)
gen3$alpha_persistence_OF<-as.factor(gen3$alpha_persistence_OF)
gen3$alpha_persistence_NN<-as.factor(gen3$alpha_persistence_NN)
gen3$beta_persistence_S<-as.factor(gen3$beta_persistence_S)
gen3$beta_persistence_OF<-as.factor(gen3$beta_persistence_OF)
gen3$beta_persistence_NN<-as.factor(gen3$beta_persistence_NN)
gen3$TTR<-as.numeric(gen3$TTR)
gen3$por_thematicity_ptw<-as.numeric(gen3$por_thematicity_ptw)
gen3$pum_thematicity_ptw<-as.numeric(gen3$pum_thematicity_ptw)
gen3$final_sibilancy<-as.factor(gen3$final_sibilancy)
gen3$time<-as.numeric(gen3$time)
gen3$register<-as.factor(gen3$register)
gen3$period<-as.factor(gen3$period)

gen3_f2<-genitive_type ~ por_length_words + pum_length_words +animacy + 
  alpha_persistence_S + alpha_persistence_OF + alpha_persistence_NN + beta_persistence_S + 
  beta_persistence_OF + beta_persistence_NN + TTR + por_thematicity_ptw + pum_thematicity_ptw + 
  final_sibilancy + time
gen3_f2

##fitting multinomial model
install.packages("nnet")
library(nnet)
gen3_multinom<-multinom(gen3_f2, data=gen3)
summary(gen3_multinom)
##model evaluation
##### Convert probabilities to log-odds for each non-baseline category
probs <- predict(gen3_multinom, type = "probs")
library(pROC)
## Identify the reference category
ref <- levels(gen3$genitive_type)[1]   # should be "NN" in the setup
## List of non-reference categories
cats <- setdiff(levels(gen3$genitive_type), ref)
## Compute C-values for each internal logit
C_values <- sapply(cats, function(cat) {
  ## Binary outcome: 1 = cat, 0 = ref
  y_bin <- ifelse(gen3$genitive_type == cat, 1,
                  ifelse(gen3$genitive_type == ref, 0, NA))
  ## Remove rows belonging to other categories
  keep <- !is.na(y_bin)
  roc_obj <- roc(y_bin[keep], probs[keep, cat])
  auc(roc_obj)
})
C_values
## Without random effects, C-value of OF is o.8018, and for S is 0.70. The inner binomial models do not perform well with such low C-values.
summary(gen3$genitive_type)
## there are only 106 s-genitives and 470 of-genitives, while 2351 of-genitives. The data is pretty sparse. Bayesian hierarchical multinomial model would be more efficient

## Bayesian multinomial regression, family=categorical(), with default priors
install.packages("brms")
library(brms)
brm3<- brm(
  gen3_f2,
  data = gen3,
  family = categorical(),
  chains = 2,
  iter = 2000,
  cores = 2,
  seed = 123
)
summary(brm3)
prior_summary(brm3)
plot(brm3)

## The simple brm_multinomial model is not 

## Trying to model groups as random effects, using bayesian hierarchical multinomial logit regression
gen3_f3<-genitive_type ~ por_length_words + pum_length_words +animacy + 
  alpha_persistence_S + alpha_persistence_OF + alpha_persistence_NN + beta_persistence_S + 
  beta_persistence_OF + beta_persistence_NN + TTR + por_thematicity_ptw + pum_thematicity_ptw + 
  final_sibilancy + time+(1|register)
gen3_f3
scale(gen3$por_length_words)
scale(gen3$pum_length_words)
scale(gen3$TTR)
scale(gen3$time)

priors <- c(
  # Intercepts
  set_prior("normal(0, 5)", class = "Intercept", dpar = "muOF"),
  set_prior("normal(0, 5)", class = "Intercept", dpar = "muS"),
  
  # Slopes
  set_prior("normal(0, 2)", class = "b", dpar = "muOF"),
  set_prior("normal(0, 2)", class = "b", dpar = "muS"),
  
  # Random-effect SDs
  set_prior("student_t(3, 0, 1)", class = "sd", dpar = "muOF"),
  set_prior("student_t(3, 0, 1)", class = "sd", dpar = "muS")
)


brm3_random <- brm(
  gen3_f3,
  data   = gen3,
  family = categorical(),
  prior  = priors,
  chains = 2,
  iter   = 4000,
  warmup = 1000,
  cores  = 2,
  seed   = 123,
  control = list(adapt_delta = 0.95)
)

summary(brm3_random)
prior_summary(brm3_random)
plot(brm3_random)
