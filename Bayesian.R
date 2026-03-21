##brm2
library(brms)
library(bayesplot)
brm2<-brm(gen2_f,
          data=gen2,
          family=bernoulli(link="logit"), ## specifying the binary logistic regression modeling
          warmup=500, ## the warmup iterations per chain, which is not used for inference 
          iter=2000, ## real number of iterations per chain, including burn-in
          chains=2, ## 2 MCMC chains
          cores=2, ## 2 CPU cores used (the same as the number of chains)
          seed=123) ## the random seeds for reproducibility
summary(brm2)
prior_summary(brm2)
saveRDS(brm2, "brm2.rds")

#brm3
## Bayesian multinomial regression, family=categorical(), with default priors
library(brms)
brm3<- brm(
  gen3_f1,
  data = gen3,
  family = categorical(),
  chains = 2,
  iter = 2000,
  cores = 2,
  seed = 123
)
summary(brm3)
saveRDS(brm3, "brm3.rds")
prior_summary(brm3)

## Bayesian multinom with random effect
gen3_f2<-genitive_type ~ por_length_words + pum_length_words +animacy + TTR + 
  final_sibilancy + time
gen3_f2

priors <- c(
  # Intercepts
  set_prior("normal(0.8, 1)", class = "Intercept", dpar = "muOF"),
  set_prior("normal(-0.7, 1)", class = "Intercept", dpar = "muS"),
  
  # Slopes
  set_prior("normal(0, 1)", class = "b", dpar = "muOF"),
  set_prior("normal(0, 1)", class = "b", dpar = "muS")
)


brm3_1 <- brm(
  gen3_f2,
  data   = gen3,
  prior= priors,
  family = categorical(),
  chains = 2,
  iter   = 4000,
  warmup = 1000,
  cores  = 2,
  seed   = 123
)
summary(brm3_1)
saveRDS(brm3_1, "brm3_1.rds")


##adding random effects-register
gen3_f3<-genitive_type ~ por_length_words + pum_length_words +animacy + TTR + 
  final_sibilancy + time + (1|register)
gen3_f3

##setting tighter priors
priors <- c(
  # Intercepts
  set_prior("normal(0.8, 1)", class = "Intercept", dpar = "muOF"),
  set_prior("normal(-0.7, 1)", class = "Intercept", dpar = "muS"),
  
  # Slopes
  set_prior("normal(0, 1)", class = "b", dpar = "muOF"),
  set_prior("normal(0, 1)", class = "b", dpar = "muS"),
  
  # Random-effect SDs
  set_prior("student_t(3, 0, 1)", class = "sd", dpar = "muOF"),
  set_prior("student_t(3, 0, 1)", class = "sd", dpar = "muS")
)


brm3_random_r <- brm(
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
summary(brm3_random_r)
saveRDS(brm3_random_r, "brm3_random_r.rds")

##adding random effects- filename
gen3$filename<-as.factor(gen3$filename)
gen3_f4<-genitive_type ~ por_length_words + pum_length_words +animacy + TTR + 
  final_sibilancy + time + (1|filename)
gen3_f4

brm3_random_f <- brm(
  gen3_f4,
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
summary(brm3_random_f)
saveRDS(brm3_random_f, "brm3_random_f.rds")

##adding random effects-possessor head noun
gen3$por_head_noun<-as.factor(gen3$por_head_noun)
gen3_f5<-genitive_type ~ por_length_words + pum_length_words +animacy + TTR + 
  final_sibilancy + time + (1|por_head_noun)
gen3_f5

brm3_random_pr <- brm(
  gen3_f5,
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

summary(brm3_random_pr)
saveRDS(brm3_random_pr, "brm3_random_pr.rds")

##adding random effects-possessum head noun
gen3$pum_head_noun<-as.factor(gen3$pum_head_noun)
gen3_f6<-genitive_type ~ por_length_words + pum_length_words +animacy + TTR + 
  final_sibilancy + time + (1|pum_head_noun)
gen3_f6
brm3_random_pm <- brm(
  gen3_f6,
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

summary(brm3_random_pm)
saveRDS(brm3_random_pm, "brm3_random_pm.rds")

#adding random effects-possessum +possessor head noun
gen3_f7<-genitive_type ~ por_length_words + pum_length_words +animacy + TTR + 
  final_sibilancy + time + (1|pum_head_noun)+ (1|por_head_noun)
gen3_f7

brm3_random_head <- brm(
  gen3_f7,
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

summary(brm3_random_head)
saveRDS(brm3_random_head, "brm3_random_head.rds")

#adding random effects-possessum +possessor head noun +register
gen3_f8<-genitive_type ~ por_length_words + pum_length_words +animacy + TTR + 
  final_sibilancy + time + (1|pum_head_noun)+ (1|por_head_noun) +(1|register)
gen3_f8

brm3_random_head_r <- brm(
  gen3_f8,
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

summary(brm3_random_head_r)
saveRDS(brm3_random_head_r, "brm3_random_head_r.rds")

#adding random effects-possessum +possessor head noun +register+filename
gen3_f9<-genitive_type ~ por_length_words + pum_length_words +animacy + TTR + 
  final_sibilancy + time + (1|pum_head_noun)+ (1|por_head_noun) +(1|register)+(1|filename)
gen3_f9

brm3_random4 <- brm(
  gen3_f9,
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

summary(brm3_random4)
## there is 1 divergent transition after warmup
