########################################################################################################
#### MODEL FITTING AND MODEL SELECTION FOR MODELLING THE TRANSPORT OF ALIEN AMPHIBIANS IN AUSTRALIA ####
########################################################################################################


### Fit a set of 8 candidate logistic including different covariates
### Load the lme4 library to conduct the GLMMs

library(lme4)

fit1<-glmer(Transport~(1|Family)+area+long, data=transport, family = binomial)
fit2<-glmer(Transport~(1|Family)+long, data=transport, family = binomial)
fit3<-glmer(Transport~(1|Family)+lat+long, data=transport, family = binomial)
fit4<-glmer(Transport~(1|Family)+area+status, data=transport, family = binomial)
fit5<-glmer(Transport~(1|Family)+area+lat, data=transport, family = binomial)
fit6<-glmer(Transport~(1|Family)+area, data=transport, family = binomial)
fit7<-glmer(Transport~(1|Family)+lat, data=transport, family = binomial)
fit8<-glmer(Transport~(1|Family)+status, data=transport, family = binomial)

### Load the AICcmodavg library to conduct the model selection procedures
library(AICcmodavg)


candidate<-list(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8)	### Create a list with the 8 candidate models
name<-c("fit1", "fit2", "fit3", "fit4", "fit5", "fit6", "fit7","fit8")

aic<-aictab(cand.set=candidate, modnames=name, sort = TRUE, second.ord = TRUE)	### Conduct the model selection procedures
