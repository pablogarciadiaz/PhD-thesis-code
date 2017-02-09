###########################################################################################################
#### MODEL FITTING AND MODEL SELECTION FOR MODELLING THE INTRODUCTION OF ALIEN AMPHIBIANS IN AUSTRALIA ####
###########################################################################################################


### Fit a set of 9 candidate logistic including different covariates


fit1<-glm(Introduccion~pathway, data=intro, family=binomial())
fit2<-glm(Introduccion~pathway+size, data=intro, family=binomial())
fit3<-glm(Introduccion~long, data=intro, family=binomial())
fit4<-glm(Introduccion~area+long, data=intro, family=binomial())
fit5<-glm(Introduccion~size, data=intro, family=binomial())
fit6<-glm(Introduccion~area, data=intro, family=binomial())
fit7<-glm(Introduccion~status, data=intro, family=binomial())
fit8<-glm(Introduccion~lat, data=intro, family=binomial())
fit9<-glm(Introduccion~are+size, data=intro, family=binomial())

### Load the AICcmodavg library to conduct the model selection procedures

library(AICcmodavg)

candidate<-list(fit1, fit2,fit3, fit4,fit5, fit6,fit7, fit8,fit9)		### Create a list with the 9 candidate models
name<-c("fit1", "fit2", "fit3", "fit4", "fit5","fit6", "fit7", "fit8","fit9")
aic<-aictab(cand.set=candidate, modnames=name, sort = TRUE, second.ord = TRUE)	### Conduct the model selection procedures