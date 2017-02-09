###############################################################################################
##### BAYESIAN MODEL SELECTION FOR MODELLING THE ESTABLISHMENT SUCCESS OF SLIDER TURTLES ######
###############################################################################################



model{

#### Non-informative prior distributions

	alpha~dnorm(-1, 0.1)   			### Intercept of establishment

#### Loop for estimating the slopes and probabilities of inclusion of a covariate in the establishmentcomponent


	for (j in 1:parameter.establishment){     	### parameter.establishment: number of covariates
     	 ind[j]~dbern(0.5)           			### probability of inclusion of the covariates
     	 betaT[j]~dnorm(-1, 0.1)
     	 beta[j]<-ind[j]*betaT[j]	   
	}



	alpha.p ~ dnorm(-1, 0.1)	### Prior for the intercept of the detection component


#### Loop for estimating the slopes and probabilities of inclusion of a covariate in the dection component


	for (j in 1:parameter.p){   	 ### parameter.p: number of covariates
   	  indb[j]~dbern(0.5)   		### Probability of inclusion of a covariate
    	  betabT[j]~dnorm(-1, 0.1)
    	  beta.pr[j]<-indb[j]*betabT[j]
	}



#### Loop for estimating the complete model (establishment and detection)

	for (i in 1:sample.size) {  
	logit(y.establishment[i])<-alpha[genus[i]]+inprod(indep[i,],beta)     		### Establishment; indep: a matrix containing the covariates for the establishment
	establishment[i]~dbern(y.establishment[i]) 					### Probability distribution of establishment
	logit(y.det[i])<-alpha.p + inprod(detc[i,], beta.pr)	      	 		### Detection; detc: a matrix containing the covariates for the detection
	det[i]<-y.det[i]*establishmenh[i]				       
	obs[i] ~ dbern(det[i])    				       			### Probability distribition of the records {0,1}

}


}
