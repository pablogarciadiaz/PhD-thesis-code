###################################################################################################
##### BAYESIAN MODEL SELECTION FOR MODELLING THE INTRODUCTION AND DETECTION OF ALIEN TURTLES ######
###################################################################################################



model{

#### Non-informative prior distributions

	alpha[1:n.genera]~dmnorm(mu[], tau)    ### Genus-specific intercepts; n.genera: number of genera; tau is a matrix indicating the precision of the distribution (1/ variance)

	for (j in 1:n.genus){			#### Prior for the mean of the Multivariate Normal Distribution (-1 for all the genera)
		mu[j]<-(-1)
		}

	tau<-inverse(mat)			### Inverse of the matrix
	mat<-sigma.phy*sigma.phy*Tree[,]        ### Tree: phylogenetic correlation matrix
	sigma.phy~dunif(0, 100)			

#### Loop for estimating the slopes and probabilities of inclusion of a covariate in the introduction component


	for (j in 1:parameter.introduction){     	### parameter.introduction: number of covariates
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



#### Loop for estimating the complete model (i.e., introduction and detection)

	for (i in 1:sample.size) {  
	logit(y.introduction[i])<-alpha[genus[i]]+inprod(indep[i,],beta)     		### Introduction; indep: a matrix containing the covariates for the introduction
	introduction[i]~dbern(y.introduction[i]) 					### Probability distribution of introduction
	logit(y.det[i])<-alpha.p + inprod(detc[i,], beta.pr)	      	 		### Detection; detc: a matrix containing the covariates for the detection
	det[i]<-y.det[i]*introduction[i]				       
	obs[i] ~ dbern(det[i])    				       			### Probability distribition of the records ?{0,1}

}


}
