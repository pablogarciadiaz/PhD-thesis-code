#####################################################################################################################################
#### BAYESIAN REGULARISED POISSON-BINOMIAL MODEL FOR MODELLING THE ABUNDANCE AND DETECTION OF ALIEN REPTILES IN cHRISTMAS ISLAND ####
#####################################################################################################################################

model{

	### Prior distributions for the abundance part of the model

	alpha.ab~dnorm(0, 0.1)			### Normal prior distribution for the abundance intercept
	
	### Double exponential (Laplace) priors for the slopes of the abundance model - for running the Bayesian regularisation approach

	for(i in 1:n.covab){			### n.covab: number of abundance covariates - 10 in our model	
		beta.ab[i]~ddexp(0, s.ab)	### Prior for the slopes in the model; use of a double exponential (or Laplace) distribution for obtaining a regularised model
	}

	s.ab~dunif(0.1, 5)			### Prior for the scale parameter of the double exponential distribution


	### Prior distributions for the detection part of the model	


	for(i in 1:n.day){
	alpha.det[i]~dnorm(0, tau.det)		#### Normal prior distribution for the abundance intercepts; n.days
	}

	tau.det<-1/sd.det			### Precision of the detection intercepts

	sd.det~dunif(1, 100)			### Uniform prior distribution for the variance of the detection intercepts

	
	for (i in 1:n.covdet){			### n.covdet: number of covariates in the detection model, n = 11
		beta.det[i]~ddexp(0, s.det)
	}
		

	s.det~dunif(0.1, 5)			### Prior for the scale parameter of the double exponential distribution		



	### Likelihood for the species abundance

	for (i in 1:n.sites){			### n.sites: number of surveying sites; n = 34
	
	log(mu[i])<-alpha.ab+beta.ab[1]*dist[i]+beta.ab[2]*tree[i]+beta.ab[3]*bush1[i]+beta.ab[4]*bush2[i]+beta.ab[5]*grass1[i]+
			beta.ab[6]*grass2[i]+beta.ab[7]*rocks[i]+beta.ab[8]*human[i]+beta.ab[9]*length[i]+beta.ab[10]*elev[i]

	abundance[i]~dpois(mu[i])		### Poisson distribution of abundance

	}

	### Likelihood of the species detection

	for (i in 1:sample.size){	### sample.size: number of sites surveyed for alien reptiles by number of repeated survey occasions (n = 102)

		logit(p[i])<-alpha.det[day[i]]+beta.det[1]*tground[i]+beta.det[2]*t10[i]+beta.det[3]*tunder[i]+beta.det[4]*tair[i]+beta.det[5]*tree[site[i]]+beta.det[6]*bush1[site[i]]+
			beta.det[7]*bush2[site[i]]+beta.det[8]*grass1[site[i]]+beta.det[9]*grass2[site[i]]+beta.det[10]*rocks[site[i]]+beta.det[11]*human[site[i]]
		
		
		count[i]~dbin(p[i], abundance[site[i]])		### Binomial distribution of the number of individuals recorded in each site during each repeated survey occasion. 


	}


}