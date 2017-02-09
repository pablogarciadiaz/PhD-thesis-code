#############################################################################################################
#### BAYESIAN REGULARISED POISSON-LOG REGRESSION FOR MODELLING ALIEN FISH SPECIES RICHNESS IN AUSTRALIA ####
#############################################################################################################


model{

	

	##### Defining prior distributions
	
	### Topographic division-specific intercepts

	alpha.dv[1:n.division]~dmnorm(alphac.mean[], tau.dv[,])		#### n.division: number of topographic divisions, n = 11

	for (j in 1:n.division){
	alphac.mean[j]<-0

	}

	tau.dv[1:n.division, 1:n.division]~dwish(S3.dv, n.division+1)	#### Precision matrix estimated using a Wishart prior with degress of  freedom = n.division+1




	
	for (j in 1:n.cov){		#### n.cov: number of covariates, n = 8
		beta[j]~ddexp(0, b)	#### Prior for the slopes in the model; use of a double exponential (or Laplace) distribution for obtaining a regularised model
	}

	b~dunif(0.1, 5)	#### Prior for the scale parameter of the double exponential distribution

	


	#### Loop containing the model likelihood ####



	for (i in 1:sample.size){		### sample.size = number of drainages, n = 141

		#### Alien fish richness modelled as a function of the 8 covariates using a Poisson-log regression ####
	

		log(lambda[i])<-alpha.dr+beta[1]*area[i]+beta[2]*HFI[i]+beta[3]*flength[i]+beta[4]*facc[i]+beta[5]*up.slope[i]+beta[6]*up.elev[i]+
		beta[7]*min.temp[i]+beta[8]*max.temp[i]		
		
		richness[i]~dpois(p[i], r)

	}

}

