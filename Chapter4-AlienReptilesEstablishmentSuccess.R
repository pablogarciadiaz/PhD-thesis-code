##########################################################################################################################
##### BAYESIAN REGULARISED LOGISTIC REGRESION FOR MODELLING TH ESTABLISHMENT SUCCESS OF ALIEN REPTILES IN AUSTRALIA ######
##########################################################################################################################


model{
	
	#### Non-informative prior distributions for the Order, origin, and region-specific intercepts


	#### Prior multivariate normal distribution for the Order-specific intercept

	alpha.or[1:n.order]~dmnorm(alphaor.mean[], tau.order[,])		#### n.order: number of reptile orders (testudines and squamata). 

	for (j in 1:n.order){
		alphaor.mean[j]<-0
		}

	tau.order[1:2,1:2]~dwish(S3.order[,],2)					### Wishart distribution with two degrees of freedom

	sigma.order[1:2,1:2]<-inverse(tau.order[,])				### Variance-covariance matrix for the Order-specific intercept

	corr.order<-sigma.order[1,2]/(sqrt(sigma.order[1,1])*sqrt(sigma.order[2,2]))	### Correlation matrix for the Order-specific intercept



	#### Prior multivariate normal distribution for the origin-specific intercept

	alpha.g[1:n.origin]~dmnorm(alphag.mean[], tau.origin[,])		### n.origin: number of different origins for introduced reptiles (alien and domestic exotics)

	for (j in 1:n.origin){
		alphag.mean[j]<-0
		}

	tau.origin[1:2,1:2]~dwish(S3.origin[,],2)				### Wishart distribution with two degrees of freedom				
	
	sigma.origin[1:2,1:2]<-inverse(tau.origin[,])				### Variance-covariance matrix for the origin-specific intercept


	corr.origin<-sigma.origin[1,2]/(sqrt(sigma.origin[1,1])*sqrt(sigma.origin[2,2]))	### Correlation matrix for the origin-specific intercept



	#### Prior multivariate normal distribution for the region-specific intercept

	alpha.is[1:n.geography]~dmnorm(alphais.mean[], tau.is[,])		### n.geography: number of different regions for introduced reptiles (mainland vs island)		

	for (j in 1:n.geography){
		alphais.mean[j]<-0
		}

	tau.is[1:2,1:2]~dwish(S3.is[,],2)					### Wishart distribution with two degrees of freedom
	
	sigma.is[1:2,1:2]<-inverse(tau.is[,])					### Variance-covariance matrix for the region-specific intercept

	corr.is<-sigma.is[1,2]/(sqrt(sigma.is[1,1])*sqrt(sigma.is[2,2]))	### Correlation matrix for the origin-specific intercept


	######## Double exponential (Laplace) priors for the slopes of the model - for running the Bayesian regularisation approach

	for (j in 1:n.covs){							### n.covs: number of covariates (six in our model)
     		 beta[j]~ddexp(0, b)
	}

	b~dunif(0.1, 2)

	######## Bayesian missing data imputation procedures for turtles (data points 1:13 in our database)

	for (i in 1:13){
		indepd[i]~dnorm(mean.turtles, tau.turtles)		### Modelling the preferred body temperature of turtles by means of a Normal distribution	
		indepdb[i]<-abs(indepd[i]-median.warm[i])		### Absolute thermal safety margin; median.warm: median average temperatures of the warmest month in the region
	}

	mean.turtles~dnorm(26.2, 0.1)					### 26.2: mean preferred body temperature of turtle species for those species for which we had data.

	tau.turtles<-1/var.turtles
	var.turtles<-sd.turtles*sd.turtles
	sd.turtles~dunif(1, 100)					### sd.turtles: set to be a uniform distribution bounded by 0 and 10, to allow the imputation procedures to explore a wide parameter space


	######## Bayesian missing data imputation procedures for squamates (data points 14:71 in our database)

	for (i in 14:71){
     		indepd[i]~dnorm(mean.sqs, tau.sqs)
     		indepdb[i]<-abs(indepd[i]-median.warm[i])
	 }


	mean.sqs~dnorm(31.7, 0.1)
	tau.sqs<-1/var.sqs
	var.sqs<-sd.sqs*sd.sqs
	sd.sqs~dunif(1, 100)


	#### Likelihood for the complete Bayesian regularised logistic regression
	#### sample.size: 71 species-by-region data of introduced reptiles on islands and mainland Australia

	for (i in 1:sample.size) {							
  	logit(p[i])<-alpha.or[order[i]]+alpha.g[origin[i]]+alpha.is[geography[i]]+beta[1]*indepa[i]+beta[2]*indepb[i]
	+beta[3]*indepc[i]+beta[4]*indepdb[i]+beta[5]*indepe[i]+beta[6]*indepf[i]
  	estab[i]~dbern(p[i])  								### Bernoulli distribution - whether the species has established self-sustaining populations (1) or not (0)
  	}


}