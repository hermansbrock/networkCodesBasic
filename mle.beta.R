#Title: MLE for beta
#Author: Brock Hermans
#
# Description:
#   This code takes a value of beta and then simulates 10 epidemics. 
#   The code then uses 'joshs_code.R' to find the value of beta that maximises the joint likelihood of the 10 final epidemic size distributions
#
mle.beta <- function(beta,N,net=FALSE,network=0){
  if (N<=1) {stop("The population size must be greater than 1")}
  #
  # SIR model:
  if (net==FALSE){
    n <- 10
    a <- matrix(c(rep(N-1,n),rep(1,n),rep(beta,n)),byrow=FALSE,ncol=3)
    k <- apply(a,1,sir_cts)
  }
  #
  # Network model:
  else {
    k <- c()
    for (i in 1:10){
      k <- c(k,disease_simulation(network,beta))      
    }
  }
  #
  #
  Beta <- seq(1,7,0.1)
  lik <- c()
  for (j in Beta){
    pNe <- joshs_code(j,N)
    lik <- c(lik, prod(pNe[k+1]))
  }
  return(Beta[which(lik==max(lik))])
}