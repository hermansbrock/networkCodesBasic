#Title: MLE for Ne
#Author: Brock Hermans

# Description:
#   This code mirrors the code 'mle.beta.R', except it takes the input of a vector of Ne 
#   values and outputs the MLE for beta.
mle.beta.ne <- function(Ne,N){
  Beta <- seq(1,7,0.1)
  lik <- c()
  for (j in Beta){
    pNe <- joshs_code(j,N)
    lik <- c(lik, prod(pNe[Ne+1]))
  }
  return(Beta[which(lik==max(lik))])
}