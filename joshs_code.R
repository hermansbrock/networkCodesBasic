#Title: Black and Ross' code
#Author: Dr Andrew Black and Dr Joshua Ross (adapted for R by Brock Hermans)
#
# Description:
#   This function accepts and input for a value of beta and the population size.
#   It calculates the final epidemic size distribution, assuming that one of the N individuals is infective and the rest are susceptible.
#   The input Beta, is actually beta/gamma (set gamma=1)
#
#
joshs_code <- function(Beta,N){
  #
  if (N<=1) {stop("population size must be greater than 1")}
  #
  #
  beta  <- Beta/(N-1)
  q <- t(rep(0,N+1))
  q[2] <- 1
  for (Z2 in 0:(N-2)){ #Stop at N-2 instead of N
    for (Z1 in ((Z2+1):(N-1))){
      p1 <- 1/(1+1/(beta*(N-Z1)))
      q[Z1+2]=q[Z1+2]+q[Z1+1]*p1
      q[Z1+1]=q[Z1+1]*(1-p1)
    }
  }
  return(q) #Final epidemic size distribution
  plot(0:N,q,type="s") #Plot the distribution
}