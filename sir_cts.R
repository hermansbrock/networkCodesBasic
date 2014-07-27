#Title: SIR simulation (continuous time)
#Author: Brock Hermans
#
# Description:
#   This function takes inputs for the initial number of infected people, initial number
#   of susceptibles, and rates beta and gamma which govern the rate at which we move
#   from one state to the other.
#
sir_cts <- function(y,gamma=1) {
  if (length(y)!=3) { stop( "y must contain the number of susceptibles, infectives, and beta rate")}
  N <- y[1]+y[2]
  S <- c(y[1]) #Keep track of the susceptibles
  I <- c(y[2]) #Keep track of the infectives
  R <- c(0) #Keep track of those recovered
  T <- c(0) #Keep track of the times at which a transition is made
  ind <- 1 #An indicator variable that keeps track of how many transitions have occured
  while (I[ind]!=0) {
    t.star <- rexp(1,(y[3]*S[ind]*I[ind]/(N-1))+gamma*I[ind]) #Generate a time
    T <- c(T,T[ind]+t.star) #Update the time vector
    if (S[ind]==0){ #If we have no susceptibles, the only possible transition is an infective moves to the recovered group
      S <- c(S,S[ind])
      I <- c(I,I[ind]-1)
      R <- c(R,R[ind]+1)
      ind <- ind+1 }
    else {
      tran <- sample(x=c(0,1),size=1,replace=TRUE,prob=c(y[3]*S[ind]*I[ind]/N,gamma*I[ind]))
      if (tran==0){
        S <- c(S,S[ind]-1)
        I <- c(I,I[ind]+1)
        R <- c(R,R[ind])
        ind <- ind+1} 
      else {
        S <- c(S,S[ind])
        I <- c(I,I[ind]-1) 
        R <- c(R,R[ind]+1) 
        ind <- ind+1}}
  }
  return(R[length(R)])
}