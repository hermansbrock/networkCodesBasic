#Title: Network SIR simulation
#Author: Brock Hermans
#
# Description:
#   This code simulates an epidemic on a given network, x, for an input infection rate, \beta.
#   It outputs the final epidemic size, but keeps track of the transitions and times of the transitions.
#   The times of the transitions are based on a continuous time model.
disease_simulation <- function(x,beta,gamma=1,...) {
  j=0 #Keep track of the number of transitions
  #
  #
  N <- x$N 
  while (sum(x$points[,3]==1) != 0) { #i.e. while we still have infectives
    j=j+1
    #
    # Find the susceptible, infective and recovered nodes:
    susc <- which(x$points[,3]==0,arr.ind=TRUE) 
    infec <- which(x$points[,3]==1,arr.ind=TRUE)
    recov <- which(x$points[,3]==2,arr.ind=TRUE)
    #
    #
    #Find the susceptible nodes connected to infective nodes:
    p <- x$edges[which(x$edges[,1] %in% infec),]
    if (is.null(p)){
      episize <- sum(x$points[,3]==2)+sum(x$points[,3]==1)
      break
    }
    if (is.null(dim(p))){
      p <- matrix(p,ncol=2)
    }
    p <- p[which(p[,2] %in% susc),2]
    #
    #
    temp <- length(p)
    #Add to the vector of susceptible nodes the infective nodes. This now gives us a 
    # vector of possible transitions.
    p <- c(infec,p)
    #
    #
    t <- rexp(1,length(infec)*gamma+temp*beta) #Time to next transition
    #
    #Simulate the next transition
    if (length(p)==1){
      transition <- p
    } else {
      transition <- sample(x=p,size=1,prob=c(rep(gamma,length(infec)),rep(beta/(dim(x$points)[1]-1),temp)))
    }
    x$points[transition,3] <- x$points[transition,3]+1
    #
    #
    episize <- sum(x$points[,3]==2) #Current size of the epidemic
  }
  #
  # Return the final epidemic size when we have no more infectives.
  return(episize)
  }


