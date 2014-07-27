#Title: Erdos-Renyi network
#Author: Brock Hermans
#
# Description:
#   This code generates an Erdos-Renyi network by starting with a fully connected network and then
#   Removing edges at random to get only percent of the original edges
partial_network <- function(n,percent){
  network <- network_simulate(n,1,1,fc=TRUE)
  if (percent!=0){
    num <- round(dim(network$edges)[1]/2*(1-percent))
    ind <- sample(seq(1,dim(network$edges)[1],2),num,replace=FALSE)
    for (k in 1:length(ind)){
      ele <- network$edges[ind[k],]
      network$A[ele[1],ele[2]] <- 0
      network$A[ele[2],ele[1]] <- 0
    }
    network$edges <- network$edges[-c(ind,ind+1),]
  }
  return(network)
}