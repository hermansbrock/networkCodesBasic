#Title: Waxman summary statistics
#Author: Brock Hermans
#
# Description:
#   This function accepts an input network and a value for beta. It outputs multiple
#   summary statistics for the network including:
#     - Number of nodes
#     - Value for beta
#     - Percentage of possible edges in the network  
#     - Value for \hat{\beta} using 'joshs_code.R'
#     - Number of nodes that the infective is connected to
#     - Mean degree for the nodes
#     - Average size of the subgraphs
#     - Number of subgraphs
#
#
waxmanNetworkSummaryStatistics <- function(net,beta){
  networkSummary <- list()
  networkSummary$N <- net$N
  networkSummary$beta <- beta
  networkSummary$percent <- dim(net$edges)[1]/(net$N*(net$N-1))
  #
  #
  networkp <- getSubGraphs(net)
  #
  #
  networkSummary$beta.hat <- mle.beta(beta,net$N,net=TRUE,network=networkp) #MLE for beta using Black and Ross' method
  networkSummary$infec.con <- sum(networkp$A[which(networkp$points[,3]==1),]) #Number of nodes the infected node is connected to
  networkSummary$mean.con <- mean(rowSums(networkp$A)) #Average number of connections across all nodes
  #
  #
  networkSummary$sub.size <- mean(table(networkp$graph[,2])) #Average size of the subgraphs
  networkSummary$sub.no <- max(networkp$graph[,2]) #Number of subgraphs  
  #
  #
  class(networkSummary) <- "networkSummary"
  return(networkSummary)
}