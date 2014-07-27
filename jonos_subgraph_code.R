#Title: Subgraph identification
#Author: Dr Jonathan Tuke
#
# Description:
#   This code takes a network and gets the subgraphs of that network

createList = function(N){
  lst = list()
  for(i in 1:N){
    lst[[i]] = i
  }
  return(lst)
}
findList = function(a,lst){
  return(which(sapply(lst,function(x){a %in% x }))) 
}
combineList = function(a,b,lst){
  lst[[a]] = c(lst[[a]],lst[[b]])
  lst[[b]] = NULL
  return(lst)
}
#
#
getSubGraphs = function(x){
  require(dplyr)
  if(class(x)!="network") stop("Need a network object")
  N = nrow(x$points)
  graph = createList(N)
  edges = x$edges
  for(i in 1:nrow(edges)){
    a = findList(edges[i,1],graph)
    b = findList(edges[i,2],graph)
    if(a != b){
      graph = combineList(a,b,graph)
    }
  }
  df = data.frame(node = 1:N, graph = 0)
  for(i in 1:length(graph)){
    df[graph[[i]],2] = i
  }
  x$graph = df
  return(x)
}