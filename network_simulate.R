network_simulate <- function(n,delta,alpha,fc=FALSE){
  network=list()
  x1 <- runif(n)
  y1 <- runif(n)
  s1 <- rep(0,n)
  s1[sample(1:n,size=1)] <- 1
  df <- data.frame(x=x1,y=y1,s1)
  network$points <- df
  
  L <- sqrt(2)
  network$N <- n

  A <- matrix(rep(0,n*n),ncol=n)
  #E <- For each pair of elements, see if there's a connection between it. 
  #If so, add it to the matrix
  
  E <- matrix(ncol=2)
  for (j in 1:(n-1)){
    for (k in (j+1):n){
      d.j.k <- sqrt(sum((df[j,1:2]-df[k,1:2])^2))
      p.edge <- delta*exp(-d.j.k/(L*alpha))
      if (fc==TRUE){
        p.edge=2}
      U <- runif(1,0,1)
      if (U<p.edge){
        E <- matrix(c(t(E),j,k,k,j),ncol=2,byrow=TRUE)
        A[j,k] <- 1
        A[k,j] <- 1

      }
    }
  }
  
  E <- E[-1,]
  
  network$edges = E
  network$A = A
  class(network) = "network"
  return(network)


 # d = dist(df)
  #U = matrix(runif(n*n),ncol=n)
  #d = as.matrix(d * beta * exp(-d/(sqrt(2)*alpha)))
  #A = 1 *(d <= U)
  #A[lower.tri(A)] = 0
  #A = A + t(A)
  #diag(A)=0
  #
  
}


print.network = function(x,...){
  cat("First 6 nodes\n\n")
  print(head(x$nodes))
  cat("First 6 edges\n\n")
  print(head(x$edges))
  print(head(x$graph))
}