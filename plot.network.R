#Title: Plotting networks
#Author: Brock Hermans
#
# Description:
#   This plot function plots network objects. If the network input has the subgraphs identified
#   then it plots the subgraphs with different colours.
#
plot.network <- function(x,size=4,...) {
  require(ggplot2)  
  df = x$points
  df$s1[df$s1==0] = "Susceptible"
  df$s1[df$s1==2] = "Recovered"
  df$s1[df$s1==1] = "Infected"
  from <- df[x$edges[,1],]
  to <- df[x$edges[,2],]
  segments = data.frame(x1=from$x,y1=from$y,x2 = to$x,y2=to$y)
  p = ggplot(aes(x,y,col=s1),data=df)
  #
  # If we have subgraphs identified, plot the network with subgraphs identified
  if(!is.null(x$graph)){
    segments$graph = factor(x$graph[x$edges[,1],2])
    p = p + 
      geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2,col=graph),data=segments) +
      labs(col="Subgraphs")
  }
  #
  # If we don't know the subgraphs, do a simple plot
  else {p <- p +
          geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2),data=segments,col="black") +
          labs(col="Status")}
  p <- p + geom_point(size=size) + theme_bw()
  return(p)  
}