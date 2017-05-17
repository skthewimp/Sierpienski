# code to generate sierpienski gaskets of different shapes based on 
# the method shown in this youtube video: https://www.youtube.com/watch?v=kbKtFN71Lfs
# the code prints the gasket

sierpienski <- function(xs, # x coordinates of vertices of original polygon
                     ys, # y coordinates of vertices of original polygon 
                     xstart, # x coordinate of starting point from which we start simulation
                     ystart, # y coordinate of starting point from which we start simulation
                     distcover = 0.5, # when the point moves towards a vertex, what proportion of the distance is covered in a single move?
                     numstep = 1000 # number of points to generate in the simulation
                     )
{
  if(length(xs) != length(ys)) return(NA)
  probs <- rep((1/length(xs)),length(xs))
  xvec <- c(xstart)
  yvec <- c(ystart)
  for(i in 1:numstep)
  {
    chosenvert <- sample(1:length(xs),1,prob=probs)[1]
    xstart <- distcover * xs[chosenvert] + (1-distcover)*xstart
    ystart <- distcover * ys[chosenvert] + (1-distcover)*ystart
    xvec <- c(xvec,xstart)
    yvec <- c(yvec,ystart)
  }
  print(qplot(xvec,yvec) + theme_bw(24) + scale_x_continuous('',breaks=c()) + scale_y_continuous('',breaks=c()))
}