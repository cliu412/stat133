#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
m<-matrix(data=sample(0:2,r*c,replace=TRUE,prob=c(1-p,p/2,p/2)),nrow=r,ncol=c)
   return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){

  mstar<-matrix(c(m[,2:ncol(m)],m[,1]),nrow=nrow(m),ncol=ncol(m))
  stayingput.red<-2*(m==2)+1*(m==1&mstar!=0)
  movers.red<-1*(m==1&mstar==0)
  movers.red<-movers.red[,c(ncol(m),1:(ncol(m)-1))]
  mnew.red<-stayingput.red+movers.red
  mmoon<-mnew.red[c(nrow(mnew.red),1:(nrow(mnew.red)-1)),]
  stayingput.blue<-1*(mnew.red==1)+2*(mnew.red==2&mmoon!=0)
  movers.blue<-2*(m==2&mmoon==0)
  movers.blue<-movers.blue[c(2:nrow(m),1),]
  mnew.red.blue<-stayingput.blue+movers.blue
  return(mnew.red.blue)

}
#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  
  m=bml.init(r,c,p)
  x=(bml.step(m))
  for (i in 1:1000){
    n=x
    x=bml.step(x)
    if (all.equal(n,x)==TRUE){
      return (paste("GRIDLOCK!! at traffic step",i))
    }
  } 
  return ("Traffic went 1000 steps with no GRIDLOCK!")
}
