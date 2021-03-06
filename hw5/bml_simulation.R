#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

bml.run<-function (r,c,p,d,n){
  
  results=c()
  for (q in 1:d){
    m=bml.init(r,c,p)
    x=(bml.step(m))
    for (i in 1:n){
      f=x
      x=bml.step(x)
      if (all.equal(f,x)==TRUE){
        results=c(results,i)
        break
     }
      if (i==n){
        results=c(results,"Free Flow")
      }
    }
  }
  return (list(c(results), mean(results)))
  
}
