# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  
  has.medicine<-matrix(nrow=n.doctors,ncol=n.days)
  has.medicine[,1]<-initial.doctors
  for (i in 1:(n.days-1)){
    meet.index<-sample(n.doctors,size=2)
    meet.medicines<-has.medicine[meet.index,i]
    has.medicine[,i+1]<-has.medicine[,i]
    if (sum(meet.medicines)==1&&runif(1)<p){
      has.medicine[meet.index,i+1]=1
    }
  }
  return (has.medicine)

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output

}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
initial.doctors=sample(c(0,1),10,replace=TRUE,prob=c(0.9,0.1))
n.doctors=length(initial.doctors)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

plotda<-data.frame(p=rep(c(0.1,0.3,0.5,0.7,0.9),each=15),
                   days=rep(1:15,times=10),
                   n.doctors=c(colSums(simA),colSums(simB),colSums(simC),colSums(simD),colSums(simE)))
ggplot(plotda)+geom_step(aes(x=days,y=n.doctors,col=factor(p)))
