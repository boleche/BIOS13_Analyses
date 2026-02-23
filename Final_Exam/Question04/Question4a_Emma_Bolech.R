
# Function with initial pop size (N0) and max generations (tmax) which returns
# the number of individuals in the population after tmax generations.


population <- function(N0, tmax) {   # opening function with N0 and tmax input
  total_pop <- N0     # initiating total_pop vec with initial N0
  for (t in 1:tmax) {     # loop through each generation
    total_pop <- rpois(total_pop, 1)    # create a poisson dist with a mean of 1 for the current pop size 

    total_pop <- sum(total_pop)     # sum up the poisson dist and replace it as the current_pop size
  }

  return(total_pop)  # return this summed total_pop size (this should be the ending pop size)
  
}


#ex. for starting population size of 10 and 10 generations

print(paste("The final population size is", population(10, 10)))
