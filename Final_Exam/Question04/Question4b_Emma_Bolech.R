
# Function with N0 and tmax as input that calls "population" function and repeats the simulation
# 1000 times to calculate the risk of extinction. Outputs a probability of extinction
# as a percentage.

# Same function as in Question4a
population <- function(N0, tmax) {   # opening function with N0 and tmax input
  total_pop <- N0     # initiating total_pop vec with initial N0
  for (t in 1:tmax) {     # loop through each generation
    total_pop <- rpois(total_pop, 1)    # create a poisson dist with a mean of 1 for the current pop size 

    total_pop <- sum(total_pop)     # sum up the poisson dist and replace it as the current_pop size
  }

  return(total_pop)  # return this summed total_pop size (this should be the ending pop size)
  
}

# New function defined for Question4b
extinction <- function(N0, tmax) {  # opening function with N0 and tmax input
  extinction_vec <- numeric(1000)   # opening an empty vector with 1000 entries (initially all 0)
  for (i in 1:1000) {     # loop through each simulation (1 through 1000)
    extinction_vec[i] <- population(N0, tmax)   # append the ending population size for each simulation in the vec
  }
  no_of_zeros <- sum(extinction_vec == 0)   # same the number of 0s in the extinction vector 
  prob_extinction <- (no_of_zeros/1000) *100  # calculate the probability of extinction out of 1000 simulations as a percentage

  return(prob_extinction)   # return this probability 

}

#ex. for starting population size of 10 and 10 generations

print(paste("The probability of extinction after 1000 simulations is", extinction(10, 10), "%"))


