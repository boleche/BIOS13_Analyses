# solving for the c* equilibrium for an arbitrary set of parameters to double check for stability

Kmax <-  0.2     # maximal uptake rate for a single cell 
kn <-  0.5  # half saturation constant
n <-  50   # denisty of cells per unit volume

I <-  100    # constant inflow
mu <- 0.6   # outflow 

kinetics_chemostat <- function(c) {
  I - mu*c + (-(Kmax*c)/(kn + c) * n) # nutrient uptake function (takes in time, nutrient [], and constants)

}

c_equil <- uniroot(kinetics_chemostat, interval = c(0, 1000))  # using uniroot to find the "root" of the original chemostat kinetics equation (where it == 0)

c_equil <- c_equil$root  # saving the root numeric portion of uniroots output 



kinetics_chemostat_deriv <- function(c) {   # defining a new function with the derivitive of our originial chemostat kinetics equation (solved for on paper)
  -mu - ((Kmax*n*kn)/(kn + c)^2)
}

f_prime_c_star <- round(kinetics_chemostat_deriv(c = c_equil), 4)  # solving and saving the result as a rounded decimal 


print(paste("This is the f'(c*) result: ", f_prime_c_star))  #  printing the result of f'(c*) --> one can see this is negative even when parameters are changed within limits
