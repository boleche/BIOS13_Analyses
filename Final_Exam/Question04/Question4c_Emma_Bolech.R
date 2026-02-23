
# Function with N0 as input that plots the risk of extinction within 100 generations by calling on
# the "extinction" function. Allows for multiple N0 entries.

install.packages("ggplot2")  # install necessary ggplot package for ggplot2 plotting
library(ggplot2)


# Same function as in Question4a
population <- function(N0, tmax) {   # opening function with N0 and tmax input
  total_pop <- N0     # initiating total_pop vec with initial N0
  for (t in 1:tmax) {     # loop through each generation
    total_pop <- rpois(total_pop, 1)    # create a poisson dist with a mean of 1 for the current pop size 

    total_pop <- sum(total_pop)     # sum up the poisson dist and replace it as the current_pop size
  }

  return(total_pop)  # return this summed total_pop size (this should be the ending pop size)
  
}


# Same function defined for Question4b
extinction <- function(N0, tmax) {  # opening function with N0 and tmax input
  extinction_vec <- numeric(1000)   # opening an empty vector with 1000 entries (initially all 0)
  for (i in 1:1000) {     # loop through each simulation (1 through 1000)
    extinction_vec[i] <- population(N0, tmax)   # append the ending population size for each simulation in the vec
  }
  no_of_zeros <- sum(extinction_vec == 0)   # same the number of 0s in the extinction vector 
  prob_extinction <- (no_of_zeros/1000) *100  # calculate the probability of extinction out of 1000 simulations as a percentage

  return(prob_extinction)   # return this probability 

}

# N0 list from exam question 
N0_ex <- seq(10, 100, by = 10)

# New function defined for Question4c
risk_100 <- function(N0) {  # opening function with N0 as input
  risk_df <- data.frame(   # opening an empty data frame 
    N0s = numeric(length(N0)), # defines N0s as the x (with the length of # of N0s from input)
    prob = numeric(length(N0))  # defines prob as the y (with the length of # of N0s from input)
  )

  for (i in seq_along(N0)) {   # loop through number of N0 entries
    risk_df$N0s[i] <- N0[i]   # append the N0 of current index as x column in df
    risk_df$prob[i] <- extinction(N0[i], 100)   # call on extinction prob function and append the result in the prob column (set at 100 generations)
  }

  plot <- ggplot(data = risk_df, aes(x = N0s, y = prob)) +   # use ggplot to plot the df with N0 as the x and percent extinction prob as y
    geom_line(color = "blue", linewidth = 2) +  # change color of line and make the width thicker
    scale_y_continuous(limits = c(100,0), # y - axis limits
                    breaks = seq(0,100, by = 10)) + # adjusting y axis spacing and tick marks
    scale_x_continuous(limits = c(100,10), # x - axis limits
                    breaks = seq(10,100, by = 10)) + # adjusting x axis spacing and tick marks
    labs(   # adding labels
    title = ("Probability of Extinction After 1000 Simulations for Varying\n Initial Populations with 100 Generations"),
    y = ("Probability of Extinction (%)"),
    x = ("Initial Population Size")
    ) +
    theme( # adjusting aesthetics 
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey90"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 14, face = "bold"), 
      axis.title.y = element_text(size = 14, face = "bold"), 
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"))

  return(plot)  # return the plot

}

#ex. for starting population sizes defined as list in exam and 100 generations as hard coded in risk_100 function

print(risk_100(N0_ex))


