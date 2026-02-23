install.packages("ggplot2") # installing ggplot2
library(ggplot2) # loading in ggplot2

theta <- seq(0.2, 3, by = 0.2) # seq of numbers by 0.2 from 0.2 - 3
r <- 1 # setting an arbitrary initial growth rate of 1
K <- 100 # setting an arbitrary carrying capacity of 100
n <- seq(1, 150) # setting up arbitrary population sizes of 1-150 (starting at 1 to avoid error when dividing by n ( cannot divide by 0))

per_cap_growth <- function(n, theta) {   # growth rate function as defined in the exam to call on for calculating growth rate
  (((r*n)*(1-((n/K)^theta))))/(n)
}

plot <- ggplot(data.frame(n = n), aes(x = n)) +   # opening plot with population sizes between 0-150 on the x-axis
  geom_hline(yintercept = 0, color = "red") + # adding a blue line which shows where the per capita growth rate is 0 (equillibriums)
  scale_y_continuous(limits = c(1.5,-3), # y - axis -300 to 100 limits
                    breaks = seq(-3,1.5, by = 1)) + # adjusting y axis spacing and tick marks
  labs(
    title = "Per Capita Growth Rate vs. Population Size\nFor Different Theta Values",
    y = ("Per Capita Growth Rate ((dn/dt)/n)"),
    x = ("Population Size (n)")
  )+
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

for (i in seq_along(theta)) {   # initiate a for loop that loops through the index of 10 random theta values
  th <- theta[i] # save the theta value at index i 
  df <- data.frame(n = n, growth = per_cap_growth(n, th), theta = th) # create a data frame that stores the population size and per capita growth rate at the current theta value (also add the current theta as a column for coloring)
  plot <- plot + geom_line(data = df, aes(x = n, y = growth, color = theta)) # add this line (pop size vs. growth rate) to the existing plot, color by theta column

}

print(plot) # generate the plot

