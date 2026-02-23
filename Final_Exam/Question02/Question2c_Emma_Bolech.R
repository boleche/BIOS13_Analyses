install.packages("ggplot2") # installing ggplot2
library(ggplot2) # loading in ggplot2

theta <- seq(0.2, 3, by = 0.2) # seq of numbers by 0.2 from 0.2 - 3
r <- 1 # setting an arbitrary initial growth rate of 1
K <- 100 # setting an arbitrary carrying capacity of 100
n <- seq(0, 150) # setting up arbitrary population sizes of 0-150

growth <- function(n, theta) {   # growth rate function as defined in the exam to call on for calculating growth rate
  ((r*n)*(1-((n/K)^theta)))
}

plot <- ggplot(data.frame(n = n), aes(x = n)) +   # opening plot with population sizes between 0-150 on the x-axis
  geom_hline(yintercept = 0, color = "red") + # adding a blue line which shows where the growth rate is 0 (equillibriums)
  scale_y_continuous(limits = c(100,-400), # y - axis limits
                    breaks = seq(-400,100, by = 100)) + # adjusting y axis spacing and tick marks
  labs(   # adding labels
    title = "Growth Rate vs. Population Size\nFor Different Theta Values",
    y = ("Growth Rate (dn/dt)"),
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
  df <- data.frame(n = n, growth = growth(n, th), Theta = th) # create a data frame that stores the population size and growth rate at the current theta value
  plot <- plot + geom_line(data = df, aes(x = n, y = growth, color = Theta)) # add this line (pop size vs. growth rate) to the existing plot

}

print(plot) # generate the plot

