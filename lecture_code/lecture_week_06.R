

# Demonstrate grid approximation of the posterior for a 
# globe tossing problem with 6 water observations out of 
# 9 globe tosses

#==========================================================


# 1) Define the grid to be used to compute the posterior

# 20 grid points, bounded by 0 and 1 (p_grid)
p_grid <- seq(0, 1, length.out = 20)



#==========================================================


# 2) Compute/define the value of the prior at each 
# parameter value on the grid

# In this case, simply choose a flat prior (prior)
prior <- rep(1, 20)

  

# Plot it with plot()
plot(p_grid, prior)


#==========================================================


# 3) Compute the likelihood at each parameter value on 
# the grid

# likelihood with dbinom
likelihood <- dbinom(6, size = 9, p = p_grid)


# plot it
plot(p_grid, likelihood)


#==========================================================


# 4) Compute the unstandardized posterior at each 
# parameter value on the grid

# The unstandardized posterior is simply the product of 
# the likelihood and prior
unstd.posterior <- likelihood * prior


# Again, could visualize
plot(p_grid, unstd.posterior, pch = 19, type = "b")


# Note that this unstandardized posterior is not a proper 
# probability distribution since it does not add to 1
sum(unstd.posterior)

#==========================================================


# 5) Standardize the posterior

posterior <- unstd.posterior/sum(unstd.posterior)



# This standardized posterior is a now proper probability 
# distribution
sum(posterior)


# Visualize the posterior
plot(p_grid, posterior, pch = 19, type = "b",
     xlab = "proportion of water on globe",
     ylab = "posterior probability")

