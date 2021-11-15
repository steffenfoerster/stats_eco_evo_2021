### ~~~~~~~~~~~~~~~~~~~~~~~~
### LECTURE CODE WEEK 7 ####
### ~~~~~~~~~~~~~~~~~~~~~~~~


### PART 1: Recap of probability functions ####


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Probability mass function for the binomial distribution ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of trials: 10
# Probability of success: 0.3

prob.mass <- dbinom(0:10, size = 10, p = 0.3)


# We can plot these probability masses
plot(
  x = 0:10, 
  y = prob.mass,
  xlab = "Number of successful binomial trials",
  ylab = "Probability",
  pch = 19
)

# Check that the pmf adds to 1. 
# Q: What does this mean? 
sum(prob.mass)




### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Random generation function for the binomial distribution ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 20 simulated draws (observations) from the binomial distribution
# Each observation is composed of 10 trials
draws.10 <- rbinom(n = 20, size = 10, prob = 0.3)

  

### Plot the distribution
library(rethinking)
simplehist(
  x = draws.10, 
  xlim = c(0, 10),
  xlab = "Number of successes observed"
)

# Base R alternative with hist()
hist(draws.10, breaks = seq(0,10,1))



# 100 simulated draws (observations)
draws.100 <- rbinom(n = 100, size = 10, prob = 0.3)

simplehist(
  x = draws.100, 
  xlim = c(0, 10),
  xlab = "Number of successes observed"
)

# 10,000 simulated draws (observations)
draws.10000 <- rbinom(10000, size = 10, prob = 0.3)

simplehist(
  x = draws.10000, 
  xlim = c(0, 10),
  xlab = "Number of successes observed"
)




### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Probability density function for the normal distribution ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Parameters: 
# 1) mean = 2.5
# 2) standard deviation = 1.5

# Plot the probability density function for the
# normal distribution
curve(
  dnorm(x, mean = 2.5, sd = 1.5), 
  from = -10, 
  to = 10,
  ylab = "Probability density"
)


# 10 simulated draws (observations) from the 
# normal distribution
rnorm(10, mean = 2.5, sd = 1.5)




### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Probability density function for the Cauchy distribution ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Parameters: 
# 1) location = 2.5
# 2) scale = 1.5

# Plot the probability density function for the
# Cauchy distribution
curve(
  dcauchy(x, location = 2.5, scale = 1.5),
  from = -10, 
  to = 10,
  ylab = "Probability density"
)

# 10 simulated draws (observations) from the 
# Cauchy distribution
rcauchy(10, location = 2.5, scale = 1.5)



### Comparison: plot both normal distribution and cauchy distribution side by side
curve(
  dnorm(x, mean = 2.5, sd = 1.5), 
  from = -10, 
  to = 10,
  ylab = "Probability density",
  col = "red"
)
curve(
  dcauchy(x, location = 2.5, scale = 1.5),
  from = -10, 
  to = 10,
  ylab = "Probability density",
  col = "blue", 
  add = TRUE
)














### Part 2: Sampling from distributions #####


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Sampling from a posterior distribution ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# First, let's generate a posterior distribution using
# grid approximation (with 101 points). The data observed 
# are 6 successes out of 9 binomial trials

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior / sum(likelihood * prior)


# Plot the posterior
plot(
  x = p_grid, 
  y = posterior,
  pch = 19, # see ?pch for marker options
  type = "l", # p for points (see ?plot for other options)
  xlab = "Proportion of water on globe",
  ylab = "Posterior probability"
)




# Generate 10,000 samples of p (the proportion of water
# parameter) from the grid-approximate posterior
samples <- sample(x = p_grid, 
                  size = 100000, 
                  prob = posterior, 
                  replace = TRUE)

# Plot the posterior samples
plot(samples, 
     pch = 19, 
     col = alpha("black", 0.2) # alpha adds transparency
     )

# Plot probability density estimates using rethinking package
dens(samples, 
     xlim = c(0, 1),
     xlab = "Proportion of water on globe")




# Grid-approximate posterior for 5 successes in 5
# binomial trials (leads to an extremely skewed 
# posterior distribution for p)
likelihood2 <- dbinom(5, size = 5, prob = p_grid)
posterior2 <- likelihood2 * prior / sum(likelihood2 * prior)


# Plot the posterior
plot(
  p_grid, posterior2,
  pch = 19, type = "b",
  xlab = "Proportion of water on globe",
  ylab = "Posterior probability"
)

# Generate 10,000 samples of p (the proportion of water
# parameter) from the grid-approximate posterior
samples2 <- sample(
  p_grid, size = 10000, 
  prob = posterior2, replace = TRUE
)

# Plot the posterior samples
plot(samples2, pch = 19, col = alpha("black", 0.2))
dens(samples2, xlim = c(0, 1),
     xlab = "Proportion of water on globe")




### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Interval of defined boundary ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Probability of proportion of water being > 0.3 AND < 0.7
sum(samples > 0.3 & samples < 0.7) / length(samples)






### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Percentile and highest posterior density intervals ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Illustrate the difference between PI and HPDI on skewed distribution
# 50% PI
PI(samples2, prob = 0.5)

dens(samples2,
     xlab = "Proportion of water on globe",
     main = "50% PI shaded")

shade(
  density(samples2, adjust = 0.5), 
  PI(samples2, prob = 0.5),
  col = "darkgrey"
)


# 50% HPDI
HPDI(samples2, prob = 0.5)

dens(samples2, 
     xlab = "Proportion of water on globe",
     main = "50% HPDI shaded")

shade(
  density(samples2, adjust = 0.5), 
  HPDI(samples2, prob = 0.5),
  col = "darkgrey"
)





# With a less skewed posterior, the PI and HPDI are
# often going to be very, very similar

# 70% PI
PI(samples, prob = 0.7)

dens(samples, 
     xlab = "Proportion of water on globe",
     main = "70% PI shaded")
shade(
  density(samples, adjust = 0.5), 
  PI(samples, prob = 0.7),
  col = "darkgrey"
)

# 70% HPDI
HPDI(samples, prob = 0.7)

dens(samples,
     xlab = "Proportion of water on globe",
     main = "70% HPDI shaded")
shade(
  density(samples, adjust = 0.5), 
  HPDI(samples, prob = 0.7),
  col = "darkgrey"
)





### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Sampling to simulate prediction   ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# binomial trials of size 10, given the posterior in samples2
preds <- rbinom(n = 10000, size = 10, prob = samples2)


plot(preds, pch = 19, col = alpha("black", 0.2))
simplehist(preds, xlim = c(0, 10))


# Note that even if we fix the probability of success value
# there would still be variation in our outcomes because
# the data-generating process is subject to outcome 
# uncertainty
test.outcomes <- rbinom(10000, size = 10, prob = 0.6)

simplehist(test.outcomes, xlim = c(0, 10))
