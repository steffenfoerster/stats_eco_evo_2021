library(rethinking)
library(dplyr)


# Gaussian model of human height data collected by Nancy Howell ####

## Prepare Howell data for analysis ####
data(Howell1)
kungdata <- filter(Howell1, age >= 18) # filter to 18+ years of age
# kungdata <- Howell1[Howell1$age>=18, ]
dens(kungdata$height)



## Visualize priors ####
curve(
  dnorm(x, 178, 20), 
  from = 50, to = 300,
  main = "Prior for mu",
  xlab = "mu",
  ylab = "Density"
)


curve(
  dunif(x, 0, 50), 
  from = -10, to = 60,
  main = "Prior for sigma",
  xlab = "sigma",
  ylab = "Density"
)



## What do these priors imply about expected heights? ####
#### Get the prior predictive distribution by sampling from the prior ####
sample_mu <- rnorm(10000, 178, 20)
sample_sigma <- runif(10000, 0, 50)
prior_h <- rnorm(10000, sample_mu, sample_sigma)
dens(prior_h)

#### What happens if we change the prior for mu? ####
sample_mu <- rnorm(10000, 178, 100)
prior_h <- rnorm(10000, sample_mu, sample_sigma)
dens(prior_h)





### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Fit Gaussian model using quap() - Model 4.1 ####
m4.1 <- quap( 
  data = kungdata, # specify data to fit
  alist( 
    # define statistical model
    height ~ dnorm(mu, sigma), # likelihood
    mu ~ dnorm(178, 20) , # prior for the mean parameter
    sigma ~ dunif(0, 50) # prior for the sd parameter
  )
)

### Summarize model output ####
precis(m4.1, prob = 0.89) # shows PIs by default for quap() fits

### Note on the interpretation of marginal distributions: ####
# The probabilities of each value of mu (mean), averaging over the 
# probabilities of each value of sigma (SD), are given by a normal 
# distribution with the model estimated mean and standard deviation



### Extract posterior samples for all parameters ####
post <- extract.samples(m4.1, n = 10000)
head(post, 10)



### Plot posterior samples for both parameters ####
# These are called marginal posterior density plots
dens(
  post$mu, 
  main = "Posterior for mu", 
  xlab = "mu"
)
dens(
  post$sigma, 
  main = "Posterior for sigma", 
  xlab = "sigma"
)


# Combinations of mean and sd that are consistent with the data
plot(
  post$mu, post$sigma,
  xlab = "mu", ylab = "sigma",
  pch = 19,
  col = col.alpha("black", 0.2)
)






### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Fit linear model with predictor (weight) ####

### Visualize the height/weight relationship in the raw data ####
plot(
  height ~ weight, data = kungdata,
  pch = 19
)

### Visualize priors for the intercept and slope parameters ####
# Intercept
curve(
  dnorm(x, 178, 20), 
  from = -10, to = 300,
  main = "Prior for a (intercept)",
  xlab = "a",
  ylab = "Density"
)
# Slope
curve(
  dnorm(x, 0, 10), 
  from = -50, to = 50,
  main = "Prior for b (slope)",
  xlab = "b",
  ylab = "Density"
)



### Is our prior for slope reasonable? ####
# Prior predictive distribution
a <- rnorm(100, 178, 20)
b <- rnorm(100, 0, 10)

# Set up plot and axes ranges (optional)
plot(NULL, 
     xlim = range(kungdata$weight), 
     ylim = c(-100, 400), 
     xlab = "weight", 
     ylab = "height")

# Plot 100 lines relating height to weight given a and b
# The curve function plugs in different values for x to draw the line
for(i in 1:100) {
  curve(
    a[i] + b[i] * (x - mean(kungdata$weight)),
    add = TRUE, 
    col = alpha("seagreen", 0.5)
  )
}


### Log normal distribution of b instead ####
# b ~ Log-normal(0,1), means that the log of b has normal distribution
b <- rlnorm(100, 0, 1)

for (i in 1:100) {
  curve(
    a[i] + b[i]*(x-mean(kungdata$weight)),
    add = TRUE, 
    col = alpha("seagreen", 0.5)
  )
}




### Fit the model - Model 4.2 ####
m4.2 <- quap( 
  data = kungdata,
  alist(
    height ~ dnorm(mu, sigma), # likelihood
    mu <- a + b*weight, # model mu using a linear formula
    a ~ dnorm(178, 20), # prior for the intercept parameter
    b ~ dlnorm(0, 1), # prior for the slope parameter
    sigma ~ dunif(0, 50) # prior for the sd parameter
  )
)

### Summarize model output ####
precis(m4.2)



### Extract posterior samples for all parameters ####
post <- extract.samples(m4.2, n = 10000)
head(post, 10)


### Plot posterior samples of a (intercept) and b (slope) ####
dens(
  post$a, 
  main = "Posterior for a (intercept)", 
  xlab = "a"
)
dens(
  post$b, 
  main = "Posterior for b (slope)", 
  xlab = "b"
)






### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Visualizing posterior parameter estimates ####


## Show the QUAP trend line ####

# First recognize the information that "coef()" gives you
precis(m4.2)
coef(m4.2)

plot(
  height ~ weight, data = kungdata,
  pch = 19
)

abline(a = coef(m4.2)["a"], b = coef(m4.2)["b"], lwd = 3)


### Show uncertainty in trend line ####
head(post, 10)

plot(
  height ~ weight, data = kungdata,
  pch = 19
)

for (i in 1:100) {
  abline(
    a = post$a[i], b = post$b[i],
    col = alpha("seagreen", 0.1)
  )
}






### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Generating model-based predictions ####
# Generate 10,000 predicted heights at weight = 50 kilograms
preds.50 <- rnorm(
  10000,
  mean = post$a + post$b*50,
  sd = post$sigma
)

dens(
  preds.50,
  xlab = "Predicted Height (cm) for an Individual of 50 kg"
)







### START HERE on 11/15

# Or rethinking has built-in convenience functions to
# generate predicted mu values (link) or full 
# predictions (sim) for you

### Using link() ####
mu.50 <- link(m4.2, data = list(weight = 50), n = 10000)
dens(
  mu.50,
  xlab = "Predicted Mean Height (cm) for an Individual of 50 kg"
)


### Using sim() ####
preds.50 <- sim(m4.2, data = list(weight = 50), n = 10000)
dens(
  preds.50, col = "darkgreen",
  xlab = "Predicted Height (cm) for an Individual of 50 kg"
)



### Predictions of mean heights ####
weights <- seq(25, 70, 1)
mu <- link(m4.2, data = data.frame(weight = weights))
View(mu)

mu.mean = apply(mu, 2, mean)
mu.PI = apply(mu, 2, PI, prob = 0.89)

plot(height ~ weight, data = kungdata, col = col.alpha("blue", 0.5))
lines(weights, mu.mean) # base function that connects two coordinates
shade(mu.PI, weights) # rethinking function that plots 


### Predictions of simulated heights ####
sim.height <- sim(m4.2, data = list(weight = weights), n = 1000)
View(sim.height)

plot(height ~ weight, kungdata, col = col.alpha("blue", 0.5))
lines(weights, mu.mean)

# Compute PI for each simulated height
height.PI = apply(sim.height, 2, PI, prob = 0.89)
shade(height.PI, weights)

# Compute HDPI for each simulated height
mu.HPDI = apply(mu, 2, HPDI, prob = 0.89)
shade(mu.HPDI, weights)



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Use PPD to check model fit to data ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(NULL, ylab = "density", xlab = "height", 
     xlim = c(100, 200), ylim = c(0, 0.05))
for(i in 1:100){
  if(i > 1){ dens(sim.height[i,],  
                  col = alpha("seagreen", 0.5), 
                  add = TRUE)}
}
dens(kungdata$height, lwd = 3, add = TRUE)






### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Centering predictor variables ####

kungdata$weight.c <- kungdata$weight - mean(kungdata$weight)
plot(weight.c ~ weight, data = kungdata, pch = 19)
cor(kungdata$weight, kungdata$weight.c) # perfect correlation

m4.3 <- quap(
  data = kungdata,
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight.c,
    a ~ dnorm(178, 100),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  )
)

precis(m4.3)

# Remember: the intercept is the value of the linear
# portion of the model when all predictors equal 0. 
# That's still what the "a" parameter in model m4.3 
# is telling us. It's just that our predictor has 
# changed meaning slightly, so our interpretation is 
# different. The value of "a" now corresponds to the
# expected mean height of someone with average weight.

plot(
  height ~ weight.c, data = kungdata, 
  pch = 19
)


# Can also explicitly define parameter start values
# Not necessary in this case, but sometimes needed to 
# help quap() accurately describe the posterior shape

m4.4 <- quap(
  data = kungdata,
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight.c,
    a ~ dnorm(178, 100),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  start = list(a = 100, b = 0.1, sigma = 2)
)

precis(m4.4)



# Standardize a predictor variable ####

kungdata$weight.s <- (kungdata$weight - mean(kungdata$weight)) / sd(kungdata$weight)

# Both centering and standardizing can be done with scale() function
kungdata$weight.s <- scale(kungdata$weight, center = TRUE, scale = TRUE)


