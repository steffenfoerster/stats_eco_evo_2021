### Model comparison
### ~~~~~~~~~~~~~~~~~~~~
library(rethinking)
library(dplyr)


# Using the urban fox dataset, lets compare the following 5 models using WAIC or PSIS based model comparisons 
# with weight as the outcome variable in each: 
# - avgfood + groupsize + area
# - avgfood + groupsize
# - groupsize + area
# - avgfood
# - area


data(foxes)
d <- foxes

d <- d %>% 
  mutate_at(vars(W = weight, 
                 A = area, 
                 F = avgfood, 
                 G = groupsize), 
            scale)


# Run the models
m1 <- quap(data = d, 
           alist(W ~ dnorm(mu, sigma),
                 mu <- a + bF*F + bG*G + bA*A,
                 a ~ dnorm(0,0.2),
                 c(bF, bG, bA) ~ dnorm(0, 0.5), 
                 sigma ~ dexp(1)))

m2 <- quap(data = d, 
           alist(W ~ dnorm(mu, sigma),
                 mu <- a + bF*F + bG*G,
                 a ~ dnorm(0,0.2),
                 c(bF, bG) ~ dnorm(0, 0.5), 
                 sigma ~ dexp(1)))

m3 <- quap(data = d, 
           alist(W ~ dnorm(mu, sigma),
                 mu <- a + bG*G + bA*A,
                 a ~ dnorm(0,0.2),
                 c(bG, bA) ~ dnorm(0, 0.5), 
                 sigma ~ dexp(1)))

m4 <- quap(data = d, 
           alist(W ~ dnorm(mu, sigma),
                 mu <- a + bF*F,
                 a ~ dnorm(0,0.2),
                 bF ~ dnorm(0, 0.5), 
                 sigma ~ dexp(1)))

m5 <- quap(data = d, 
           alist(W ~ dnorm(mu, sigma),
                 mu <- a + bA*A,
                 a ~ dnorm(0,0.2),
                 c(bA) ~ dnorm(0, 0.5), 
                 sigma ~ dexp(1)))


# Look at coefficients
coeftab(m1, m2, m3, m4, m5)

# Compare model fit
compare(m1, m2, m3, m4, m5)

# Look at the pairwise standard errors
compare(m1, m2, m3, m4, m5)@dSE




