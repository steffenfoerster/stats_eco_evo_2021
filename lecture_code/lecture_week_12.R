## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lecture week 12, Interactions ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(rethinking)
library(dplyr)


## Import and prep the country data ####
data(rugged)
d <- rugged

# Extract countries with GDP data
dd <- d[complete.cases(d$rgdppc_2000), ]

# Make log version of outcome (GDP in year 2000)
dd$log_gdp <- log(dd$rgdppc_2000)

### Transform variables
# For GDP, we want the mean to be 1
#   so country GDP is in relation to world mean GDP
#   0.8 means 80% of world mean GDP, and so on... 
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)

# For ruggedness, we want min to be 0 (flat) and max to be 1
# Usual standardization would put mean at 0, which would not be "flat"
dd$rugged_std <- dd$rugged / max(dd$rugged)

# Make AFRICA index variable (1, 2)
dd$cid <- ifelse(dd$cont_africa == 1, 1, 2)





## Fit a linear model predicting GDP with ruggedness ####
m8.1 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a + bR * rugged_std,
    a ~ dnorm(1, 1),
    bR ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = dd)

precis(m8.1, prob = 0.89, digits = 4)



## Quick prior predictive plot
# Note: As we have a lot of data, even really bad priors would be washed out by model
prior <- extract.prior(m8.1)
plot(NULL, xlim = c(0,1), ylim = c(0.5, 1.5),
     xlab = "ruggedness std", ylab = "log GDP std")
# Ad min and max GDP to plot
abline(h = min(dd$log_gdp_std), lty = 2)
abline(h = max(dd$log_gdp_std), lty = 2)
abline(v = mean(dd$rugged_std, lty = 3))
rugvalues <- seq(-0.1, 1.1, length.out = 50)
mu <- link(m8.1, post = prior, data = data.frame(rugged_std = rugvalues))
for(i in 1:50){
  lines(rugvalues, mu[i,], col = col.alpha("black", 0.3))
}


## Change slope and standard deviation priors
# Most lines should go through mean ruggedness, should not predict GDP values out of range, 
#   and ruggedness should not explain most of variation in GDP
m8.1 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a + bR*rugged_std,
    a ~ dnorm(1, 0.1),
    bR ~ dnorm(0, 0.1),
    sigma ~ dexp(1)
  ),
  data = dd)
precis(m8.1)



# Plot model predictions
mu <- link(m8.1, data = data.frame(rugged_std = rugvalues))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.97)

plot(log_gdp_std ~ rugged_std, data = dd,
     xlab = "Terrain Ruggedness Index",
     ylab = "log(GDP year 200)",
     col = "red4"
)
mtext("Linear regression", 3)
lines(rugvalues, mu.mean, col = "red4")
shade(mu.PI, rugvalues, col = col.alpha("red4", 0.3))








## Multiple regression with ruggedness and continent identity ####

### Using dummy variable approach for continent indicator
m8.2.1 <- quap(
  data = dd, 
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a + bR * rugged_std + bA * cont_africa,
    a ~ dnorm(1, 0.1),
    bR ~ dnorm(0, 0.1),
    bA ~ dnorm(0, 0.1),
    sigma ~ dexp(1)
  ))
precis(m8.2.1)


### Using index variable approach (preferred)
m8.2.2 <- quap(
  data = dd,
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + bR * rugged_std, 
    a[cid] ~ dnorm(1, 0.1), 
    bR ~ dnorm(0, 0.1), 
    sigma ~ dexp(1)
  ))
precis(m8.2.2, depth = 2)
# Note that a[2] is the same and a[1] is exactly a - bA from m8.2.1



# Plot model predictions
cf.Africa <- data.frame(cont_africa = 1, rugged_std = rugvalues)
cf.NotAfrica <- data.frame(cont_africa = 0, rugged_std = rugvalues)

mu.Africa <- link(m8.2.1, data = cf.Africa)
mu.Africa.mean <- apply(mu.Africa, 2, mean)
mu.Africa.PI <- apply(mu.Africa, 2, PI, prob = 0.97)

mu.NotAfrica <- link(m8.2.1, data = cf.NotAfrica)
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI, prob = 0.97)

# par(mfrow=c(1,1))
plot(log_gdp_std ~ rugged_std, data = dd,
     xlab = "Terrain Ruggedness Std",
     ylab = "log(GDP year 2000) Std",
     col = ifelse(dd$cont_africa == 1, rangi2, "black")
)
mtext("Multiple regression, no interaction", 3)
lines(rugvalues, mu.Africa.mean, col = rangi2)
shade(mu.Africa.PI, rugvalues, col = col.alpha(rangi2, 0.3))
lines(rugvalues, mu.NotAfrica.mean)
shade(mu.NotAfrica.PI, rugvalues)
# When plotting multiple relationships in one graph, add legend! 
legend(x = 0.75, y = 1.3, 
       legend = c("Africa", "Not Africa"),
       col = c("#8080FF", "black"), lty = 1, cex = 0.7)






## Fit a multiple regression with an interaction term ####

# Using dummy variable coding
m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a + bR * rugged_std + bA * cont_africa + bAR * rugged_std * cont_africa,
    a ~ dnorm(1, 0.1),
    bA ~ dnorm(0, 0.1),
    bR ~ dnorm(0, 0.1),
    bAR ~ dnorm(0, 0.1),
    sigma ~ dexp(1)
  ),
  data = dd)

# Using index variable coding
m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * rugged_std,
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.1),
    sigma ~ dexp(1)
  ),
  data = dd)

precis(m8.3, depth = 2)



# Generate model-based predictions
cf.Africa <- data.frame(cid = 1, rugged_std = rugvalues)
cf.NotAfrica <- data.frame(cid = 2, rugged_std = rugvalues)

mu.Africa <- link(m8.3, data = cf.Africa)
mu.Africa.mean <- apply(mu.Africa, 2, mean)
mu.Africa.PI <- apply(mu.Africa, 2, PI, prob = 0.97)

mu.NotAfrica <- link(m8.3, data = cf.NotAfrica)
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI, prob = 0.97)


# Plot predictions for each level of categorical predictor
par(mfrow = c(1, 1))

plot(log_gdp_std ~ rugged_std, 
     data = dd,
     xlab = "Terrain Ruggedness Index Std",
     ylab = "log(GDP year 200) Std",
     col = ifelse(dd$cid == 1, rangi2, "black")
)
mtext("Multiple regression, with interaction", 3)  
lines(rugvalues, mu.Africa.mean, col = rangi2)
shade(mu.Africa.PI, rugvalues, 
      col = col.alpha(rangi2, 0.3))
lines(rugvalues, mu.NotAfrica.mean)
shade(mu.NotAfrica.PI, rugvalues)
legend(x = 0.75, y = 1.3, 
       legend = c("Africa", "Not Africa"),
       col = c("#8080FF", "black"), lty = 1, cex = 0.7)


## Note, if plotting two separate figures side by side, follow code
#     from chapter 8.1.4, page 249 (2nd edition)





## Interaction between 2 continuous predictors ####

# How do ruggedness and distance to coast affect GDP?
m8.4 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a + bR * rugged_std + bC * log(dist_coast+0.01) + bRC * rugged_std * log(dist_coast+0.01),
    a ~ dnorm(1, 0.1),
    bR ~ dnorm(0, 0.1),
    bC ~ dnorm(0, 0.1),
    bRC ~ dnorm(0, 0.1), 
    sigma ~ dexp(1)
  ),
  data = dd)
precis(m8.4, digits = 4)


# For plotting interactions, need to pick some values for one predictor 
#   and examine effect of the other predictor

# Use ntile() function from dplyr to bin distance to coast and ruggedness into quartiles
dd <- dd %>% mutate(dist_coast_q = ntile(scale(dist_coast+0.01), 4))

# Evaluate effects of ruggedness at 4 quartiles for distance to coast
par(mfrow=c(1,4), mar=c(5,5,4,2))
for(c in 1:4){
  idx <- which(dd$dist_coast_q == c)
  plot(dd$rugged_std[idx], dd$log_gdp_std[idx], 
       xlim = c(0,1), 
       ylim = c(0.5, 1.5),
       main = paste("dist coast quart: ", c), 
       xlab = "ruggedness std.", 
       ylab = "log(GDP) std.", 
       cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex = 2)
  mu <- link(m8.4, data = data.frame(dist_coast = c, 
                                     rugged_std = dd$rugged_std))
  for(i in 1:20){
    lines(dd$rugged_std, mu[i,], col = col.alpha("black", 0.3), pch = 17, cex = 4)
  }
}
dev.off() # reset graphics device



