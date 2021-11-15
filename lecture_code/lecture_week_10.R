
### Multiple regression
### ~~~~~~~~~~~~~~~~~~~~
library(rethinking)
library(dplyr)


### Example 1: Divorce rate and marriage rate + age at marriage ####

data("WaffleDivorce")
d <- WaffleDivorce

### Standardize variables
d <- d %>% 
  mutate_at(vars(D = Divorce, 
                 M = Marriage, 
                 A = MedianAgeMarriage), 
            scale)


### Linear model predicting divorce rate with age at marriage ####
# Exponential distribution to force sigma to be positive
curve(dexp(x, 1), from = -1, to = 5, main = "Prior for sigma")

m5.1 <- quap(
  data = d,
  alist(D ~ dnorm(mu, sigma), 
        mu <- a + bA * A,
        a ~ dnorm(0, 0.2),
        bA ~ dnorm(0, 0.5),
        sigma ~ dexp(1))
)
precis(m5.1)

# Interpretation: an increase in age at marriage by 0.57 standard deviations
# leads to a decrease in divorce rate by 1 standard deviation
sd(d$Divorce)




### Sample from priors and plot implied relationships

# Extract sample from prior (default: n=10000)
prior <- extract.prior(m5.1)
# Simulate mean divorce rate
mu <- link(m5.1, post = prior, data = list(A = c(-2,2)))
# Plot simulated relationships
plot(NULL, xlim = c(-2,2), ylim=c(-2,2), 
     xlab = "Median age at marriage", ylab = "Divorce rate (%)")
for(i in 1:50){
  lines(c(-2,2), mu[i,],
        col = col.alpha("blue", 0.4))
}




### Plot posterior predictions

# List of ages to estimate divorce rates at
A_seq <- seq(from = -3, to = 3, length.out = 100)
mu <- link(m5.1, data = list(A = A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
plot(D ~ A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)




### Alternative version 
### For reference, you can run the model without standardization. 
### The result will be much the same! 

# curve(dnorm(x, 50, 10), from = -20, to = 100, main = "Prior for a (intercept)")
# curve(dnorm(x, 0, 0.5), from = -5, to = 5, main = "Prior for b (slope)")
# curve(dexp(x, 2), from = -1, to = 5, main = "Prior for sigma")
# 
# m5.1r <- quap(
#   data = d,
#   alist(Divorce ~ dnorm(mu, sigma), 
#         mu <- a + bA * MedianAgeMarriage,
#         a ~ dnorm(20, 10),
#         bA ~ dnorm(0, 0.5),
#         sigma ~ dexp(1))
# )
# precis(m5.1r)
# 
# prior <- extract.prior(m5.1r)
# mu <- link(m5.1r, post = prior, data = list(MedianAgeMarriage = c(0,100)))
# plot(NULL, xlim = c(0,100), ylim=c(0,100), 
#      xlab = "Median age at marriage", ylab = "Divorce rate (%)")
# for(i in 1:50){
#   lines(c(0,100), mu[i,],
#         col = col.alpha("blue", 0.4))
# }
# 
# A_seq <- seq(from = 18, to = 90, length.out = 100)
# mu <- link(m5.1r, data = list(MedianAgeMarriage = A_seq))
# mu.mean <- apply(mu, 2, mean)
# mu.PI <- apply(mu, 2, PI)
# plot(Divorce ~ MedianAgeMarriage, data = d, col = rangi2)
# lines(A_seq, mu.mean, lwd = 2)
# shade(mu.PI, A_seq)





### Linear model predicting divorce rate with marriage rate ####
m5.2 <- quap(
  data = d,
  alist(D ~ dnorm(mu, sigma), 
        mu <- a + bM * M,
        a ~ dnorm(0, 0.2),
        bM ~ dnorm(0, 0.5),
        sigma ~ dexp(1))
)
precis(m5.2)

### Linear model adding age + marriage rate ####
m5.3 <- quap(
  data = d, 
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  )
)
precis(m5.3)

# Note how slope for marriage rate is close to 0. Why?


# Plot coefficients and PI
coeftab_plot(coeftab(m5.1, m5.2, m5.3), pars = c("bA", "bM"))





### Example 2: Energy content of primate milk ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prepare milk energy data for analysis
data(milk)
d <- milk


# Plot raw data
plot(kcal.per.g ~ neocortex.perc,
     data = d, col = "cornflowerblue", pch = 19,
     xlim = c(0, 100), ylim = c(0, 1))


### Standardize variables
d <- d %>% 
  mutate(log_mass = log(mass)) %>% 
  mutate_at(vars(K = kcal.per.g, 
                 N = neocortex.perc, 
                 M = log_mass), 
            scale)


### Linear regression of energy content with neocortex percent ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m5.5 <- quap(
  data = d,
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N,
    a ~ dnorm(0, 1),
    bN ~ dnorm(0, 1),
    sigma ~ dexp(1)
  )
)

# Created error due to missing values
# Remove NAs in 3 variables of interest
dcc <- d[complete.cases(d$K, d$N, d$M), ]

# Rerun
m5.5 <- quap(
  data = dcc,
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N,
    a ~ dnorm(0, 1),
    bN ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  )
)



# Summarize model output
precis(m5.5, digits = 3)
# precis(m5.5, digits = 3, prob = 0.9)



### Sample from priors and plot implied relationships

# Extract sample from prior (default: n=10000)
prior <- extract.prior(m5.5)
# Simulate mean kcal
mu <- link(m5.5, post = prior, data = list(N = c(-2,2)))
# Plot simulated relationships
plot(NULL, xlim = c(-2,2), ylim=c(-2,2), 
     xlab = "neocortex percent (std)", ylab = "kcal per g (std)")
for(i in 1:50){
  lines(c(-2,2), mu[i,],
        col = col.alpha("blue", 0.4))
}

# This doesn't look good! Go back and tighten intercept prior
m5.5 <- quap(
  data = dcc,
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N,
    a ~ dnorm(0, 0.1),
    bN ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  )
)
# Then rerun lines 210 through 219





### Plot posterior predictions

# List of neocortex percentages to simulate at
xseq <- seq(from = -3, to = 3, length.out = 100)
mu <- link(m5.5, data = list(N = xseq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
plot(K ~ N, data = dcc)
lines(xseq, mu.mean, lwd = 2)
shade(mu.PI, xseq)


### You could plot your posterior estimates and PIs
# post1 <- extract.samples(m5.5, n = 10000)
# # Plot posterior samples
# dens(post1$a, main = "Posterior for a (intercept)", xlab = "a", 
#      show.HPDI = 0.9)
# dens(post1$bn, main = "Posterior for bn (slope)", xlab = "bn",
#   show.HPDI = 0.9) 




### Linear regression of energy content with log body mass ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m5.6 <- quap(
  data = dcc,
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  )
)

precis(m5.6, digits = 3)
# Compare to neocortex as predictor
precis(m5.5, digits = 3)





### Multiple regression: neocortex percentage and log body mass ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fit linear regression using quap()
m5.7 <- quap(
  data = dcc,
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N + bM * M,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  )
)

precis(m5.7, digits = 3)


# Plot coefficients and PI
coeftab_plot(coeftab(m5.5, m5.6, m5.7), par = c("bM", "bN"))



# # Plot posterior samples
# post3 <- extract.samples(m5.7, n = 10000)
# dens(
#   post3$a,
#   main = "Posterior for a (intercept)", 
#   xlab = "a",
#   show.HPDI = 0.9
# )
# dens(
#   post3$bN,
#   main = "Posterior for bn (slope)",
#   xlab = "bn",
#   show.HPDI = 0.9
# ) 
# dens(
#   post3$bM,
#   main = "Posterior for bm (slope)",
#   xlab = "bm",
#   show.HPDI = 0.9
# ) 







### Visualization techniques ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### . --- Posterior predictive plot ####
par(mfrow = c(1, 1))

mu <- link(m5.7) # default is 1,000 predictions of mean
# Note when no data range specified, uses what it finds in model data
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Plot predicted mean over observed values
plot(mu_mean ~ dcc$K, col = rangi2, ylim = c(-2,2), 
     xlab = "Observed kcal per g (std)",
     ylab = "Predicted kcal per g (std)")
# Perfect prediction implies perfect correlation and slope = 1
# For one unit increase in observed values the predicted mean increases by 1
abline(a = 0, b = 1, lty = 2)
# Plot PIs for each observation
for(i in 1:nrow(dcc)){
  lines(rep(dcc$K[i],2), mu_PI[,i], col = rangi2)
}

# Label some outliers (interactive)
identify(x = dcc$K, y = mu_mean, labels = dcc$species)



### . --- Counterfactual plots ####
par(mfrow = c(1, 1))


# Hold N = 0
xseq = seq(from = min(dcc$M)-0.15, 
           to = max(dcc$M)+0.15, 
           length.out = 100)

mu <- link(m5.7, data = data.frame(M = xseq, N = 0))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
plot(NULL, xlim = range(dcc$M), ylim = range(dcc$K), 
     main = "Counterfactual holding N = 0", 
     xlab = "log body mass (std)", 
     ylab = "kcal per g (std)")
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)


# Hold M = 0
xseq = seq(from = min(dcc$N)-0.15, 
           to = max(dcc$N)+0.15, 
           length.out = 100)

mu <- link(m5.7, data = data.frame(N = xseq, M = 0))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
plot(NULL, xlim = range(dcc$N), ylim = range(dcc$K), 
     main = "Counterfactual holding M = 0", 
     xlab = "log neocortex percent (std)", 
     ylab = "kcal per g (std)")
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)






### Understanding multiple regression ####
### --> go through on your own time
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### The code below illustrates how using two predictor variables
###   is equivalent to regressing one variable on residuals of the others


# Using lm() as a shortcut to visualize best fit line and compute residuals
fit <- lm(neocortex.perc ~ log_mass, data = dcc)
abline(fit)
mu <- coef(fit)[1] + coef(fit)[2] * dcc$log_mass
dcc$np.residual <- resid(fit)


plot(neocortex.perc ~ log_mass, data = dcc,
     col = "cornflowerblue", pch = 19)

for (i in 1:length(dcc$np.residual)) {
  
  x <- dcc$log_mass[i] # x location of line segment
  y <- dcc$neocortex.perc[i] # observed endpoint of line segment
  # draw the line segments
  lines(c(x, x), c(mu[i], y), 
        lwd = 0.8, col = col.alpha("black", 0.8))
}


# Fit a linear regression using neocortex percentage RESIDUALS as predictor
m.np.residual <- quap(
  data = dcc,
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bnr * np.residual,
    a ~ dnorm(0, 100),
    bnr ~ dnorm(0, 1),
    sigma ~ dunif(0, 1) 
  )
)


fit <- lm(log_mass ~ neocortex.perc, data = dcc)
abline(fit)
mu <- coef(fit)[1] + coef(fit)[2] * dcc$neocortex.perc
dcc$lbm.residual <- resid(fit)

m.lbm.residual <- quap(
  data = dcc,
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bmr * lbm.residual,
    a ~ dnorm(0, 100),
    bmr ~ dnorm(0, 1),
    sigma ~ dunif(0, 1) 
  )
)

precis(m.np.residual)
precis(m.lbm.residual)

# The take home message is that the multiple regression
# using both of the predictor variables effectively 
# controls for the values of the other predictor, giving
# us equivalent results to what we would see if we ran a
# bivariate regression with predictor residuals







### Categorical predictor variables ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Import Howell data
data(Howell1)
d <- Howell1
str(d)
d$male

# In this case, "male" is already a dummy variable, but we
# could easily create it with an "ifelse()" statement if
# it was a string variable (see book for examples)


# Fit a linear regression using sex as a predictor of 
# height
m5.15 <- quap(
  data = d,
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bm * male,
    a ~ dnorm(178, 100),
    bm ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  )
)

# Summarize the fit model
precis(m5.15)

# Generate posterior samples
post <- extract.samples(m5.15, n = 10000)

# To visualize the expected mean height value for females
dens(
  post$a, 
  xlim = c(120, 160),
  xlab = "Mean height (cm)",
  show.HPDI = 0.5
)

# To visualize the expected mean height value for males
dens(
  post$a + post$bm, 
  col = "darkred", 
  add = TRUE,
  show.HPDI = 0.5
)


