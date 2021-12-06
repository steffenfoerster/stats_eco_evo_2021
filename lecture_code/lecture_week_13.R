library(rethinking)
library(dplyr)


# Introducing MCMC estimation ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Rugged dataset from previous week
data(rugged)
d <- rugged
dd <- d[complete.cases(d$rgdppc_2000), ]
dd$log_gdp <- log(dd$rgdppc_2000)
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse(dd$cont_africa == 1, 1, 2)

## Trim data to ensure we don't run into any Stan issues
dd.trim <- list(
  log_gdp_std = dd$log_gdp_std, 
  rugged_std = dd$rugged_std,
  cid = as.integer(dd$cid)
)



## Fit the model using ulam()
m8.5 <- ulam(
  data = dd.trim,
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * rugged_std,
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.1),
    sigma ~ dexp(1)
  ), 
  chains = 4
)


## Stan fit model summary functions
show(m8.5)
summary(m8.5)
pairs(m8.5)



## Parameter trace plots

# Using the rethinking plotting method
traceplot(m8.5, chains = 1)
traceplot(m8.5)

# Using the rstan plotting method
rstan::traceplot(m8.5@stanfit)






# POISSON GLM ####
# ~~~~~~~~~~~~~~~~

# Import the Kline dataset
data(Kline)
d <- Kline



# Generate modified predictor variables
d$Pop_std <- scale(log(d$population))
d$contact_id <- ifelse(d$contact == "high", 2, 1)

dl <- list(
  Tools = d$total_tools, 
  Pop = as.numeric(d$Pop_std), 
  cid = as.integer(d$contact_id)
)


# Fit a Poisson GLM with the effects of log population
# size, contact rate, and their interaction
m13.1 <- ulam(
  data = dl,
  alist(
    Tools ~ dpois(lambda),
    log(lambda) <- a[cid] + b[cid] * Pop,
    a[cid] ~ dnorm(3, 0.5),
    b[cid] ~ dnorm(0, 0.2)
  ), 
  chains = 4
)

traceplot(m13.1@stanfit)
precis(m13.1, prob = 0.97, depth = 2)

# To get expected tool count, take exp() of coefficients
exp(3.61)

# Or take sample from posterior
post <- extract.samples(m13.1)
mean(exp(post$a))
mean(exp(post$b))

# Note that we can leave out Pop because its mean is 0
# else exp(post$a + post$b * Population size)



### Generate model-based predictions


## Generate counterfactual data for high and low contact islands
counterfactual.high <- data.frame(Pop = 0, cid = 2)
counterfactual.low <- data.frame(Pop = 0, cid = 1)

# Generate predictions using sim()
preds.high.sim <- sim(m13.1, n = 5000, data = counterfactual.high)
preds.low.sim <- sim(m13.1, n = 5000, data = counterfactual.low)

head(preds.high.sim)
head(preds.low.sim)


## Visualize the predictions
### Density plots
dens(preds.high.sim[,1], 
     xlab = "", ylab = "",
     xlim = c(0, 80), 
     ylim = c(0, 0.1), 
     col = col.alpha("red", 0.9), 
     main = "High contact predictions")

dens(preds.low.sim[,1], 
     xlab = "", ylab = "",
     xlim = c(0, 80), 
     ylim = c(0, 0.1),
     col = col.alpha("blue", 0.5), 
     main = "Low contact predictions", 
     add = TRUE)

### Or simple histograms
par(mfrow = c(2, 1))
simplehist(preds.high.sim[,1], 
           xlab = "", ylab = "",
           xlim = c(0, 80),
           ylim = c(0, 100), 
           col = col.alpha("red", 0.9), 
           main = "High contact predictions")
simplehist(preds.low.sim[,1], 
           xlab = "", ylab = "",
           xlim = c(0, 80),
           ylim = c(0, 100),
           col = col.alpha("blue", 0.5), 
           main = "Low contact predictions")
dev.off()







# BIONOMIAL GLM ####


## Review probabilities and odds ####

# Generate a vector of probability values
probabilities <- seq(from = 0, to = 1, by = 0.01)
probabilities

# Compute the odds for each of these probabilities and 
# plot the relationship
odds <- probabilities / (1 - probabilities)
odds
dev.off()
plot(probabilities, odds, type = "n")
lines(probabilities, odds)


# Compute the log-odds for each of these probabilities 
# and plot the relationship
log_odds <- log(odds)
log_odds
plot(probabilities, log_odds, type = "n")
lines(probabilities, log_odds)


# Convert some log-odds values back to probabilities
# using the logistic function
log_odds_seq <- seq(from = -5, to = 5, by = 1)
log_odds_seq
probs_seq <- logistic(log_odds_seq) # same as inv_logit(log_odds_seq)
plot(log_odds_seq, probs_seq, type = "n")
lines(log_odds_seq, probs_seq)




## LOGISTIC regression ####
## Do on your own!!
data(chimpanzees)
d <- chimpanzees


# Make data list with only the variables we need
dlist <- list(
  actor = d$actor,
  pulled_left = d$pulled_left,
  prosocial_left = d$prosoc_left,
  condition = d$condition
)



### Fit a binomial GLM with interaction term ####
# see page 528 for justification of prior

m13.2 <- ulam(
  data = dlist,
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + bp * prosocial_left + bc * condition + bpc * prosocial_left * condition,
    a[actor] ~ dnorm(0, 1.5), 
    bp ~ dnorm(0, 0.5), 
    bc ~ dnorm(0, 0.5),
    bpc ~ dnorm(0, 0.5)
  ), 
  chains = 4
)

traceplot(m13.2)
precis(m13.2, prob = 0.97, depth = 2)


# Convert estimates into relative odds or absolute probabilities
# Coefficient for b, the slope for condition (partner presence) is -0.17
exp(0.58) # on the odds scale
logistic(-0.53) # on the probability scale, same as inv_logit()


# Extract samples from posterior and plot effects
post <- extract.samples(m13.2)

# Probability of pulling left for each actor
p_left <- logistic(post$a)
dev.off()
plot(precis(as.data.frame(p_left)), xlim = c(0,1))


### Model based predictions for partner present / absent
# Note: This can be done in many different ways

# Construct a data frame that holds actors and predictors
dat <- data.frame(actor = rep(1:7, each = 4), 
                  prosocial_left = rep(c(rep(0,2), rep(1,2)), times = 7), 
                  condition = rep(0:1, times = 14))
pred.post <- link(m13.2, data = dat) # note that link function already takes the inverse link by default
pred.mu <- apply(pred.post, 2, mean)
pred.ci <- apply(pred.post, 2, PI)
dat$mu <- pred.mu
dat$ci_l <- pred.ci[1,]
dat$ci_h <- pred.ci[2,]


library(ggthemes) # I use it below for a colorblind scale
ggplot(dat, aes(x = factor(condition, levels = c(0, 1), labels = c("Present", "Absent")), 
                y = mu, 
                fill = factor(prosocial_left, levels = c(0, 1), labels = c("Right", "Left")))) + 
  geom_bar(stat = "identity", 
           position = position_dodge()) + 
  geom_errorbar(aes(ymin = ci_l, ymax = ci_h), width = 0.2, 
                position = position_dodge(0.9)) + 
  scale_fill_colorblind() + 
  theme_minimal() + 
  facet_grid(~actor) + 
  labs(x = "Partner present or absent", 
       y = "Predicted probability of pulling left\n(97% Compatibility Interval)", 
       fill = "Prosocial option on left or right") + 
  theme(legend.position = "bottom", 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12))
  




### Alternative model without the actor specific intercept ####
m13.3 <- ulam(
  data = dlist,
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + bp * prosocial_left + bpc * prosocial_left * condition,
    a ~ dnorm(0, 1.5), 
    bp ~ dnorm(0, 0.5), 
    bpc ~ dnorm(0, 0.5)
  ), chains = 4, log_lik = TRUE
)

precis(m13.3, prob = 0.97)




# Visualize predictions from model

# Dummy data for predictions across treatments
d.pred <- data.frame(
  prosocial_left = c(0, 1, 0, 1), # right/left/right/left
  condition = c(0, 0, 1, 1) # control/control/partner/partner
)

# Build predictions for probability of success using "link()"
preds.p <- link(m13.3, data = d.pred)

# Summarize the probability prediction values
preds.p.mean <- apply(preds.p, 2, mean)
preds.p.PI <- apply(preds.p, 2, PI, prob = 0.9)

# Generate an empty plot frame with good axes
plot(0, 0, type = "n", xaxt = "n",
     xlab = "prosoc_left/condition",
     ylab = "proportion pulled left",
     xlim = c(1, 4), ylim = c(0, 1))
axis(1, at = 1:4, labels = c("0/0", "1/0", "0/1", "1/1"))

# Plot raw data, one trend for each of 7 individual chimpanzees using "by()"
p <- by(d$pulled_left,
        list(d$prosoc_left, d$condition, d$actor), mean)
for (chimp in 1:7)
  lines(1:4, as.vector(p[, , chimp]), 
        col = rangi2, lwd = 1.5)

# Superimpose posterior predictions
lines(1:4, preds.p.mean)
shade(preds.p.PI, 1:4)







## AGGREGATED BINOMIAL GLM ####


### Snow goose color binomial models

# Make hypothetical snow geese data in aggregated binomial format
geese <- data.frame(
  blue_geese = c(215, 84, 7),
  total_geese = c(500, 300, 25),
  study_site = c(1, 2, 3)
)

# Add on a variable indicating the proportion of blue
# morphs at each study site
geese$prop_blue <- geese$blue_geese / geese$total_geese


# Generate dummy variables for site affiliation
geese$site_B <- ifelse(geese$study_site == 2, 1, 0)
geese$site_C <- ifelse(geese$study_site == 3, 1, 0)


### Fit a binomial GLM ####
### using site to predict the probability of a goose being the blue morph
m13.4 <- ulam(
  data = geese,
  alist(
    blue_geese ~ dbinom(size = total_geese, prob = p),
    logit(p) <- a + b_site_B * site_B + b_site_C * site_C,
    a ~ dnorm(0, 2),
    b_site_B ~ dnorm(0, 1),
    b_site_C ~ dnorm(0, 1)
  ), 
  chains = 4
)


## Or using index coding
# m13.4 <- ulam(
#   data = geese,
#   alist(
#     blue_geese ~ dbinom(size = total_geese, prob = p),
#     logit(p) <- a[study_site],
#     a[study_site] ~ dnorm(0, 2)
#   ), 
#   chains = 4
# )


traceplot(m13.4)
precis(m13.4, depth = 2, prob = 0.97)


# See what the priors imply about probability of being a blue goose
prior <- extract.prior(m13.4)
p <- logistic(prior$a)
dev.off()
dens(p) # If this doesn't look right, change intercept prior and redo




### Visualize model inference ####
# Various options below, be sure you understand them

# Extract samples from the model posterior
post <- extract.samples(m13.4, n = 10000)

# Show the posterior distribution of the intercept
# parameter (on the log-odds scale), which corresponds
# to study site A
dens(
  post$a, 
  xlab = "Intercept parameter (log-odds scale)"
)
# Show the posterior distribution of the implied 
# probability of blue morphs at study site A
dens(
  logistic(post$a),
  xlab = "Implied probability of a blue goose"
)


# Plot the implied probability of a goose being blue by 
# plotting posterior parameter samples transformed through
# the logistic function

# site A (intercept or reference category)
dens(logistic(post$a), xlim = c(0, 0.5),
     xlab = "Implied probability of a blue goose")
# site B
dens(logistic(post$a + post$b_site_B), 
     add = TRUE, col = "blue")
# site C
dens(logistic(post$a + post$b_site_C), 
     add = TRUE, col = "green")


# You can do this same type of prediction plot 
# using "link()"

counterfactual.siteA <- data.frame(site_B = 0, site_C = 0)
counterfactual.siteB <- data.frame(site_B = 1, site_C = 0)
counterfactual.siteC <- data.frame(site_B = 0, site_C = 1)

probs.siteA <- link(m13.4, data = counterfactual.siteA)
probs.siteB <- link(m13.4, data = counterfactual.siteB)
probs.siteC <- link(m13.4, data = counterfactual.siteC)

dens(probs.siteA, xlim = c(0, 0.5),
     xlab = "Implied probability of a blue goose")
dens(probs.siteB, 
     add = TRUE, col = "blue")
dens(probs.siteC, 
     add = TRUE, col = "green")




