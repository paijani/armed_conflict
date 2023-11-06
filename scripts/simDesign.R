## set simulation parameters
n <- 100 # sample size
pz <- 0.2 # probability of Z = 1
alpha0 <- 0 # logit probability of x = 1 in non-smokers (z = 0)
alpha1 <- 1 # log odds ratio of x = 1 in smokers (z = 1) vs non-smokers
beta0 <- -3 # logit probability of y = 1 in non-coffee drinkers (x = 0) and non-smokers (z = 0)
beta1 <- 0
beta2 <- 2
simnum <- 1000 # number of iterations to run in the simulation
# empty vectors to store the values
unadj.p <- adj.p <- rep(NA, simnum)
for(s in 1:simnum){
  ## generate confounder Z from a binomial distribution
  z <- rbinom(n, size = 1, prob = pz)
  ## compute probability of observing X = 1 from the inverse logit function
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  ## randomly generate binary variable X from the above probability
  x <- rbinom(n, size = 1, prob = px)
  ## repeat above to randomly generate binary variable Y
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(n, size = 1, prob = py)
  ## combine three random variables into a data frame
  dat <- data.frame(lung = y, coffee = x, smoke = z)
  ## fit unadjusted logistic regression model
  unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
  unadj.coef <- summary(unadj.mod)$coef
  ## fit adjusted logistic regression model
  adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  adj.coef <- summary(adj.mod)$coef
  ## save p-values for coffee from both models in a vector
  unadj.p[s] <- unadj.coef[2,4]
  adj.p[s] <- adj.coef[2,4]
  ## show simulation progress
  print(s)
}
## calculate the type 1 error rate from each model
mean(ifelse(unadj.p < 0.05, 1, 0))
mean(ifelse(adj.p < 0.05, 1, 0))