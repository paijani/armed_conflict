#load libraries

library(SimDesign)
library(dplyr)
library(tidyr)
library(ggplot2)

#Design
Design <- createDesign(n = c(50, 500),
                      pz = c(0.2, 0.8),
                      alpha0 = c(-1, 0, 1),
                      alpha1 = c(0, 0.5, 1))
head(Design)

#Generate 

Generate <- function (condition, fixed_objects = NULL ) {
  n <- condition$n
  pz <- condition$pz
  alpha0 <- condition$alpha0
  alpha1 <- condition$alpha1
  beta0 = -3
  beta1 = 0
  beta2 = 2
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
  dat
}

#Analyze
## fit unadjusted logistic regression model
Analyse.unadj <- function(condition, dat, fixed_objects = NULL) {
  mod <- glm(lung ~ coffee, data = dat, family = "binomial")
  beta <- coef(mod)[2]
  pval <- summary(mod)$coef[2,4]
  ret <- c(unadj = unname(pval))
  ret
}

## fit adjusted logistic regression model
Analyse.adj <- function(condition, dat, fixed_objects = NULL) {
  mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  beta <- coef(mod)[2]
  pval <- summary(mod)$coef[2,4]
  ret <- c(adj = unname(pval))
  ret
}

#Summarise
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- EDR(results, alpha = 0.05)
  ret
}

res <- runSimulation(design = Design, replications = 1000,
                         parallel = TRUE, 
                         generate = Generate,
                         analyse = list(Analyse.unadj, Analyse.adj),
                         summarise = Summarise, save_results = TRUE)

head(res)

# Create a plot that summarizes the results

reslong <- res %>%
  pivot_longer(cols = c("unadj", "adj"),
               names_to = "model",
               values_to = "edr")

reslong$n <- factor(reslong$n, levels = c(50,100,500),
                              ordered = TRUE, labels=c("n == 50", "n == 100", "n == 500"))
reslong$alpha0 <- factor(reslong$alpha0, levels = c(-1,0,1),
                         ordered = TRUE, labels=c("alpha[0] == -1", "alpha[0] == 0", "alpha[0] == 1"))


ggplot(reslong, aes(x = alpha1, y = edr, color = model, 
                    linetype = as.factor(pz), shape = as.factor(pz), 
                    group = interaction(model, pz))) +
  geom_point(size = 2) + 
  geom_line(linewidth = 1) + 
  scale_color_discrete(labels=c('Adjusted', 'Unadjusted')) + 
  facet_grid(n ~ alpha0, labeller = label_parsed) + 
  theme_bw(base_size = 12) + 
  geom_hline(yintercept = 0.05, color = "darkgray", linetype = "longdash") + 
  labs(y = "Type I error", x = bquote(alpha[1]~values), color = "Model", shape = "Pr(Z=1)", linetype = "Pr(Z=1)", 
       title = "Empirical type I error rates for adjusted and unadjusted models \nfrom each simulation scenario (gray dashed horizontal line at 0.05) ")
