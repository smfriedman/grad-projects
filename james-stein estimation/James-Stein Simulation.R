library(tidyverse)

# set up: mu_i ~ N(M,A), x_i ~ N(mu_i, 1)

# params
M = 5  # mean of mu prior
A = 3  # variance of mu prior
N = 50

# generate data
set.seed(15)
mu = rnorm(mean=M, sd=sqrt(A), n=N)
x  = sapply(mu, function(mean) rnorm(mean=mean, sd=1, n=1))

# expected total square error, i.e. risk
B = A/(A+1)
risk.mle   = N
risk.bayes = N * B
risk.js    = N * B + 3 * (1 - B)

# MLE of mu's
mu.hat.mle   = x
sse.mle = sum( (mu.hat.mle - mu)^2 )

# Bayes estimator 
# can only do because we know M, A
mu.hat.bayes = M + B * (x - M)
sse.bayes    = sum( (mu.hat.bayes - mu)^2 )

# JS estimator of mu's
M.hat = mean(x) # unbiased estimator of M
S     = sum( (x - mean(x))^2 )
B.hat = 1 - (N - 3) / S

mu.hat.js = M.hat + B.hat * (x - M.hat)
sse.js    = sum( (mu.hat.js - mu)^2 )

# summarize error
summary = data.frame(
  "Estimator"    = c("MLE",    "Bayes",    "JS"),
  "Expected Total Sq Err (Risk)" = c(risk.mle, risk.bayes, risk.js),
  "Realized Total Sq Err" = c(sse.mle,  sse.bayes,  sse.js)
)
summary

# JS shrniks MLE value by a factor of B.hat toward mu.hat
# Let's try shrinking to an arbitrary contant, 0, by a small amount and compare to MLE
arb.shrinkage.target = 0
arb.shrinkage.factor = 0.99

mu.hat.arb  = arb.shrinkage.target + arb.shrinkage.factor * (x - arb.shrinkage.target)
sse.arb     = sum( (mu.hat.arb - mu)^2 )

cat("Total square error of MLE is ", sse.mle, ", compared with total square error of small shrinkage toward arbitrary constant ",
    sse.arb, sep = "")

# Visualize
mu.df = data.frame(
  "Index"          = 1:N,
  "Truth"          = mu,
  "JS"             = mu.hat.js,
  "Arb.Shrinkage"  = mu.hat.arb,
  "MLE"            = mu.hat.mle
)

# Plot Ideas borrowed from [1] (see footnote)

# Plot MLE, JS, and Truth
mu.df %>%
  gather(type, value, c(2,3,5)) %>%
  mutate(type = factor(type, levels = c("Truth","JS","MLE"))) %>%
  arrange(Index, type) %>% 
  ggplot(aes(x=value, y=type)) +
    geom_point(color="black") +
    geom_path(aes(group=Index),lty=2,color="grey") +
    ggtitle("MLE vs. JS") + 
    xlab("Estimated/True Params") +
    ylab("Estimator/Truth") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) 

# Plot MLE, Small Shrinkage to 0, and Truth
mu.df %>%
  gather(type, value, c(2,4,5)) %>%
  mutate(type = factor(type, levels = c("Truth","Arb.Shrinkage","MLE"))) %>%
  arrange(Index, type) %>% 
  ggplot(aes(x=value, y=type)) +
    geom_point(color="black") +
    geom_path(aes(group=Index),lty=2,color="grey") +
    ggtitle("MLE vs. Small Downward Shrinkage") + 
    xlab("Estimated/True Params") +
    ylab("Estimator/Truth") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))



# What about repeated trials?
set.seed(15)
runs = 1000
SSEs = as.data.frame(matrix(nrow=1000, ncol=2))
colnames(SSEs) = c("MLE", "JS")
for(i in 1:runs){
  # generate mu and sample x
  mu = rnorm(mean=M, sd=sqrt(A), n=N)
  x  = sapply(mu, function(mean) rnorm(mean=mean, sd=1, n=1))
  
  # MLE of mu's
  mu.hat.mle   = x
  sse.mle = sum( (mu.hat.mle - mu)^2 )
  
  # JS estimator of mu's
  M.hat = mean(x) # unbiased estimator of M
  S     = sum( (x - mean(x))^2 )
  B.hat = 1 - (N - 3) / S
  
  mu.hat.js = M.hat + B.hat * (x - M.hat)
  sse.js    = sum( (mu.hat.js - mu)^2 )
  
  # store
  SSEs[i,"MLE"] = sse.mle
  SSEs[i,"JS"] = sse.js
}

# plot
SSEs %>%
  gather(type, value) %>%
  mutate(type = factor(type, levels = c("MLE", "JS"))) %>%
  ggplot(aes(x=value, color=type, fill=type)) +
    geom_histogram(position="dodge") +
    ggtitle("MLE vs. JS SSEs Over 1000 Trials") + 
    xlab("Total Square Error") +
    ylab("Count") + 
    labs(color = "Estimator", fill="Estimator") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) 
