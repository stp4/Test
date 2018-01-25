#  install.packages("devtools")


#  Download JAGS-4.3.0.exe
#  devtools::install_github("rasmusab/bayesian_first_aid")
library(BayesianFirstAid)
set.seed(1)
#bayes.binom.test(c(39, 25))
fit <- bayes.binom.test(x = 257, n = 305, 0.842623)
fit
summary(fit)
 plot(fit)


binom.test(257, n = 305)
