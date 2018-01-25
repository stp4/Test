#install.packages("devtools")
#This package requires a working instalation of JAGS. 

library(devtools)
#install_github("rasmusab/bayesian_first_aid")
library(BayesianFirstAid)
n_r_l <- c(43,275)
n_res <- c(170,1454)

prop.test(n_r_l, n_res)

fit<-bayes.prop.test(n_r_l, n_res)
plot(fit)


fit<-bayes.binom.test(c(300, 225))

plot(fit)
