library(e1071)

cont.run <- function(burn.in, reps, n, d, l, s) {
  tr <- rep(0, n)
  r <- rep(0, reps)
  for (i in 1:reps) {
    sig <- rnorm(1, 0, d)
    mul <- 1
    if (sig < 0) {
      sig <- -sig
      mul <- -1
    }
    r[i] <- mul * sum(sig > tr) / (l * n)
    tr[runif(n) < s] <- abs(r[i])
  }
  return(kurtosis(r[burn.in:reps]))
}

system.time(replicate(10,
                      cont.run(1000, 10000, 1000, 0.005, 10.0, 0.01)))