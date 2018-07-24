#2016final
#q6 don't know what you are doing...
#q7


#q8
m <- 0.05
a <- 0.05
u <- 1
H <- pnorm((m + a * u)/sqrt(u)) - exp(-2 * a * m) * pnorm((- m + a * u)/sqrt(u))
H

#2017
#6a
p.firm1.default.in.year.1 <- 1/(1 + exp(3 + 2 * -0.15))
p.firm2.default.in.year.1 <- 1/(1 + exp(3 + 2 * -0.12))
p.firm1.default.in.year.1
p.firm2.default.in.year.1
rho <- 0.4
N <- 320000
p.firm1.default.in.year.2 <- NULL
p.firm2.default.in.year.2 <- NULL
p.firm1.default.in.year.3 <- NULL
p.firm2.default.in.year.3 <- NULL
p.both.firm.default.in.year.2 <- NULL
p.both.firm.default.in.year.3 <- NULL
if.firm1.default.in.year.1 <- rep(0,N)
if.firm2.default.in.year.1 <- rep(0,N)
if.firm1.default.in.year.2 <- rep(0,N)
if.firm2.default.in.year.2 <- rep(0,N)
if.firm1.default.in.year.3 <- rep(0,N)
if.firm2.default.in.year.3 <- rep(0,N)
if.both.firm.default.in.year.2 <- rep(0,N)
if.both.firm.default.in.year.3 <- rep(0,N)
random11 <- runif(N)
random21 <- runif(N)
random12 <- runif(N)
random22 <- runif(N)
random13 <- runif(N)
random23 <- runif(N)
for (i in 1:N){
  m.year1 <- rnorm(1,0.1,0.2)
  if.firm1.default.in.year.2 <- rep(0,N)
  r1.year1 <-  m.year1 * sqrt(rho) + sqrt(1 - rho) * rnorm(1,0.1,0.2)
  r2.year1 <-  m.year1 * sqrt(rho) + sqrt(1 - rho) * rnorm(1,0.1,0.2)
  m.year2 <- rnorm(1,0.1,0.2)
  r1.year2 <-  m.year2 * sqrt(rho) + sqrt(1 - rho) * rnorm(1,0.1,0.2)
  r2.year2 <-  m.year2 * sqrt(rho) + sqrt(1 - rho) * rnorm(1,0.1,0.2)
  p.firm1.default.in.year.2[i] <- 1/(1 + exp(3 + 2 * r1.year1))
  p.firm2.default.in.year.2[i] <- 1/(1 + exp(3 + 2 * r2.year1))
  p.both.firm.default.in.year.2[i] <- p.firm1.default.in.year.2[i] * p.firm2.default.in.year.2[i]
  p.firm1.default.in.year.3[i] <- 1/(1 + exp(3 + 2 * r1.year2))
  p.firm2.default.in.year.3[i] <- 1/(1 + exp(3 + 2 * r2.year2))
  p.both.firm.default.in.year.3[i] <- p.firm1.default.in.year.3[i] * p.firm2.default.in.year.3[i]
  if (random11[i] < p.firm1.default.in.year.1) if.firm1.default.in.year.1[i] <- if.firm1.default.in.year.1[i] + 1
  if (random21[i] < p.firm2.default.in.year.1) if.firm2.default.in.year.1[i] <- if.firm2.default.in.year.1[i] + 1
}
for (i in 1:N){
  if (random12[i] < p.firm1.default.in.year.2[i] && if.firm1.default.in.year.1[i] == 0) if.firm1.default.in.year.2[i] <- if.firm1.default.in.year.2[i] + 1
  if (random22[i] < p.firm2.default.in.year.2[i] && if.firm2.default.in.year.1[i] == 0) if.firm2.default.in.year.2[i] <- if.firm2.default.in.year.2[i] + 1
}
for (i in 1:N){
  if (if.firm1.default.in.year.2[i] == 1 && if.firm2.default.in.year.2[i] == 1) if.both.firm.default.in.year.2[i] <- if.both.firm.default.in.year.2[i] + 1
}
for (i in 1:N){
  if (random13[i] < p.firm1.default.in.year.3[i] && if.firm1.default.in.year.1[i] == 0 && if.firm1.default.in.year.2[i] == 0) if.firm1.default.in.year.3[i] <- if.firm1.default.in.year.3[i] + 1
}
sum(if.firm1.default.in.year.1)/N
sum(if.firm2.default.in.year.1)/N
sum(if.firm1.default.in.year.2)/N
sum(if.firm1.default.in.year.3)/N
sum(if.both.firm.default.in.year.2)/N

#7a
m <- 1
u <- 1
probability.of.surviving.past.u <- 1 - 2*pnorm(-m/sqrt(u))
default.probability <- 1 - probability.of.surviving.past.u
default.probability

#7b
no.of.simulation <- 5000
obligor <- rep(10,20)
no.of.steps <- 100
no.of.default <- rep(0,no.of.simulation)
for (k in 1:no.of.simulation){
  m0 <- rep(1,20)
  dm <- rep(1,20)
  mindm <- rep(1,20)
  for (j in 1:no.of.steps){
    rho <- 0.4
    m <- rnorm(1)
    z <- sqrt(rho) * m + sqrt(1 - rho) * rnorm(20)
    for (i in 1:20){
      dm[i] <- dm[i] + z[i] * sqrt(1/no.of.steps)
      mindm[i] <- pmin(dm[i],mindm[i])#
    }
  }
  for (i in 1:20){
    if(mindm[i] < 0) no.of.default[k] <- no.of.default[k] + 1
  }
}
quantile(no.of.default,0.95)

#7c
possible.loss <- NULL
for (k in 1:no.of.simulation){
  m0 <- rep(1,20)
  distance.to.default <- rep(1,20)
  tau <- rep(0,20)
  LGD <- rep(0,20)
  for (j in 1:no.of.steps){
    common.factor <- rnorm(1)
    z <- sqrt(rho) * common.factor + sqrt(1 - rho) * rnorm(20)
    for (i in 1:20){
      distance.to.default[i] <- distance.to.default[i] + z[i] * sqrt(1/no.of.steps)
      if (distance.to.default[i] < 0 && tau[i] == 0){
        tau[i] <- j * 1/no.of.steps
        LGD[i] <- 10 / (1 + exp(sum(distance.to.default)/20 - 1))
      } 
    }
  }
  possible.loss[k] <- sum(LGD)
}
quantile(possible.loss,0.95)