# p-values in lecture 07: https://youtu.be/0Jc6Kgw5qc0?t=2519

library(rethinking)
library(dagitty)

# M1
m1.dag <- dagitty( "dag {
    U [unobserved]
    X -> Y
    X <- U <- A -> C -> Y
    U -> B <- C
    V [unobserved]
    C <- V -> Y
}")
coordinates(m1.dag) <- list(y=c(A=0,U=1,C=1,B=2,X=3,Y=3,V=2),x=c(U=0,X=0,A=1,B=1,V=3,C=2,Y=2))
# drawdag(m1.dag)
m1.adjSets <- adjustmentSets( m1.dag , exposure="X" , outcome="Y" )
# returns { A } : adding V which is a fork adds a backdoor on Y which is closed
# as long as we don't condition on C.

# M2
m2.dag <- dagitty("dag {
  X -> Z -> Y
}")
m2.N <- 1e3
m2.X <- rnorm(m2.N)
m2.Z <- rnorm(m2.N,m2.X)
m2.Y <- rnorm(m2.N,m2.Z)
m2.d <- data.frame(X=m2.X,Y=m2.Y,Z=m2.Z)
m2.postXZ <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + bZ * Z + bX * X,
    a ~ dnorm(0,1),
    bZ ~ dnorm(0,1),
    bX ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),
  data=m2.d
)
m2.samplesXZ <- extract.samples(m2.postXZ)

m2.postX <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + bX * X,
    a ~ dnorm(0,1),
    bX ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),
  data=m2.d
)
m2.postZ <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + bZ * Z,
    a ~ dnorm(0,1),
    bZ ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),
  data=m2.d
)

# X and Y are highely correlated, i.e. they contain
# same information
# cor(m2.X,m2.Z)
# The value of bX when used in a MRT is way off because
# because X is highly (positively) correlated to Z.
# The difference with multicollinearity seems to be that
# in this case X and Y "go together" instead of in opposite
# directions. When used in a multivariate regression the
# information in bX totally disappears.
# precis(m2.postXZ)
# precis(m2.postX)
# precis(m2.postZ)
# plot(coeftab(m2.postXZ,m2.postX,m2.postZ),par=c("bZ","bX"))

# M3
m3.dag_1 <- dagitty( "dag {
  X -> Y
  X <- Z -> Y
  Z <- A -> Y
}")
coordinates(m3.dag_1) <- list(x=c(X=0,Z=1,A=2,Y=2),y=c(X=1,Z=0,A=0,Y=1))
# m3.dagitty_1 <- drawdag(m3.dag_1)
m3.adjSets_1 <- adjustmentSets( m3.dag_1 , exposure="X" , outcome="Y" )

m3.dag_2 <- dagitty( "dag {
  Z <- X -> Y
  Z -> Y
  Z <- A -> Y
}")
coordinates(m3.dag_2) <- list(x=c(X=0,Z=1,A=2,Y=2),y=c(X=1,Z=0,A=0,Y=1))
# m3.dagitty_2 <- drawdag(m3.dag_2)
m3.adjSets_2 <- adjustmentSets( m3.dag_2 , exposure="X" , outcome="Y" )

m3.dag_3 <- dagitty( "dag {
  Z <- X -> Y
  Z <- Y
  Z <- A -> X
}")
coordinates(m3.dag_3) <- list(x=c(X=0,Z=1,A=0,Y=2),y=c(X=1,Z=0,A=0,Y=1))
# m3.dagitty_3 <- drawdag(m3.dag_3)
m3.adjSets_3 <- adjustmentSets( m3.dag_3 , exposure="X" , outcome="Y" )

m3.dag_4 <- dagitty( "dag {
  Z <- X -> Y
  Y <- Z <- A -> X
}")
coordinates(m3.dag_4) <- list(x=c(X=0,Z=1,A=0,Y=2),y=c(X=1,Z=0,A=0,Y=1))
# m3.dagitty_4 <- drawdag(m3.dag_4)
m3.adjSets_4 <- adjustmentSets( m3.dag_4 , exposure="X" , outcome="Y" )

# H1
data(WaffleDivorce)
h1.d <- WaffleDivorce

# H3
h3.dag <- dagitty("dag {
  A -> F
  W <- F -> S -> W
}")
coordinates(h3.dag) <- list(x=c(F=0,A=1,W=1,S=2),y=c(F=1,A=0,W=2,S=1))
# Paths
# 1) A -> F -> W : open (pipe), condition on F to close
# 2) A -> F -> S -> W
h3.adjSets <- adjustmentSets(h3.dag,exposure="A",outcome="W")
# {}
# UHU?! Why there's no conditioning on F / S ?
data(foxes)
h3.d <- data.frame(
  G <- standardize(foxes$group),
  F <- standardize(foxes$avgfood),
  S <- standardize(foxes$groupsize),
  A <- standardize(foxes$area),
  W <- standardize(foxes$weight)
)
# h3.dcc <- h3.d[with(h3.d, complete.cases(G,F,S,A,W)),]
h3.post <- quap(
  alist(
    # A -> F -> W
    W ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0,.3),
    bA ~ dnorm(0,.3),
    sigma ~ dexp(1)
  ),
  data=h3.d
)
# prior predictive simulation
h3.prior <- extract.prior(h3.post)
h3.sigmas <- c(-2,2)
# The list needs to have a key equal to one of the variables
# in the data frame
h3.mu <- link(h3.post,post=h3.prior,data=list(A=h3.sigmas))
# plot(NULL,xlim=h3.sigmas,ylim=h3.sigmas) ; for (i in 1:50) lines(h3.sigmas,h3.mu[i,],col=col.alpha("black",.3))

h3.sim_data <- data.frame(A=h3.sigmas)
h3.sim <- sim(h3.post,data=h3.sim_data,vars=c("W"))
# plot(h3.d$W ~ h3.d$A,col=col.alpha(rangi2,.5),xlab="Manipulated area (std)",ylab="Counterfactual weight (std)") ; lines(h3.sigmas,apply(h3.sim,2,mean)) ; shade(apply(h3.sim,2,PI,prob=.95),h3.sim_data$A)
# plot(h3.sim_data$A,colMeans(h3.sim),ylim=h3.sigmas,type="l",xlab="Manipulated A",ylab="Counterfactual W") ; shade(apply(h3.sim,2,PI),h3.sim_data$A)
# The counterfactal is a straight line which means that
# changing the area won't impact weight much

# H4
# {}
h4.adjSets <- adjustmentSets(h3.dag,exposure="F",outcome="W")
h4.post <- quap(
  alist(
    # F -> S -> W
    W ~ dnorm(mu, sigma),
    mu <- a + bF * F,
    a ~ dnorm(0,.3),
    bF ~ dnorm(0,.4),
    sigma ~ dexp(1)
  ),
  data=h3.d
)
# prior predictive simulation
h4.prior <- extract.prior(h4.post)
h4.sigmas <- c(-2,2)
h4.mu <- link(h4.post,post=h4.prior,data=list(F=h4.sigmas))
# plot(NULL,xlim=h4.sigmas,ylim=h4.sigmas) ; for (i in 1:50) lines(h4.sigmas,h4.mu[i,],col=col.alpha("black",.5))

h4.sim_data <- data.frame(A=h4.sigmas)
h4.sim <- sim(h4.post,data=h4.sim_data,vars=c("W"))
# plot(h4.d$W ~ h4.d$A,col=col.alpha(rangi2,.5),xlab="Manipulated avg. food (std)",ylab="Counterfactual weight (std)") ; lines(h4.sigmas,apply(h4.sim,2,mean)) ; shade(apply(h4.sim,2,PI,prob=.95),h4.sim_data$A)
# plot(h4.sim_data$A,colMeans(h4.sim),ylim=h4.sigmas,type="l",xlab="Manipulated A",ylab="Counterfactual W") ; shade(apply(h4.sim,2,PI),h4.sim_data$A)
# The counterfactal is a straight line which means that
# changing the area won't impact weight much

# H5
# { F }
h5.adjSets <- adjustmentSets(h3.dag,exposure="S",outcome="W")
h5.post <- quap(
  alist(
    # F -> S -> W
    W ~ dnorm(mu, sigma),
    mu <- a + bF * F + bS * S,
    a ~ dnorm(0,.4),
    bS ~ dnorm(0,.3),
    bF ~ dnorm(0,.4),
    sigma ~ dexp(1)
  ),
  data=h3.d
)
precis(h5.post)
#        mean   sd  5.5% 94.5%
# a      0.00 0.09 -0.14  0.14
# bS    -0.15 0.09 -0.30 -0.01
# bF     0.00 0.40 -0.64  0.64
# sigma  0.98 0.06  0.88  1.08
# Group size seems to be negatively correlated to weight.
# Given wht we know from H4 and H3 i.e. that area and
# avg food don't seem to be correlated with weight, having
# a bigger group size means each fox in the group has less
# food to eat and therefore cannot grow weight.

# prior predictive simulation
h5.ppsim <- function() {
  h5.prior <- extract.prior(h5.post)
  h5.sigmas <- c(-2,2)
  h5.data <- data.frame(S=mean(h3.d$S),F=h5.sigmas)
  h5.mu <- link(h5.post,post=h5.prior,data=h5.data)
  plot(NULL,xlim=h5.sigmas,ylim=h5.sigmas)
  for (i in 1:50) lines(h5.sigmas,h5.mu[i,],col=col.alpha("black",.5))
}
h5.csim <- function() {
  h5.sim <- sim(h5.post,data=h5.data,vars=c("W"))
  h5.mean <- apply(h5.sim,2,mean)
  h5.PI <- apply(h5.sim,2,PI)
  plot(h3.d$W ~ h3.d$S,col=col.alpha(rangi2,.5),xlab="Manipulated group size (std)",
    ylab="Counterfactual weight (std)")
  lines(h5.sigmas,h5.mean)
  shade(h5.PI,h5.sim_data$A)
}
