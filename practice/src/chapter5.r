library(rethinking)
library(dagitty)

# M1
# X -> Z <- Y
# X -> Y
N <- 100
# season of the year
X <- rnorm(N)
# quantity of alcohol drunk
Y <- rnorm(N,X)
# spectators in a soccer game
Z <- rnorm(N,X)

m1.d <- data.frame(Z,Y,X)
# pairs(m1.d)
# or equivalently:
# pairs(~Z + X + Y)

m1.post.X <- quap(
  alist(
    Z ~ dnorm( mu , sigma ) ,
    mu <- a + bX*X ,
    a ~ dnorm( 0 , 1 ) ,
    bX ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )
  ),
  data=m1.d
)
precis(m1.post.X)

m1.post.Y <- quap(
  alist(
    Z ~ dnorm( mu , sigma ) ,
    mu <- a + bY*Y ,
    a ~ dnorm( 0 , 1 ) ,
    bY ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )
  ),
  data=m1.d
)
precis(m1.post.Y)

m1.post.XY <- quap(
  alist(
    Z ~ dnorm( mu , sigma ) ,
    mu <- a + bY*Y + bX*X,
    a ~ dnorm( 0 , 1 ) ,
    bX ~ dnorm( 0 , 1 ) ,
    bY ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )
  ),
  data=m1.d
)
precis(m1.post.XY)

# plot( coeftab(m1.post.Y,m1.post.X,m1.post.XY), par=c("bX","bY") )

# M2
# X <- U -> Y
# X -> Z <- Y
N <- 100
# luck
U <- rnorm(N)
# management
X <- rnorm(U)
# product
Y <- rnorm(N,U)
# success of a company
Z <- rnorm(N,X-Y)

m2.d <- data.frame(Z,Y,X)
# pairs(m2.d)

m2.post.X <- quap(
  alist(
    Z ~ dnorm( mu , sigma ) ,
    mu <- a + bX*X ,
    a ~ dnorm( 0 , 1 ) ,
    bX ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )
  ),
  data=m2.d
)
precis(m2.post.X)

m2.post.Y <- quap(
  alist(
    Z ~ dnorm( mu , sigma ) ,
    mu <- a + bY*Y ,
    a ~ dnorm( 0 , 1 ) ,
    bY ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )
  ),
  data=m2.d
)
precis(m2.post.Y)

m2.post.XY <- quap(
  alist(
    Z ~ dnorm( mu , sigma ) ,
    mu <- a + bY*Y + bX*X,
    a ~ dnorm( 0 , 1 ) ,
    bX ~ dnorm( 0 , 1 ) ,
    bY ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )
  ),
  data=m2.d
)
precis(m2.post.XY)

# plot( coeftab(m2.post.Y,m2.post.X,m2.post.XY), par=c("bX","bY") )


# M3
data(WaffleDivorce)
m3.d <- WaffleDivorce
m3.d$D <- standardize(m3.d$Divorce)
m3.d$M <- standardize(m3.d$Marriage)
m3.d$A <- standardize(m3.d$MedianAgeMarriage)

m3.dag <- dagitty("dag{ D -> M }")
# m3.post <- quap(
#   alist(
#     M ~ dnorm( mu , sigma ) ,
#     mu <- a + bD * D, # Error in bD * D : non-numeric argument to binary operator: WHY ?!
#     a ~ dnorm( 0 , 1 ) ,
#     bD ~ dnorm( 0 , 1 ) ,
#     sigma ~ dexp( 1 )
#   ),
#   data=m3.d
# )
# precis(m3.post)

# set.seed(11)
# m3.prior <- extract.prior(m3.post)
# m3.mu <- link( m3.post , post=m3.prior , data=list( A=c(-2,2) ) )
# plot( NULL , xlim=c(-2,2) , ylim=c(-2,2) )
# for ( i in 1:50 ) lines( c(-2,2) , m3.mu[i,] , col=col.alpha("black",0.4) )

# M4
data(WaffleDivorce)
m4.d <- WaffleDivorce
m4.d$D <- standardize(m3.d$Divorce)
m4.d$M <- standardize(m3.d$Marriage)
m4.d$A <- standardize(m3.d$MedianAgeMarriage)
m4.d$LatterDaySaintsPercentage <- c(0.0077, 0.0453, 0.0610, 0.0104, 0.0194, 0.0270, 0.0044, 0.0057, 0.0041, 0.0075, 0.0082, 0.0520, 0.2623, 0.0045, 0.0067, 0.0090, 0.0130, 0.0079, 0.0064, 0.0082, 0.0072, 0.0040, 0.0045, 0.0059, 0.0073, 0.0116, 0.0480, 0.0130, 0.0065, 0.0037, 0.0333, 0.0041, 0.0084, 0.0149, 0.0053, 0.0122, 0.0372, 0.0040, 0.0039, 0.0081, 0.0122, 0.0076, 0.0125, 0.6739, 0.0074, 0.0113, 0.0390, 0.0093, 0.0046, 0.1161)
logLDS <- log(m4.d$LatterDaySaintsPercentage)
m4.d$LDS <- standardize(logLDS)


m4.post <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + bA * A + bM * M + bLDS + LDS ,
        a ~ dnorm( 0 , 0.2 ) ,
        bA ~ dnorm( 0 , 0.5 ) ,
        bM ~ dnorm( 0 , 0.5 ) ,
        bLDS ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ), data = m4.d )

# H1
h1.dag <- dagitty("dag{ M -> A -> D }")
impliedConditionalIndependencies(h1.dag)
data(WaffleDivorce)
d <- WaffleDivorce
d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)
h1.d <- data.frame(D=d$D,A=d$A,M=d$M)
pairs(h1.d)
# A raise in M _could_ be the cause of a drop i A

# H2
data(WaffleDivorce)
h2.D <- standardize(WaffleDivorce$Divorce)
h2.M <- standardize(WaffleDivorce$Marriage)
h2.A <- standardize(WaffleDivorce$MedianAgeMarriage)
h2.d <- data.frame(D=h2.D,A=h2.A,M=h2.M)
h2.post <- quap(
  alist(
    # M -> A
    A ~ dnorm(mu,sigma),
    mu <- alpha + M * bM,
    alpha ~ dnorm(0,0.3),
    bM ~ dnorm(0,0.5),
    sigma ~ dexp(1),
    # A -> D
    D ~ dnorm(mu,sigma),
    mu <- alpha + A*bA,
    alpha ~ dnorm(0,0.3),
    bA ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ),
  data=h2.d
)
# Halve M by halving the range between its min and max values
h2.M_seq <- seq(from=min(h2.d$M/2),to=max(h2.d$M/2),length.out=30)
h2.sim_data <- data.frame(M=h2.M_seq)
# Run a counterfactual on A and D
h2.sim <- sim(h2.post,data=h2.sim_data,vars=c("A","D"))
par(mfrow=c(2,1))
plot(
  x=h2.sim_data$M,y=colMeans(h2.sim$D),
  ylim=c(-2,2),type="l",
  xlab="manipulated M",ylab="counterfactual D"
)
shade(apply(h2.sim$D,2,PI),h2.sim_data$M)
mtext("Total counterfactual effect of M on D")
plot(
  x=h2.sim_data$M,y=colMeans(h2.sim$A),
  ylim=c(-2,2),type="l",
  xlab="manipulated M",ylab="counterfactual A"
)
shade(apply(h2.sim$A,2,PI),h2.sim_data$M)
mtext("Counterfactual effect of M on A" )
par(mfrow=c(1,1))
