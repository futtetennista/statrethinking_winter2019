library(rethinking)

plot_prior <- function(alpha,beta,data,N=1e2) {
  plot( NULL , xlim=range(data$weight) , ylim=c(-100,400) ,
      xlab="weight" , ylab="height" )
  abline( h=0 , lty=2 )
  abline( h=272 , lty=1 , lwd=0.5 )
  xbar <- mean(data$weight)
  for ( i in 1:N ) {
    curve(
      alpha[i] + beta[i] * (x - xbar) ,
      from=min(data$weight) ,
      to=max(data$weight) ,
      add=TRUE ,
      col=col.alpha("black",0.2)
    )
  }
}

# M1
m1.sample_mu <- rnorm(1e4,mean=0,sd=10)
m1.sample_sigma <- rexp(1e4,rate=1)
m1.sample_prior <- rnorm(1e4,mean=m1.sample_mu,sd=m1.sample_sigma)
# dens(m1.sample_prior,norm.comp=T)
m2.sol <- m1.sample_prior

# M2
# m2.sol <- quap(
#   alist(
#     likelihood ~ dnorm(mu,sigma),
#     mu ~ dnorm(0,10),
#     sigma ~ dexp(1)
#   ),
#   data=list())


# M4
data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 & d$age <=30, ]

m3.sample_sigma_unif <- runif(1e4,0,1)
m3.sample_mu <- rnorm(1e4,mean=140,sd=20)
m3.sample_prior_unif <- rnorm(1e4,mean=m3.sample_mu,sd=m3.sample_sigma_unif)
# dens(m3.sample_prior_unif,norm.comp=F)
m3.sample_mu_lin <- rnorm(1e4,mean=140,sd=20) + rlnorm(1e4,0,1)
m3.sample_sigma_exp <- rexp(1e4,rate=1)
m3.sample_prior_exp <- rnorm(1e4,mean=m3.sample_mu_lin,sd=m3.sample_sigma_exp)
# dens(m3.sample_prior_exp,norm.comp=F,col="blue",add=T)
# m3.sol <- quap(
#   alist(
#     height ~ dnorm(mu,sigma),
#     mu <- a + b * (year - ?),
#     a <- dnorm(178,20),
#     b <- dlnorm(0,1),
#     sigma ~ dexp(1)
#   ),
#   data=d2)


# M7
# Removing xbar causes an error:
# Error in quap(alist(height ~ dnorm(mu, sigma), mu <- a + b * weight, a ~  :
#   non-finite finite-difference value [2]
# Start values for parameters may be too far from MAP.
# Try better priors or use explicit start values.
# If you sampled random start values, just trying again may work.
# Start values used in this attempt:
# a = 175.460481825845
# b = 1.23156396084101
# sigma = 41.7062222608365
data(Howell1); d <- Howell1; m7.data <- d[ d$age >= 18, ]
set.seed(2971)

# MAKE SURE FITTING THE MODEL IS NOT DONE INSIDE A FUNCTION!
# R behaves unexpectedly in that case, the linear regression
# is messed up for some reason, at least judging by the plot
# of the mean line. This doesn't happen if `quap` is run outside
# a function. Not sure why that's the case.
m7.post <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b * weight ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ),
  data=m7.data
)

m4.3.xbar <- mean(m7.data$weight)
m4.3.post <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b * ( weight - m4.3.xbar ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ),
  data=m7.data
)

m7.plot <- function() {
  plot( height ~ weight , data=m7.data , col=rangi2 )

  m7.samples <- extract.samples(m7.post)
  m7.a_map <- mean(m7.samples$a)
  m7.b_map <- mean(m7.samples$b)
  curve(m7.a_map + m7.b_map * x , add=TRUE, col="blue")

  m4.3.post.samples <- extract.samples(m4.3.post)
  m4.3.post.a_map <- mean(m4.3.post.samples$a)
  m4.3.post.b_map <- mean(m4.3.post.samples$b)
  curve(m4.3.post.a_map + m4.3.post.b_map * (x - m4.3.xbar), add=TRUE,col="red")
}

# H1
h1.post.samples <- extract.samples( m4.3.post )
h1.ws <- c(46.95,43.72,64.78,32.59,54.63)
xbar <- mean(d2$weight)
h1.mus <- vector(mode="list")
h1.pi89 <- vector(mode="list")
for (i in 1:length(h1.ws)) {
  h1.mus[[i]] <- h1.post.samples$a + h1.post.samples$b * ( h1.ws[i] - xbar )
  h1.pi89[[i]] <- PI( h1.mus[[i]], prob=0.89 )
}

h1.plot <- function (i) {
  dens( h1.mus[[i]] , col=rangi2 , lwd=2 , xlab=concat("mu|weight=",h1.ws[i]) )
}
h1.means <- Map(mean,h1.mus)
for (i in 1:length(h1.means)) {
  names(h1.means)[i] <- concat(h1.ws[i],"kg")
  names(h1.pi89)[i] <- concat(h1.ws[i],"kg")
}

# or better
data(Howell1); d <- Howell1
# visualise prior
# set.seed(2971)
h1.a <- rnorm(1e2,140,40)
h1.b <- rlnorm(1e2,0,1)
# plot_prior(h1.a,h1.b,d,"a ~ norm(150,30), b ~ lnorm(0,1)")
# fit the model
xbar <- mean(d$weight)
h1.post <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b * ( weight - xbar ) ,
        a ~ dnorm( 150 , 30 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d )

# Plot point-estimate of the regression
plot_point_est <- function(post,data) {
  plot( height ~ weight , data=data , col=rangi2 )
  samples <- extract.samples( post )
  a_map <- mean(samples$a)
  b_map <- mean(samples$b)
  curve( a_map + b_map*(x - xbar) , add=T , col=col.alpha("red",0.4))
}

# Extract N samples from the posterior distr and plot a regression
plot_samples <- function(post,data,N=20) {
  plot( height ~ weight , data=data , col=rangi2 )
  samples <- extract.samples( post , n=N )
  mtext(concat("N=",N))
  xbar <- mean(data$weight)
  for ( i in 1:N ) {
    curve(
      samples$a[i] + samples$b[i] * (x - xbar) ,
      col=col.alpha("black",0.3) ,
      add=TRUE
    )
  }
}

#
h1.ws <- c(46.95,43.72,64.78,32.59,54.63)
# use link to compute mu for each sample from posterior and for each weight
h1.mu <- link( h1.post , data=data.frame(weight=h1.ws) )
h1.mu.mean <- apply(h1.mu,2,mean)
h1.mu.pi89 <- apply(h1.mu,2,PI,prob=.89)
# plot( height ~ weight , data=d , col=col.alpha(rangi2,.5) ) ; lines(h1.ws,h1.mu.mean) ; shade(h1.mu.pi89,h1.ws) ; points(h1.ws,h1.mu.mean,pch="+")
h1.res <- data.frame(
  individual = 1:length(h1.ws),
  weight = h1.ws,
  height = h1.mu.mean,
  low = h1.mu.pi89[1,],
  hi = h1.mu.pi89[2,]
)

# H2
h2.d <- d[d$age < 18,]

# h2.mu <- 100
# h2.sigma <- 10
# h2.age <- 10
# h2.sample_alpha <- rnorm(1e4, h2.mu , h2.sigma )
# h2.sample_beta  <- dlnorm(1e4, 0 , 1 )
# h2.sample_mean <- h2.sample_alpha + h2.sample_beta * (h2.age - h2.xbar)
# # using an exp distr. for sigma has the efx of concentrating the density
# # around the mean, having much slimmer tails
# h2.sample_sigma <- rexp(1e4, rate=.1 )
# h2.sample_prior <- rnorm(1e4,mean=h2.sample_mean,sd=h2.sample_sigma)
# dens(h2.sample_prior,norm.comp=T,col="blue",add=F)
# h2.sample_alpha <- rnorm(1e4, h2.mu , h2.sigma )
# h2.sample_beta  <- dlnorm(1e4, 0 , 1 )
# h2.sample_mean <- h2.sample_alpha + h2.sample_beta * (h2.age - h2.xbar)
# h2.sample_sigma <- runif(1e4, 0 , 50 )
# h2.sample_prior <- rnorm(1e4,mean=h2.sample_mean,sd=h2.sample_sigma)
# dens(h2.sample_prior,norm.comp=T,col="red",add=T)

# Plot the prior to get an understanding of what it's saying
# a <- rnorm(1e4,140,30) ; b <- rlnorm(1e4,0,.3) ; plot_prior(a,b,h2.d)
h2.xbar <- mean(h2.d$weight)
h2.post <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b * ( weight - h2.xbar ) ,
    a ~ dnorm( 140 , 30 ) ,
    b ~ dlnorm( 0 , .3 ) ,
    sigma ~ dunif( 0 , 50 )
  ),
  data=h2.d
)
h2.res.a <- precis(h2.post)
#         mean   sd   5.5%  94.5%
# a     108.33 0.61 107.36 109.31
# b       2.70 0.07   2.59   2.81
# sigma   8.44 0.43   7.75   9.13
# Looking at b: for every kg there's an increase of 2.7cm
# therefore we expect an increase of ~27cm for every 10kg
# increase in weight.

# (b) The data doens't seem to fit a linear regression very well.
# The height has seems to have some more curvy relationship to the
# weight for this dataset. The assumption that height is growing
# linearly with height for underaged people doesn't seem to hold.
h2.weight_range <- seq(from=0,to=ceiling(max(h2.d$weight)) + 10,by=1)
h2.mu <- link(h2.post,data=data.frame(weight=h2.weight_range))
h2.mean <- apply(h2.mu,2,mean)
h2.pi89 <- apply(h2.mu,2,PI,prob=0.89)
h2.plot <- function() {
  plot(height ~ weight,data=h2.d,col=col.alpha(rangi2,0.3))
  lines(h2.weight_range,h2.mean)
  shade(h2.pi89, h2.weight_range)
}

# H3
h3.d <- d
h3.prior.plot <- function() {
  a <- rnorm(1e4,165,30)
  b <- rlnorm(1e4,0,.5)
  plot_prior(a,b,h3.d)
}
h3.xbar <- mean(h3.d$weight)
h3.post <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b * log(weight) ,
    # mu <- a + b * (log(weight) - log(xbar)) ,
    a ~ dnorm( 165 , 30 ) ,
    b ~ dlnorm( 0 , .5 ) ,
    sigma ~ dunif( 0 , 50 )
  ),
  data=h3.d
)

h3.plot <- function() {
  plot(height ~ weight,data=h3.d,col=col.alpha(rangi2,.5))
  for (i in length(h3.weights)) {
    xs <- head(rep(1e2,h3.weights[i]))
    ys <- head(h3.sim_heights[,i])
    points(xs,ys)
  }

  h3.weights <- seq(from=0,to=ceiling(max(d$weight)),by=1)
  # 97% interval for the mean
  h3.mu <- link(h3.post,data=data.frame(weight=h3.weights))
  h3.mean <- apply(h3.mu,2,mean)
  h3.mean_pi97 <- apply(h3.mu,2,PI,prob=.97)
  lines(h3.weights,h3.mean)
  shade(h3.mean_pi97,h3.weights)

  # 97% PI for predicted weight
  h3.sim_heights <- sim(h3.post,data=list(weight=h3.weights))
  h3.height_pi97 <- apply(h3.sim_heights,2,PI,prob=.97)
  shade(h3.height_pi97,h3.weights)
}

# plot( height ~ weight , data=h2.d , col=rangi2
# h2.post.samples <- extract.samples(h2.post)
# h2.post.a_map <- mean(h2.post.samples$a)
# h2.post.b_map <- mean(h2.post.samples$b)
# curve( h2.post.a_map + h2.post.b_map * x , add=TRUE, col="red" )

# M8
library(splines)
data(cherry_blossoms)
m8.d <- cherry_blossoms
m8.d2 <- m8.d[complete.cases(m8.d$doy),]

par(mfrow=c(3,1))

m8.f <- function(knots=15) {
  knot_list <- quantile( m8.d2$year , probs=seq(0,1,length.out=knots) )
  B <- bs(
    m8.d2$year,
    knots=knot_list[-c(1,knots)] ,
    degree=3,
    intercept=TRUE
  )

  m8.post <- quap(
      alist(
          D ~ dnorm( mu , sigma ) ,
          mu <- a + B %*% w ,
          a ~ dnorm(100,10),
          w ~ dnorm(0,10),
          sigma ~ dexp(1)
      ),
      data=list( D=m8.d2$doy , B=B ) ,
      start=list(w=rep(0,ncol(B)))
  )

  m8.post.samples <- extract.samples( m8.post )
  w <- apply( m8.post.s$w , 2 , mean )

  plot( NULL , xlim=range(m8.d2$year) , ylim=c(-6,6) ,
      xlab="year" , ylab="basis * weight", main=concat("Knots=",knots) )

  return(list(w=w,B=B))

}

m8.plot <- function() {
  # res15 <- m8.f()
  # for (i in 1:ncol(res15$B)) {
  #   lines( m8.d2$year , res15$w[i] * res15$B[,i] )
  # }
  res30 <- m8.f(30)
  for (i in 1:ncol(res30$B)) {
    lines( m8.d2$year , res30$w[i] * res30$B[,i] )
  }
}

num_knots <- 30
knot_list <- quantile( m8.d2$year , probs=seq(0,1,length.out=num_knots) )
B30 <- bs(
  m8.d2$year,
  knots=knot_list[-c(1,num_knots)],
  degree=2,
  intercept=TRUE
)

m8.post30 <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + B %*% w ,
        a ~ dnorm(100,10),
        w ~ dnorm(0,10),
        sigma ~ dexp(1)
    ),
    data=list( D=m8.d2$doy , B=B30 ) ,
    start=list(w=rep(0,ncol(B30)))
)

m8.post30.s <- extract.samples( m8.post30 )
w30 <- apply( m8.post30.s$w , 2 , mean )

plot( NULL , xlim=range(m8.d2$year) , ylim=c(-6,6) ,
  xlab="year" , ylab="basis * weight", main=concat("# Knots=",num_knots)
)

for ( i in 1:ncol(B30) ) {
  lines( m8.d2$year , w30[i] * B30[,i] )
}

# m8.mu <- link( m8.post )
# m8.mu_PI <- apply(m8.mu,2,PI,0.97)
# plot( m8.d2$year , m8.d2$doy , col=col.alpha(rangi2,0.3) , pch=16 )
# shade( m8.mu_PI , m8.d2$year , col=col.alpha("black",0.5) )
