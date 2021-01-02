library(rethinking)

unif_prior <- function(p_grid) rep(1,1000)
grid_approx <- function(mk_likelihood,mk_prior=unif_prior,points=1e3) {
  p_grid <- seq(from=0,to=1,length.out=points)
  likelihood <- mk_likelihood(p_grid)
  prior <- mk_prior(p_grid)
  posterior.ustd <- likelihood * prior
  posterior.std <- posterior.ustd / sum(posterior.ustd)
  return(list(post=posterior.std,grid=p_grid))
}

set.seed(100)

# E1
res1 <- grid_approx(function(p_grid) dbinom(6,size=9,prob=p_grid))
sample_distrs1 <- sample(res1$grid,prob=res1$post,size=1e4,replace=TRUE)
e1 <- sum(sample_distrs1 < .2) / length(samples1)
# mean(sample_distrs1 < .2)

# E2
e2 <- sum(sample_distrs1 > .8) / length(samples1)

# E3
e3 <- sum(sample_distrs1 > .2 & samples1 > .8) / length(samples1)

# E4
e4 <- quantile(sample_distrs1,.2)

# E5
e5 <- quantile(sample_distrs1,.8)

# E6
e6 <- HPDI(sample_distrs1,.66)

# E7
e7 <- PI(sample_distrs1,.66)

# M1
m1.sol <- grid_approx(function(p_grid) dbinom(8, size=15, prob=p_grid))
# plot(x=res2$grid,y=res2$post,type="l")

# M2
m2.sample_distrs <- sample(m1.sol$grid,prob=m1.sol$post,size=1e4,replace=TRUE)
m2.sol <- HPDI(m2.sample_distrs,.9)

# M3
m3.ppc <- rbinom(1e4,size=15,prob=m2.sample_distrs)
# simplehist(m3.ppc)
m3.sol <- mean(m3.ppc == 8)

# M4
m4.ppc <- rbinom(1e4,size=9,prob=m2.sample_distrs)
m4.sol <- mean(m4.ppc == 6)

# M5
m5.res <- grid_approx(mk_likelihood=function(p_grid) dbinom(8, size=15, prob=p_grid),
            mk_prior=function(p_grid) ifelse(p_grid < .5,0,1))
# plot(x=m5.res$grid,y=m5.res$post,type="l")

m5.sample_distrs <- sample(m5.res$grid,prob=m5.res$post,size=1e4,replace=TRUE)
m5.m2.sol <- HPDI(m5.sample_distrs,.9)
m5.m3.ppc <- rbinom(1e4,size=15,m5.sample_distrs)
m5.m3.sol <- mean(m5.m3.ppc == 8)
m5.m4.ppc <- rbinom(1e4,size=9,m5.sample_distrs)
m5.m4.sol <- mean(m5.m4.ppc == 6)
# simplehist(m5b.sol)

m5.real_p.res <- grid_approx(mk_likelihood=function(p_grid) dbinom(8, size=15, prob=p_grid),
                   mk_prior=function(p_grid) ifelse(p_grid < .7,0,1))
plot(x=m5.real_p.res$grid,y=m5.real_p.res$post,type="l")
m5.real_p.sample_distrs <- sample(m5.real_p.res$grid,prob=m5.real_p.res$post,size=1e4,replace=TRUE)
m5.real_p.m2.sol <- HPDI(m5.real_p.sample_distrs,.9)
m5.real_p.m3.ppc <- rbinom(1e4,size=15,m5.real_p.sample_distrs)
m5.real_p.m3.sol <- mean(m5.real_p.m3.ppc == 8)
m5.real_p.m4.ppc <- rbinom(1e4,size=9,m5.real_p.sample_distrs)
m5.real_p.m4.sol <- mean(m5.real_p.m4.ppc == 6)

# H1
data(homeworkch3)
boy <- 1
girl <- 0
births <- c(birth1,birth2)
h1.res <- grid_approx(function(p_grid) dbinom(sum(births == boy),size=length(births),prob=p_grid))
plot(x=h1.res$grid,y=h1.res$post,type="l")
h1.sol <- h1.res$grid[which.max(h1.res$post)]

# H2
h2.sample_distrs <- sample(h1.res$grid,prob=h1.res$post,size=1e4,replace=TRUE)
h2.sol <- HPDI(h2.sample_distrs,c(.50,.89,.97))

# H3
h3.ppc <- rbinom(1e4,size=200,prob=h2.sample_distrs)
dens(h3.ppc,show.HPDI=T) ; abline(v=sum(births),col="green")
h3.sol <- all.equal(mean(h3.ppc)/length(births),mean(births))
# The prediction of the model is plausible in this case

# H4
h4.res <- grid_approx(function(p_grid) dbinom(sum(birth1 == boy),size=length(birth1),prob=p_grid))
h4.sample_distrs <- sample(h4.res$grid,prob=h4.res$post,size=1e4,replace=TRUE)
h4.ppc <- rbinom(1e4,size=100,prob=h4.sample_distrs)
dens(h4.ppc,show.HPDI=T) ; abline(v=sum(birth1),col="green")
h4.sol <- all.equal(mean(h4.ppc)/length(birth1),mean(birth1))
# The prediction of the model is plausible in this case

# H5
h5.b1_girls <- sum(birth1 == girl)
h5.res <- grid_approx(function(p_grid) dbinom(h5.b1_girls,size=length(birth1),prob=p_grid))
h5.sample_distrs <- sample(h5.res$grid,prob=h5.res$post,size=1e4,replace=TRUE)
h5.ppc <- rbinom(1e4,size=h5.b1_girls,prob=h5.sample_distrs)
dens(h5.ppc,show.HPDI=T) ; abline(v=sum(birth2[which(birth1 == girl)]),col="green")
# The model is way off in this case, the real data i.e. 39 is extremenly unplausible in the model.
# Since the model assumes that sex between 1st and 2nd births are independent events, this
# assumption might turn out to be wrong and having a 1st-born girl makes it more likely to have
# a 2nd girl.
# Another possibility is that the data is biased in some way.

# h5.res <- grid_approx(function(p_grid) dbinom(sum(birth1 == girl),size=length(birth1),prob=p_grid))
# h5.sample_distrs <- sample(h5.res$grid,prob=h5.res$post,size=1e4,replace=TRUE)
# trials <- 100
# h5.ppc <- rbinom(1e4,size=trials,prob=h5.sample_distrs)
# # count 1st-born girls
# h5.b1_girls <- rep(trials, length(h5.ppc)) - h5.ppc
# # simulate n 2nd births where n is the number of girls in the 1st births
# h5.b1_res <-
#   grid_approx(function(p_grid) dbinom(sum(h5.b1_girls),size=trials*length(h5.ppc),prob=p_grid))
# h5.b1_sample_distrs <- sample(h5.b1_res$grid,prob=h5.b1_res$post,size=1e4,replace=TRUE)
# h5.b1_ppc <- rbinom(1e4,size=100,prob=h5.b1_sample_distrs)
# dens(h5.b1_ppc,show.HPDI=T) ; abline(v=sum(birth2),col="green")
