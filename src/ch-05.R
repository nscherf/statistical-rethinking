library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce
#let's get a quick overview of the data
precis(d)
d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)
#let's check again if all went well with our additional standardized columns
precis(d)
m5.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + beta * A,
    a ~ dnorm(0, 0.02),
    beta ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d)

set.seed(10)
prior <- extract.prior( m5.1 ) 
mu <- link( m5.1 , post=prior, data=list( A=c(-2,) ) )
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50 ) lines ( c(-2, 2), mu[i, ], col = col.alpha("black", 0.4))