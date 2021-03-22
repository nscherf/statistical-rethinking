# code from chapter 4 on linear regression

library(rethinking)
data(Howell1); 
d <- Howell1;
d2 <- d[ d$age >= 18 , ]

precis(d2)

#extract a subset of the cases and estimate a regression model
N <- 352
dN <- d2[ 1:N ,]
mN <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b * (weight - mean(weight)) ,
    a ~ dnorm( 178, 20) ,
    b ~ dlnorm( 0, 1) ,
    sigma ~ dunif( 0, 50 )
  ) , data = dN)

# sample from the posterior
post <- extract.samples( mN, n=20 )

plot( dN$weight, dN$height , 
      xlim = range(d2$weight) , ylim = range(d2$height) , 
      col = rangi2, xlab = "weight" , ylab = "height")
      mtext(concat("N = ", N))
      
for ( i in 1:20 )
  curve (post$a[i] + post$b[i]*(x-mean(dN$weight)) ,
         col = col.alpha("black", 0.3), add = TRUE)

      