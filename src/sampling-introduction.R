# the globe tossing example from chapter 3

p_grid <- seq( from=0, to=1, length.out=1000 )
prior <- rep( 1, 1000 )
likelihood <- dbinom( 6, size=9, prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(2025)
samples <- sample( p_grid , prob=posterior, size = 1e4, replace=TRUE )

# 3E1 posterior probability below p = 0.2
sum(posterior[p_grid < 0.2])
sum(samples < 0.2) / 1e4

# 3E2 posterior probability above p = 0.8
sum(samples > 0.8) / 1e4

# 3E3 posterior probability between p = 0.2 and p = 0.8
sum(samples > 0.2 & samples < 0.8) / 1e4

# 3E4 value of p below which lies 20% of posterior 
quantile( samples, 0.2)
# just a quick check
sum(posterior[p_grid < quantile( samples, 0.2)])

# 3E5 20% of the posterior probability lies above which value of p?
quantile(samples, 0.8)

# 3E6 narrowest interval containing 66% of posterior probability
HPDI(samples, prob=0.66)

# 3E7 interval with equal posterior probability
PI(samples, prob=0.66)
