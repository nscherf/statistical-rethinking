library(rethinking)
library(ggplot2)

p_grid <- seq(from=0, to=1, length.out = 20) 
prior <- rep(1, 20)
likelihood <- dbinom(6, size=9, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)
p_data <- data.frame(p=p_grid, posterior=posterior)

ggplot(data=p_data, aes(x=p, y=posterior)) + geom_line() + theme_minimal()

