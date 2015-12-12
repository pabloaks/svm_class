rm(list=ls())
if(! require(ggplot2))
	{install.packages("ggplot2")}
library("ggplot2")
## (a) 
x = (1:800)/200
theta = 1
exp_fun = theta*exp(-theta*x)
#plot(x,exp_fun, type = "l", col = "red", lwd = 3)

##b
set_x = c(1,2,4)
like_ind = NULL
theta = 1
for(i in 1:length(set_x))
{ 
	temp = theta*exp(-theta*set_x[i])
	like_ind = cbind(like_ind, temp)
}
p1 <- ggplot() + geom_point(alpha = 0.4, colour = "blue", aes(x = set_x, y = as.vector(like_ind), size = 0.31))
p1 <- p1 + geom_point(alpha = 1.0, colour = "red", aes(x = x, y = exp_fun, size = 0.3)) + theme(legend.position="none")
p1



## g
rm(list=ls())
if(! require(ggplot2))
	{install.packages("ggplot2")}
library("ggplot2")

theta = (1:1600)/400
alpha = 2.0
beta = 0.2
num = 256
gamma_fn = (beta**(alpha))/exp(lgamma(alpha))*(theta**(alpha-1))*exp(-beta*theta)
rand_exp = as.vector(rexp(n = num, rate = 1))

for(i in 1:num)
{
	alpha = alpha + 1
	beta = beta + rand_exp[i]
	if(i == 4)
	{gamma_fn4 = exp(alpha*log(beta) - lgamma(alpha) + (alpha-1)*log(theta) - beta*theta)}
	if(i == 8)
	{gamma_fn8 = exp(alpha*log(beta) - lgamma(alpha) + (alpha-1)*log(theta) - beta*theta)}
	if(i == 16)
	{gamma_fn16 = exp(alpha*log(beta) - lgamma(alpha) + (alpha-1)*log(theta) - beta*theta)}
	if(i == 256)
	{gamma_fn256 = exp(alpha*log(beta) - lgamma(alpha) + (alpha-1)*log(theta) - beta*theta)}
}
p1 <- ggplot() 
p1 <- p1 + geom_point(colour = "tomato", alpha = 10/10, aes(x = theta, y = gamma_fn256))
p1 <- p1 + geom_point(colour = "gray58", alpha = 5/10, aes(x = theta, y = gamma_fn16))
p1 <- p1 + geom_point(colour = "black", alpha = 1/10, aes(x = theta, y = gamma_fn8))
p1 <- p1 + geom_point(colour = "lightslateblue", alpha = 1/10, aes(x = theta, y = gamma_fn4))
p1 <- p1 + geom_point(colour = "springgreen3", alpha = 1/10, aes(x = theta, y = gamma_fn))
p1 <- p1 + labs(list(title="Gamma Posterior pdf", x="Theta", y=" ")) + layer(geom="jitter")
p1
