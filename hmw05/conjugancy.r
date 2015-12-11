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
