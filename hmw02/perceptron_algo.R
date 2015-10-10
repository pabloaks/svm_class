rm(list=ls())
if(! require(plyr))
	{install.packages("plyr")}
if(! require(ggplot2))
	{install.packages("ggplot2")}
if(! require(gridExtra))
	{install.packages("gridExtra")}

library("plyr")
library("ggplot2")
library("gridExtra")

################# FUNCTIONS #################
### fakedata(w, n)
#Inputs
#w:  w[1:d] is the normal vector of a hyperplane, 
#    w[d+1] = -c is the negative offset parameter. 
#n: sample size
#Outputs
#S: n by (d+1) sample matrix with last col 1
#y: vector of the associated class labels
fakedata <- function(w, n)
{
	if(! require(MASS))
		{install.packages("MASS")}
	if(! require(mvtnorm))
		{install.packages("mvtnorm")}

	require(MASS)
	require(mvtnorm)

	# obtain dimension
	d <- length(w)-1

	# compute the offset vector and a Basis consisting of w and its nullspace
	offset <- -w[length(w)] * w[1:d] / sum(w[1:d]^2)
	Basis <- cbind(Null(w[1:d]), w[1:d])	 

	# Create samples, correct for offset, and extend
	# rmvnorm(n,mean,sigme) ~ generate n samples from N(0,I) distribution
	S <- rmvnorm(n, mean=rep(0,d),sigma = diag(1,d)) %*%  t(Basis) 
	S <- S + matrix(rep(offset,n),n,d,byrow=T)
	S <- cbind(S,1)

	# compute the class assignments
	y <- as.vector(sign(S %*% w))

	# add corrective factors to points that lie on the hyperplane.
	S[y==0,1:d] <- S[y==0,1:d] + runif(1,-0.5,0.5)*10^(-4)
	y = as.vector(sign(S %*% w))
	return(list(S=S, y=y))
}

### classify(s, z) 
#Inputs
#s: (d + 1) each row is homogeneous coordinate representation of a single obs
#z: (v, -c) v: normal vector perceptron weight factor. c: offset parameter
#Outputs
#y: vector of associated class labels
classify <- function(s, z)
{
	y = as.vector(sign(s%*%z))
	return(y)
}

### perceptrain(s,y)
#Inputs
#s: (d + 1) each row is homogeneous coordinate representation of a single obs
#y: vector of associated class labels
#Outputs
#z: optimal perceptron result
#Z_history: evolution of z over algorithm
perceptrain <- function(s,y)
{
	#random generate initial hyperplane
	dim_v = dim(s)[2] - 1
	v = rnorm(dim_v)
	v = v/sqrt(v%*%v)
	c = rnorm(1)
	z = c(v, -c)
	Z_history = rbind(z)

	count_k = 1
	while(cost_percept(s,y,z) > 0)
	{
		z = z + gradient_cost_percept(s,y,z) / count_k;
		Z_history = rbind(Z_history, z)
		count_k = count_k + 1;
	}
	return(list(z = z, Z_history = Z_history))
}
#same as perceptrain + normalizing normal vector and correspondigly adjust offset at every iteration
perceptrain_norm <- function(s,y)
{
	#random generate initial hyperplane
	dim_v = dim(s)[2] - 1
	v = rnorm(dim_v)
	v = v/sqrt(v%*%v)
	c = rnorm(1)
	z = c(v, -c)
	Z_history = rbind(z)

	count_k = 1
	while(cost_percept(s,y,z) > 0)
	{
		z = z + gradient_cost_percept(s,y,z) / count_k;
		norm_z = sqrt(z[1]^2 + z[2]^2)
		z = z / norm_z
		Z_history = rbind(Z_history, z)
		count_k = count_k + 1;
	}
	return(list(z = z, Z_history = Z_history))
}

### cost_percept(s,y,z)
#Inputs
#s: (d + 1) each row is homogeneous coordinate representation of a single obs
#z: (v, -c) v: normal vector perceptron weight factor. c: offset parameter
#y: vector of associated class labels
#Outputs
#scalar perceptron cost at z
cost_percept <- function(s,y,z)
{
	return(sum(abs(s%*%z)*(y == -classify(s,z))))
}

### gradient_cost_percept(s,y,z)
#Inputs
#s: (d + 1) each row is homogeneous coordinate representation of a single obs
#z: (v, -c) v: normal vector perceptron weight factor. c: offset parameter
#y: vector of associated class labels
#Outputs
#vecotr of gradient of perceptron cost w.r.t. z	
gradient_cost_percept <- function(s, y, z)
{
	costmat = s*((y == -classify(s,z))*y)
	num_i= dim(costmat)[1]
	temp_sum = 0

	for(i in 1:num_i)
	{
		temp_sum = temp_sum + costmat[i,]
	}
	return(temp_sum)
}

### hyper_eq_line(x, y, off)
#Inputs
#x, y: coordinate of normal vector
#off: offset of hyperplane perpendicular to normal vector
#Outputs
#slope, inter: equation of the line perpendicular to normal vector
hyper_eq_line <- function(x, y, off)
{
	vh_norm = sqrt(x^2 + y^2)
	slope = -x/y
	inter = -(y- slope*x)* off / (vh_norm^2)
	return(list(slope = slope, inter = inter))
}
#####################################################

######## main driver of algo ########

#random generate vh and c
dim_v = 2
v = rnorm(dim_v)
v = v/sqrt(v%*%v)
c = rnorm(1)
z = c(v, -c)

#generate seperable data, split between +ve & -ve
#train perceptron, get eq line, plot them
train_sep_data = fakedata(z, 100)
sd.df = data.frame(train_sep_data)
pos_class = subset(sd.df, y > 0)
neg_class = subset(sd.df, y < 0)
result_z = perceptrain_norm(train_sep_data$S,train_sep_data$y)
hyper = hyper_eq_line(result_z$z[1],result_z$z[2],result_z$z[3])
sd.df = mutate(sd.df, y = factor( y, levels = -1:1, labels = c("neg", "neut", "pos")))
plot_train <- ggplot(sd.df,aes(x = S.1, y = S.2)) + geom_point(aes(colour = factor(y))) 
plot_train <- plot_train + geom_abline(intercept = hyper$inter, slope = hyper$slope, colour = "black", size = 0.5) 
plot_train <- plot_train + theme(legend.position="none") + labs(list(title = "Perceptron - Train Data", x = "dim1", y = "dim2"))

#generate test data, and classify it 
test_sep_data = fakedata(z, 100)
test_sep.df = data.frame(test_sep_data)
pos_class_test = subset(test_sep.df, y > 0)
neg_class_test = subset(test_sep.df, y < 0)
result_z_test = perceptrain(test_sep_data$S, test_sep_data$y)
hyper_test = hyper_eq_line(result_z_test$z[1], result_z_test$z[2], result_z_test$z[3])
test_sep.df = mutate(test_sep.df, y = factor( y, levels = -1:1, labels = c("neg", "neut", "pos")))
plot_test <- ggplot(test_sep.df,aes(x = S.1, y = S.2)) + geom_point(aes(colour = factor(y))) 
plot_test <- plot_test + geom_abline(intercept = hyper$inter, slope = hyper$slope, colour = "black", size = 0.5) 
plot_test <- plot_test + theme(legend.position="none") + labs(list(title = "Perceptron - Test Data", x = "dim1", y = "dim2"))

#plotting the evolution of normal vector to the hyperplane
#red point is start point, blue is answer
test.df = data.frame(result_z$Z_history[,3]*result_z$Z_history[,1:2])
last_z = dim(result_z$Z_history)[1]
plot_normv <- ggplot(test.df, aes(x = X1, y = X2)) + geom_point(alpha = 5/10)
plot_normv <- plot_normv + geom_point(colour = "blue", aes(x = test.df[last_z,1], y= test.df[last_z,2],size = 1.0)) 
plot_normv <- plot_normv + geom_point(colour = "red" ,aes(x = test.df[1,1], y = test.df[1,2],size = 1.0))  
plot_normv <- plot_normv + theme(legend.position="none") + labs(list(title = "Evolution of Normal Vector", x = "dim1", y = "dim2")) 

#plotting the evolution of hyperplane, if too many pts subset
i = 1
inc_i = max(1,floor(last_z / 50))
plot_hyper <- ggplot() +  geom_point(size = 0.25, aes(x = 0, y= 0)) + expand_limits(x=c(-4, 4), y = c(-4, 4))
while(i < last_z)
{
	hyper = hyper_eq_line(result_z$Z_history[i,1],result_z$Z_history[i,2],result_z$Z_history[i,3])
	plot_hyper <- plot_hyper + geom_abline(intercept = hyper$inter, slope = hyper$slope, colour = "grey", alpha = 0.8)
	i = i + inc_i
}
hyper = hyper_eq_line(result_z$Z_history[last_z,1],result_z$Z_history[last_z,2],result_z$Z_history[last_z,3])
plot_hyper <- plot_hyper + geom_abline(intercept = hyper$inter, slope = hyper$slope, colour = "blue", size = 1.25)
plot_hyper <- plot_hyper + labs(list(title = "Evolution of Hyperplane", x = "dim1", y = "dim2")) 

grid.arrange(plot_train, plot_test, ncol = 2)
x11()
grid.arrange(plot_normv, plot_hyper, ncol = 2)
