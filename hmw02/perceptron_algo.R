rm(list=ls())
library("dplyr")
library("ggplot2")

#random generate vh and c
dim_v = 2
v = rnorm(dim_v)
v = v/sqrt(v%*%v)
c = rnorm(1)
z = c(v, -c)

seperable_data = fakedata(z, 1000)
sd.df = data.frame(seperable_data)
pos_class = subset(sd.df, y > 0)
neg_class = subset(sd.df, y < 0)

result_z = perceptrain(seperable_data$S,seperable_data$y)

#find separating line, only works for 2d now, would need to geenralize
c_off = -result_z$z[3] / sqrt(result_z$z[1]^2 + result_z$z[2]^2)
y_coor = result_z$z[2] / sqrt(result_z$z[1]^2 + result_z$z[2]^2)
x_coor = result_z$z[1] / sqrt(result_z$z[1]^2 + result_z$z[2]^2)
slopes = -x_coor/y_coor
inter = (y_coor - slopes*x_coor)* c_off

sd.df = mutate(sd.df, y = factor( y, levels = -1:1, labels = c("neg", "neut", "pos")))
ggplot(sd.df,aes(x = S.1, y = S.2)) + geom_point(aes(colour = factor(y))) + geom_abline(intercept = inter, slope = slopes, colour = "black", size = 0.5)


## classify(s, z) 
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

## perceptrain(s,y)
#Inputs
#s: (d + 1) each row is homogeneous coordinate representation of a single obs
#y: vector of associated class labels
#Outputs
#z: 
#Z_history:
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

## cost_percept(s,y,z)
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

## gradient_cost_percept(s,y,z)
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

#Given my prof
#Inputs
#w:  w[1:d] is the normal vector of a hyperplane, 
#    w[d+1] = -c is the negative offset parameter. 
#n: sample size

#Outputs
#S: n by (d+1) sample matrix with last col 1
#y: vector of the associated class labels

fakedata <- function(w, n){

if(! require(MASS))
{
	install.packages("MASS")
}
if(! require(mvtnorm))
{
	install.packages("mvtnorm")
}

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

} # end function fakedata
