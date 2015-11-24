rm(list=ls())

################# FUNCTIONS #################
MultinomialEM <- function(H,K,tau)
{
	n = dim(H)[1]
	d = dim(H)[2]
	H = H
	
	t = H[sample(n,K),]
	c = matrix(1/K,1,K)
	c = as.vector(c)
	a = matrix(1/K,n,K)

	tolerance = 2*tau
	while(tolerance > tau)
	{
		phi = exp(H%*%t(log(t)))
		a_temp = a
		a = c*phi/rowSums(c*phi)
		c = colSums(a)/n
		t = t(a)%*%H / rowSums(t(a)%*%H)
		tolerance = norm(a - a_temp, "O")
	}
	m = matrix(1,n,1)
	for(i in 1:n)
	{
		m[i] = which.max(a[i,])
	}
	return(m)
}
##############################################
rm(list=ls())

################# FUNCTIONS #################
MultinomialEM <- function(H,K,tau)
{
	n = dim(H)[1]
	d = dim(H)[2]
	H = H
	
	##randomly initialize centroids,
	##assignment probs and mix weights
	t = H[sample(n,K),]
	c = matrix(1/K,1,K)
	c = as.vector(c)
	a = matrix(1/K,n,K)

	tolerance = 2*tau
	while(tolerance > tau)
	{
		phi = exp(H%*%t(log(t)))
		a_temp = a
		a = c*phi/rowSums(c*phi)
		c = colSums(a)/n
		t = t(a)%*%H / rowSums(t(a)%*%H)
		tolerance = norm(a - a_temp, "O")
	}
	m = matrix(1,n,1)
	for(i in 1:n)
	{
		m[i] = which.max(a[i,])
	}
	return(m)
}
##############################################
############### driver of algo ###############
H<-matrix(readBin("histograms.bin", "double", 640000), 40000, 16)
H = H + 0.01
par(mfrow = c(1,3))
for(j in 3:5)
{
	tau = 0.001
	K = j
	ans = matrix(MultinomialEM(H,K,tau),200,200)
	image(ans)
	title(main=paste("Clusters k = ", K," -- tau = ", tau))
}
