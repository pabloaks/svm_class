rm(list = ls())
if(! require(ggplot2))
	{install.packages("ggplot2")}

########  FUNCTIONS  ########
### AdaBoost(X, y, B)
AdaBoost <- function(X, y, B)
{
	allPars = NULL; alphas = NULL
	w = rep(1/dim(X)[1], dim(X)[1])
	for( i in 1:B)
	{
		temp_par = train(X, w, y)
		label = classify(X, temp_par)
		temp_alpha = log((1-temp_par$error)/temp_par$error)
		allPars = rbind(allPars, c(temp_par$j, temp_par$theta, temp_par$m))
		alphas = c(alphas, temp_alpha) 
		for(j in 1:length(w))
		{
			if (label[j] != y[j])	
				w[j] = w[j]*exp(temp_alpha)
		}
	}
	colnames(allPars) <- c("j", "theta", "m")
	return(list(alphas = alphas, allPars = allPars))
}
### train(X, w, y)
train <- function(X, w, y)
{
	errors = NULL; theta = NULL; m = NULL
	for(j in 1:dim(X)[2])
	{		
		stump = tree_stump(X[,j], y, w)
		errors = c(errors, stump$error)
		theta = c(theta, stump$thetas)
		m = c(m, stump$m)
	}
	j = which.min(errors)
	pars = list(j = j, theta = theta[j], m = m[j], error = errors[j])
	return(pars)
}
### tree_stump(r, label, w) # finds optimal theta for given attribute
tree_stump <- function(r, label, w)
{
	thetas = candidate_thetas(r)
	min_error = 1; pos = 88888; m = 88888
	for( i in 1:length(thetas))
	{
		temp_error = counterror(r,thetas [i],label,w)
		if (temp_error$error < min_error)
		{	
			min_error = temp_error$error
			pos = i
			m = temp_error$m
		}
	}
	return(list(thetas = thetas[pos], m = m, error = min_error))
}
### candidate_thetas(temp_col) # finds candidate thetas for given j
candidate_thetas <- function(temp_col)
{
	temp_col = sort(temp_col)
	unique_val = temp_col[1]
	for(i in 2:length(temp_col))
	{
		if(temp_col[i] > unique_val[length(unique_val)])
			unique_val = c(unique_val, temp_col[i])
	}
	thetas = NULL
	for(i in 2:length(unique_val))
	{
		thetas = c(thetas, (unique_val[i-1] + unique_val[i]) / 2.0)
	}
	return(thetas)
}
### counterror(r, theta, y, w)
counterror <- function(r, theta, y, w)
{
	error = 0
	for( i in 1:length(r))
	{
		if( (r[i] < theta) & (y[i] > 0) )
			error = error + w[i]
		if( (r[i] > theta) & (y[i] < 0) )
			error = error + w[i]
	}
	error = error / sum(w)
	m = 1
	if (error > 0.50)
	{
		error = 1 - error
		m = -1
	}
	return(list(error = error, m = m))
}
### classify(X, pars)
classify <- function(X, pars)
{
	label = NULL
	col_att = X[,pars$j]
	for(i in 1:length(col_att))
	{
		if (col_att[i] < pars$theta)
			label = c(label, -1*pars$m)
		else
			label = c(label, pars$m)
	}
	return(as.vector(label))
}
### agg_class(X, alpha, allPars)
agg_class <- function(X, alpha, allPars)
{
	temp_sum = 0
	for( i in 1:length(alpha))
	{
		temp_par = list(j = allPars[i,"j"], theta = allPars[i,"theta"], m = allPars[i,"m"])
		temp_sum = temp_sum + classify(X,temp_par)*alpha[i]
	}
	return(sign(temp_sum))
}
### accuarcy(y1, y2)
accuarcy <- function(y1, y2)
{
	if(length(y1) != length(y2))
		return -8888
	count = 0
	for( i in 1:length(y1))
	{
		if(y1[i] == y2[i])
			count = count + 1
	}
	return( count/length(y1))
}
###############################################
### load data, sample of 200, 256 factors (16x16)
uspsdata.df <- read.table("C:/Users/Juan/Desktop/StatML/R/uspsdata.txt")
uspscl.df <- read.table("C:/Users/Juan/Desktop/StatML/R/uspscl.txt")
data_train = uspsdata.df
class_train = uspscl.df$V1

##cross-validate with k-fold method 
train_error = NULL
validate_error = NULL
num_k  = 5
k_scale = length(class_train) / num_k
B = 100
for(j in 0:(num_k-1))
{
	k_subset = seq(k_scale) + k_scale*j
	temp_train_error = NULL
	temp_validate_error = NULL
	ab_train = AdaBoost(data_train[-k_subset,], class_train[-k_subset], B) 
	for(i in 1:B)
	{
		ypred_train = agg_class(data_train[-k_subset,], ab_train$alphas[1:i], ab_train$allPars)
		train_acc = 1 - accuarcy(class_train[-k_subset], ypred_train)
		temp_train_error = c(temp_train_error, train_acc)
		ypred_validate = agg_class(data_train[k_subset,], ab_train$alphas[1:i], ab_train$allPars)
		validate_acc = 1 - accuarcy(class_train[k_subset], ypred_validate)
		temp_validate_error = c(temp_validate_error, validate_acc)
	}
	train_error = rbind(train_error, temp_train_error)
	validate_error = rbind(validate_error, temp_validate_error)
	print(j)
}
train_error = colSums(train_error) / num_k
validate_error = colSums(validate_error) / num_k
b_star = which.min(validate_error)

plot_linear <- ggplot() + geom_point(colour="blue", aes(x=seq(B), y=train_error), size=I(4), alpha=I(0.5)) 
plot_linear <- plot_linear + geom_point(colour="red", aes(x=seq(B), y=validate_error), size=I(4), alpha=I(0.5))
plot_linear <- plot_linear + labs(list(title="Cross Validation", x="number of weak learners", y="error rate")) + layer(geom="jitter")
plot_linear
