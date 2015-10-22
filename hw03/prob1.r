rm(list = ls())
### load data, sample of 200, 256 factors (16x16)
uspsdata.df <- read.table("C:/Users/Juan/Desktop/StatML/R/uspsdata.txt")
uspscl.df <- read.table("C:/Users/Juan/Desktop/StatML/R/uspscl.txt")

########  FUNCTIONS  ########
### train(X, w, y)
train <- function(X, w, y)
{
	errors = NULL
	tj = NULL
	m = NULL
	for(j in 1:dim(X)[2])
	{		
		err = bestpart(X[,j], y, w)
		errors = c(errors, err$error)
		tj = c(tj, err$part)
		m = c(m, err$m)
	}
	j = which.min(errors)
	return(list(j = j, tj = tj[j], m = m[j], error = errors[j]))
}

### bestpart(r, label)
bestpart <- function(r, label, w)
{
	part = partition(r)
	min_error = 1
	pos = 888
	m = 888
	for( i in 1:length(part))
	{
		temp_error = counterror(r,part[i],label,w)
		if (temp_error$error < min_error)
		{	
			min_error = temp_error$error
			pos = i
			m = temp_error$m
		}
	}
	return(list(part = part[pos], m = m, error = min_error))
}
### partition(temp_col)
partition <- function(temp_col)
{
	temp_col = sort(temp_col)
	unique_val = temp_col[1]
	for(i in 2:length(temp_col))
	{
		if(temp_col[i] > unique_val[length(unique_val)])
			unique_val = c(unique_val, temp_col[i])
	}
	part = NULL
	for(i in 2:length(unique_val))
	{
		part = c(part, (unique_val[i-1] + unique_val[i]) / 2.0)
	}
	return(part)
}
### counterror(r,t,label,w)
counterror <- function(r,threshold,label,w)
{
	error = 0
	for( i in 1:length(r))
	{
		if( (r[i] < threshold) & (label[i] > 0) )
			error = error + w[i]
		if( (r[i] > threshold) & (label[i] < 0) )
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
		if (col_att[i] < pars$tj)
			label = c(label, -1*pars$m)
		else
			label = c(label, pars$m)
	}
	return(label)
}

### AdaBoost(X, y, B)
AdaBoost <- function(X, y, B)
{
	allJs = NULL
	allTJs = NULL
	allMs = NULL
	alphas = NULL
	w = (1:dim(X)[1])*0+1/dim(X)[1]
	for( i in 1:B)
	{
		temp_par = train(X, w, y)
		label = classify(X, temp_par)
		temp_alpha = log((1-temp_par$error)/temp_par$error)
		allJs = c(allJs, temp_par$j)
		allTJs = c(allTJs, temp_par$tj)
		allMs = c(allMs, temp_par$m)
		alphas = c(alphas, temp_alpha) 
		for(j in 1:length(w))
		{
			if (label[j] != y[j])	
				w[j] = w[j]*exp(temp_alpha)
		}
	}
	allPars = list(allJs = allJs, allTJs = allTJs, allMs = allMs)
	return(list(alphas = alphas, allPars = allPars))
}
	
###############################################
		
xx = AdaBoost(uspsdata.df, uspscl.df$V1, 4)



	
