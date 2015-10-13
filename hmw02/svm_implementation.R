rm(list = ls())

if(! require(ggplot2))
	{install.packages("ggplot2")}
if(! require(gridExtra))
	{install.packages("gridExtra")}

library("ggplot2")
library("gridExtra")

# sample of 200, 256 factors (16x16)
uspsdata.df <- read.table("C:/Users/Juan/Desktop/StatML/R/uspsdata.txt")
uspscl.df <- read.table("C:/Users/Juan/Desktop/StatML/R/uspscl.txt")

## remove 20% for testing
num_sample = dim(uspsdata.df)[1]
num_rand_sample = num_sample * 0.20
random_rows = sample(nrow(uspsdata.df), num_rand_sample)
data_test = uspsdata.df[random_rows,]
class_test = uspscl.df[random_rows,]
data_train = uspsdata.df[-random_rows,]
class_train = uspscl.df[-random_rows,]
dat.train = data.frame(x = data_train, y = as.factor(class_train))
dat.test = data.frame(x = data_test, y = as.factor(class_test))


##cross-validate with k-fold method for linear case
train_error = NULL
validate_error = NULL
num_k  = 10
k_scale = (num_sample - num_rand_sample) / num_k

for(j in 0:(num_k-1))
{
	k_subset = seq(k_scale) + k_scale*j
	temp_train_error = NULL
	temp_validate_error = NULL
	for(i in 1:20)
	{
		temp_cost = 10^(i-15)
		svm_train = svm(y~., data = dat.train[-k_subset,], kernel="linear", cost = temp_cost, scale = F)
		ypred_train = predict(svm_train, dat.train[-k_subset,])
		train_table = table(predict = ypred_train, truth = dat.train[-k_subset,]$y)
		train_acc = (train_table[1,2] + train_table[2,1])/sum(train_table)
		temp_train_error = c(temp_train_error, train_acc)
		ypred_validate = predict(svm_train, dat.train[k_subset,])
		validate_table = table(predict = ypred_validate, truth = dat.train[k_subset,]$y)
		validate_acc = (validate_table[1,2] + validate_table[2,1])/sum(validate_table)
		temp_validate_error = c(temp_validate_error, validate_acc)
	}
	train_error = rbind(train_error, temp_train_error)
	validate_error = rbind(validate_error, temp_validate_error)
}
train_error = colSums(train_error) / num_k
validate_error = colSums(validate_error) / num_k
cost_star = 10^(which.min(validate_error) - 15)
svm_test = svm(y~., data = dat.train, kernel = "linear", cost = cost_star, scale = F)
ypred_test = predict(svm_test, dat.test)
test_table = table(predict = ypred_test, truth = dat.test$y)
test_error = (test_table[1,2] + test_table[2,1])/sum(test_table)
print(test_table)
print(test_error)

plot_linear <- ggplot() + geom_point(colour="blue", aes(x=seq(20), y=train_error), size=I(5), alpha=I(0.4)) 
plot_linear <- plot_linear + geom_point(colour="red", aes(x=seq(20), y=validate_error), size=I(5), alpha=I(0.4))
plot_linear <- plot_linear + labs(list(title="Cross Validation", x="cost: 10^(i-10)", y="error rate")) + layer(geom="jitter")
plot_linear

##cross-validate with k-fold method for SVM with margin and RBF kernel
train_error = 0
validate_error = 0

for(j in 0:(num_k-1))
{
	k_subset = seq(k_scale) + k_scale*j
	temp_train_error_gamma = NULL
	temp_validate_error_gamma = NULL
	for(m in 1:10)
	{
		temp_train_error_cost = NULL
		temp_validate_error_cost = NULL
		temp_gamma = 100^(m-10)
		for(i in 1:20)
		{
			temp_cost = 10^(i-10)
			svm_train = svm(y~., data = dat.train[-k_subset,], kernel="radial", cost = temp_cost, gamma = temp_gamma, scale = F)
			ypred_train = predict(svm_train, dat.train[-k_subset,])
			train_table = table(predict = ypred_train, truth = dat.train[-k_subset,]$y)
			train_acc = (train_table[1,2] + train_table[2,1])/sum(train_table)
			temp_train_error_cost = c(temp_train_error_cost, train_acc)
			ypred_validate = predict(svm_train, dat.train[k_subset,])
			validate_table = table(predict = ypred_validate, truth = dat.train[k_subset,]$y)
			validate_acc = (validate_table[1,2] + validate_table[2,1])/sum(validate_table)
			temp_validate_error_cost = c(temp_validate_error_cost, validate_acc)
		}
		temp_train_error_gamma = rbind(temp_train_error_gamma, temp_train_error_cost, deparse.level = 0)
		temp_validate_error_gamma = rbind(temp_validate_error_gamma, temp_validate_error_cost, deparse.level = 0)
	}
	train_error = train_error + temp_train_error_gamma / num_k
	validate_error = validate_error + temp_validate_error_gamma / num_k
}
min_ind = which(validate_error == min(validate_error), arr.ind=TRUE) 
cost_star = 10^(min_ind[1,2] - 10)
gamma_star = 100^(min_ind[1,1] - 10)
svm_test = svm(y~., data = dat.train, kernel="radial", cost=cost_star, gamma=gamma_star, scale=F)
ypred_test = predict(svm_test, dat.test)
test_table = table(predict = ypred_test, truth = dat.test$y)
test_error = (test_table[1,2] + test_table[2,1])/sum(test_table)

##contour printing of 2d cross-validation
validate_3d = melt(validate_error)
names(validate_3d) = c("x","y","z")
plot_rbf <- ggplot(validate_3d, aes(x,y,z=z)) + geom_tile(aes(fill = z)) + stat_contour()
plot_rbf <- plot_rbf + labs(list(title="2D Cross Validation", x="gamma: 100^(i-10)", y="cost: 10^(i-10)")) 
plot_rbf

##cost cross-section & gamma cros-section
cost_cross_val = validate_error[,min_ind[1,2]]
cost_cross_train = train_error[,min_ind[1,2]]
plot_cost <- ggplot() + geom_point(colour="blue", aes(x=seq(10), y=cost_cross_train), size=I(5), alpha=I(0.4)) 
plot_cost <- plot_cost + geom_point(colour="red", aes(x=seq(10), y=cost_cross_val), size=I(5), alpha=I(0.4))
plot_cost <- plot_cost + labs(list(title="Cost cross-section", x="gamma: 100^(i-10)", y="error rate")) + layer(geom="jitter")
gamma_cross_val = validate_error[min_ind[1,1],]
gamma_cross_train = train_error[min_ind[1,1],]
plot_gamma <- ggplot() + geom_point(colour="blue", aes(x=seq(20), y=gamma_cross_train), size=I(5), alpha=I(0.4)) 
plot_gamma <- plot_gamma + geom_point(colour="red", aes(x=seq(20), y=gamma_cross_val), size=I(5), alpha=I(0.4))
plot_gamma <- plot_gamma + labs(list(title="Gamma cross-section", x="cost: 10^(i-10)", y="error rate")) + layer(geom="jitter")
grid.arrange(plot_cost, plot_gamma, ncol = 2)
