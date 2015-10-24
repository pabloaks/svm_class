import string
import random
import math

FILENAME_CLASS = "C:/Users/Juan/Desktop/StatML/R/uspscl.txt"
FILENAME_ATTS = "C:/Users/Juan/Desktop/StatML/R/uspsdata.txt"

def load_class():
    print "loading 200 class labels..."
    inFile = open(FILENAME_CLASS, 'r', 0)
    lines = inFile.readlines()
    classList = [];
    for i in lines:
        classList.append(float(i.split()[0]))
    print " ",len(classList), "classes loaded"
    inFile.close()
    return classList

def load_attributes():
    print "loading 16*16 attributes for 200 samples..."
    inFile = open(FILENAME_ATTS, 'r', 0)
    lines = inFile.readlines()
    attsList = []
    for i in lines:
        temp_list = i.split()
        temp_list2 = []
        for j in temp_list:
            temp_list2.append(float(j))
        attsList.append(temp_list2)
    print " ",len(lines), "samples loaded"
    inFile.close()
    return attsList

def get_column(x, j):
    col = []
    for i in x:
        col.append(i[j])
    return col

def candidate_thetas(r):
    x = list(r)
    x.sort()
    unique_val = []
    unique_val.append(x[0])
    for i in range(1,len(x)):
        if x[i] > unique_val[-1]:
            unique_val.append(x[i])
    thetas = []
    for i in range(1,len(unique_val)):
        thetas.append((unique_val[i-1] + unique_val[i])/2.0)
    return thetas

def count_error(r,theta,y,w):
    error = 0.0;
    for i in range(0,len(r)):
        if (r[i] < theta) and (y[i] > 0):
            error += w[i]
        if (r[i] > theta) and (y[i] < 0):
            error += w[i]
    error = error/sum(w)
    m = 1
    if error > 0.50:
        error = 1.0 - error
        m = -1
    d = {}
    d["m"] = m
    d["error"] = error
    return d

def tree_stump(r, y, w):
    d = {}
    d["error"] = 1.0
    d["theta"] = 88888.0;
    d["m"] = 88888.0;
    thetas = candidate_thetas(r)
    for i in thetas:
        temp_error = count_error(r,i,y,w)
        if (temp_error["error"] < d["error"]):
            d["error"] = temp_error["error"]
            d["theta"] = i
            d["m"] = temp_error["m"]
    return d

def list_min(x):
    temp_min = x[0]
    loc_min = 0
    for i in range(1, len(x)):
        if x[i] < temp_min:
            loc_min = i
            temp_min = x[i]
    return loc_min

def train (x,w,y):
    errors=[]
    theta=[]
    m = []
    for i in range(0,len(x[0])):
        stump = tree_stump(get_column(x,i),y,w)
        errors.append(stump["error"])
        theta.append(stump["theta"])
        m.append(stump["m"])
    j = list_min(errors)
    d = {}
    d["j"] = j
    d["theta"] = theta[j]
    d["m"] = m[j]
    d["error"] = errors[j]
    return d

def classify(x, pars):
    label = []
    col_att = get_column(x,pars["j"])
    for i in col_att:
        if i < pars["theta"]:
            label.append(-pars["m"])
        else:
            label.append(pars["m"])
    return label

def AdaBoost(x,y,b):
    allPars = []
    alphas = []
    w = []
    for i in range(0,len(x)):
        w.append(1.0/len(x))
    for i in range(0,b):
        temp_par = train(x,w,y)
        label = classify(x, temp_par)
        temp_alpha = math.log((1 - temp_par["error"])/temp_par["error"])
        allPars.append(temp_par)
        alphas.append(temp_alpha)
        for j in range(0,len(x)):
            if (label[j] != y[j]):
                w[j] = w[j]*math.exp(temp_alpha)
    d = {}
    d["alphas"] = alphas
    d["allPars"] = allPars
    return d

def agg_class(x, alpha, allPars):
    temp_sum = []
    for i in range(0, len(x)):
        temp_sum.append(0.00)
    for i in range(0, len(alpha)):
        temp_class = classify(x,allPars[i])
        for j in range(0, len(x)):
            temp_sum[j] = temp_sum[j] + temp_class[j]*alpha[i]
    for j in range(0, len(x)):
        if temp_sum[j] > 0:
            temp_sum[j] = 1.0
        else:
            temp_sum[j] = -1.0
    return temp_sum

def accuarcy(y1,y2):
    if len(y1) != len(y2):
        return -88888
    count = 0.0
    for i in range(0,len(y1)):
        if y1[i] == y2[i]:
            count += 1.0
    return count/len(y1)

########################################################################
y = load_class()
x = load_attributes()

b = 100
num_k  = 5
k_scale = len(x)/5
## data for k-fold
bx1 = x[k_scale*0:k_scale*1]
by1 = y[k_scale*0:k_scale*1]
bx2 = x[k_scale*1:k_scale*2]
by2 = y[k_scale*1:k_scale*2]
bx3 = x[k_scale*2:k_scale*3]
by3 = y[k_scale*2:k_scale*3]
bx4 = x[k_scale*3:k_scale*4]
by4 = y[k_scale*3:k_scale*4]
bx5 = x[k_scale*4:k_scale*5]
by5 = y[k_scale*4:k_scale*5]
x1 = bx2 + bx3 + bx4 + bx5
y1 = by2 + by3 + by4 + by5
x2 = bx1 + bx3 + bx4 + bx5
y2 = by1 + by3 + by4 + by5
x3 = bx1 + bx2 + bx4 + bx5
y3 = by1 + by2 + by4 + by5
x4 = bx1 + bx2 + bx3 + bx5
y4 = by1 + by2 + by3 + by5
x5 = bx1 + bx2 + bx3 + bx4 
y5 = by1 + by2 + by3 + by4 
xfold = [x1, x2, x3, x4, x5]
yfold = [y1, y2, y3, y4, y5]
xval = [bx1, bx2, bx3, bx4, bx5]
yval = [by1, by2, by3, by4, by5]

train_error = []
validate_error = []
for j in range(0,num_k):
    temp_ab = AdaBoost(xfold[j], yfold[j], b)
    temp_train = []
    temp_val = []
    for i in range(1,b+1):
        ypred_train = agg_class(xfold[j], temp_ab["alphas"][0:i], temp_ab["allPars"][0:i])
        acc = round(1 - accuarcy(ypred_train, yfold[j]), 6)
        temp_train.append(acc)
        ypred_val = agg_class(xval[j], temp_ab["alphas"][0:i], temp_ab["allPars"][0:i])
        acc = round(1 - accuarcy(ypred_val, yval[j]), 6)
        temp_val.append(acc)
    train_error.append(temp_train)
    validate_error.append(temp_val)

train_err = []
val_err = []
for i in range(0,b):
    train_err.append(0.00)
    val_err.append(0.00)
for i in range(0,b):
    for j in range(0,num_k):
        train_err[i] += train_error[j][i] / num_k
        val_err[i] += validate_error[j][i] / num_k
        
print train_err
print val_err
