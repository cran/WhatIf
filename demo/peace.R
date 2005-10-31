########  Example:  WhatIf using Doyle and Sambanis (2000) U.N. peacekeeping data set  ########

#Load sample observed covariate data
data(f)

#Load sample counterfactual data
data(cf)

#Run whatif using default options
my.result <- whatif(data = f, cfact = cf)

#Look at results of convex hull test
my.result$in.hull

#Look at Gower's distance between first counterfactual and each of the 122 data points
my.result$gowers.dist[1, ]

#Look at geometric variance of covariates
my.result$geom.var

#Look at proportion of data points nearby all 122 counterfactuals
my.result$sum.stat

#Run whatif supplying own value for parameter 'nearby'
my.result <- whatif(data = f, cfact = cf, nearby = 0.110)

#Calculate mean proportion of data points nearby all 122 counterfactuals
mean(my.result$sum.stat)

#Look at default cumulative frequency distribution for first counterfactual
#and data points
my.result$cum.freq[1, ]

#Plot raw cumulative frequency distribution for first counterfactual 
plot(my.result, numcf = 1)

#Print summary on screen
summary(my.result)

#Identify control units not on support of treatment units
my.result.cntrl <- whatif(formula = ~ decade + wartype + logcost +
wardur + factnum + factnumsq + trnsfcap + treaty + develop + exp, 
data = f[f$untype4 == 1,], cfact = f[f$untype4 == 0,], )

#Print convex hull test results on screen
my.result.cntrl$in.hull

#Identify units on common support of both treatment and control groups
f2cf <- f
f2cf$untype4 <- 1 - f2cf$untype4
my.result.comb <- whatif(data = f, cfact = f2cf)

#Print convex hull test results on screen
my.result.comb$in.hull

