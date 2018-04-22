# **********************************************************************************
#                            MSCA 673 - DR. K. BUYUKKURT
#             MBA CAR PREFERENCES - CALIBRATION AND VALIDATION SAMPLES
#  
# File Name: mba car preferences calibration and validation samples.r
# Last Revision: 8 April, 2018
# **********************************************************************************

# READ IN THE "CALIBRATION" (OR "ESTIMATION") SAMPLE.
mba.df = read.csv("mba car preferences calibration sample.csv") # the data set is read in as data frame
mba = mba.df[,-1] # delete the first column indicating subject number
mdat = data.matrix(mba)
mdist = dist(mdat, method="euclidian")
library(NbClust)
nbres <- NbClust(mdat, min.nc=2, max.nc=6, method="kmeans", 
                 index="all")
print(nbres)
# There does not seem to be a clear guidance regarding the number of clusters.

# K-means, k=3
km3 = kmeans(mdat, centers= 3, iter.max=100, nstart=20, algorithm="Hartigan-Wong")
km3
(rsq3 = km3$betweenss / km3$totss) # Rsquare = only 21.5 % 

# K-means, k=4
km4 = kmeans(mdat, centers= 4, iter.max=100, nstart=20, algorithm="Hartigan-Wong")
km4
(rsq4 = km4$betweenss / km4$totss) # Rsquare = 28.0 % 

# K-means, k=5
km5 = kmeans(mdat, centers= 5, iter.max=100, nstart=20, algorithm="Hartigan-Wong")
km5
(rsq5 = km5$betweenss / km5$totss) # Rsquare = 33.2 % 

# K-means, k=6
km6 = kmeans(mdat, centers= 6, iter.max=100, nstart=20, algorithm="Hartigan-Wong")
km6
(rsq6 = km6$betweenss / km6$totss) # Rsquare = 36.9 % 

# K-means, k=7
km7 = kmeans(mdat, centers= 7, iter.max=100, nstart=20, algorithm="Hartigan-Wong")
km7
(rsq7 = km7$betweenss / km7$totss) # Rsquare = 40.6 % 

knum = c(3,4,5,6,7)
rs = c(rsq3,rsq4,rsq5,rsq6,rsq7)
#**********************************************************************
# Plot R Square values as k, number of clusters, increases
plot(knum,rs, type="b", col="blue", lwd=5, xlab="Number of Clusters (k)", 
     ylab="R Square", 
     main="R Square for Number of Clusters 2 through 7")
# No clear bend in the line graph to guide us in deciding on the 
# number of clusters.

# *********************************************************************
# Let us set k=5 to replicate the reported study in the PDF file 
# "Preference Segmentation - MBA Car Preferences Discusion.PDF"
# K-means, k=5
# 100 different random starts are chosen (nstart=100), and the best 
# result is reported
km5 = kmeans(mdat, centers= 5, iter.max=100, nstart=100, algorithm="Hartigan-Wong")
km5
km5$cluster # cluster membership of the subjects
km5$centers # cluster centers, used for interpreting the clusters
km5$size # cluster sizes (number of subjects in each of five clusters)
km5$betweenss
km5$tot.withinss
km5$totss
# Print the transpose of km5$centers to match the table given in the 
# related PDF document
t(km5$centers)
# Do the centers for the clusters match when we compare the results here
# with the results reported in the PDF file? Somewhat?
# Cluster No.    vs Cluster No in PDF file  (p.300)
#**************       *******************
#     4                       1 ?
#     5                       5 ?
#     3                       3 ?
#     2                       2 ?
#     1                       1 ?


(rsq5 = km5$betweenss / km5$totss) # R square for the sample

#*********************************************************************
# WE WANT TO INTERRET HOW WELL THE CLUSTER SOLUTION ACCOUNTS FOR THE VARIATION 
# IN EACH VARIABLE. SO, LET US CALCULATE RSQUARE FOR EACH VARIABLE

# *****************************************************************
# First, let us partition BSS to variables of the study
km5$size # cluster sizes, needed in the following computation
gm = colMeans(mdat)
gm # grand mean for the total sample

# compute deviations of cluster centers from the grand mean
devfm = km5$centers - rbind(gm,gm,gm,gm,gm)
# Compute squares of deviations from the grand mean for each variable
ssd = devfm^2 # squares of deviations
ssd
# multiply each row by cluster size
ssd.size = diag(km5$size) %*% ssd
ssd.size
(bss.var = apply(ssd.size, 2, sum))# between groups sum of squares for each variable
sum(bss.var) # check this by comparing it to km5$betweenss computed above. They are equal.
# Note that we partitioned BSS to the variables of the study.

# *************************************************************************
# Compute variance of each variable and then multiply by (sample size - 1) to find
# the sum of squares around the variable mean
nrow(mdat) # sample size
tss.var = (apply(mdat, 2, var))* (nrow(mdat)-1)
tss.var # total sum of squares of deviations around the mean of each variable

# Compute R square for each variable
(rsq.var = bss.var/tss.var)
# Compare the Rsquare value for each variable with the corresponding value in Table
# 8.8 of the file that is titled 
# "Preference Segmentation - MBA car preference....PDF".

# ********************************************************************888
# Cluster Validation
# Read in Validation Sample
# READ IN THE "CALIBRATION" (OR "ESTIMATION") SAMPLE.
mbaV.df = read.csv("mba car preferences validation sample.csv")
# the data set is read in as data frame
mbaV = mbaV.df[,-1] # delete the first column indicating subject number
mdatV = data.matrix(mbaV)

# Assign the subjects of the validation sample using the estimated mean vectors 
# for the five clusters. Compute the sum of squares of deviations from the 
# mean vector of each cluster, and then assign the observation to the 
# cluster where this quantity is the smallest. 
ks=5 # number of clusters
nvar = ncol(mdatV)
(ck = km5$center) # matrix of cluster centers for k-means result
ns=nrow(mdatV)# sample size for validation sampe
cindex = rep(0, times=ns)
for (i in 1:ns){
  sss= rep(0, times=ks)
  z = mdatV[i,]
  for (j in 1:ks){
    sss[j]= sum((z-ck[j,])^2) 
    # sum of squares of deviations from cluster means
  }
  cindex[i]=which.min(sss)
}
cindex
# The above vector of integer values indicates which of five clusters
# each observation is assigned to. (k=1,2,3,4, or 5)


# Now, apply k-means clustering to the validation sample using
# random starts. As we did with the "calibration sample" we will try
# 100 random starts and choose the best one.
km5VR = kmeans(mdatV, centers= startm, iter.max=100, nstart=100,
               algorithm="Hartigan-Wong")
km5VR$centers

# number of subjects in five clusters for the validation sample
table(cindex) # When we assigned subjects to clusters based on ss
table(km5VR$cluster) # Assignment by k-means algorithm

# cross tabulate cluster memberships for the two runs with the validation sample
# above
table(cindex, km5VR$cluster)
# Interpret the above cross tabulation. 
# For example, take a look at the first row. 
# 20 subjects were assigned to Cluster 1. K-means algorithm assigned 
# 19 of those subjects to the same cluster and assigned only 1 of those 
# to another cluster. Ideally, "perfect validation" would involve assigning
# the same group of subjects to the same cluster by both approaches.
# Which group of subjects seem to be somewhat differently assigned by the two 
# approaches? (Take a look a third row of the crosstab.)


# NOw. let us checkthe stability of the cluster solution more formally.
# We can use the Rand Index and Adjusted Rand Index of the "flexclust" package 
#for this purpose.
install.packages("flexclust")
library(flexclust)
# Compute Rand Index without correction for chance. 
# This is the originally suggested Rand Index which varies between 0.00 and 1.00.
# 0.0 indicates very poor stability of cluster solution, and 1.00 indicates
# excellent stability of cluster solution (all subjects are assigned to 
# the same clusters in the two results)
randIndex(cindex, km5VR$cluster,correct=FALSE )# RI: Rand Index
# RI = 0.87 is rather good! :) However, some of the "matches" (that is, 
# the assignment of some subjects to the same clusters by the two approaches
# could have happened by chance only. Huber and Arabie (1985) suggested 
# the Adjusted Rand Index (ARI)to reduce the effect of pure chance. 
# Now, compute and print ARI value:
randIndex(cindex, km5VR$cluster, correct=TRUE) # ARI: Adjusted Rand Index
# Note that ARI=0.624 while RI=0.870. As we observe here,
# ARI is usually less than RI. Use ARI rather than RI to judge 
# the stability of your clustering results.

# Steinley (2004), "Properties of the Hubert-Arabie Adjusted
# Rand Index," Psychological Methods, Vol.9, No.3, 386-396 conducts a 
# thorough Monte Carlo simulation to examine the properties of the ARI
# and recommends the following heuristic values to judge the stability
# and quality of cluster recovery in validation studies:
#  ARI > 0.90 :         ExcellenT
#  0.80 > ARI =< 0.90 : Good
#  0.65 > ARI =< 0.80 : Moderate
#  ARI < 0.65         : Poor

# Steinly also offers a simulation based test to check if values of computed ARI 
# close to the above thresholds are significanly different them. For example, is 
# the observed value of ARI=0.624 in our case significantly different than the 
# heuristic threshold value of 0.65? 

# We will not discuss the nature of the simulation based test suggested by 
# Steinley (2004). We will only note that for the data on MBA
# car preferences the guality of cluster recovery
# by the two methods in validation is moderate at best. 


# We used ARI to judge the stability (similarity) of cluster assignment in the 
# validation framework. Both RI and ARI can be used in many other contexts. 
# For example, we can examine if two clustering algorithms 
# result in similar assignment of subject (or objects) to the same
# clusters. A specific example could be the comparison of a k-means result to 
# a k-medoids result. RI and ARI are also used extensively in classification problems.








