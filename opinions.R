#===========================================================================
#                             MSCA 673 - DR. K. BUYUKKURT
#                                TAKE-HOME EXAM2 - PART 1
#                                  CLUSTER ANALYSIS
#    FILE NAME: OPINIONS.R
#    LAST REVISION: 18 APRIL, 2018
#=============================================================================
# if the following R packages are not installed on your PC, you need to
# install them. 
library(NbClust)
library(cluster)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(cluster)
library(dendextend)
library(flexclust)


set.seed(123)

d = read.csv("opinions.csv") 
head(df)
str(d)
dim(d) # 770 subjects, 24 variables

# NbClust
# The following statement takes pretty long to run. Be patient
# Wait for it to end. 
nbres <- NbClust(d, min.nc=2, max.nc=8, method="kmeans", 
                 index="all")
print(nbres)
nbres$Best.nc

#*********************************************************************
# K-means cluster analysis
kmres <- kmeans(d, 4, iter.max=500, algorithm="Hartigan-Wong", nstart=100) 
kmres
str(kmres)
kmres$cluster # cluster membership of subjects as determined by k-means
centers = kmres$centers # cluster centers
rownames(centers) = c("Cluster1", "Cluster2", "Cluster3", "Cluster4")
# Print Cluster Means after transposing the related matrix
# Rows are variables and columns are clusters
# This way of presenting the matrix matches the following heatmap
t.center = t(centers)
# Now vertically flip t.center matrix
ind = seq(from=24, to=1, by=-1)
ind
flipped.t.center= t.center[ind,]
flipped.t.center 


#************************************************************
# Convert the matrix of center into a form that is needed by GGPLOT function
cm = melt(centers) 

# HEATMAP OF CLUSTER CENTERS ESTIMATED BY KMEANS
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')
# Expand your plot area in RStudio before executing the following statement
ggplot(cm, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colours = hm.palette(100)) +
  ylab('Variables') +
  xlab('Clusters') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# A different color pallette if you prefer this one.
# Lighter color indicates higher value
hm.palette <- colorRampPalette(rev(brewer.pal(9, 'YlOrRd')), space='Lab')
ggplot(cm, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colours = hm.palette(100)) +
  ylab('Variables') +
  xlab('Clusters') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#**********************************************************
# Now, apply PAM
pamres <- pam(d, 4, metric="euclidean", stand=FALSE)
pamres
pamres$clustering # cluster memberships
pamres$clusinfo # Silhouette statistics for four clusters

sil <-silhouette(pamres)
print(sil) # Silhouette values for individual subjects
unsil <- unclass(sil) # We need to "unclass" SIL and then define it as a data.frame.
sildf <- as.data.frame(unsil)# define unsil as a data.frame
class(sildf) # checking if sildf is a data frame (so that we can use the next "by" function)
avsil <- by(sildf$sil_width, sildf$cluster, mean ) # compute average silhouette width per cluster
# Average of silhouette widths for each cluster. 
avsil

#*************************************************************
# Average Linkage Method (HIERARCHICAL CLUSTERING)
hc <- hclust(dist(d), "ave")
hc
# Enlarge the plot area before you execute the following statement
plot(hc) 
# If the plot uses only half of the plot area on the right
# enter dev.off() below in the console just after R prompt > .
# > dev.off()
# Cut dendrogram at k-4 clusters to identify cluster membersip
ave.clustering = cutree(hc, k=4) 
# cluster membership when we stop combining clusters at 4 clusters.

# ****************************************************************
# CLUSTER VALIDATION USING THE RESULST OF K-MEANS, PAM AND SINGLE 
# LINKAGE METHOD.


#===============================================================
# K-means vs. PAM
# Crosstabulate k-means cluster membership and PAM cluster membership
# K-means clusters are rows in the result below
# Pam clusters are columns
table(kmres$cluster, pamres$clustering)
# Compute Rand Index  RI
randIndex(kmres$cluster, pamres$clustering,correct=FALSE )
# Now, compute Adjusted Rand Index ARI
randIndex(kmres$cluster, pamres$clustering,correct=TRUE)

#==============================================================
# K-means vs Average  Linkage Method
# K-means clusters are rows in the result below
# Average Linkage clusters are columns
table(kmres$cluster, ave.clustering)
# Compute Rand Index  RI
randIndex(kmres$cluster, ave.clustering,correct=FALSE )
# Now, compute Adjusted Rand Index ARI
randIndex(kmres$cluster, ave.clustering,correct=TRUE)

# ===========================================================
# Average Linkage Method vs PAM
# Average Linkage clusters are rows
table(ave.clustering, pamres$clustering)
# Compute Rand Index  RI
randIndex(ave.clustering,pamres$clustering,correct=FALSE )
# Now, compute Adjusted Rand Index ARI
randIndex(ave.clustering,pamres$clustering,correct=TRUE)

