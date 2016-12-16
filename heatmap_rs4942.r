# Rosechell Spencer III
# rs4942
# heatmap_rs4942.r
# December 8, 2016

# set working directory
setwd("/home/rs4942")

# read file
data2cluster = read.table("data2cluster.txt", sep = "\t", header = T, row.names = 1)

# create appropriate levels/factors
my_factors = factor(rep(c( "RootCtrl","RootTrt","ShootCtrl","ShootTrt"), times=3))

# apply function(MEAN) over all data according to levels/factor
ExpAverage = apply(data2cluster, 1, tapply, my_factors, mean)

# create log data matrix
# transpose data
# apply log2 function according to each column/gene
LogDataM = as.matrix((log2(ExpAverage)))

# clustering log transformed data
# and distance object
# euclidean distance is default
data.dist = as.dist(1-cor(LogDataM))
data.hclust = hclust(data.dist, method="average")

############ sanity check for quality ###############
# adjust plot margins
# par(mar = c(2,2,2,2))
# plot(data.hclust)

# create a heatmap object which uses functions
# performs clustering
# produce dendrograms along each axis
heatmap.2(as.matrix(data.dist), 
          col= greenred(75),
          hclustfun = hclust2,
          distfun = dist2,
          scale = "row",
          cexCol = 0.6,
          Colv= TRUE,
          sepcolor= "black",
          dendrogram = "both",
          key = TRUE,
          density.info = "none",
          trace = "none",
          cexRow = 0.4)

# refresh plot
# dev.off()