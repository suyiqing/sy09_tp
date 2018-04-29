data(iris)
summary(iris)
plot(iris[,-1])
plot(iris)
plot(iris[,1:4], col=c("red","green","blue")[iris$Species])

#Classical multidimensional scaling (MDS) of a data matrix.
cmdscale

mut <- read.csv("donnees/mutations2.csv", header=T, row.names=1)
mut <- as.dist(mut, diag=T, upper=T)
mds_mut <- cmdscale(mut)
plot(mds_mut)
text(mds_mut, row.names(mds_mut), pos=1)

library(MASS)
mut.sh <- Shepard(mut, mds_mut)
plot(mut.sh)
mds_mut3 <- cmdscale(mut, k=3)
mut.sh3 <- Shepard(mut, mds_mut3)
plot(mut.sh3)
mds_mut4 <- cmdscale(mut, k=4)
mut.sh4 <- Shepard(mut, mds_mut4)
plot(mut.sh4)
mds_mut5 <- cmdscale(mut, k=5)
mut.sh5 <- Shepard(mut, mds_mut5)
plot(mut.sh5)

# interaction computes a factor which represents the interaction of the given factors.
# The result of interaction is always unordered.
interaction
#exemple
a <- gl(2, 4, 8)
b <- gl(2, 2, 8, labels = c("ctrl", "treat"))
s <- gl(2, 1, 8, labels = c("M", "F"))
interaction(a, b)
#[1] 1.ctrl  1.ctrl  1.treat 1.treat 2.ctrl  2.ctrl  2.treat 2.treat
#Levels: 1.ctrl 2.ctrl 1.treat 2.treat

mut_hc <- hclust(mut) 
plot(mut_hc)
