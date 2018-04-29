notes <- read.csv("donnees/sy02-p2016.csv", na.strings="", header=T)
notes$nom <- factor(notes$nom, levels=notes$nom)
notes$correcteur.median <- factor(notes$correcteur.median, levels=c("Cor1","Cor2","Cor3","Cor4","Cor5","Cor6","Cor7","Cor8"))
notes$correcteur.final <- factor(notes$correcteur.final, levels=c("Cor1","Cor2","Cor3","Cor4","Cor5","Cor6","Cor7","Cor8"))
notes$niveau <- factor(notes$niveau, ordered=T)
notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"), ordered=T)

# valeur manquante
notes[which(is.na(notes$dernier.diplome.obtenu)),]
notes[which(is.na(notes$note.median)),]
notes[which(is.na(notes$correcteur.median)),] # lie avec note.median
notes[which(is.na(notes$note.final)),] # un peu lie avec median
notes[which(is.na(notes$correcteur.final)),] # lie avec final
notes[which(is.na(notes$note.totale)),] # lie avec median et final
notes[which(is.na(notes$resultat)),] # res lie avec note totale

plot(notes)
notes

boxplot(resultat~dernier.diplome.obtenu, data = notes)
boxplot(resultat~specialite, data = notes)
boxplot(resultat~niveau, data = notes)
boxplot(note.median~correcteur.median, data = notes)
boxplot(note.final~correcteur.final, data = notes)

# median
# aggregate : Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form
moy.median <- aggregate(note.median~correcteur.median, data=notes, FUN=mean)
names(moy.median) <- c("correcteur","moy.median")
std.median <- aggregate(note.median~correcteur.median, data=notes, FUN=sd)
names(std.median) <- c("correcteur","std.median")
# merge : Merge two data frames by common columns or row names, or do other versions of database join operations.
median <- merge(moy.median, std.median)

# final
moy.final <- aggregate(note.final~correcteur.final, data=notes, FUN=mean)
names(moy.final) <- c("correcteur","moy.final")
std.final <- aggregate(note.final~correcteur.final, data=notes, FUN=sd)
names(std.final) <- c("correcteur","std.final")
final <- merge(moy.final, std.final)

# correcteurs
correcteurs <- merge(median, final, all=T)
corr.acp <- correcteurs[-c(2,3),]

mf1 <- as.matrix(corr.acp[,2:5])
mf <- scale(mf1, scale = FALSE)
covariance <- cov(mf)
inertieTotal <- sum(eigen(covariance)$values)
eigen(covariance)$values[1]/inertieTotal
# composants pricipals
vecteurP <- eigen(covariance)$vectors
# coord dans ACP
corrACP <- mf1 %*% vecteurP
# question 2
plot(corrACP[,c(2,1)], pch=20, asp=1)
# comparer avec jai calcule dans tp2, un peu diff
var(corr.acp[,2])
print(inertie(corr.acp[,2:5]))
var(corr.acp[,2])/inertie(corr.acp[,2:5])
inertie <- function(donnee)
{
  res <- 0
  p <- dim(donnee)[2]
  for(j in 1:p)
  {
    tmp <- donnee[,j]
    moy <- mean(donnee[,j])
    res <- res + sum((tmp - moy)^2)
  }
  #ici le poids est 1/n
  res <- 1/length(tmp) * res
}

# question 3 p43
correlation <- cor(mf1, corrACP)
#D <- diag(1/(sqrt((n-1)/n)*apply(X, 2, sd))) %*% U %*% sqrt(L)
plot(-1:1,-1:1,type="n",xlab="Axe 1",ylab="Axe 2")
points(x = correlation[,1], y = correlation[,2])
text(correlation[,c(1,2)], row.names(correlation), pos=4);
abline(h=0)
abline(v=0)
curve(sqrt(1-x^2),-1,1,add=TRUE)
curve(-sqrt(1-x^2),-1,1,add=TRUE)

# question 4
corrACP[,1] %*% t(vecteurP[,1])

# q5 = q2
corr2.acp <- correcteurs[c(2,3),]
corr2.acp[1,4] <- mean(corr.acp[,4])
corr2.acp[1,5] <- mean(corr.acp[,5])
corr2.acp[2,2] <- mean(corr.acp[,2])
corr2.acp[2,3] <- mean(corr.acp[,3])
mf2 <- as.matrix(corr2.acp[,2:5])
for(j in 1:4)
{
  #apply(mf2[,j], mf2[,j] - mean(mf[,j]))
  mf2[,j] <- mf2[,j] - mean(corr.acp[,j+1])
}
corrACP2 <- mf2 %*% vecteurP
plot(corrACP[,c(2,1)], pch=20, asp=1)
points(x = corrACP2[,1], y = corrACP2[,2])

plot(corrACP[,c(3,1)], pch=20, asp=1)
points(x = corrACP2[,1], y = corrACP2[,3])


# Crab

library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]
summary(crabs)
summary(crabsquant)
boxplot(crabsquant)

class(crabsquant)
crabsquant_c <- scale(crabsquant, scale = FALSE)
cov_crab <- cov(crabsquant_c)
eigen(cov_crab)$values
cor(crabsquant_c)
plot(crabsquant_c)
# composants pricipals
vp_crab <- eigen(cov_crab)$vectors
# coord dans ACP
crabACP <- crabsquant_c %*% vp_crab
plot(crabACP[,c(2,1)], pch=20, asp=1)
plot(crabACP[,c(2,1)], col=c("red", "blue")[crabs$sp], pch=c(21,24)[crabs$sp])

# Pima

Pima <- read.csv("donnees/Pima.csv", header = T)
Pima$z <- factor(Pima$z)
plot(Pima)
  
  
  
  
  
  
  
  
  

