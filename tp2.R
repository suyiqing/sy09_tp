rm(list=ls())

notes <- read.csv("donnees/median-sy02-p2014.csv", header=F, na.strings=c("NA","ABS"))
names(notes) <- c("branche","note")
notes <- notes[-which(is.na(notes$note)),]

notes$branche <- as.character(notes$branche)
notes$branche <- substr(notes$branche,1,2)
notes$branche <- as.factor(notes$branche)

hist.facteur(notes$branche, notes$note)
boxplot(note~branche, data = notes, xlab = "branche", ylab = "notes en branche")

#creer table
inter <- seq(0, 20, by=2)
tab <- rep(0, length(inter) - 1)
for (name in names(summary(notes$branche))){
  h <- hist(plot=F, notes$note[branche==name], breaks=inter)
  tab <- rbind(tab, h$counts)
}
tab <- as.data.frame(tab[-1,])
row.names(tab) <- names(summary(notes$branche))
#chisq.test(tab, simulate.p.value = TRUE)
chisq.test(tab) # 0.6466 -> indep

h1 <- hist(plot=F, notes$note[branche=="GI"], breaks=inter)
shapiro.test(h1$counts) # 0.3499 -> gaussien
h2 <- hist(plot=F, notes$note[branche=="GP"], breaks=inter)
shapiro.test(h2$counts) # 0.08024 -> gaussien
t.test(h1$counts, h2$counts) # 0.4494 -> non significative





babies <- read.table("donnees/babies23.txt", header = T)
babies <- babies[c(7,5,8,10,12,13,21,11)]
names(babies)<-c("bwt","gestation","parity","age","height","weight","smoke","education")

babies[babies$bwt == 999, 1] <- NA
babies[babies$gestation == 999, 2] <- NA
babies[babies$age == 99, 4] <- NA
babies[babies$height == 99, 5] <- NA
babies[babies$weight == 999, 6] <- NA
babies[babies$smoke == 9, 7] <- NA
babies[babies$education == 9, 8] <- NA

babies$smoke <- factor(c("NonSmoking","Smoking","NonSmoking","NonSmoking")[babies$smoke+1])
babies$education <- factor(babies$education,ordered=T)

dim(babies)
length(is.na(babies$gestation))
dim(babies[which(is.na(babies$gestation)),])

