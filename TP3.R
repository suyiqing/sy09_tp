rm(moy)

notes <- read.table("donnees/notes.txt", header=T)
class(notes)

summary(notes)
boxplot(notes)

plot(notes)
plot(math~scie, data=notes, pch=20, asp=1)
text(notes[,c("scie","math")], row.names(notes), pos=1)
plot(fran~lati, data=notes, pch=20, asp=1)
text(notes[,c("lati","fran")], row.names(notes), pos=1)
plot(d.m~1, data=notes, pch=20, asp=1)
text(notes[,c("d.m")], row.names(notes), pos=1)

#presentation de dispersion, ajouter deux colonnes pour les individus
# pas correct : mean(notes$math+notes$scie)
# pas correct : mean(notes$fran+notes$lati)
notes_moy <- cbind(notes, (notes$math+notes$scie)/2)
notes_moy <- cbind(notes_moy, (notes$fran+notes$lati)/2)
names(notes_moy) <- c(names(notes), "moy_sci", "moy_litt")
notes_moy
plot(moy_sci~moy_litt, data=notes_moy, pch=20, asp=1)
text(notes_moy[,c("moy_litt","moy_sci")], row.names(notes_moy), pos=1)

#P matrice de passage
P <- matrix(c(1/2,1/2,0,0,0,0,0,1/2,1/2,0,1/2,-1/2,0,0,0,0,0,1/2,-1/2,0,0,0,0,0,1),c(5,5))
tmp <- P %*% t(P)
# Nprime coord dans la nouvelle base
Nprime <- t(solve(P) %*% t(notes))
Ncano <- t(P %*% t(Nprime))

plot(Nprime[,c(2,1)], pch=20, asp=1)
text(Nprime[,c(2,1)], row.names(Nprime), pos=1)
plot(Nprime[,c(3,1)], pch=20, asp=1)
text(Nprime[,c(3,1)], row.names(Nprime), pos=1)
plot(Nprime[,c(4,2)], pch=20, asp=1)
text(Nprime[,c(4,2)], row.names(Nprime), pos=1)

t <- sqrt(2)/2
P2 <- matrix(c(t,t,0,0,0,0,0,t,t,0,t,-t,0,0,0,0,0,t,-t,0,0,0,0,0,1),c(5,5))
P2 %*% t(P2)
Nprime2 <- t(solve(P2) %*% t(notes))
plot(Nprime2[,c(2,1)], pch=20, asp=1)
text(Nprime2[,c(2,1)], row.names(Nprime2), pos=1)
plot(Nprime2[,c(3,1)], pch=20, asp=1)
text(Nprime2[,c(3,1)], row.names(Nprime2), pos=1)
plot(Nprime2[,c(4,2)], pch=20, asp=1)
text(Nprime2[,c(4,2)], row.names(Nprime2), pos=1)


P3 <- matrix(c(t,0,t,0,0,0,t,0,t,0,t,0,-t,0,0,0,t,0,-t,0,0,0,0,0,1),c(5,5))
P3 %*% t(P3)
Nprime3 <- t(solve(P3) %*% t(notes))
plot(Nprime3[,c(2,1)], pch=20, asp=1)
text(Nprime3[,c(2,1)], row.names(Nprime3), pos=1)
plot(Nprime3[,c(3,1)], pch=20, asp=1)
text(Nprime3[,c(3,1)], row.names(Nprime3), pos=1)
plot(Nprime3[,c(4,2)], pch=20, asp=1)
text(Nprime3[,c(4,2)], row.names(Nprime3), pos=1)

#calcul de inertie - si c'est grande c'est bien
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
print(inertie(notes[,c(1,2)]))
print(inertie(Nprime[,c(1,2)]))
print(inertie(Nprime2[,c(1,2)]))
print(inertie(Nprime3[,c(1,2)]))

print(inertie(notes))
print(inertie(Nprime))
print(inertie(Nprime2))
print(inertie(Nprime3))
