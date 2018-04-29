hist.facteur <- function(var_qua, var_quan){
  #assurer dans le meme dimension
  inter <- seq(min(var_quan),max(var_quan),by=(max(var_quan)-min(var_quan))/10)
  #names(summary(iris$Species))]
  h <- rep(0, length(inter) - 1)
  for(lev in levels(var_qua)){
    h1 <- hist(plot=F, var_quan[var_qua==lev], breaks=inter)
    h <- rbind(h, h1$count)
  }
  h <- h[-1,]
  #mycols <- runif(length(inter), min = 1, max = length(colors()))
  #mycols <- sample(1:length(colors()), length(levels(var_qua)), replace = FALSE)
  barplot(h, space=0, legend=levels(var_qua), main="Histogramme") #, col = mycols
}
hist.facteur(Species, Petal.Length)

