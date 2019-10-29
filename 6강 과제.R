library(ade4)
data(deug)
head(deug)
head(deug$tab)
summary(deug$tab)

cor_matrix = cor(deug$tab)
library(corrplot)
corrplot(cor_matrix)

library(stats)
pca = princomp(deug$tab, cor=T, scores=T)
pca
summary(pca)
biplot(pca, cex=0.7)


write.csv(deug$tab, "C:/Users/Choi Sung Wook/Desktop/deug.csv")

