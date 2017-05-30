library(FactoMineR)
library(factoextra)


#### Explore principal componets

res.suopa <- PCA(x_countries_above_2_train[,2:67], scale.unit=TRUE, ncp=10, graph=T)
dimdesc(res.suopa, axes=c(1:10))


##### Hierarchical Clustering


d <- dist(x_countries_above_2_train, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2") 
plot(fit,main="Hierarchical Clustering (All Data)") # display dendogram
groups <- cutree(fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=3, border="red")

##### Explore

res.pca <- PCA(x_countries_above_2_train[,2:68], graph = FALSE)
print(res.pca)

eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])

fviz_screeplot(res.pca, ncp=10)
head(res.pca$var$coord)

fviz_pca_var(res.pca)

head(res.pca$var$cos2)

fviz_pca_var(res.pca, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()

head(res.pca$var$contrib)
# Contributions of variables on PC1
fviz_pca_contrib(res.pca, choice = "var", axes = 1)

fviz_pca_contrib(res.pca, choice = "var", axes = 1:2)

fviz_pca_var(res.pca, col.var="contrib")


head(res.pca$ind$coord)


fviz_pca_ind(res.pca)

head(res.pca$ind$cos2)

fviz_pca_ind(res.pca, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50) + theme_minimal()

head(res.pca$ind$contrib)
