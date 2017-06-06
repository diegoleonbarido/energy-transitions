source('/Users/diego/Desktop/Projects_Code/vtreat/R/cleanTreatment.R')
source('/Users/diego/Desktop/Projects_Code/vtreat/R/deviationFact.R')
source('/Users/diego/Desktop/Projects_Code/vtreat/R/effectTreatmentC.R')
source('/Users/diego/Desktop/Projects_Code/vtreat/R/effectTreatmentN.R')
source('/Users/diego/Desktop/Projects_Code/vtreat/R/indicatorTreatment.R')
source('/Users/diego/Desktop/Projects_Code/vtreat/R/isBadTreatment.R')
source('/Users/diego/Desktop/Projects_Code/vtreat/R/outOfSample.R')
source('/Users/diego/Desktop/Projects_Code/vtreat/R/prevalenceFact.R')
source('/Users/diego/Desktop/Projects_Code/vtreat/R/utils.R')
source('/Users/diego/Desktop/Projects_Code/vtreat/R/vtreat.R')
source('/Users/diego/Desktop/Projects_Code/vtreat/R/vtreatImpl.R')

library(vtreat)

# design treatment plan
treatmentsN <- designTreatmentsN(x_countries_above_2_train,setdiff(colnames(x_countries_above_2_train),'country_diff'),'country_diff',verbose=FALSE)

#Need to create variable types for the different groups depending on whether they belong to one group (footprint) or the other (financing)
scoreFrame = treatmentsN$scoreFrame
scoreFrame$vartype = ifelse(grepl("noise", scoreFrame$varName), "noise", "signal")

dotplot_identity(scoreFrame, "varName", "sig", "vartype") + 
  coord_flip()  + ggtitle("Y-Aware: Variable Significance estimatees") #+ 
  #scale_color_manual(values = c("noise" = "#d95f02", "signal" = "#1b9e77")) 


# prepare the treated frames, with y-aware scaling
examplePruneSig = 1.0 
dTrainNTreatedYScaled <- prepare(treatmentsN,x_countries_above_2_train,pruneSig=examplePruneSig,scale=TRUE)
#dTestNTreatedYScaled <- prepare(treatmentsN,dTest,pruneSig=examplePruneSig,scale=TRUE)

# get the variable ranges
ranges = vapply(dTrainNTreatedYScaled, FUN=function(col) c(min(col), max(col)), numeric(2))
rownames(ranges) = c("vmin", "vmax") 
rframe = as.data.frame(t(ranges))  # make ymin/ymax the columns
rframe$varName = rownames(rframe)
varnames = setdiff(rownames(rframe), "country_diff")
rframe = rframe[varnames,]
rframe$vartype = ifelse(grepl("noise", rframe$varName), "noise", "signal")

#Barbell Plot
barbell_plot(rframe, "varName", "vmin", "vmax", "vartype") +
  coord_flip() + ggtitle("y-scaled variables: ranges") + 
  scale_color_manual(values = c("noise" = "#d95f02", "signal" = "#1b9e77"))




vars <- setdiff(colnames(dTrainNTreatedYScaled),'country_diff')
# prcomp defaults to scale. = FALSE, but we already scaled/centered in vtreat- which we don't want to lose.
dmTrain <- as.matrix(dTrainNTreatedYScaled[,vars])
#dmTest <- as.matrix(dTestNTreatedYScaled[,vars])
princ <- prcomp(dmTrain, center = FALSE, scale. = FALSE)
dotplot_identity(frame = data.frame(pc=1:length(princ$sdev), 
                                    magnitude=princ$sdev), 
                 xvar="pc",yvar="magnitude") +
  ggtitle("Y-Scaled variables: Magnitudes of singular values")






proj <- extractProjection(20,princ)
rot5 <- extractProjection(5,princ)
rotf = as.data.frame(rot5)
rotf$varName = rownames(rotf)
rotflong = gather(rotf, "PC", "loading", starts_with("PC"))
rotflong$vartype = ifelse(grepl("noise", rotflong$varName), "noise", "signal")

dotplot_identity(rotflong, "varName", "loading", "vartype") + 
  facet_wrap(~PC,nrow=1) + coord_flip() + 
  ggtitle("Y-Scaled Variable loadings, first five principal components") + 
  scale_color_manual(values = c("noise" = "#d95f02", "signal" = "#1b9e77"))


# apply projection
projectedTrain <- as.data.frame(dmTrain %*% proj,
                                stringsAsFactors = FALSE)
# plot data sorted by principal components
projectedTrain$country_diff <- dTrainNTreatedYScaled$country_diff
ScatterHistN(projectedTrain,'PC1','PC2','country_diff',
             "Y-Scaled Training Data projected to first two principal components")

# Regression with the first two principal components

ncomp = 20
varexpr = paste(paste("PC", 1:ncomp, sep=''), collapse='+')
fmla = paste("country_diff ~", varexpr)

model <- lm(fmla,data=projectedTrain)
summary(model)


projectedTrain$estimate <- predict(model,newdata=projectedTrain)
trainrsq = rsq(projectedTrain$estimate,projectedTrain$country_diff)

ScatterHist(projectedTrain,'estimate','country_diff','Recovered model versus truth (y aware PCA train)',
            smoothmethod='identity',annot_size=3)


summary(princ)

vars <- apply(princ$x, 2, var)  
props <- vars / sum(vars)
princ_cumulative <- cumsum(props)
plot(princ_cumulative,xlab="Principal Component",ylab="Cumulative Proportion of Variance",ylim=c(0,1),xlim=c(0,50))

pca_analysis <- princ

loads <- as.data.frame(with(pca_analysis, unclass(rotation)))

for(i in 1:length(names(loads))){
  loads[,i] <- (abs(loads[,i])/sum(abs(loads[,i])))*100
}

names_df <- as.data.frame(dmTrain)
loads$var_name <- names(names_df)

gathered_loads <- loads %>% gather(key='var_name',value='value',PC1:PC45)
names(gathered_loads)[2]<-'pc_name'

aggregates <- aggregate(gathered_loads$value,by=list(gathered_loads$var_name),FUN=mean)

#Find which are the variables that contribute the most to the principal components
aggregates_sort <- aggregates[order(-aggregates$x),]
gen_ren_eia[order(gen_ren_eia$Country.Name,gen_ren_eia$year),]

#PC1
pc1 <- subset(gathered_loads,gathered_loads$pc_name=='PC1') %>% mutate(PC1=value) %>% select(var_name,PC1)
pc2 <- subset(gathered_loads,gathered_loads$pc_name=='PC2') %>% mutate(PC2=value) %>% select(PC2)
pc3 <- subset(gathered_loads,gathered_loads$pc_name=='PC3') %>% mutate(PC3=value) %>% select(PC3)
pc4 <- subset(gathered_loads,gathered_loads$pc_name=='PC4') %>% mutate(PC4=value) %>% select(PC4)
pc5 <- subset(gathered_loads,gathered_loads$pc_name=='PC5') %>% mutate(PC5=value) %>% select(PC5)
pc6 <- subset(gathered_loads,gathered_loads$pc_name=='PC6') %>% mutate(PC6=value) %>% select(PC6)
pc7 <- subset(gathered_loads,gathered_loads$pc_name=='PC7') %>% mutate(PC7=value) %>% select(PC7)
pc8 <- subset(gathered_loads,gathered_loads$pc_name=='PC8') %>% mutate(PC8=value) %>% select(PC8)
pc9 <- subset(gathered_loads,gathered_loads$pc_name=='PC9') %>% mutate(PC9=value) %>% select(PC9)
pc10 <- subset(gathered_loads,gathered_loads$pc_name=='PC10') %>% mutate(PC10=value) %>% select(PC10)


# Order by varname or value and then rbind for heatplot

d_pcs <- do.call("cbind", list(pc1, pc2, pc3,pc4,pc5,pc6,pc7,pc8,pc9,pc10))
d_pcs_melt <- melt(d_pcs,id=c("var_name"))

# Plotting the contribution of each principal component to the total variance
ggplot(d_pcs_melt,aes(variable,var_name)) + geom_tile(aes(fill=value), colour = "white") + scale_fill_gradient(low="white",high="steelblue")



#####################################################
#####################################################
#####################################################
# Kmeans


comp <- data.frame(princ$x[,1:20])

k.max <- 20
data <- comp
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Silhouette Score
pamk.best <- pamk(data)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(data, pamk.best$nc))

# Three clusters as evaluated through the elbow method
k <- kmeans(comp,3,nstart=50,iter.max = 1000)



sil = silhouette(k$cluster, dis)
windows() 
plot(sil)

palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp[,1:5], col=k$clust, pch=16)

sort(table(k$clust))

clust <- names(sort(table(k$clust)))

row.names(x_countries_above_2_train)

row.names(x_countries_above_2_train[k$clust==clust[1],])
row.names(x_countries_above_2_train[k$clust==clust[2],])
row.names(x_countries_above_2_train[k$clust==clust[3],])
row.names(x_countries_above_2_train[k$clust==clust[4],])
row.names(x_countries_above_2_train[k$clust==clust[5],])
row.names(x_countries_above_2_train[k$clust==clust[6],])
row.names(x_countries_above_2_train[k$clust==clust[7],])
row.names(x_countries_above_2_train[k$clust==clust[8],])
row.names(x_countries_above_2_train[k$clust==clust[9],])
row.names(x_countries_above_2_train[k$clust==clust[10],])


############## Explore groups within clusters

cluster_analysis <- countries_above_2
cluster_analysis$Cluster <- ifelse(cluster_analysis$Country.Name %in% c("Aruba","Cape Verde", "Falkland Islands (Islas Malvinas)","Faroe Islands","Iceland","Korea, South", "Netherlands Antilles", "New Caledonia", "Taiwan", "Togo"),"Cluster 1", ifelse(cluster_analysis$Country.Name %in% c( "Australia", "Austria", "Belgium", "Canada", "Croatia", "Czech Republic", "Denmark", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Japan", "Latvia", "Lithuania", "Luxembourg", "Netherlands", "New Zealand", "Poland", "Portugal", "Singapore", "Slovakia", "Spain", "Sweden", "Switzerland", "Thailand", "United Kingdom", "United States", "Uruguay"),"Cluster 2","Cluster 3"))

#sumss3<- summary(subset(cluster_analysis,cluster_analysis$Cluster=="Cluster 3"))
#write.csv(sumss1,"caca1.csv")
#write.csv(sumss2,"caca2.csv")
#write.csv(sumss3,"caca3.csv")

cluster_analysis_middle <- middle
cluster_analysis_middle$Cluster <- ifelse(cluster_analysis_middle$Country.Name %in% c("Aruba"),"Cluster 1", ifelse(cluster_analysis_middle$Country.Name %in% c("Mauritania", "Mongolia", "Morocco", "Papua New Guinea", "Philippines", "Thailand", "Togo", "Tunisia"),"Cluster 2",ifelse(cluster_analysis_middle$Country.Name %in% c("Costa Rica", "El Salvador", "Guatemala", "Honduras", "Jamaica", "Kenya", "Malta", "Nicaragua", "Sri Lanka"),"Cluster 3",ifelse(cluster_analysis_middle$Country.Name %in% c("Argentina", "Brazil", "China", "Colombia", "Ecuador", "Ethiopia", "India", "Indonesia", "Mexico", "Peru", "South Africa"),"Cluster 4","Cluster 5"))))

sumss5<- summary(subset(cluster_analysis_middle,cluster_analysis_middle$Cluster=="Cluster 5"))
write.csv(sumss1,"caco1.csv")
write.csv(sumss2,"caco2.csv")
write.csv(sumss3,"caco3.csv")
write.csv(sumss4,"caco4.csv")
write.csv(sumss5,"caco5.csv")













#########
#########
#########
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






############### 
############### 
############### Hierarchical Clustering of Prediction  Variables


p <- princ

loadings = p$rotation[]

x = loadings[,1]
y = loadings[,2]
z = loadings[,3] 

hc = hclust(dist(cbind(x,y)), method = 'ward.D2')
plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2')
rect.hclust(hc, k=3, border='red')


hcd = as.dendrogram(hc)
plot(hcd)

plot(hcd, type = "triangle")


op = par(mfrow = c(2, 1))
plot(cut(hcd, h = 0.8)$upper[[2]], main = "Upper tree of cut at h=75")
plot(cut(hcd, h = 0.8)$lower[[2]], main = "Second branch of lower tree with cut at h=75")

hc = hclust(dist(cbind(x,y)), method = 'ward.D2')
plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2')
rect.hclust(hc, k=3, border='red')


x_countries_above_2_train




###########
###########






