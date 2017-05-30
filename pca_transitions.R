###### Scaled example

#x_countries_above_2_train <- x_countries_above_2[-c(2,4,5,9,10,23,45),]
x_countries_above_2_train <- x_countries_above_2
x_countries_above_2_test <- x_countries_above_2[c(2,4,5,9,10,23,45),]

# scale the data
dTrainNTreatedXscaled <- as.data.frame(scale(x_countries_above_2[,colnames(x_countries_above_2)!='country_diff'],center=TRUE,scale=TRUE),stringsAsFactors = FALSE)
dTrainNTreatedXscaled$country_diff <- x_countries_above_2$country_diff

dTestNTreatedXscaled <- as.data.frame(x_countries_above_2_test,center=TRUE,scale=TRUE,stringsAsFactors = FALSE)
dTestNTreatedXscaled$country_diff <- x_countries_above_2_test$country_diff


# get the variable ranges
ranges = vapply(dTrainNTreatedXscaled, FUN=function(col) c(min(col), max(col)), numeric(2))
rownames(ranges) = c("vmin", "vmax") 
rframe = as.data.frame(t(ranges))  # make ymin/ymax the columns
rframe$varName = rownames(rframe)
varnames = setdiff(rownames(rframe), "country_diff") #y & country_diff
rframe = rframe[varnames,]
rframe$vartype = ifelse(grepl("noise", rframe$varName), "noise", "signal")

summary(dTrainNTreatedXscaled[, c("country_diff","wef_qoi", "wef_elec","icrg_qog", "policy_num")])


# Scaled feature ranges
barbell_plot(rframe, "varName", "vmin", "vmax", "vartype") +
  coord_flip() + ggtitle("x scaled variables: ranges") 


#Dropping variables that have a variance of zero as the pca won't run without it
dTrainNTreatedXscaled_dropped <- dTrainNTreatedXscaled[, colSums(is.na(dTrainNTreatedXscaled)) != nrow(dTrainNTreatedXscaled)]
dTrainNTreatedXscaled_dropped <- dTrainNTreatedXscaled[, sapply(dTrainNTreatedXscaled, function(v) var(v, na.rm=TRUE)!=0)]
dTestNTreatedXscaled_dropped <- dTestNTreatedXscaled[, sapply(dTrainNTreatedXscaled, function(v) var(v, na.rm=TRUE)!=0)]

# The principal component analysis
#vars = setdiff(colnames(dTrainNTreatedXscaled), "country_diff")
vars = setdiff(colnames(dTrainNTreatedXscaled_dropped), "country_diff")

#dmTrain <- as.matrix(dTrainNTreatedXscaled[,vars])
dmTrain <- as.matrix(dTrainNTreatedXscaled_dropped[,vars])
#dmTest <- as.matrix(dTestNTreatedXscaled_dropped[,vars])

#Dropping variables that might have mean 0
#for(i in 1:dim(dmTrain)[1]) {
# if(mean(dmTrain[,i])==0){
#    print(i)}
#else{}
#}
#dmTrain <- dmTrain[,-c(28,30)]

princ <- prcomp(dmTrain,center = TRUE,scale. = TRUE) 
dotplot_identity(frame = data.frame(pc=1:length(princ$sdev), 
                                    magnitude=princ$sdev), 
                 xvar="pc",yvar="magnitude") +
  ggtitle("x scaled variables: Magnitudes of singular values")



# Looking at the five first principal components

rot5 <- extractProjection(5,princ)
rotf = as.data.frame(rot5)
rotf$varName = rownames(rotf)
rotflong = gather(rotf, "PC", "loading", starts_with("PC"))
rotflong$vartype = ifelse(grepl("noise", rotflong$varName), "noise", "signal")

dotplot_identity(rotflong, "varName", "loading", "vartype") + 
  facet_wrap(~PC,nrow=1) + coord_flip() + 
  ggtitle("x scaled variable loadings, first 5 principal components") + 
  scale_color_manual(values = c("noise" = "#d95f02", "signal" = "#1b9e77"))


# get all the principal components
# not really a projection as we took all components!
projectedTrain <- as.data.frame(predict(princ,dmTrain),stringsAsFactors = FALSE)
projectedTrain$country_diff <- dTrainNTreatedXscaled_dropped$country_diff
ncomp = 50

# here we will only model with the first ncomp principal components
varexpr = paste(paste("PC", 1:ncomp, sep=''), collapse='+')
fmla = paste("country_diff ~", varexpr)

model <- lm(fmla,data=projectedTrain)
summary(model)

projectedTrain$estimate <- predict(model,newdata=projectedTrain)
ScatterHist(projectedTrain,'estimate','country_diff','Recovered 20 variable model versus truth (train)',
            smoothmethod='identity',annot_size=3)

plot(projectedTrain$estimate,projectedTrain$country_diff)
cor(projectedTrain$estimate,projectedTrain$country_diff)



###### Evaluating the principal components

pca_analysis <- princ

loads <- as.data.frame(with(pca_analysis, unclass(rotation)))


for(i in 1:length(names(loads))){
  loads[,i] <- (abs(loads[,i])/sum(abs(loads[,i])))*100
}

names_df <- as.data.frame(dmTrain)
loads$var_name <- names(names_df)

gathered_loads <- loads %>% gather(key='var_name',value='value',PC1:PC53)

aggregates <- aggregate(gathered_loads$value,by=list(gathered_loads$var_name),FUN=mean)

#Find which are the variables that contribute the most to the principal components
aggregates_sort <- aggregates[order(-aggregates$x),]
gen_ren_eia[order(gen_ren_eia$Country.Name,gen_ren_eia$year),]










####### Y-scaled PCA

dTrain <- pepe[,1:3]
dTrain$
  
  
  treatmentsN <- designTreatmentsN(dTrain,setdiff(colnames(dTrain),'country_diff'),'country_diff',verbose=FALSE)

treatmentsN <- designTreatmentsN(dTrain,setdiff(colnames(dTrain),'country_diff'),'country_diff',
                                 weights=NULL,
                                 minFraction=,smFactor=NULL,
                                 rareCount=NULL,rareSig=NULL,
                                 collarProb=NULL,
                                 splitFunction=,ncross=NULL,
                                 verbose=FALSE,
                                 parallelCluster=NULL)

for(i in 1:length(unique(dTrain$current_non_hydro_gen))){
  print(paste(class(dTrain[,i]),i))
}


