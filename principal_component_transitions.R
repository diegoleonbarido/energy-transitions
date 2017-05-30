######

# https://www.r-bloggers.com/principal-components-regression-pt-1-the-standard-method/

#### Prediction

#Libraries & Path
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)

#Function libraries
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/BinaryYScatterPlot.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/ClevelandDotPlot.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/ConditionalSmoothedScatterPlot.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/DiscreteDistribution.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/DistributionPlot.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/DoubleDensity.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/DoubleHistogram.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/GainCurve.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/PRPlot.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/ROC.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/ScatterBoxPlot.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/ScatterHist.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/ScatterHistC.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/ShadedDensity.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/sharedFunctions.R')
source('/Users/diego/Desktop/Projects_Code/WVPlots/R/WVPlots.R')


devtools::install_github('WinVector/WVPlots', build_vignettes=TRUE)

setwd('/Users/diego/Desktop/Data/energy_transitions')


###############################
##############################
#Preparing data for PCA

quality_governance_pca <- read.csv('qog_std_cs_jan17.csv') %>% mutate(Country.Name=cname)
quality_governance_pca$Country.Name <- gsub('Cyprus (1975-)','Cyprus',quality_governance_pca$Country.Name,fixed=TRUE)
quality_governance_pca$Country.Name <- gsub('Ethiopia (1993-)','Ethiopia',quality_governance_pca$Country.Name,fixed=TRUE)
quality_governance_pca$Country.Name <- gsub('France (1963-)','France',quality_governance_pca$Country.Name,fixed=TRUE)

eia_diff_merge <- merge(eia_diff,quality_governance_pca, by='Country.Name')

#Either include all of it or not
#eia_diff_merge_policy <- merge(eia_diff_merge,policies_agg)
eia_diff_merge_policy <- merge(eia_diff,policies_agg)


eia_pca <- merge(eia_diff_merge_policy,wb_frame,by="Country.Name")
eia_pca <- merge(eia_pca,fossil_subsidies,by="Country.Name")
eia_pca <- merge(eia_pca,financing,by="Country.Name")
eia_pca <- merge(eia_pca,eco_print,by="Country.Name") 
eia_pca <- merge(eia_pca,hdi_diff, by="Country.Name")

countries_above_2 <- subset(eia_pca,eia_pca$country_diff>=1)

countries_above_2$dollar_km <- (countries_above_2$investments_udmm*1000000)/countries_above_2$land_area
countries_above_2$dollar_person <- (countries_above_2$investments_udmm*1000000)/countries_above_2$population
countries_above_2$aid_gdp <- (countries_above_2$net_assist)/countries_above_2$Nominal.GDP.US...billions

#Setting up the principal components
# Running with the full merge
#countries_above_2 <- subset(countries_above_2, select=-c(version,ccode,cname,ccodealp,ccodecow,ccodewb,country_hdi_name,Income.Group,Data.Quality,Population..millions.,Region,Per.Capita.GDP,Country.region,X,X.1,Electricity_GDP,Electricity_Billions,Electricity_capita,year))
# Running with the partial merge
#countries_above_2 <- subset(countries_above_2, select=-c(year,X.1,X,Country.region,Data.Quality,Region,Income.Group,country_hdi_name))
#countries_above_2$Per.Capita.GDP<- as.numeric(as.character(countries_above_2$Per.Capita.GDP))

#Checking for factors and getting rid of them
for(i in 1:length(names(countries_above_2))){
  ifelse(class(countries_above_2[,i])=="factor", print(paste(class(countries_above_2[,i]),i)),NA)
}
countries_above_2$country_diff <- as.numeric(countries_above_2$country_diff)
countries_above_2$current_non_hydro_gen <- as.numeric(countries_above_2$current_non_hydro_gen)
row.names(countries_above_2) <- countries_above_2$Country.Name


x_countries_above_2 <- countries_above_2[,-1] #Remove name
#x_countries_above_2 <- x_countries_above_2[,-1] #Remove diff
x_countries_above_2 <- x_countries_above_2[,-2] #Remove actual generation


#Removing countries with NAs then coming back to them
for(i in 1:length(names(x_countries_above_2))){
  for (j in 1:length(x_countries_above_2[,i])){
  if(is.na(x_countries_above_2[j,i])==TRUE){x_countries_above_2[j,i] <- 0} else {}
  }
}


###### Scaled example

#x_countries_above_2_train <- x_countries_above_2[-c(2,4,5,9,10,23,45),]
x_countries_above_2_test <- x_countries_above_2[c(2,4,5,9,10,23,45),]

# scale the data
dTrainNTreatedXscaled <- as.data.frame(x_countries_above_2,center=TRUE,scale=TRUE,stringsAsFactors = FALSE)
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
  coord_flip() + ggtitle("x scaled variables: ranges") + 
  scale_color_manual(values = c("noise" = "#d95f02", "signal" = "#1b9e77"))


#Dropping variables that have a variance of zero as the pca won't run without it
dTrainNTreatedXscaled_dropped <- dTrainNTreatedXscaled[, sapply(dTrainNTreatedXscaled, function(v) var(v, na.rm=TRUE)!=0)]
dTestNTreatedXscaled_dropped <- dTestNTreatedXscaled[, sapply(dTrainNTreatedXscaled, function(v) var(v, na.rm=TRUE)!=0)]

# The principal component analysis
vars = setdiff(colnames(dTrainNTreatedXscaled_dropped), "country_diff")

dmTrain <- as.matrix(dTrainNTreatedXscaled_dropped[,vars])
dmTest <- as.matrix(dTestNTreatedXscaled_dropped[,vars])

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
ncomp = 46

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


