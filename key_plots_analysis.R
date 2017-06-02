##################### Vars
##################### Deleted Vars related to CO2 emissions from the original file

#This file has different plots
# 1. %Renewable Energy Generation vs . World Bank Development Indicators
# 2. %Renewable Energy Generation vs  Climatescope Variables
# 3. %Renewable Generation vs. Environmental Peforrmance Index Plots
# 4. %Changes in the last 10 years vs. World Development Indicators
# 5. %Changes in the last 10 years vs. Climatescope Variables
# 6. $Change in Renewables in the last 10 Years vs. %Change in Renewables Everywhere Else 
# 7. Small subset for variables that we missed in these places
# 8. K-Means Clustering

######

# https://www.r-bloggers.com/principal-components-regression-pt-1-the-standard-method/
# https://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
# https://www.r-bloggers.com/pca-and-k-means-clustering-of-delta-aircraft/
# https://www.r-bloggers.com/principal-components-regression-pt-2-y-aware-methods/
# http://www.sthda.com/english/wiki/principal-component-analysis-how-to-reveal-the-most-important-variables-in-your-data-r-software-and-data-mining#at_pco=smlwn-1.0&at_si=592b5aa2ee527e1a&at_ab=per-2&at_pos=0&at_tot=1

#Libraries & Path
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library()
library(rql)
library(RColorBrewer)
library(scales)
library(fpc)
library(cluster)
library(reshape)
library(corrgram)
library(corrplot)
library(vtreat)
library(WVPlots)
library(FactoMineR)
library(factoextra)
library(FactoMineR)
library(factoextra)

setwd('/Users/diego/Desktop/Data/energy_transitions')

install.packages(c('ggplot2','tidyr',
                   'devtools','knitr'))
devtools::install_github('WinVector/WVPlots',
                         build_vignettes=TRUE)

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

############
#Functions

# Number and Missing Countries
missing_countries <- function(datadf1,datadf2){
  countries <- subset(datadf1$Country.Name,!(datadf1$Country.Name %in% unique(datadf2$Country.Name)))
  missing_countries <- length(unique(datadf1$Country.Name)) - length(unique(datadf2$Country.Name))
  return(list(countries,missing_countries))
}

# Capitalize 
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#Trim
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Clean World Bank Names

wb_names <- function(df_wb){
  df_wb$Country.Name <- gsub('Brunei Darussalam','Brunei',df_wb$Country.Name,fixed=TRUE)
  df_wb$Country.Name <- gsub("Cote d'Ivoire",'Cote dIvoire (IvoryCoast)',df_wb$Country.Name,fixed=TRUE)
  df_wb$Country.Name <- gsub("Egypt, Arab Rep.",'Egypt',df_wb$Country.Name,fixed=TRUE)
  df_wb$Country.Name <- gsub("Hong Kong SAR, China",'Hong Kong',df_wb$Country.Name,fixed=TRUE)
  df_wb$Country.Name <- gsub("Iran, Islamic Rep.",'Iran',df_wb$Country.Name,fixed=TRUE)
  df_wb$Country.Name <- gsub("Korea, Rep.",'Korea, South',df_wb$Country.Name,fixed=TRUE)
  df_wb$Country.Name <- gsub("Russian Federation",'Russia',df_wb$Country.Name,fixed=TRUE)
  df_wb$Country.Name <- gsub("Slovak Republic",'Slovakia',df_wb$Country.Name,fixed=TRUE)
  return(df_wb)
}

# WV Plots
barbell_plot = function(frame, xvar, ymin, ymax, colorvar=NULL) {
  if(is.null(colorvar)) {
    gplot = ggplot(frame, aes_string(x=xvar))
  } else {
    gplot = ggplot(frame, aes_string(x=xvar, color=colorvar))
  }
  
  gplot + geom_point(aes_string(y=ymin)) + 
    geom_point(aes_string(y=ymax)) +
    geom_linerange(aes_string(ymin=ymin, ymax=ymax)) +
    ylab("value")
}

dotplot_identity = function(frame, xvar, yvar, colorvar=NULL) {
  if(is.null(colorvar)) {
    gplot = ggplot(frame, aes_string(x=xvar, y=yvar, ymax=yvar))
  } else {
    gplot = ggplot(frame, 
                   aes_string(x=xvar, y=yvar, ymax=yvar, 
                              color=colorvar))
  }
  gplot + geom_point() + geom_linerange(aes(ymin=0))
}

extractProjection <- function(ndim,princ) {
  # pull off the rotation.  
  proj <- princ$rotation[,1:ndim] 
  # sign was arbitrary, so flip in convenient form
  for(i in seq_len(ndim)) {
    si <- sign(mean(proj[,i]))
    if(si!=0) {
      proj[,i] <- proj[,i]*si
    }
  }
  proj
}

rsq <- function(x,y) {
  1 - sum((y-x)^2)/sum((y-mean(y))^2)
}

## end functions ##



######
###### Data

#Regions
regions <- read.csv('regions.csv') %>% mutate(region=X) %>% select(Country.Name,region)

#World Bank
world_bank <- read.csv('world_bank_data.csv') %>% gather(key='year',value='value',X1960..YR1960.:X2016..YR2016.)
world_bank$value <- as.numeric(world_bank$value)
world_bank$Country.Name <- as.character(world_bank$Country.Name)
world_bank$Series.Name <- as.character(world_bank$Series.Name)
world_bank <- subset(world_bank,world_bank$Country.Name != "Not classified" )

for(i in 1:17){
  coso <- subset(world_bank,world_bank$Series.Name == unique(world_bank$Series.Name)[i])
  country_value <- c()
  country_name <-c()
  
  for(j in 1:length(unique(coso$Country.Name))){
    if(dim(na.omit(subset(coso,coso$Country.Name==unique(coso$Country.Name)[j])))[1]!=0){
      country_value[j] <- tail(na.omit(subset(coso,coso$Country.Name==unique(coso$Country.Name)[j])),1)$value
      country_name[j] <-  tail(na.omit(subset(coso,coso$Country.Name==unique(coso$Country.Name)[j])),1)$Country.Name
    } else{}
  }
  
  if(i==1){
    wb_pop <- do.call(rbind,Map(data.frame,Country.Name=country_name,population=country_value))
  } else if(i==2){
    wb_land <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,land_area=country_value)))  
  } else if(i==3) {
    wb_pump <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,pump_price=country_value)))
  } else if(i==4){
    wb_eimp <- do.call(rbind,Map(data.frame,Country.Name=country_name,energy_imports=country_value))
  } else if(i==5){
    wb_orents <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,oil_rents=country_value)))
  } else if(i==6) {
    wb_rents <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,rents=country_value)))
  } else if(i==7){
    wb_fexp <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,fuel_exports=country_value)))
  } else if(i==8){
    wb_e_intensity_primary <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,e_intensity_primary=country_value)))
  } else if(i==9){
    wb_e_gdp <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,e_use_gdp=country_value)))
  } else if(i==10){
    wb_gdp_capita <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,gdp_capita =country_value)))
  } else if(i==11){
    wb_gdp_net_aid <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,net_aid =country_value)))
  } else if(i==12){
    wb_gdp_net_aid_assist <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,net_assist =country_value)))
  } else if(i==13){
    wb_industry <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,industry =country_value)))
  } else if(i==14){
    wb_pop_dense <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,pop_dens =country_value)))
  } else if(i==15){
    wb_popII <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,population =country_value)))
  } else if(i==16){
    wb_eimpII <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,energy_imports =country_value)))
  } else if(i==17){
    wb_e_gdpII <- na.omit(do.call(rbind,Map(data.frame,Country.Name=country_name,e_use_gdp =country_value)))
  }
}

wb_frame <- merge(wb_popII,wb_land,by='Country.Name',all=TRUE)
wb_frame <- merge(wb_frame,wb_pump,by='Country.Name',all=TRUE)
wb_frame <- merge(wb_frame,wb_eimpII,by='Country.Name',all=TRUE)
wb_frame <- merge(wb_frame,wb_rents,by='Country.Name',all=TRUE)
wb_frame <- merge(wb_frame,wb_orents,by='Country.Name',all=TRUE)
wb_frame <- merge(wb_frame,wb_fexp,by='Country.Name',all=TRUE)
wb_frame <- merge(wb_frame,wb_e_intensity_primary,by='Country.Name',all=TRUE)
wb_frame <- merge(wb_frame,wb_e_gdpII,by='Country.Name',all=TRUE)
wb_frame <- merge(wb_frame,wb_gdp_capita,by='Country.Name',all=TRUE)
wb_frame <- merge(wb_frame,wb_gdp_net_aid,by='Country.Name',all=TRUE)
wb_frame <- merge(wb_frame,wb_gdp_net_aid_assist,by='Country.Name',all=TRUE)
wb_frame <- merge(wb_frame,wb_industry,by='Country.Name',all=TRUE)
wb_frame <- merge(wb_frame,wb_pop_dense,by='Country.Name',all=TRUE)

wb_frame <- wb_names(wb_frame)

### Calculating Difference in Energy Intensity for GDP, Energy Use and Energy Intensity
#Energy use (kg of oil equivalent) per $1,000 GDP (constant 2011 PPP)
#Energy intensity level of primary energy (MJ/$2011 PPP GDP)
#GDP per capita (constant 2010 US$)

var_to_clean <- subset(world_bank,world_bank$Series.Name=='Energy intensity level of primary energy (MJ/$2011 PPP GDP)')
country_var_diff <- c()
country_var_name <- c()
country_var_diff_pct <- c()
current_non_hydro_gen <- c()
for(i in 1:length(unique(var_to_clean$Country.Name))){
  country <- subset(var_to_clean,var_to_clean$Country.Name==unique(var_to_clean$Country.Name)[i])
  country <-  country[complete.cases(country$value),]
  if(dim(country)[1]>0){
    country_var_diff[i] <- tail(country$value,1) - head(country$value,1)
    country_var_diff_pct[i] <- ((tail(country$value,1) - head(country$value,1))/head(country$value,1))*100
    country_var_name[i] <- unique(country$Country.Name)
  } else{}
}

energy_intensity_change <- data.frame(country_var_name,country_var_diff) %>% mutate(Country.Name=country_var_name,Intense_Change=country_var_diff) %>% select(Country.Name,Intense_Change)
energy_use_change <- data.frame(country_var_name,country_var_diff,country_var_diff_pct) %>% mutate(Country.Name=country_var_name,EUSE_Change=country_var_diff,EUSE_pct_change=country_var_diff_pct) %>% select(Country.Name,EUSE_Change,EUSE_pct_change)
energy_use_change <- wb_names(energy_use_change)
gdp_change <- data.frame(country_var_name,country_var_diff,country_var_diff_pct) %>% mutate(Country.Name=country_var_name,gdp_change=country_var_diff,gdp_pct_change=country_var_diff_pct) %>% select(Country.Name,gdp_change,gdp_pct_change)
gdp_change <- wb_names(gdp_change)




#Climatescope
climatescope_invesments <- read.csv('country_investments_years.csv') %>% mutate(Country.Name=country_name) %>% select(Country.Name,investments_udmm,year)
#Keep only the latest value
for(i in 1:length(unique(climatescope_invesments$Country.Name))){
  country <- tail(subset(climatescope_invesments,climatescope_invesments$Country.Name==unique(climatescope_invesments$Country.Name)[i]),1)
  if(i==1){
    climatescope <- country
    climatescope$Country.Name <- simpleCap(as.character(climatescope$Country.Name))
  } else{
    country$Country.Name <- simpleCap(as.character(country$Country.Name))
    climatescope <- rbind(climatescope,country)
  }
}

climatescope$Country.Name <- gsub("Costa-rica",'Costa Rica',climatescope$Country.Name,fixed=TRUE)
climatescope$Country.Name <- gsub("Cote-ivoire",'Cote dIvoire (IvoryCoast)',climatescope$Country.Name,fixed=TRUE)
climatescope$Country.Name <- gsub("Dominican-republic",'Dominican Republic',climatescope$Country.Name,fixed=TRUE)
climatescope$Country.Name <- gsub("El-salvador",'El Salvador',climatescope$Country.Name,fixed=TRUE)
climatescope$Country.Name <- gsub("South-africa",'South Africa',climatescope$Country.Name,fixed=TRUE)
climatescope$Country.Name <- gsub("Sri-lanka",'Sri Lanka',climatescope$Country.Name,fixed=TRUE)
climatescope$Country.Name <- gsub("Trinidad-and-tobago",'Trinidad and Tobago',climatescope$Country.Name,fixed=TRUE)

#Clean energy investments from IRENA
clean_investments <- read.csv('clean_energy_investments.csv') %>% mutate(Country.Name=Country,investments_udmm=investment,year=2017) %>% select(Country.Name,investments_udmm,year)
clean_investments$investments_udmm <- sub(",","",clean_investments$investments_udmm)
clean_investments$investments_udmm <- as.numeric(as.character(clean_investments$investments_udmm))

# Calling climatescope and clean_investments financing
financing <- rbind(climatescope,clean_investments)



#### Climatescope and Other Price Attractiveness
climatescope_price_attractiveness <- read.csv('price_attractiveness.csv') %>% mutate(Country.Name=country_name) %>% select(Country.Name,amount_dol_mwh,price_type)

      for(i in 1:length(unique(climatescope_price_attractiveness$Country.Name))){
        country <- tail(subset(climatescope_price_attractiveness,climatescope_price_attractiveness$Country.Name==unique(climatescope_price_attractiveness$Country.Name)[i]),1)
        if(i==1){
          climatescope_price <- country
          climatescope_price$Country.Name <- simpleCap(as.character(climatescope_price$Country.Name))
        } else{
          country$Country.Name <- simpleCap(as.character(country$Country.Name))
          climatescope_price <- rbind(climatescope_price,country)
        }
      }
      
      climatescope_price$Country.Name <- gsub("Costa-rica",'Costa Rica',climatescope_price$Country.Name,fixed=TRUE)
      climatescope_price$Country.Name <- gsub("Cote-ivoire",'Cote dIvoire (IvoryCoast)',climatescope_price$Country.Name,fixed=TRUE)
      climatescope_price$Country.Name <- gsub("Dominican-republic",'Dominican Republic',climatescope_price$Country.Name,fixed=TRUE)
      climatescope_price$Country.Name <- gsub("El-salvador",'El Salvador',climatescope_price$Country.Name,fixed=TRUE)
      climatescope_price$Country.Name <- gsub("South-africa",'South Africa',climatescope_price$Country.Name,fixed=TRUE)
      climatescope_price$Country.Name <- gsub("Sri-lanka",'Sri Lanka',climatescope_price$Country.Name,fixed=TRUE)
      climatescope_price$Country.Name <- gsub("Trinidad-and-tobago",'Trinidad and Tobago',climatescope_price$Country.Name,fixed=TRUE)

allegra_prices <- read.csv('price_attractiveness_allegra.csv') %>% select(Country.Name,Retail.Avg,Residential,Commercial,Industrial)
allegra_prices$Retail.Avg <- as.numeric(as.character(allegra_prices$Retail.Avg))
allegra_prices$Residential <- as.numeric(as.character(allegra_prices$Residential))
allegra_prices$Commercial <- as.numeric(as.character(allegra_prices$Commercial))
allegra_prices$Industrial <- as.numeric(as.character(allegra_prices$Industrial))
names(allegra_prices)[2] <- 'Retail Avg'

# Calling climatescope and clean_investments price attractiveness
allegra_prices <- melt(allegra_prices,id=c("Country.Name"))
names(allegra_prices)[2] <- 'price_type'
names(allegra_prices)[3] <- 'amount_dol_mwh'

price_attractiveness <- rbind(climatescope_price,allegra_prices) 





#Fossil Fuels
fossil_subsidies <- read.csv('fossil_subsidies.csv')

fossil_subsidies$Country.Name <- gsub("Côte d'Ivoire",'Cote dIvoire (IvoryCoast)',fossil_subsidies$Country.Name,fixed=TRUE)
fossil_subsidies$Country.Name <- gsub("Hong Kong SAR",'Hong Kong',fossil_subsidies$Country.Name,fixed=TRUE)
fossil_subsidies$Country.Name <- gsub("Korea, South",'Korea',fossil_subsidies$Country.Name,fixed=TRUE)
fossil_subsidies$Country.Name <- gsub("Slovak Republic",'Slovakia',fossil_subsidies$Country.Name,fixed=TRUE)



#Governance
quality_governance <- read.csv('qog_bas_ts_jan17.csv') %>% select(cname,year,wdi_energyimp)
quality_governance_II <- read.csv('qog_bas_cs_jan17.csv') 
quality_governanceIII <- read.csv('qog_std_cs_jan17.csv') %>% mutate(Country.Name=cname) %>% select(Country.Name,wef_qoi,wef_elec,icrg_qog)
quality_governanceIII$Country.Name <- gsub('Cyprus (1975-)','Cyprus',quality_governanceIII$Country.Name,fixed=TRUE)
quality_governanceIII$Country.Name <- gsub('Ethiopia (1993-)','Ethiopia',quality_governanceIII$Country.Name,fixed=TRUE)
quality_governanceIII$Country.Name <- gsub('France (1963-)','France',quality_governanceIII$Country.Name,fixed=TRUE)

quality_governanceIV <- read.csv('qog_std_ts_jan17.csv')  %>% mutate(Country.Name=cname) %>% select(Country.Name,year,wdi_eneimp)
quality_governanceIV$Country.Name <- gsub('Cyprus (1975-)','Cyprus',quality_governanceIV$Country.Name,fixed=TRUE)
quality_governanceIV$Country.Name <- gsub('Ethiopia (1993-)','Ethiopia',quality_governanceIV$Country.Name,fixed=TRUE)
quality_governanceIV$Country.Name <- gsub('France (1963-)','France',quality_governanceIV$Country.Name,fixed=TRUE)

policies <- read.csv('renewable_policies_iea.csv')
policies$Country.Name <- gsub('Korea','Korea, South',policies$Country.Name,fixed=TRUE)
#Cleaning for Merge
unique_policies_countries_df <- as.data.frame(unique(policies$Country.Name))
names(unique_policies_countries_df)[1] <- 'Country.Name'
unique_policies_countries_df$policy <- 1
policies <- merge(policies,unique_policies_countries_df,by='Country.Name')
policies_agg <- aggregate(policies$policy,by=list(policies$Country.Name),FUN=sum) %>% mutate(Country.Name=Group.1,policy_num=x) %>% select(Country.Name,policy_num)

#Ecological Footpring
eco_print <- read.csv('ecological_footprint.csv') %>% mutate(Country.Name=Country.region)
eco_print$Country.Name <- gsub('United States of America','United States',eco_print$Country.Name,fixed=TRUE)
eco_print$Country.Name <- gsub('Viet Nam','Vietnam',eco_print$Country.Name,fixed=TRUE)


#Human Development Indicators
hdi <- read.csv('/Users/diego/Desktop/Data/energy_transitions/HDI.csv') %>% mutate(Country.Name=Country) %>% gather(key='year',value='value',X1980:X2014)
hdi$Country.Name <- trim(as.character(hdi$Country.Name))

country_hdi_diff <- c()
country_hdi_name <- c()
for(i in 1:length(unique(hdi$Country.Name))){
  country <- subset(hdi,hdi$Country.Name==unique(hdi$Country.Name)[i])
  country <-  country[complete.cases(country$value),]
  if(dim(country)[1]>0){
    country_hdi_diff[i] <- tail(country$value,1) - head(country$value,1)
    country_hdi_name[i] <- unique(country$Country.Name)
  } else{}
}

hdi_diff <- data.frame(country_hdi_name,country_hdi_diff) %>% mutate(Country.Name=country_hdi_name,hdi_diff=country_hdi_diff)

###### EIA Data
#Calculating the latest energy consumption for all countries from EIA Data

eia_electricity <- read.csv('eia_electricity.csv')

country_names <- subset(eia_electricity,eia_electricity$units != 'Billion Kwh')
country_names <- subset(country_names,country_names$Country.Name!= 'Tide, Wave, Fuel Cell') %>% select(Country.Name)

consumption_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Consumption')) %>% gather(key='year',value='value',X1980:X2014)
names(consumption_eia)[2] <- 'Var'
generation_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Generation')) %>% gather(key='year',value='value',X1980:X2014)
names(generation_eia)[2] <- 'Var'
nuclear_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Nuclear'))  %>% gather(key='year',value='value',X1980:X2014)
names(nuclear_eia)[2] <- 'Var'
renewables_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Renewables'))  %>% gather(key='year',value='value',X1980:X2014)
names(renewables_eia)[2] <- 'Var'
hydro_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Hydroelectricity'))  %>% gather(key='year',value='value',X1980:X2014)
names(hydro_eia)[2] <- 'Var'
non_hydro_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Non-Hydroelectric Renewables')) %>% gather(key='year',value='value',X1980:X2014)
names(non_hydro_eia)[2] <- 'Var'
geothermal_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Geothermal')) %>% gather(key='year',value='value_geo',X1980:X2014)
names(geothermal_eia)[2] <- 'Var'
wind_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Wind')) %>% gather(key='year',value='value_wind',X1980:X2014)
names(wind_eia)[2] <- 'Var'
solar_tide_wave_fuel_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Solar, Tide, Wave, Fuel Cell'))
names(solar_tide_wave_fuel_eia)[2] <- 'Var'
tide_wave_fuel_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Tide, Wave, Fuel Cell'))
names(tide_wave_fuel_eia)[2] <- 'Var'
tide_wave_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Tide and Wave'))
names(tide_wave_eia)[2] <- 'Var'
solar_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Solar')) %>% gather(key='year',value='value_solar',X1980:X2014)
names(solar_eia)[2] <- 'Var'
biomass_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Biomass and Waste')) %>% gather(key='year',value='value_bio',X1980:X2014)
names(biomass_eia)[2] <- 'Var'
fossil_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Fossil Fuels'))
names(fossil_eia)[2] <- 'Var'
hydro_pumped_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Hydroelectric Pumped Storage')) 
names(hydro_pumped_eia)[2] <- 'Var'

# Calculating the last value for non-hyrorenewables as percentage generation

gen_ren_eia <- merge(generation_eia,non_hydro_eia,by=c("Country.Name","year"))

gen_ren_eia <- gen_ren_eia[order(gen_ren_eia$Country.Name,gen_ren_eia$year),]
gen_ren_eia$value.x <- as.numeric(gen_ren_eia$value.x)
gen_ren_eia$value.y <- as.numeric(gen_ren_eia$value.y)
gen_ren_eia$Country.Name <- as.character(gen_ren_eia$Country.Name)
gen_ren_eia$tot_ren <- gen_ren_eia$value.y/gen_ren_eia$value.x
gen_ren_eia$tot_ren_pct <- (gen_ren_eia$value.y/gen_ren_eia$value.x)*100
gen_ren_eia <- gen_ren_eia[c('Country.Name','year','tot_ren','tot_ren_pct')]

country_diff <- c()
country_name <- c()
current_non_hydro_gen <- c()
for(i in 1:length(unique(gen_ren_eia$Country.Name))){
  country <- subset(gen_ren_eia,gen_ren_eia$Country.Name==unique(gen_ren_eia$Country.Name)[i])
  country <-  country[complete.cases(country$tot_ren_pct),]
  if(dim(country)[1]>0){
    country_diff[i] <- tail(country$tot_ren_pct,1) - head(country$tot_ren_pct,1)
    current_non_hydro_gen[i] <- tail(country$tot_ren_pct,1)
    country_name[i] <- unique(country$Country.Name)
  } else{}
}

eia_diff <- na.omit(data.frame(country_name,country_diff,current_non_hydro_gen))
eia_diff$country_diff <- as.integer(eia_diff$country_diff)
eia_diff$current_non_hydro_gen <- as.integer(eia_diff$current_non_hydro_gen)
names(eia_diff)[1] <- 'Country.Name'
#eia_diff <- subset(eia_diff,eia_diff$country_diff!=0) %>% mutate(Country.Name=country_name) %>% select(Country.Name,country_diff)
#eia_diff <- subset(eia_diff,eia_diff$country_diff!=0) %>% mutate(Country.Name=country_name) %>% select(Country.Name,country_diff)


##### Percentage of Geothermal, Biomass and Hydro as Percentage of Total Generation

percents_gen <- merge(generation_eia,geothermal_eia,by=c("Country.Name","year"))
percents_gen <- merge(percents_gen,biomass_eia,by=c("Country.Name","year"))

percents_gen$pct_geo <- as.numeric(percents_gen$value_geo)/as.numeric(percents_gen$value)*100
percents_gen$pct_bio <- as.numeric(percents_gen$value_bio)/as.numeric(percents_gen$value)*100
percents_gen <- subset(percents_gen,select = c(Country.Name,year,pct_geo,pct_bio))

country_diff <- c()
country_name <- c()
current_geo <- c()
for(i in 1:length(unique(percents_gen$Country.Name))){
  country <- subset(percents_gen,percents_gen$Country.Name==unique(percents_gen$Country.Name)[i])
  country <-  country[complete.cases(country$pct_geo),]
  if(dim(country)[1]>0){
    current_geo[i] <- tail(country$pct_geo,1)
    country_name[i] <- unique(country$Country.Name)
  } else{}
}

eia_geo <- na.omit(data.frame(country_name,current_geo))
names(eia_geo)[1] <- "Country.Name"

country_diff <- c()
country_name <- c()
current_bio<- c()
for(i in 1:length(unique(percents_gen$Country.Name))){
  country <- subset(percents_gen,percents_gen$Country.Name==unique(percents_gen$Country.Name)[i])
  country <-  country[complete.cases(country$pct_bio),]
  if(dim(country)[1]>0){
    current_bio[i] <- tail(country$pct_bio,1)
    country_name[i] <- unique(country$Country.Name)
  } else{}
}

eia_bio <- na.omit(data.frame(country_name,current_bio))
names(eia_bio)[1] <- "Country.Name"







#########
#########
##### PLots

#Policy
eia_diff_merge <- merge(eia_diff,quality_governanceIII,by='Country.Name')
eia_diff_merge <- merge(eia_diff_merge,quality_governanceIII, by='Country.Name')
eia_diff_merge_policy <- merge(eia_diff_merge,policies_agg)

missing_countries(eia_diff_merge,eia_diff_merge_policy)[[1]]
missing_countries(eia_diff_merge,eia_diff_merge_policy)[[2]]

eia_diff_merge_policy$Country.Name <- as.character(eia_diff_merge_policy$Country.Name)

#ggplot(eia_diff_merge_policy,aes(policy_num,country_diff)) + geom_point(aes(size = icrg_qog.x,colour=wef_qoi.x),alpha=0.6) + geom_text(aes(label=Country.Name),hjust=0, vjust=-1,size = 2) + ylim(-10,60) + theme_bw() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + theme(legend.position="bottom") + xlab("Number of Pro-Renewable Energy Polices since 1974") + ylab("Percentage Point Change 1980-2014, Non-Hydro Renewable Energy Generation") + labs(size="Quality of Governance", colour="Quality of Infrastructure")
ggplot(subset(eia_diff_merge_policy,eia_diff_merge_policy$country_diff!=0),aes(policy_num,country_diff)) + geom_point(aes(size = icrg_qog.x,colour=wef_qoi.x),alpha=0.6) + geom_text(aes(label=Country.Name),hjust=0, vjust=-1,size = 2) + ylim(-10,60) + theme_bw() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + theme(legend.position="bottom") + xlab("Number of Pro-Renewable Energy Polices since 1974") + ylab("Percentage Point Change 1980-2014, Non-Hydro Renewable Energy Generation") + labs(size="Quality of Governance", colour="Quality of Infrastructure")


#Fuel Exports and Pump Prices Plots

eia_fuel_merge <- merge(eia_diff,wb_frame,by="Country.Name")

missing_countries(eia_diff,eia_fuel_merge)[[1]]
missing_countries(eia_diff,eia_fuel_merge)[[2]]
eia_fuel_merge$Country.Name <- as.character(eia_fuel_merge$Country.Name)

ggplot(eia_fuel_merge,aes(pump_price,country_diff)) + geom_point(aes(size = fuel_exports,colour=energy_imports),alpha=0.6) + scale_colour_gradient(low = "red", high = "light blue") + geom_text(aes(label=Country.Name),hjust=0, vjust=-1,size = 2) +xlim(0,2.5) + ylim(-20,60)+ theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + theme(legend.position="bottom") + xlab("Pump Price ($US/liter)") + ylab("Percentage Point Change 1980-2014, Non-Hydro Renewable Energy Generation") + labs(size="Fuel exports (% of merchandise exports)", colour="Energy imports, net (% of energy use)")

# Resource Rents

eia_wb_imf <- merge(eia_fuel_merge,fossil_subsidies)

missing_countries(eia_fuel_merge,eia_wb_imf)[[1]]
missing_countries(eia_fuel_merge,eia_wb_imf)[[2]]
eia_wb_imf$Country.Name <- as.character(eia_wb_imf$Country.Name)

ggplot(eia_wb_imf,aes(rents,country_diff)) + geom_point(aes(size = Total_GDP,colour=log(Total_Billions)),alpha=0.4) + scale_colour_gradient(low = "light green", high = "dark blue") + geom_text(aes(label=Country.Name),hjust=0, vjust=-1,size = 2) + ylim(-20,60) + theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + theme(legend.position="bottom") + xlab("Total Natural Resource Rents (% of GDP)") + ylab("Percentage Point Change 1980-2014, Non-Hydro Renewable Energy Generation") + labs(size="Fossil Fuel Subsidies (% of GDP)", colour="ln(Fossil Fuel Subsidies Billions)")
#ggplot(eia_wb_imf,aes(rents,country_diff)) + geom_point(aes(size = Total_GDP,colour=log(Total_Billions)),alpha=0.4) + scale_colour_gradient(low = "light green", high = "dark blue") + geom_text(subset(eia_wb_imf,eia_wb_imf$country_diff!=0),aes(label=Country.Name),hjust=0, vjust=-1,size = 2) + ylim(-20,60) + theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + theme(legend.position="bottom") + xlab("Total Natural Resource Rents (% of GDP)") + ylab("Percentage Point Change 1980-2014, Non-Hydro Renewable Energy Generation") + labs(size="Fossil Fuel Subsidies (% of GDP)", colour="ln(Fossil Fuel Subsidies Billions)")


# Climatescope and Other Resources

eia_wb_climatescope <- merge(eia_fuel_merge,financing,by=c("Country.Name"))
eia_wb_climatescope$dollar_km <- (eia_wb_climatescope$investments_udmm*1000000)/eia_wb_climatescope$land_area
eia_wb_climatescope$dollar_person <- (eia_wb_climatescope$investments_udmm*1000000)/eia_wb_climatescope$population

missing_countries(eia_fuel_merge,eia_wb_climatescope)[[1]]
missing_countries(eia_fuel_merge,eia_wb_climatescope)[[2]]

ggplot(eia_wb_climatescope,aes(log(dollar_km),country_diff)) + geom_point(aes(size = investments_udmm,colour=log(dollar_person)),alpha=0.4) + scale_colour_gradient(low = "grey", high = "blue") + geom_text(aes(label=Country.Name),hjust=0, vjust=-1,size = 2) + ylim(-20,60) + theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + theme(legend.position="bottom") + xlab("log(Dollars Invested per Square Kilometer)") + ylab("Percentage Point Change 1980-2014, Non-Hydro Renewable Energy Generation") + labs(size="Billions of Dollars ($US)", colour="log($US Invested per Capita)")


# Price Attractiveness

eia_price_attractiveness <- merge(eia_diff,price_attractiveness,by=c("Country.Name"))
    missing_countries(eia_diff,eia_price_attractiveness)[[1]]
    missing_countries(eia_diff,eia_price_attractiveness)[[2]]
    
grouped_prices_mean <- aggregate(eia_price_attractiveness$amount_dol_mwh,by=list(eia_price_attractiveness$Country.Name),FUN=mean,na.rm=TRUE) %>% mutate(Country.Name=Group.1, Mean_Elec_Prices=x) %>% select(Country.Name,Mean_Elec_Prices)
grouped_prices_mean_merge <- merge(grouped_prices_mean,eia_diff,by=c("Country.Name"))

ggplot(grouped_prices_mean_merge,aes(Mean_Elec_Prices,country_diff)) + geom_point() + scale_colour_gradient(low = "grey", high = "blue") + geom_text(aes(label=Country.Name),hjust=0, vjust=-1,size = 2) + ylim(-20,60) + theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + theme(legend.position="bottom") + xlab("Mean: Spot, Retail, Commercial and Industrial Energy Prices ($US/MWh)") + ylab("Percentage Point Change 1980-2014, Non-Hydro Renewable Energy Generation") + labs(size="Billions of Dollars ($US)", colour="log($US Invested per Capita)")

## Prepare for merge with data below
eia_price_attractiveness_merge <- cast(eia_price_attractiveness,Country.Name~price_type,mean,value='amount_dol_mwh')



### Humand Development Impact Score and Energy Use

sosi2 <- merge(energy_use_change,eia_diff,by=c('Country.Name'))
sosi2 <- merge(sosi2,gdp_change,by=c('Country.Name'))
sosi3 <- merge(sosi2,hdi_diff,by=c('Country.Name'))


ggplot(sosi3,aes(country_diff,EUSE_pct_change)) + geom_point(aes(size=gdp_pct_change,colour=hdi_diff),alpha=0.4) + 
  scale_colour_gradient(low = "white", high = "dark blue") + 
  geom_text(data=subset(sosi2,sosi2$country_diff>2 | sosi2$country_diff< -1 | sosi2$EUSE_pct_change< -50 | 
                          sosi2$EUSE_pct_change>30 | sosi2$Country.Name=="China" ),aes(country_diff,EUSE_pct_change,
                                                                                       label=Country.Name),hjust=1, vjust=-1,size = 2) +  scale_size_continuous(range=c(1,8)) + geom_hline(aes(yintercept=0),colour="light grey") + geom_vline(aes(xintercept=0),colour="light grey") +
  ylim(-100,100) + xlim(-15,60) + theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.position="bottom") + xlab("Percentage Point Change 1980-2014, Non-Hydro Renewable Energy Generation") + ylab("% Change Energy Intensity (MJ/$2011 PPP GDP)") + labs(size="% Change GDP per Capita", colour="Change in HDI Score") +
  annotate("text", x = 47, y = -90, label = "Efficient Low-Carbon Development") +
  annotate("rect", xmin = 0, xmax = 60, ymin = 0, ymax = -100, alpha = .1,colour="dark green")


# How have energy use intense 


# Demand Downscaling

consumption_eia$year <- gsub('X','',consumption_eia$year)
consumption_eia$year <- as.numeric(consumption_eia$year)
consumption_eia$value <- as.numeric(consumption_eia$value )
consumption_eia$Country.Name <- as.character(consumption_eia$Country.Name)

wb_poulation <- subset(world_bank,world_bank$Series.Name=='Population, total')
wb_poulation$year <-  as.numeric(gsub("\\.", "", sub(".*..YR", "", as.character(wb_poulation$year)),perl = TRUE))

pop_cons <- merge(wb_poulation,consumption_eia,by=c('Country.Name','year'))

#Need to complete the merge it's still too poorly merged
missing_countries(consumption_eia,pop_cons)[[1]]
missing_countries(consumption_eia,pop_cons)[[2]]

pop_cons$kwh_capita <- (pop_cons$value.y*1000000000)/pop_cons$value.x

#Plot all countries one by one
plot_list = list()
plot.names = list()
for (i in 1:length(unique(pop_cons$Country.Name))) {
  pop_i <- subset(pop_cons,pop_cons$Country.Name==unique(pop_cons$Country.Name)[i])
  plot_list[[i]] <- ggplot(pop_i, aes(x=year, y=kwh_capita, group=Country.Name)) + geom_line() +geom_text(aes(label=year))
  plot.names[[i]] <- unique(pop_cons$Country.Name)[i]
}

options(warn=1)
# Save plots to jpeg making a separate file for each plot.
for (i in 1:length(unique(pop_cons$Country.Name))) {
  mypath <- file.path("/Users/diego/Desktop/Projects/energy_transitons/plots/energy_capita",paste(plot.names[[i]],".jpg",sep = ""))
  jpeg(file=mypath)
  print(plot_list[[i]]) 
  dev.off()
}
options(warn=0)

# Separating countries that peeked
pop_cons$country_lab <- ifelse(pop_cons$Country.Name=='United Arab Emirates' |pop_cons$Country.Name=='Dominican Republic' | pop_cons$Country.Name=='American Samoa' | pop_cons$Country.Name=='Lybia' | pop_cons$Country.Name=='Australia' | pop_cons$Country.Name=='Belgium' | pop_cons$Country.Name=='Luxembourg' | pop_cons$Country.Name=='Netherlands' | pop_cons$Country.Name=='Bulgaria'| pop_cons$Country.Name=='New Zealand' | pop_cons$Country.Name=='Cyprus'| pop_cons$Country.Name=='Czech Republic' | pop_cons$Country.Name=='Portugal' | pop_cons$Country.Name=='Denmark' | pop_cons$Country.Name=='Singapore'| pop_cons$Country.Name=='Finland'| pop_cons$Country.Name=='Spain' | pop_cons$Country.Name=='France' | pop_cons$Country.Name=='Sweden' | pop_cons$Country.Name=='Germany' | pop_cons$Country.Name=='Switzerland' | pop_cons$Country.Name=='Greece' | pop_cons$Country.Name=='United Kingdom' | pop_cons$Country.Name=='Ireland' | pop_cons$Country.Name=='United States' | pop_cons$Country.Name=='Italy' | pop_cons$Country.Name=='Japan' | pop_cons$Country.Name=='Bahamas' | pop_cons$Country.Name=='Bahrain' | pop_cons$Country.Name=='Barbados' | pop_cons$Country.Name=='Belize' | pop_cons$Country.Name=='Bermuda' | pop_cons$Country.Name=='Comoros' | pop_cons$Country.Name=='Jamaica' | pop_cons$Country.Name=='Moldova' | pop_cons$Country.Name=='Puerto Rico' | pop_cons$Country.Name=='South Africa','Peak 2005-2013, Reduction Maintained',ifelse(pop_cons$Country.Name=='Algeria'| pop_cons$Country.Name=='Ecuador' | pop_cons$Country.Name=='Angola' | pop_cons$Country.Name=='Ethiopia' | pop_cons$Country.Name=='Jordan' | pop_cons$Country.Name=='Bolivia' | pop_cons$Country.Name=='Malaysia' | pop_cons$Country.Name=='Brazil' | pop_cons$Country.Name=='Nepal' | pop_cons$Country.Name=='Cambodia' | pop_cons$Country.Name=='Morocco' | pop_cons$Country.Name=='Chile' | pop_cons$Country.Name=='Nepal' | pop_cons$Country.Name=='Philippines' | pop_cons$Country.Name=='Paraguay' | pop_cons$Country.Name=='Costa Rica' | pop_cons$Country.Name=='Paraguay' | pop_cons$Country.Name=='Saudi Arabia' | pop_cons$Country.Name=='Thailand' | pop_cons$Country.Name=='Trinidad and Tobago' | pop_cons$Country.Name=='Turkey' | pop_cons$Country.Name=='Uruguay','Small Impact, Instant Recovery',ifelse(pop_cons$Country.Name=='China'| pop_cons$Country.Name=='Vietnam' | pop_cons$Country.Name=='Indonesia'| pop_cons$Country.Name=='India' | pop_cons$Country.Name=='Bangladesh','Continued Exponential Growth',ifelse(pop_cons$Country.Name=='Canada' | pop_cons$Country.Name=='Norway','Early Peak - 2000s','Nothing'))))
peak_energy <- subset(pop_cons,pop_cons$country_lab !="Nothing")
peak_energy_max <- aggregate(kwh_capita~Country.Name + country_lab,peak_energy,FUN=max,na.rm=TRUE) #%>% mutate(Country.Name=Group.1,peak_energy=x) %>% select(Country.Name,peak_energy,country_lab)

#Merge with income capita and ecological footprint
wb_peak_e <- merge(peak_energy_max,wb_frame,by=c('Country.Name'))
wb_peak_eco <- merge(wb_peak_e,eco_print,by=c('Country.Name'))

missing_countries(wb_peak_e,wb_peak_eco)[[1]]
missing_countries(wb_peak_e,wb_peak_eco)[[2]]

ggplot(wb_peak_eco,aes(log(gdp_capita),log(kwh_capita),group=country_lab)) + geom_point(aes(size = Total.Ecological.Footprint,colour = factor(country_lab)),alpha=0.4) + geom_text(aes(label=Country.Name),hjust=0, vjust=-1,size=2) + xlim(6,12) + theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + theme(legend.position="bottom",legend.direction="vertical") + xlab("log(GDP per Capita ($US 2010))") + ylab("log(Electricity (kWH) per Capita)") + labs(size="Ecological Footprint",colour="Impact on Demand, Financial & Peak Oil Price Crises")



##################### Stats for Paper

#Top Renewable Countries
top_countries <- na.omit(eia_diff)[order(-eia_diff$current_non_hydro_gen),]

germany_income <- subset(world_bank,world_bank$Country.Name=='Germany' & world_bank$Series.Name=='GDP per capita (constant 2010 US$)' )
germany_energy <- tail(subset(world_bank,world_bank$Country.Name=='Germany' & world_bank$Series.Name=='Energy use (kg of oil equivalent) per $1,000 GDP (constant 2011 PPP)' ))


denmark_income <- subset(world_bank,world_bank$Country.Name=='Denmark' & world_bank$Series.Name=='GDP per capita (constant 2010 US$)' )
denmark_energy <- tail(subset(world_bank,world_bank$Country.Name=='Denmark' & world_bank$Series.Name=='Energy use (kg of oil equivalent) per $1,000 GDP (constant 2011 PPP)' ))

nicaragua_income <- subset(world_bank,world_bank$Country.Name=='Nicaragua' & world_bank$Series.Name=='GDP per capita (constant 2010 US$)' )
nicaragua_energy <- tail(subset(world_bank,world_bank$Country.Name=='Nicaragua' & world_bank$Series.Name=='Energy use (kg of oil equivalent) per $1,000 GDP (constant 2011 PPP)' ))

costa_income <- subset(world_bank,world_bank$Country.Name=='Costa Rica' & world_bank$Series.Name=='GDP per capita (constant 2010 US$)' )
costa_energy <- tail(subset(world_bank,world_bank$Country.Name=='Costa Rica' & world_bank$Series.Name=='Energy use (kg of oil equivalent) per $1,000 GDP (constant 2011 PPP)' ))

kenya_income <- subset(world_bank,world_bank$Country.Name=='Kenya' & world_bank$Series.Name=='GDP per capita (constant 2010 US$)' )
kenya_energy <- tail(subset(world_bank,world_bank$Country.Name=='Kenya' & world_bank$Series.Name=='Energy use (kg of oil equivalent) per $1,000 GDP (constant 2011 PPP)' ))

pepe <- wb_e_gdp[order(wb_e_gdp$e_use_gdp),]

### How transitions look like over time

countries <- subset(non_hydro_eia,non_hydro_eia$Country.Name=='Germany' | non_hydro_eia$Country.Name=='Spain' | non_hydro_eia$Country.Name=='Denmark' | non_hydro_eia$Country.Name=='Germany' | non_hydro_eia$Country.Name=='Costa Rica' | non_hydro_eia$Country.Name=='Kenya' | non_hydro_eia$Country.Name=='Nicaragua')
countries$year <- as.numeric(gsub('X','',countries$year))
countries$value <- as.numeric(countries$value)

generation_countries <- subset(generation_eia,generation_eia$Country.Name=='Germany' | generation_eia$Country.Name=='Spain' | generation_eia$Country.Name=='Denmark' | generation_eia$Country.Name=='Germany' | generation_eia$Country.Name=='Costa Rica' | generation_eia$Country.Name=='Kenya' | generation_eia$Country.Name=='Nicaragua')
generation_countries$year <- as.numeric(gsub('X','',generation_countries$year))
generation_countries$value <- as.numeric(generation_countries$value)

gen_pct <- merge(generation_countries,countries,by=c('Country.Name','year'))
gen_pct$pct <- as.numeric(gen_pct$value.y)/as.numeric(gen_pct$value.x)

ggplot(gen_pct,aes(year,pct,group=Country.Name)) + geom_line(aes(colour=factor(Country.Name)))

costa <- subset(gen_pct,gen_pct$Country.Name=='Costa Rica')


#### Data for Concentric Circles

percents_by_resource <- merge(generation_eia,biomass_eia,by=c('Country.Name','year'))
percents_by_resource <- merge(percents_by_resource,solar_eia,by=c('Country.Name','year'))
percents_by_resource <- merge(percents_by_resource,wind_eia,by=c('Country.Name','year'))
percents_by_resource <- merge(percents_by_resource,geothermal_eia,by=c('Country.Name','year'))

percents_by_resource$pct_bio <- (as.numeric(percents_by_resource$value_bio)/as.numeric(percents_by_resource$value))*100
percents_by_resource$pct_solar <- (as.numeric(percents_by_resource$value_solar)/as.numeric(percents_by_resource$value))*100
percents_by_resource$pct_wind <- (as.numeric(percents_by_resource$value_wind)/as.numeric(percents_by_resource$value))*100
percents_by_resource$pct_geo <- (as.numeric(percents_by_resource$value_geo)/as.numeric(percents_by_resource$value))*100

percents_by_resource$year <-  as.numeric(gsub("X","",percents_by_resource$year))

df_resource <- percents_by_resource
#Plot all countries one by one
plot_list = list()
plot.names = list()
#pct_bio,pct_solar
con_i_sub <- df_resource[complete.cases(df_resource$pct_geo),]
for (i in 1:length(unique(con_i_sub$Country.Name))) {
  con_i <- subset(con_i_sub,con_i_sub$Country.Name==unique(con_i_sub$Country.Name)[i])
  plot_list[[i]] <- ggplot(con_i, aes(x=year, y=pct_geo),na.omit=TRUE) + geom_line() + geom_text(aes(label=year))
  plot.names[[i]] <- unique(con_i$Country.Name)
}

options(warn=1)
# Save plots to jpeg making a separate file for each plot.
for (i in 1:length(unique(con_i_sub$Country.Name))) {
  mypath <- file.path("/Users/diego/Desktop/Projects/energy_transitons/plots/geo",paste(plot.names[[i]],".jpg",sep = ""))
  jpeg(file=mypath)
  print(plot_list[[i]]) 
  dev.off()
}
options(warn=0)


# Normalizing with respect to all countries and with respect only to countries that are undergoing a transition

#Morocco 7.086989
#Brazil 8.646916
#New Zealand 14.988829
#Mauritius 17.342795
#Uruguay 16.954829
#Turkey 5.047878		
#Nicaragua
#Kenya	20 
#Chile 9.444131
#Thailand 6.237303		
#Costa Rica
#Australia 7.911474		
#Indonesia		
#India			

head(eia_diff_merge_policy)

eia_diff_merge_policy$scaled_policy <- (eia_diff_merge_policy$policy_num - min(eia_diff_merge_policy$policy_num))/(max(eia_diff_merge_policy$policy_num) - min(eia_diff_merge_policy$policy_num))
eia_diff_merge_policy$scaled_qoi <- (eia_diff_merge_policy$wef_qoi.x - min(eia_diff_merge_policy$wef_qoi.x,na.rm=TRUE))/(max(eia_diff_merge_policy$wef_qoi.x,na.rm=TRUE) - min(eia_diff_merge_policy$wef_qoi.x,na.rm=TRUE))
#scaled_policy, scaled_qoi, icrg_qog.x

eia_fuel_merge$scaled_pump_price <- (eia_fuel_merge$pump_price - min(eia_fuel_merge$pump_price,na.rm=TRUE))/(max(eia_fuel_merge$pump_price,na.rm=TRUE) - min(eia_fuel_merge$pump_price,na.rm=TRUE))
#scaled_pump_price, fuel_exports, energy_imports

eia_wb_imf$scaled_billions <- (eia_wb_imf$Total_Billions - min(eia_wb_imf$Total_Billions,na.rm=TRUE))/(max(eia_wb_imf$Total_Billions,na.rm=TRUE) - min(eia_wb_imf$Total_Billions,na.rm=TRUE))
#rents,Total_GDP,scaled_billions

eia_wb_climatescope$scaled_dollar_km <- (eia_wb_climatescope$dollar_km - min(eia_wb_climatescope$dollar_km,na.rm=TRUE))/(max(eia_wb_climatescope$dollar_km,na.rm=TRUE) - min(eia_wb_climatescope$dollar_km,na.rm=TRUE))
eia_wb_climatescope$scaled_dollar_person <- (eia_wb_climatescope$dollar_person - min(eia_wb_climatescope$dollar_person,na.rm=TRUE))/(max(eia_wb_climatescope$dollar_person,na.rm=TRUE) - min(eia_wb_climatescope$dollar_person,na.rm=TRUE))
eia_wb_climatescope$scaled_billions <- (eia_wb_climatescope$investments_udmm - min(eia_wb_climatescope$investments_udmm,na.rm=TRUE))/(max(eia_wb_climatescope$investments_udmm,na.rm=TRUE) - min(eia_wb_climatescope$investments_udmm,na.rm=TRUE))
# scaled_dollar_km, scaled_dollar_person, scaled_billions

#Writing the files for Allegra and Sophie

write.csv(eia_diff_merge_policy[,c("Country.Name","scaled_policy", "scaled_qoi", "icrg_qog.x")],'URAP/policy.csv')
write.csv(eia_fuel_merge[,c("Country.Name","scaled_pump_price", "fuel_exports", "energy_imports")],'URAP/fuel.csv')
write.csv(eia_wb_imf[,c("Country.Name","rents", "Total_GDP","scaled_billions")],'URAP/subsidies.csv')
write.csv(eia_wb_climatescope[,c("Country.Name","scaled_dollar_km","scaled_dollar_person","scaled_billions")],'URAP/invest.csv')
write.csv(eia_diff_merge_policy[,c("Country.Name","country_diff")],'URAP/renewable_change.csv')


############### Over Performers and Underperformers

eia_pca$Income_Group <- ifelse(eia_pca$gdp_capita<=10000,"low",ifelse(eia_pca$gdp_capita>10000 & eia_pca$gdp_capita<=20000,"middle",ifelse(eia_pca$gdp_capita>20000 & eia_pca$gdp_capita<=45000,"high-middle","high-income")))
performers <- na.omit(subset(eia_pca,select=c(Country.Name,country_diff,Income_Group,gdp_capita)))
performers <- aggregate(performers$country_diff,by=list(performers$Income_Group),FUN=median) %>% mutate(Income_Group=Group.1,median=x) %>% select(Income_Group,median)

eia_performers <- merge(eia_pca,performers,by="Income_Group") %>% select(Country.Name,Income_Group,gdp_capita,country_diff,median)
eia_performers$performance <- eia_performers$country_diff - eia_performers$median
eia_performers <- eia_performers[order(-eia_performers$performance),]
eia_performers$Country.Name <- factor(eia_performers$Country.Name, levels = eia_performers$Country.Name)
eia_performers_sub <- subset(eia_performers,eia_performers$performance!=0)
eia_performers_sub <- merge(eia_performers_sub, regions, by="Country.Name")


ggplot(eia_performers_sub,aes(x=Country.Name,y=performance,fill=region)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +scale_fill_brewer() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + xlab("") + ylab("Comparison with Income Group Progress: % Change since 1980")





################# 
################# 
################# 
# PCA





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
eia_diff_merge_policy <- merge(eia_diff,policies_agg,all.x = TRUE)

eia_pca <- merge(eia_diff_merge_policy,wb_frame,by="Country.Name",all.x = TRUE)
eia_pca <- merge(eia_pca,fossil_subsidies,by="Country.Name",all.x = TRUE)
eia_pca <- merge(eia_pca,financing,by="Country.Name",all.x = TRUE)
eia_pca <- merge(eia_pca,eco_print,by="Country.Name",all.x = TRUE) 
eia_pca <- merge(eia_pca,hdi_diff, by="Country.Name",all.x = TRUE)
eia_pca <- merge(eia_pca,quality_governanceIII, by="Country.Name",all.x = TRUE)
eia_pca <- merge(eia_pca,energy_intensity_change,by="Country.Name",all.x = TRUE)
eia_pca <- merge(eia_pca,energy_use_change,by="Country.Name",all.x = TRUE)
eia_pca <- merge(eia_pca,gdp_change,by="Country.Name",all.x = TRUE)
eia_pca <- merge(eia_pca,eia_price_attractiveness_merge,by="Country.Name",all.x = TRUE)
eia_pca <- merge(eia_pca,grouped_prices_mean,by="Country.Name",all.x = TRUE)
eia_pca <- merge(eia_pca,eia_geo,by="Country.Name",all.x = TRUE)
eia_pca <- merge(eia_pca,eia_bio,by="Country.Name",all.x = TRUE)


countries_above_2 <- subset(eia_pca,eia_pca$country_diff>=1)

countries_above_2$dollar_km <- (countries_above_2$investments_udmm*1000000)/countries_above_2$land_area
countries_above_2$dollar_person <- (countries_above_2$investments_udmm*1000000)/countries_above_2$population
countries_above_2$aid_gdp <- (countries_above_2$net_assist)/(countries_above_2$Nominal.GDP.US...billions*1000000000)

#Setting up the principal components
# Running with the full merge
#countries_above_2 <- subset(countries_above_2, select=-c(version,ccode,cname,ccodealp,ccodecow,ccodewb,country_hdi_name,Income.Group,Data.Quality,Population..millions.,Region,Per.Capita.GDP,Country.region,X,X.1,Electricity_GDP,Electricity_Billions,Electricity_capita,year))
# Running with the partial merge
countries_above_2 <- subset(countries_above_2, select=-c(Income_Group,Population..millions, Built.up.land.1,year,X.1,X,Country.region,Data.Quality,Region,Income.Group,country_hdi_name,Per.Capita.GDP))
#countries_above_2 <- subset(countries_above_2, select=-c(Population..millions, Built.up.land.1,year,X.1,X,Country.region,Data.Quality,Region,Income.Group,country_hdi_name,Per.Capita.GDP))
#countries_above_2$Per.Capita.GDP<- as.numeric(as.character(countries_above_2$Per.Capita.GDP))

#Checking for factors and getting rid of them
for(i in 1:length(names(countries_above_2))){
  ifelse(class(countries_above_2[,i])=="factor", print(paste(class(countries_above_2[,i]),i)),NA)
}
countries_above_2$country_diff <- as.numeric(countries_above_2$country_diff)
countries_above_2$current_non_hydro_gen <- as.numeric(countries_above_2$current_non_hydro_gen)
row.names(countries_above_2) <- countries_above_2$Country.Name

#################
# Checking distributinons to see if they should be log normalized

for(i in 1:length(names(countries_above_2))){
  if(i>1){
    plot.name <- names(countries_above_2)[i]
    mypath <- paste('/Users/diego/Desktop/Projects/energy_transitons/plots/histograms_pca/',plot.name,'.jpg',sep = '')
    hist1 <- ggplot(data=countries_above_2, aes(countries_above_2[,names(countries_above_2)[i]])) + geom_histogram() + xlab(as.character(names(countries_above_2)[i])) + theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) 
    jpeg(file=mypath)
    print(hist1)
    dev.off()
    
    mypath1 <- paste('/Users/diego/Desktop/Projects/energy_transitons/plots/histograms_pca/',plot.name,'_log.jpg',sep = '')
    hist2 <- ggplot(data=countries_above_2, aes(log(countries_above_2[,names(countries_above_2)[i]]))) + geom_histogram() + xlab(paste('Log(',as.character(names(countries_above_2)[i]),')',sep='')) + theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) 
    jpeg(file=mypath1)
    print(hist2)
    dev.off()
  }
}

# Dropping Repeated Variables
drops <- c("Income_Group",'Per.Capita.GDP','Built.up.land1','Population..millions','Population..millions.','Biocapacity.Deficit.or.Reserve')
countries_above_2 <- countries_above_2[ ,!(names(countries_above_2) %in% drops)]


#All Countries
log_countries <- countries_above_2
log_countries[,2:70]  <- log(log_countries[,2:70]+1)


#Income
higher_income <- subset(countries_above_2,countries_above_2$gdp_capita >30000)
middle <- subset(countries_above_2,countries_above_2$gdp_capita <=30000)
middle_log <- middle
middle_log[,2:69]  <- log(middle_log[,2:69]+1)

#Correlation Plot Between Variables
#All
corrplot(cor(subset(countries_above_2,select=-c(Country.Name)),use="pairwise.complete.obs"), method="color", tl.cex=0.3)
#Lower-Income Countries
corrplot(cor(subset(middle,select= -c(Country.Name)),use="pairwise.complete.obs"), method="color", tl.cex=0.3)



#######
# Chose data to use in the following analysis

data_above <- middle_log # countries_above_2, log_countries, middle_log


#######

#Full data set 
x_countries_above_2 <- data_above[,-1] #Remove name
#x_countries_above_2 <- x_countries_above_2[,-1] #Remove diff
x_countries_above_2 <- x_countries_above_2[,-2] #Remove actual generation

#Subsetting if doing a training set
#x_countries_above_2_train <- x_countries_above_2[-c(2,4,5,9,10,23,45),] # When using for training
x_countries_above_2_train <- x_countries_above_2 # When using all the data
x_countries_above_2_test <- x_countries_above_2[c(2,4,5,9,10,23,45),] # When using a test set

# Dealing with the NAs
x_countries_above_2_train[is.na(x_countries_above_2_train)] <- 0

# scale the data
dTrainNTreatedXscaled <- as.data.frame(scale(x_countries_above_2_train[,colnames(x_countries_above_2_train)!='country_diff'],center=TRUE,scale=TRUE),stringsAsFactors = FALSE)
dTrainNTreatedXscaled$country_diff <- x_countries_above_2_train$country_diff

dTestNTreatedXscaled <- as.data.frame(scale(x_countries_above_2_test[,colnames(x_countries_above_2_test)!='country_diff'],center=TRUE,scale=TRUE),stringsAsFactors = FALSE)
dTestNTreatedXscaled$country_diff <- x_countries_above_2_test$country_diff


# get the variable ranges
ranges = vapply(dTrainNTreatedXscaled, FUN=function(col) c(min(col), max(col)), numeric(2))
rownames(ranges) = c("vmin", "vmax") 
rframe = as.data.frame(t(ranges))  # make ymin/ymax the columns
rframe$varName = rownames(rframe)
varnames = setdiff(rownames(rframe), "country_diff") #y & country_diff
rframe = rframe[varnames,]
rframe$vartype = ifelse(grepl("noise", rframe$varName), "noise", "signal")


# Scaled feature ranges
barbell_plot(rframe, "varName", "vmin", "vmax", "vartype") +
  coord_flip() + ggtitle("x scaled variables: ranges") 


#Dropping variables that have a variance of zero as the pca won't run without it
dTrainNTreatedXscaled_dropped <- dTrainNTreatedXscaled[, colSums(is.na(dTrainNTreatedXscaled)) != nrow(dTrainNTreatedXscaled)]
dTestNTreatedXscaled_dropped <- dTestNTreatedXscaled[, colSums(is.na(dTrainNTreatedXscaled)) != nrow(dTrainNTreatedXscaled)]

#dTrainNTreatedXscaled_dropped <- dTrainNTreatedXscaled[, sapply(dTrainNTreatedXscaled, function(v) var(v, na.rm=TRUE)!=0)]
#dTestNTreatedXscaled_dropped <- dTestNTreatedXscaled[, sapply(dTrainNTreatedXscaled, function(v) var(v, na.rm=TRUE)!=0)]

# The principal component analysis
#vars = setdiff(colnames(dTrainNTreatedXscaled), "country_diff")
vars = setdiff(colnames(dTrainNTreatedXscaled_dropped), "country_diff")

#dmTrain <- as.matrix(dTrainNTreatedXscaled[,vars])
dTrainNTreatedXscaled_dropped[is.na(dTrainNTreatedXscaled_dropped)] <- 0
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
summary(princ)
dotplot_identity(frame = data.frame(pc=1:length(princ$sdev), 
                                    magnitude=princ$sdev), 
                 xvar="pc",yvar="magnitude") +
  ggtitle("x scaled variables: Magnitudes of singular values")

plot(princ, type = "l")


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
ncomp = 20

# here we will only model with the first ncomp principal components
varexpr = paste(paste("PC", 1:ncomp, sep=''), collapse='+')
fmla = paste("country_diff ~", varexpr)

model <- lm(fmla,data=projectedTrain)
summary(model)

projectedTrain$estimate <- predict(model,newdata=projectedTrain)
ScatterHist(projectedTrain,'estimate','country_diff','Recovered 20 variable model versus truth (train)',
            smoothmethod='identity',annot_size=3)

#ggplot(projectedTrain,aes(estimate,country_diff)) + geom_point() + xlab('Log(Estimate)')+ylab('Log(Actual)') + theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) 

trainrsq <- rsq(projectedTrain$estimate,projectedTrain$country_diff)



###### Evaluating the principal components


summary(princ)

vars <- apply(princ$x, 2, var)  
props <- vars / sum(vars)
princ_cumulative <- cumsum(props)
plot(princ_cumulative,xlab="Principal Component",ylab="Cumulative Proportion of Variance",ylim=c(0,1))

pca_analysis <- princ

loads <- as.data.frame(with(pca_analysis, unclass(rotation)))

for(i in 1:length(names(loads))){
  loads[,i] <- (abs(loads[,i])/sum(abs(loads[,i])))*100
}

names_df <- as.data.frame(dmTrain)
loads$var_name <- names(names_df)

gathered_loads <- loads %>% gather(key='var_name',value='value',PC1:PC58)
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





################################ 
################################ 
################################ 
# K-Means and Clustering on PCA


princ <- prcomp(dmTrain,center = TRUE,scale. = TRUE) 

comp <- data.frame(princ$x[,1:5])

# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))



#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
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
k <- kmeans(comp,8,nstart=50,iter.max = 1000)

palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp[,1:5], col=k$clust, pch=16)

sort(table(k$clust))

clust <- names(sort(table(k$clust)))

row.names(dTrainNTreatedXscaled_dropped)

row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[1],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[2],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[3],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[4],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[5],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[6],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[7],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[8],])





























