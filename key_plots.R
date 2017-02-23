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


#Libraries & Path
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
setwd('/Users/diego/Desktop/Data/energy_transitions')

############
#Functions


missing_countries <- function(datadf1,datadf2){
  countries <- subset(datadf1$Country.Name,!(datadf1$Country.Name %in% unique(datadf2$Country.Name)))
  missing_countries <- length(unique(datadf1$Country.Name)) - length(unique(datadf2$Country.Name))
  return(list(countries,missing_countries))
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}




######
###### Data

 #World Bank
 world_bank <- read.csv('world_bank_data.csv') %>% gather(key='year',value='value',X1960..YR1960.:X2016..YR2016.)
     world_bank$value <- as.numeric(world_bank$value)
     world_bank$Country.Name <- as.character(world_bank$Country.Name)
     world_bank$Series.Name <- as.character(world_bank$Series.Name)
     world_bank <- subset(world_bank,world_bank$Country.Name != "Not classified" )
     
     for(i in 1:10){
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
       }
     }
     
     wb_frame <- merge(wb_pop,wb_land,by='Country.Name',all=TRUE)
     wb_frame <- merge(wb_frame,wb_pump,by='Country.Name',all=TRUE)
     wb_frame <- merge(wb_frame,wb_eimp,by='Country.Name',all=TRUE)
     wb_frame <- merge(wb_frame,wb_rents,by='Country.Name',all=TRUE)
     wb_frame <- merge(wb_frame,wb_orents,by='Country.Name',all=TRUE)
     wb_frame <- merge(wb_frame,wb_fexp,by='Country.Name',all=TRUE)
     wb_frame <- merge(wb_frame,wb_e_intensity_primary,by='Country.Name',all=TRUE)
     wb_frame <- merge(wb_frame,wb_e_gdp,by='Country.Name',all=TRUE)
     wb_frame <- merge(wb_frame,wb_gdp_capita,by='Country.Name',all=TRUE)
     
     wb_frame$Country.Name <- gsub('Brunei Darussalam','Brunei',wb_frame$Country.Name,fixed=TRUE)
     wb_frame$Country.Name <- gsub("Cote d'Ivoire",'Cote dIvoire (IvoryCoast)',wb_frame$Country.Name,fixed=TRUE)
     wb_frame$Country.Name <- gsub("Egypt, Arab Rep.",'Egypt',wb_frame$Country.Name,fixed=TRUE)
     wb_frame$Country.Name <- gsub("Hong Kong SAR, China",'Hong Kong',wb_frame$Country.Name,fixed=TRUE)
     wb_frame$Country.Name <- gsub("Iran, Islamic Rep.",'Iran',wb_frame$Country.Name,fixed=TRUE)
     wb_frame$Country.Name <- gsub("Korea, Rep.",'Korea, South',wb_frame$Country.Name,fixed=TRUE)
     wb_frame$Country.Name <- gsub("Russian Federation",'Russia',wb_frame$Country.Name,fixed=TRUE)
     wb_frame$Country.Name <- gsub("Slovak Republic",'Slovakia',wb_frame$Country.Name,fixed=TRUE)

     
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

 
 ###### EIA Data
 #Calculating the latest energy consumption for all countries from EIA Data
         
eia_electricity <- read.csv('eia_electricity.csv')
         
 country_names <- subset(eia_electricity,eia_electricity$units != 'Billion Kwh')
 country_names <- subset(country_names,country_names$Country.Name!= 'Tide, Wave, Fuel Cell') %>% select(Country.Name)

 consumption_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Consumption')) %>% gather(key='year',value='value',X1980:X2014)
 names(consumption_eia)[2] <- 'Var'
 generation_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Generation')) %>% gather(key='year',value='value',X1980:X2014)
 names(generation_eia)[2] <- 'Var'
 nuclear_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Nuclear'))
 names(nuclear_eia)[2] <- 'Var'
 renewables_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Renewables'))
 names(renewables_eia)[2] <- 'Var'
 hydro_eia <- cbind(country_names,subset(eia_electricity,eia_electricity$Country.Name == 'Hydroelectricity'))
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

eia_wb_climatescope <- merge(eia_fuel_merge,climatescope,by=c("Country.Name"))
eia_wb_climatescope$dollar_km <- (eia_wb_climatescope$investments_udmm*1000000)/eia_wb_climatescope$land_area
eia_wb_climatescope$dollar_person <- (eia_wb_climatescope$investments_udmm*1000000)/eia_wb_climatescope$population

missing_countries(eia_fuel_merge,eia_wb_climatescope)[[1]]
missing_countries(eia_fuel_merge,eia_wb_climatescope)[[2]]

ggplot(eia_wb_climatescope,aes(log(dollar_km),country_diff)) + geom_point(aes(size = investments_udmm,colour=dollar_person),alpha=0.4) + scale_colour_gradient(low = "grey", high = "blue") + geom_text(aes(label=Country.Name),hjust=0, vjust=-1,size = 2) + ylim(-20,60) + theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + theme(legend.position="bottom") + xlab("log(Dollars Invested per Square Kilometer)") + ylab("Percentage Point Change 1980-2014, Non-Hydro Renewable Energy Generation") + labs(size="Billions of Dollars ($US)", colour="$US Invested per Capita")


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






