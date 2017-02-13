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
setwd('/Users/Diego/Desktop/Data/energy_transitions')

#### Read Data

all.vars <- read.csv('WB_all_vars.csv') 

#Creating New Vars
all.vars$time.diff <- (all.vars$X2012..YR2012 - all.vars$X2002..YR2002.)
all.vars$time.diffT0 <- (all.vars$X2012..YR2012 - all.vars$X2000..YR2000.)
    #Subsetting New Vars
    time.diff.frame <- all.vars %>% select(Country.Name,Series.Name,time.diff,time.diffT0)
    time.diff.frame <- subset(time.diff.frame,time.diff.frame$Series.Name != 'Present value of external debt (current US$)' & time.diff.frame$Series.Name != 'Present value of external debt (current US$)' & time.diff.frame$Series.Name != 'Present value of external debt (% of GNI)' & time.diff.frame$Series.Name != 'Population growth (annual %)' & time.diff.frame$Series.Name != 'ICT goods imports (% total goods imports)' & time.diff.frame$Series.Name != 'Energy related methane emissions (% of total)' & time.diff.frame$Series.Name != 'Ease of doing business index (1=most business-friendly regulations)' & time.diff.frame$Series.Name != 'Droughts, floods, extreme temperatures (% of population, average 1990-2009)' & time.diff.frame$Series.Name !='Combustible renewables and waste (% of total energy)')
    unique.time.diff <- as.character(unique(time.diff.frame$Series.Name))

#Time Series Data World Development Indicators
all.vars <- all.vars %>% gather(key='year',value='value',X1960..YR1960.:X2015..YR2015.)
  #Subset 
  all.vars <- subset(all.vars,all.vars$Series.Name != 'Present value of external debt (current US$)' & all.vars$Series.Name != 'Present value of external debt (current US$)' & all.vars$Series.Name != 'Present value of external debt (% of GNI)' & all.vars$Series.Name != 'Population growth (annual %)' & all.vars$Series.Name != 'ICT goods imports (% total goods imports)' & all.vars$Series.Name != 'Energy related methane emissions (% of total)' & all.vars$Series.Name != 'Ease of doing business index (1=most business-friendly regulations)' & all.vars$Series.Name != 'Droughts, floods, extreme temperatures (% of population, average 1990-2009)' & all.vars$Series.Name !='Combustible renewables and waste (% of total energy)')
  unique.vars <- unique(all.vars$Series.Name)

  
#Energy and CO2 Vars from the World Bank for the Energy Transitions PLots
energy.co2.vars <- read.csv('Data_Extract_From_World_Development_Indicators_Data.csv')
    energy.co2.vars <- energy.co2.vars %>% gather(key='year',value='value',X1960..YR1960.:X2015..YR2015.) %>% select(Country.Name,Series.Name,year,value)
    energy.co2.sort <- data.table(energy.co2.vars)[order(Country.Name,year,Series.Name)]
    energy.co2.complete <- energy.co2.sort[169:length(energy.co2.sort$Country.Name),] #Avoids all the missing data
    
    
    
#### Energy and CO2 Capita
    co2.gdpcapita <- cbind(energy.co2.complete[Series.Name =="CO2 emissions (metric tons per capita)"], energy.co2.complete[Series.Name=="GDP per capita (constant 2005 US$)"])
    names(co2.gdpcapita)[4] <- "co2_capita"
    names(co2.gdpcapita)[8] <- "gdp_capita"   
    co2.gdpcapita.df <- co2.gdpcapita[,.(Country.Name,year,co2_capita,gdp_capita)]
    
    # Get rid of 0s and NAs
    co2.gdpcapita.df.complete <- co2.gdpcapita.df[co2_capita != ".."]
    co2.gdpcapita.df.complete <- co2.gdpcapita.df.complete[co2_capita > 0]
    co2.gdpcapita.df.complete <-  as.data.frame(co2.gdpcapita.df.complete[gdp_capita != ".."])
    
    #Numeric vars
    co2.gdpcapita.df.complete$co2_capita <- as.numeric(co2.gdpcapita.df.complete$co2_capita)
    co2.gdpcapita.df.complete$gdp_capita <- as.numeric(co2.gdpcapita.df.complete$gdp_capita)
    
    #Not Countries
    co2.gdpcapita.df.complete$Country.Name <- as.character(co2.gdpcapita.df.complete$Country.Name)
    no.country <- c("Arab World", "Central Europe and the Baltics", "East Asia & Pacific (all income levels)","East Asia & Pacific (developing only)","Euro area","Europe & Central Asia (all income levels)", "Europe & Central Asia (developing only)","European Union","Fragile and conflict affected situations","Heavily indebted poor countries (HIPC)","High income","High income: nonOECD","High income: OECD", "Latin America & Caribbean (all income levels)","Latin America & Caribbean (developing only)","Least developed countries: UN classification", "Low & middle income", "Lower middle income", "Middle East & North Africa (all income levels)", "Middle East & North Africa (developing only)","Middle income","North America", "OECD members", "Other small states","Sub-Saharan Africa (all income levels)","Sub-Saharan Africa (developing only)", "Upper middle income", "World")
    co2.gdpcapita.df.sub <- co2.gdpcapita.df.complete[!(co2.gdpcapita.df.complete$Country.Name %in% no.country),]
    
    #For loop and Plotting
    
    unique.country.names <- unique(co2.gdpcapita.df.sub$Country.Name)
    #Writing a for loop to label each line
    for (i in 1:length(unique.country.names)) {
      country <- subset(co2.gdpcapita.df.sub,co2.gdpcapita.df.sub$Country.Name == unique.country.names[i])
      last <- tail(country$gdp_capita,1)
      country$last.val <- ifelse(country$gdp_capita==last,'last','notlast')
      
      if (i ==1){
        country.co2.df <- country
      } else {
        country.co2.df <- rbind(country.co2.df,country)
      }
    }
    
    # Energy GDP per Capita Plot
    co2.capita.plot <- ggplot(data=country.co2.df,aes(x=gdp_capita,y=co2_capita,group=Country.Name)) + geom_line() +  geom_text(aes(label=ifelse(last.val=='last',as.character(country.df$Country.Name),'')),hjust=0,just=0,size = 4,color='red') + xlab('GDP per Capita (Constant 2005 $US)') + ylab("CO2 emissions (metric tons per capita)") + theme(panel.background = element_blank())
    
    # Log Energy GDP per Capita Plot
    country.co2.df$log_co2 <- log(country.co2.df$co2_capita)
    country.co2.df$log_capita <- log(country.co2.df$gdp_capita)
    
    co2.capita.plot.log <- ggplot(data=country.co2.df,aes(x=log_capita,y=log_co2,group=Country.Name)) + geom_line() +  geom_text(aes(label=ifelse(last.val=='last',as.character(country.df$Country.Name),'')),hjust=0,just=0,size = 4,color='red') + xlab('log (GDP per Capita (Constant 2005 $US))') + ylab("log (CO2 emissions (metric tons per capita))") + theme(panel.background = element_blank())
    
    
    
    
    
    
    
#### Energy and GDPcapita
    energy.gdpcapita <- cbind(energy.co2.complete[Series.Name =="Electricity production from renewable sources, excluding hydroelectric (% of total)"], energy.co2.complete[Series.Name=="GDP per capita (constant 2005 US$)"])
    names(energy.gdpcapita)[4] <- "pct_gen"
    names(energy.gdpcapita)[8] <- "gdp_capita"
    vars <- c("Country.Name","year","Generation from Renewable Energy, Excluding Hydroelectric (%)","GDP Capita, Constant 2005 US4")
    energy.gdpcapita.df <- energy.gdpcapita[,.(Country.Name,year,pct_gen,gdp_capita)]
    
    # Get rid of 0s and NAs
    energy.gdpcapita.complete <- energy.gdpcapita.df[pct_gen != ".."]
    energy.gdpcapita.complete <- energy.gdpcapita.complete[pct_gen > 0]
    energy.gdpcapita.complete <-  as.data.frame(energy.gdpcapita.complete[gdp_capita != ".."])
    
    #Numeric vars
    energy.gdpcapita.complete$pct_gen <- as.numeric(energy.gdpcapita.complete$pct_gen)
    energy.gdpcapita.complete$gdp_capita <- as.numeric(energy.gdpcapita.complete$gdp_capita)
    
    #Removing IDs that are not countries
    energy.gdpcapita.complete$Country.Name <- as.character(energy.gdpcapita.complete$Country.Name)
    no.country <- c("Arab World", "Central Europe and the Baltics", "East Asia & Pacific (all income levels)","East Asia & Pacific (developing only)","Euro area","Europe & Central Asia (all income levels)", "Europe & Central Asia (developing only)","European Union","Fragile and conflict affected situations","Heavily indebted poor countries (HIPC)","High income","High income: nonOECD","High income: OECD", "Latin America & Caribbean (all income levels)","Latin America & Caribbean (developing only)","Least developed countries: UN classification", "Low & middle income", "Lower middle income", "Middle East & North Africa (all income levels)", "Middle East & North Africa (developing only)","Middle income","North America", "OECD members", "Other small states","Sub-Saharan Africa (all income levels)","Sub-Saharan Africa (developing only)", "Upper middle income", "World")
    energy.gdpcapita.sub <- energy.gdpcapita.complete[!(energy.gdpcapita.complete$Country.Name %in% no.country),]
      
    unique.country.names <- unique(energy.gdpcapita.sub$Country.Name)
    #Writing a for loop to label each line
    for (i in 1:length(unique.country.names)) {
      country <- subset(energy.gdpcapita.sub,energy.gdpcapita.sub$Country.Name == unique.country.names[i])
      last <- tail(country$gdp_capita,1)
      country$last.val <- ifelse(country$gdp_capita==last,'last','notlast')
      
      if (i ==1){
        country.df <- country
      } else {
        country.df <- rbind(country.df,country)
      }
    }
    
    # Energy GDP per Capita Plot
    energy.capita.plot <- ggplot(data=country.df,aes(x=gdp_capita,y=pct_gen,group=Country.Name)) + geom_line() +  geom_text(aes(label=ifelse(last.val=='last',as.character(country.df$Country.Name),'')),hjust=0,just=0,size = 3) + xlab('GDP per Capita (Constant 2005 $US)') + ylab("Generation from Renewable Energy, Excluding Large Hydropower (%)") + theme(panel.background = element_blank())
    
    # Log Energy GDP per Capita Plot
    country.df$log_gtp <- log(country.df$gdp_capita)
    energy.capita.plot.log <- ggplot(data=country.df,aes(x=log_gtp,y=pct_gen,group=Country.Name)) + geom_line() +  geom_text(aes(label=ifelse(last.val=='last',as.character(country.df$Country.Name),'')),hjust=0,just=0,size = 4) + xlab('log (GDP per Capita (Constant 2005 $US))') + ylab("Generation from Renewable Energy, Excluding Large Hydropower (%)") + theme(panel.background = element_blank())
    


  
  
#### ClimateScope Variables
climate.scope <- read.csv('climatescope2014.csv')
      # Change the name of some vars
      climate.scope<- subset(climate.scope,climate.scope$Policy.Var != 'VC/PE investments' & climate.scope$Policy.Var != 'Investor pressure \x8a\x97\x96 PRI signatories' & climate.scope$Policy.Var != 'Capacity building \x8a\x97\x96 env. business training' & climate.scope$Policy.Var != 'Capacity building \x8a\x97\x96 think tanks')
      unique.climatescope.vars <- unique(climate.scope$Policy.Var)



#### EPI Variables
epi.scores <- read.csv('epi_2012.csv') %>% gather(key='indicator',value='value',ACCESS.2012:EPI.2012) %>% mutate(Country.Name=country) %>% select(Country.Name,indicator,value)
epi.scores.ordered <- epi.scores[order(-epi.scores$Country.Name,)]
unique.epi <- unique(epi.scores.ordered$indicator)
    
    


##### 1. %Renewable Energy Generation vs . World Bank Development Indicators

#Note to change the variable to plot, simply plot the variable name
# Vars to investigate (NOTE: Change the path depending on where you want to plot the results)
evar <- 'Electricity production from renewable sources, excluding hydroelectric (kWh)'
evar <- 'Electricity production from renewable sources, excluding hydroelectric (% of total)' 

#For loop to create all plots
plot_list = list()
for (i in 1:length(unique.vars)) {
  
  plot.var = unique.vars[i]
  energy.sub <- subset(all.vars,all.vars$Series.Name==evar & all.vars$year == 'X2012..YR2012.') %>% mutate(eval=value) %>% select(Country.Name,eval)
  var.sub <- subset(all.vars,all.vars$Series.Name==plot.var & all.vars$year == 'X2012..YR2012.') %>% select(Country.Name,value)
  merged <- join(var.sub,energy.sub,by=('Country.Name'),type='left',match='all')
  
  p <- ggplot(merged,aes(x=value,y=eval,label=Country.Name)) + geom_point() + geom_text(aes(label=Country.Name),hjust=1, vjust=1,size=3) + xlab(plot.var) + ylab(evar)
  plot_list[[i]] = p
}

# Save plots to jpeg making a separate file for each plot.
for (i in 1:length(unique.vars)) {
   
plot.var = unique.vars[i]
name <- substring(plot.var,1,35)
mypath <- file.path("/Users/Diego/Desktop/Projects/Energy_Transitions/total_production",paste(name,".jpg",sep = ""))
jpeg(file=mypath)
print(plot_list[[i]])
dev.off()
}


##### 2. %Renewable Energy Generation vs  Climatescope Variables

#Note to change the variable to plot, simply plot the variable name
# Vars to investigate (NOTE: Change the path depending on where you want to plot the results)
evar <- 'Electricity production from renewable sources, excluding hydroelectric (kWh)'
evar <- 'Electricity production from renewable sources, excluding hydroelectric (% of total)' 


#For loop to create all plots
plot_list = list()
for (i in 1:length(unique.climatescope.vars)) {
  
  plot.var = unique.climatescope.vars[i]
  print(plot.var)
  energy.sub <- subset(all.vars,all.vars$Series.Name==evar & all.vars$year == 'X2012..YR2012.') %>% mutate(eval=value) %>% select(Country.Name,eval)
  var.sub <- subset(climate.scope,climate.scope$Policy.Var==plot.var) %>% select(Country.Name,Score)
  merged <- join(var.sub,energy.sub,by=('Country.Name'),type='left',match='all')
  
  p <- ggplot(merged,aes(x=Score,y=eval,label=Country.Name)) + geom_point() + geom_text(aes(label=Country.Name),hjust=1, vjust=1,size=3) + xlab(plot.var) + ylab(evar)
  plot_list[[i]] = p
}

# Save plots to jpeg making a separate file for each plot.
for (i in 1:length(unique.climatescope.vars)) {
  
  plot.var = unique.climatescope.vars[i]
  name <- substring(plot.var,1,35)
  mypath <- file.path("/Users/Diego/Desktop/Projects/Energy_Transitions/climatescope_percent_renewables",paste(name,".jpg",sep = ""))
  jpeg(file=mypath)
  print(plot_list[[i]])
  dev.off()
}








##### 3. %Renewable Generation vs. Environmental Peforrmance Index Plots


#Note to change the variable to plot, simply plot the variable name
# Vars to investigate (NOTE: Change the path depending on where you want to plot the results)
evar <- 'Electricity production from renewable sources, excluding hydroelectric (kWh)'
evar <- 'Electricity production from renewable sources, excluding hydroelectric (% of total)' 


epi.scores.ordered <- epi.scores[order(-epi.scores$Country.Name,)]
unique.epi <- unique(unique.epi$indicator)

#For loop to create all plots
plot_list = list()
for (i in 1:length(unique.epi)) {
  
  plot.var = unique.epi[i]
  print(plot.var)
  energy.sub <- subset(all.vars,all.vars$Series.Name==evar & all.vars$year == 'X2012..YR2012.') %>% mutate(eval=value) %>% select(Country.Name,eval)
  var.sub <- subset(epi.scores.ordered,epi.scores.ordered$indicator==plot.var) %>% select(Country.Name,value)
  merged <- join(var.sub,energy.sub,by=('Country.Name'),type='left',match='all')
  
  p <- ggplot(merged,aes(x=value,y=eval,label=Country.Name)) + geom_point() + geom_text(aes(label=Country.Name),hjust=1, vjust=1,size=3) + xlab(plot.var) + ylab(evar)
  plot_list[[i]] = p
}

# Save plots to jpeg making a separate file for each plot.
for (i in 1:length(unique.epi)) {
  
  plot.var = unique.epi[i]
  name <- substring(plot.var,1,35)
  mypath <- file.path("/Users/Diego/Desktop/Projects/Energy_Transitions/pct_EPI",paste(name,".jpg",sep = ""))
  jpeg(file=mypath)
  print(plot_list[[i]])
  dev.off()
}





##### 4. %Changes in the last 10 years vs. World Development Indicators

#Note to change the variable to plot, simply plot the variable name
# Vars to investigate (NOTE: Change the path depending on where you want to plot the results)

evar <- 'Electricity production from renewable sources, excluding hydroelectric (% of total)'
evar.plot <- '% Change RE 2002 - 2012'


#For loop to create all plots
plot_list = list()
for (i in 1:length(unique.vars)) {
  
  plot.var = unique.vars[i]
  var.sub <- subset(all.vars,all.vars$Series.Name==plot.var & all.vars$year == 'X2012..YR2012.') %>% select(Country.Name,value)
  time.sub <- subset(time.diff.frame,time.diff.frame$Series.Name == evar)
  merged <- join(var.sub,time.sub,by=('Country.Name'),type='left',match='all')
  
  p <- ggplot(merged,aes(x=value,y=time.diff,label=Country.Name)) + geom_point() + geom_text(aes(label=Country.Name),hjust=1, vjust=1,size=3) + xlab(plot.var) + ylab(evar.plot)
  plot_list[[i]] = p
}

# Save plots to jpeg making a separate file for each plot.
for (i in 1:length(unique.vars)) {
  
  plot.var = unique.vars[i]
  name <- substring(plot.var,1,35)
  mypath <- file.path("/Users/Diego/Desktop/Projects/Energy_Transitions/REpercent_change_WBIndicators/",paste(name,".jpg",sep = ""))
  jpeg(file=mypath)
  print(plot_list[[i]])
  dev.off()
}





##### 5. %Changes in the last 10 years vs. Climatescope Variables
evar <- 'Electricity production from renewable sources, excluding hydroelectric (% of total)'
evar.plot <- '% Change RE 2002 - 2012'


#For loop to create all plots
plot_list = list()
for (i in 1:length(unique(time.diff$Series.Name))) {
  
  plot.var = unique.climatescope.vars[i]
  print(plot.var)
  time.sub <- subset(time.diff.frame,time.diff.frame$Series.Name == evar)
  var.sub <- subset(climate.scope,climate.scope$Policy.Var==plot.var) %>% select(Country.Name,Score)
  merged <- join(var.sub,time.sub,by=('Country.Name'),type='left',match='all')
  
  p <- ggplot(merged,aes(x=Score,y=time.diff,label=Country.Name)) + geom_point() + geom_text(aes(label=Country.Name),hjust=1, vjust=1,size=3) + xlab(plot.var) + ylab(evar.plot)
  plot_list[[i]] = p
}


# Save plots to jpeg making a separate file for each plot.
for (i in 1:length(unique.climatescope.vars)) {
  
  plot.var = unique.climatescope.vars[i]
  print(plot.var)
  name <- substring(plot.var,1,35)
  mypath <- file.path("/Users/Diego/Desktop/Projects/Energy_Transitions/REpercent_change_Climatescope",paste(name,".jpg",sep = ""))
  jpeg(file=mypath)
  print(plot_list[[i]])
  dev.off()
}






# 6. %Change in Renewables in the last 10 Years vs. %Change in Renewables Everywhere Else 
evar <- 'Electricity production from renewable sources, excluding hydroelectric (% of total)'
evar.plot <- '% Change RE 2002 - 2012'

#For loop to create all plots
plot_list = list()
plot.list.vars = list()
j = 0
for (i in 1:length(unique.time.diff)) {
  
  plot.var = unique.time.diff[i]
  time.sub <- subset(time.diff.frame,time.diff.frame$Series.Name == plot.var)

  if (sum(time.sub$time.diff,na.rm=TRUE)!=0) {
  j = j +1   
  plot.list.vars[j] <- plot.var

  e.sub <- subset(time.diff.frame,time.diff.frame$Series.Name == evar) %>% mutate(e.diff = time.diff, Series.NameA=Series.Name) %>% select(Country.Name,Series.NameA,e.diff)
  merged <- join(e.sub,time.sub,by=('Country.Name'),type='left',match='all')
  
  p <- ggplot(merged,aes(x=time.diff,y=e.diff,label=Country.Name)) + geom_point() + geom_text(aes(label=Country.Name),hjust=1, vjust=1,size=3) + xlab(plot.var) + ylab(evar.plot)
  plot_list[[j]] = p
  }
}

# Save plots to jpeg making a separate file for each plot.
for (i in 1:length(plot.list.vars)) {
  
  plot.var = plot.list.vars[i]
  print(plot.var)
  name <- substring(plot.var,1,35)
  mypath <- file.path("/Users/Diego/Desktop/Projects/Energy_Transitions/REpercent_change_vs_percent_change",paste(name,".jpg",sep = ""))
  jpeg(file=mypath)
  print(plot_list[[i]])
  dev.off()
}




#Which elements in the list did we miss
missed.e <- unique.time.diff[which(!unique.time.diff %in% plot.list.vars)]

# 7. Small subset for variables that we missed
small.sub <- subset(all.vars, all.vars$Series.Name == 'ICT service exports (% of service exports, BoP)' | all.vars$Series.Name == 'Foreign direct investment, net (BoP, current US$)' | all.vars$Series.Name == 'Adequacy of social insurance programs (% of total welfare of beneficiary households)' | all.vars$Series.Name == 'Access to electricity (% of population)') %>% gather(key='year',value='value',X1960..YR1960.:X2015..YR2015.) %>% select(Country.Name,Series.Name,time.diffT0,year)
unique.small.sub <- unique(small.sub$Series.Name)

evar <- 'Electricity production from renewable sources, excluding hydroelectric (% of total)'
evar.plot <- '% Change RE 2002 - 2012'

#For loop to create all plots
plot_list = list()
plot.list.vars = list()
j = 0
for (i in 1:length(unique.small.sub)) {
  
  plot.var = unique.small.sub[i]
  time.sub <- subset(small.sub,small.sub$Series.Name == plot.var)
  
  if (sum(time.sub$time.diffT0,na.rm=TRUE)!=0) {
    j = j +1   
    plot.list.vars[j] <- as.character(plot.var)
    
    e.sub <- subset(time.diff.frame,time.diff.frame$Series.Name == evar) %>% mutate(e.diff = time.diff, Series.NameA=Series.Name) %>% select(Country.Name,Series.NameA,e.diff)
    merged <- join(e.sub,time.sub,by=('Country.Name'),type='left',match='all')
    
    p <- ggplot(merged,aes(x=time.diffT0,y=e.diff,label=Country.Name)) + geom_point() + geom_text(aes(label=Country.Name),hjust=1, vjust=1,size=3) + xlab(plot.var) + ylab(evar.plot)
    plot_list[[j]] = p
  }
}

# Save plots to jpeg making a separate file for each plot.
for (i in 1:length(plot.list.vars)) {
  
  plot.var = plot.list.vars[[i]]
  name <- substring(plot.var,1,35)
  mypath <- file.path("/Users/Diego/Desktop/Projects/Energy_Transitions/REpercent_change_vs_percent_change",paste(name,".jpg",sep = ""))
  jpeg(file=mypath)
  print(plot_list[[i]])
  dev.off()
}



############################################ Clustering











