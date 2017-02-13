# Quick example

#Set your directory
setwd('/Users/Diego/Desktop/Data/energy_transitions/')

# Read the data set that has the data (name of the file)
oil.rents <- read.csv('WB_all_vars.csv')
  
unique(oil.rents$Series.Name)  

# Select the name of the variable
oil.df <- subset(oil.rents,oil.rents$Series.Name == "Oil rents (% of GDP)" | oil.rents$Series.Name == "Electricity production from renewable sources, excluding hydroelectric (% of total)")

#If you want to sort only by oil, remove the second variable it would look like this:
oil.df <- subset(oil.rents,oil.rents$Series.Name == "Oil rents (% of GDP)")


# Prepare the dataframe
oil.df.2 <- oil.df %>% gather(key='year',value='value',X1960..YR1960.:X2015..YR2015.) %>% select(Country.Name,Series.Name,year,value)

# Select the year which is 2012
oil.df.3 <- subset(oil.df.2, oil.df.2$year == "X2012..YR2012.")

#Remove NAs
oil.df.4 <-  oil.df.3[complete.cases(oil.df.3),]

# Sort so that we can see the outliers
oil.df.4.sorted <- oil.df.4[order(-oil.df.4$value),] 

# First 5 outliers
head(oil.df.4.sorted)

# First 10 outliers

head(oil.df.4.sorted,10)


# Select a country that is an outlier, but not a top ten


# Make a plot

oil <- subset(oil.df.4,oil.df.4$Series.Name == "Oil rents (% of GDP)")
rene <- subset(oil.df.4,oil.df.4$Series.Name == "Electricity production from renewable sources, excluding hydroelectric (% of total)")

plot.df <- join(oil,rene,by=c("Country.Name"),type="right",match="all")

# Changing names and making a plot

names(plot.df)[4] <- "oil_rents_pct"
names(plot.df)[7] <- "renewable_pct"

head(plot.df)

plot(plot.df$oil_rents_pct,plot.df$renewable_pct)






