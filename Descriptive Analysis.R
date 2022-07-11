# Setup directory and read data

setwd("D:/Bryan Bu/Documents/Technische Hochschule Deggendorf/KI-B-4 SS22/Maschinelles Lernen/PStA")
Data <- read.csv("data.csv",header=TRUE,sep=",",fill=TRUE,stringsAsFactors=TRUE)


# Remove unused factors
Data <- subset (Data, select = -c(date, street, statezip, country))


# Trim data

# Get data index with which the price = 0

index_price_0 <- which(Data[,"price"] == 0)

# Remove the data with the price = 0

Data <- Data[-index_price_0,]

# if yr_renovated = 0, then yr_renovated = yr_built

i <- 1
while (i <= length(Data[,"yr_renovated"])) {
  if (Data[i,"yr_renovated"] == 0) {
    Data[i,"yr_renovated"] <- Data[i,"yr_built"]
  }
  i <- i + 1
}


# Correction of the false datatype

Data[,"waterfront"] <- as.factor(Data[,"waterfront"])
Data[,"view"] <- as.factor(Data[,"view"])
Data[,"condition"] <- as.factor(Data[,"condition"])
Data[,"city"] <- as.factor(Data[,"city"])


# Compute Pearson's Correlation Coefficient
# import library corrgram

library("corrgram")

Data_num <- Data[,]
r <- cor(Data_num)
corrgram(Data_num, upper.panel = panel.cor)


# Print the first 10 data in the dataset
  
  Data[1:10,]
  
  
# Summary of the data
  
  summary(Data)

  
# Boxplot and Histogram
  
  # Box plots for the metric variable data
  
  options(scipen=10000)
  
  metric_variable <- c("price","bedrooms","bathrooms","sqft_living","sqft_lot","floors","sqft_above",
                       "sqft_basement","yr_built","yr_renovated")
  par(mfrow=c(3,4))
  for (j in 1:length(metric_variable)) {
    boxplot(Data[,metric_variable[j]],main=metric_variable[j])
  }
  
  boxplot(Data[,"price"],ylim=c(70000,1000000),main="price")
  boxplot(Data[,"bedrooms"],ylim=c(2,6),main="bedrooms")
  boxplot(Data[,"bathrooms"],ylim=c(0,4),main="bathrooms")
  boxplot(Data[,"sqft_living"],ylim=c(0,5000),main="sqft_living")
  boxplot(Data[,"sqft_lot"],ylim=c(0,20000),main="sqft_lot")
  boxplot(Data[,"sqft_above"],ylim=c(0,5000),main="sqft_above")
  boxplot(Data[,"sqft_basement"],ylim=c(0,2500),main="sqft_basement")
  boxplot(Data[,"yr_renovated"],ylim=c(1950,2020),main="yr_renovated")
  
  # Histograms for the metric variable data
  
  metric_variable <- c("price","bedrooms","bathrooms","sqft_living","sqft_lot","floors","sqft_above",
                       "sqft_basement","yr_built","yr_renovated")
  for (j in 1:length(metric_variable)) {
    hist(Data[,metric_variable[j]],main=metric_variable[j])
  }
  
  # Bar charts for the categorical data
  barplot(height=c(4471,30),names.arg=c("0","1"),main="waterfront")
  barplot(height=c(4064,69,196,110,62),names.arg=c("0","1","2","3","4"),main="view")
  barplot(height=c(5,30,2818,1226,422),names.arg=c("1","2","3","4","5"),main="condition")
  
# Relationship between each variable and price
  
  # Box plots
  
  boxplot(price ~ bedrooms, data=Data)
  boxplot(price ~ bathrooms, data=Data)
  boxplot(price ~ floors, data=Data)
 
  boxplot(price ~ bedrooms, ylim=c(0,2500000), data=Data)
  boxplot(price ~ bathrooms, ylim=c(0,4000000), data=Data)
  boxplot(price ~ floors, ylim=c(0,2100000), data=Data)
  
  sqft_living = Data[order(Data$sqft_living),]
  x <- sqft_living[,"sqft_living"]
  y <- sqft_living[,"price"]
  plot(x,y,main="price depends on sqft_living",xlab="sqft_living",ylab="price",type="l")

  sqft_lot = Data[order(Data$sqft_lot),]
  x <- sqft_lot[,"sqft_lot"]
  y <- sqft_lot[,"price"]
  plot(x,y,main="price depends on sqft_lot",xlab="sqft_lot",ylab="price",type="l")

  sqft_above = Data[order(Data$sqft_above),]
  x <- sqft_above[,"sqft_above"]
  y <- sqft_above[,"price"]
  plot(x,y,main="price depends on sqft_above",xlab="sqft_above",ylab="price",type="l")

  sqft_basement = Data[order(Data$sqft_basement),]
  x <- sqft_basement[,"sqft_basement"]
  y <- sqft_basement[,"price"]
  plot(x,y,main="price depends on sqft_basement",xlab="sqft_basement",ylab="price",type="l")

  yr_built = Data[order(Data$yr_built),]
  x <- yr_built[,"yr_built"]
  y <- yr_built[,"price"]
  plot(x,y,main="price depends on yr_built",xlab="yr_built",ylab="price",pch=19)

  yr_renovated = Data[order(Data$yr_renovated),]
  x <- yr_renovated[,"yr_renovated"]
  y <- yr_renovated[,"price"]
  plot(x,y,main="price depends on yr_renovated",xlab="yr_renovated",ylab="price",pch=19)
