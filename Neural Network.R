# Setup directory and read data

setwd("D:/Desktop D/THD/Sem 4/Machine Learning/latest/Projects/PStA")
Data <- read.csv("data.csv",header=TRUE,sep=",",fill=TRUE,stringsAsFactors=TRUE)

# Trim data

# Get data index with which the price = 0

index_price_0 <- which(Data[,"price"] == 0)

# Remove the data with the price = 0

Data <- Data[-index_price_0,]

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
summary(Data)


# Remove factors that we dont use 
Data <- subset (Data, select = c(price, bedrooms, bathrooms, sqft_living, 
                                   floors ,waterfront ,view ,condition ,sqft_above ,
                                   sqft_basement, city))

# Print the first 10 data in the dataset

Data[1:10,]


# Summary of the data

summary(Data)

# Randomise the order of data 
n <- length(Data[,1])
n
index <- sample(1:n,n,replace=FALSE)
Data <- Data[index,]

# Divide the data set into training data and test data in 70:30 ratio
Data.train <- Data[1:round(n * 0.7),]
Data.test <- Data[(round(n*0.7)+1):n,]

##############################################################
#Training of Neural Network
##############################################################
library(ANN2)

x.train <- model.matrix(price ~ bedrooms + bathrooms + sqft_living + 
                    floors + waterfront + view + condition + sqft_above +
                    sqft_basement + city
                    ,Data.train)
x.train <- x.train[,-1]
summary(x.train)

y.train <- Data.train[,"price"]

# Traning of the neural network
# with 2 hidden layer, with 4 hidden units in the first layer and 3 hidden units on the second layer

model <- neuralnetwork(x.train, y.train, hidden.layers=c(4,3), regression = TRUE, 
                       loss.type = "absolute", learn.rates = 1e-04,n.epochs = 100,
                       verbose=FALSE)
plot(model)


# calculation of the mean absolute error (MAD)

x.test <- model.matrix(price ~ bedrooms + bathrooms + sqft_living + 
                         floors + waterfront + view + condition + sqft_above +
                         sqft_basement + city
                       ,Data.test)
x.test <- x.test[,-1]

y.test <- Data.test[,"price"]

prediction <- predict(model,x.test)$predictions 

mean(abs(y.test- prediction))

