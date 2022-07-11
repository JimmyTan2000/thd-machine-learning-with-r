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

# Change data values in some variables which is equal to 0 to "NA" in order to avoid wrong
# result in summary

# Remove factors that we dont use 
Data <- subset (Data, select = c(price, bedrooms, bathrooms ,sqft_living  , 
                                   floors ,waterfront ,view ,condition ,sqft_above ,
                                   sqft_basement  ))

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

##################################
# Load the mboost library
library(mboost)

# Randomise the order of data 
n <- length(Data[,1])
n
index <- sample(1:n,n,replace=FALSE)
Data <- Data[index,]

# Divide the data set into training data and test data in 70:30 ratio
Data.train <- Data[1:round(n * 0.7),]
Data.test <- Data[(round(n*0.7)+1):n,]


model <- gamboost(price ~ bedrooms + bathrooms + sqft_living  + 
                    floors + waterfront + view + condition + sqft_above +
                    sqft_basement  
                  , data=Data.train, dfbase = 4, control = boost_control(mstop = 1000))
model
par(mfrow=c(1,9)) #because there are 9 independant variables 
plot(model)


# Cross-validation:

cv10f <- cv(model.weights(model), type = "kfold")
cvm <- cvrisk(model, folds = cv10f, papply = lapply)
print(cvm)
mstop(cvm)
plot(cvm)

# Calculation of the model (Cross-validated)
model <- gamboost(price ~ bedrooms + bathrooms + sqft_living  + 
                    floors + waterfront + view + condition + sqft_above +
                    sqft_basement  
                  ,data=Data.train, dfbase = 4, control = boost_control(mstop = mstop(cvm)))
model
par(mfrow=c(1,9)) 
options(scipen=10000)
plot(model) #because there are 9 independant variables


# Calculation of the test result based on the test data

X.test <- Data.test[,c("bedrooms", "bathrooms" ,"sqft_living" , 
                       "floors" ,"waterfront" ,"view" ,"condition" ,"sqft_above" ,
                       "sqft_basement")]
prediction <- predict(model,X.test)

# Calculation of the mean absolute error (MAD)
y.test <- Data.test[,"price"]
mean(abs(y.test-prediction))