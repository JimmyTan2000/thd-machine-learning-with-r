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
summary(Data)


# Remove factors that we dont use 
Data <- subset (Data, select = c(price, bedrooms, bathrooms, sqft_living, 
                                   floors ,waterfront ,view ,condition ,sqft_above ,
                                   sqft_basement ))

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
################################################################################################################
# Training decision tree
################################################################################################################
library(tree)

#calculating the decision tree using only the train data
dtree = tree(price ~ bedrooms + bathrooms + sqft_living + 
                floors + waterfront + view + condition + sqft_above +
                sqft_basement 
              ,data = Data.train)

tuning <- cv.tree(dtree, K=5)
t <- which.min(tuning$dev)
n.terminalnode <- tuning$size[t]

model <- prune.tree(dtree, best=n.terminalnode)
plot(model)
text(model)

#calculate the prediction using test data 
x.test <- Data.test[,c("bedrooms", "bathrooms" ,"sqft_living" , 
                       "floors" ,"waterfront" ,"view" ,"condition" ,"sqft_above" ,
                       "sqft_basement")]

prediction <- predict(model, x.test)

#Calculate the mean absolute error 
y.test <- Data.test[, "price"]
mean(abs(y.test - prediction))



