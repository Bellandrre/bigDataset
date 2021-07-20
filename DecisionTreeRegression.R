DataFrame <- read.table(file="C:/Users/alinemati/Google Drive/finalproject-bigdata/datasets/kc_house_data.txt",header=TRUE,sep="\t") 



# Splitting the Data Set in 25% and 75% to get  Test and Training
dataRow = sample(1:nrow(DataFrame), size = 0.25*nrow(DataFrame))


summary(dataRow)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 9    5362   10882   10850   16276   21610

#trainData dataset 25% of our datasets
trainData = DataFrame[-dataRow,] 

summary(trainData)

#testData dataset 75% of our datasets
testData = DataFrame[dataRow,] 
summary(testData)


#Price vs. Sqft_living
par(mfrow=c(2,1))
plot(DataFrame$sqft_living , DataFrame$price,col=(c("red","blue")),
     main="Price vs. Sqft_living", xlab="Sqft_living ", ylab="Price")
boxplot1=boxplot(DataFrame$price ~ DataFrame$sqft_living, data=trainData,
                 col=(c("red","blue")),
                 main="Price vs. Sqft_living", xlab="Sqft_living", ylab="Price")


#-----------------------------------------------------------------------
#Price vs. bedrooms
plot(DataFrame$bedrooms , DataFrame$price,col=(c("red","blue")),
     main="Price vs. bedrooms", xlab="bedrooms", ylab="Price")
boxplot1=boxplot(DataFrame$price ~ DataFrame$bedrooms, data=trainData,
                 col=(c("red","blue")),
                 main="Price vs. bedrooms", xlab="bedrooms", ylab="Price")

#-----------------------------------------------------------------------
# Price vs. bathrooms
plot(DataFrame$bathrooms , DataFrame$price,col=(c("red","blue")),
     main="Price vs. bathrooms", xlab="bathrooms", ylab="Price")
boxplot1=boxplot(DataFrame$price ~ DataFrame$bathrooms, data=trainData,
                 col=(c("red","blue")),
                 main="Price vs. bathrooms", xlab="bathrooms", ylab="Price")


#-----------------------------------------------------------------------

# Price vs. grade
plot(DataFrame$grade,DataFrame$price, col=(c("red","blue")),
     main="Price vs. grade", xlab="grade", ylab="Price")

boxplot1=boxplot(DataFrame$price ~ DataFrame$grade, data=trainData,
                 col=(c("red","blue")),
                 main="Price vs. grade", xlab="grade", ylab="Price")


#-----------------------------------------------------------------------
# Price vs. sqft_lot
par(mfrow=c(2,1))
plot(DataFrame$sqft_lot ,DataFrame$price, col=(c("red","blue")),
     main="Price vs. sqft_lot", xlab="sqft_lot", ylab="Price")

boxplot1=boxplot(DataFrame$price ~ DataFrame$sqft_lot, data=trainData,
                 col=(c("red","blue")),
                 main="Price vs. sqft_lot", xlab="sqft_lot", ylab="Price")

#-----------------------------------------------------------------------
# Price vs. condition
par(mfrow=c(2,1))
plot(DataFrame$condition ,DataFrame$price, col=(c("red","blue")),
     main="Price vs. condition", xlab="condition", ylab="Price")

boxplot1=boxplot(DataFrame$price ~ DataFrame$condition, data=trainData,
                 col=(c("red","blue")),
                 main="Price vs. condition", xlab="condition", ylab="Price")

#-----------------------------------------------------------------------
# Price vs. sqft_above

par(mfrow=c(2,1))
plot(DataFrame$sqft_above ,DataFrame$price, col=(c("red","blue")),
     main="Price vs. sqft_above", xlab="sqft_above", ylab="Price")

boxplot1=boxplot(DataFrame$price ~ DataFrame$sqft_above, data=trainData,
                 col=(c("red","blue")),
                 main="Price vs. sqft_above", xlab="sqft_above", ylab="Price")

#-----------------------------------------------------------------------
# Price vs. sqft_basement

par(mfrow=c(2,1))
plot(DataFrame$sqft_basement ,DataFrame$price, col=(c("red","blue")),
     main="Price vs. sqft_basement", xlab="sqft_basement", ylab="Price")

boxplot1=boxplot(DataFrame$price ~ DataFrame$sqft_basement, data=trainData,
                 col=(c("red","blue")),
                 main="Price vs. sqft_basement", xlab="sqft_basement", ylab="Price")

#-----------------------------------------------------------------------
# Price vs. lat
par(mfrow=c(2,1))
plot(DataFrame$lat ,DataFrame$price, col=(c("red","blue")),
     main="Price vs. lat", xlab="lat", ylab="Price")

boxplot1=boxplot(DataFrame$price ~ DataFrame$lat, data=trainData,
                 col=(c("red","blue")),
                 main="Price vs. lat", xlab="lat", ylab="Price")


#-----------------------------------------------------------------------
# Price vs. sqft_living15
par(mfrow=c(2,1))
plot(DataFrame$sqft_living15 ,DataFrame$price, col=(c("red","blue")),
     main="Price vs. sqft_living15", xlab="sqft_living15", ylab="Price")

boxplot1=boxplot(DataFrame$price ~ DataFrame$sqft_living15, data=trainData,
                 col=(c("red","blue")),
                 main="Price vs. sqft_living15", xlab="sqft_living15", ylab="Price")


#-----------------------------------------------------------------------
# Price vs. sqft_lot15
par(mfrow=c(2,1))
plot(DataFrame$sqft_lot15 ,DataFrame$price, col=(c("red","blue")),
     main="Price vs. sqft_lot15", xlab="sqft_lot15", ylab="Price")

boxplot1=boxplot(DataFrame$price ~ DataFrame$sqft_lot15, data=trainData,
                 col=(c("red","blue")),
                 main="Price vs. sqft_lot15", xlab="sqft_lot15", ylab="Price")

hist(DataFrame$price)
#------------------------------------------------------------------------

vec_price_sqftliving <-aggregate(DataFrame$price~DataFrame$sqft_living, FUN=mean, data=Training)
plot(vec_price_sqftliving)

#------------------------------------------------------------------------

