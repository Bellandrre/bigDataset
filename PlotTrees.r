# libraries used
 > library("GGally")
 > library("ggplot2")
 > library("scales")
 > library("caret")
 > library("rpart")
 > library("rpart.plot")
 > library("car")
 > library("Metrics")

# Let’s load the data into R –
> kc_house_data <- read.csv(file=”kc_house_data.csv”, header = TRUE)

# Checking for NA values in dataset
> table(is.na(kc_house_data))

# Loading the required libraries – 
> library(ggplot2)

# Let's take bedrooms,bathrooms and condition as factors
> kc_house_data$bedrooms <- as.factor(kc_house_data$bedrooms)
> kc_house_data$bathrooms <- as.factor(kc_house_data$bathrooms)
> kc_house_data$sqft_living <- as.factor(kc_house_data$condition)


# Let's look at some data and it's distribution
> p1 <- qplot(bedrooms, data = kc_house_data, geom = "bar", main = "The number of houses by bedrooms")
> p2 <- qplot(bathrooms,data = kc_house_data, geom = "bar" , main = "The number of houses by bathrooms")
> plot(kc_house_data$price, ylab = "Density", xlab = "Price")
>p3 <- ggplot(kc_house_data, aes(price)) + geom_density() + 
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma, limits = c(0, 2e+06)) +
    xlab("price") +
    ggtitle("Price distribution"

# We will now try to compare the price with other features like bedrooms, bathrooms and sqft_living
> scatterplot(x =kc_house_data$price, y=kc_house_data$bedrooms)    #not dependent
> scatterplot(x =kc_house_data$price, y=kc_house_data$bathrooms)   # Seems to be depedent
> scatterplot(x =kc_house_data$price, y=kc_house_data$sqft_living) # Very much dependent


# Correlation between the features of the houses
> ggcorr(kc_house_data, hjust = 0.8, layout.exp =1) + ggtitle("Correlation b/w features")

# Splitting the data to avoid over fits
 > kc_house_data <- kc_house_data[-c(1,2)] # remove the ID and date column
 > train_data <- createDataPartition(kc_house_data$price, p = .7, list = FALSE)
 > test <- kc_house_data[train_data,]
 > train <- kc_house_data[-train_data,]

 > tree <- rpart(price ~ ., data=df)
 > tree_predict <- predict(tree_fit, test)
 > rpart.plot(tree_fit, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)
 
 #Extract true value from the dataset 
 > tree_true <- test[,1]
 
 
 
 # Results 
 > summary(tree_predict) 
 # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 #315438  315438  462801  540099  654887 5081430
 
 > summary(tree_true) 
 # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 75000  321500  450000  540100  645000 7700000
 
 > cor(tree_predict,tree_true ) # 0.8262802
 
 > rmse(tree_predict, tree_true) # 205319.8
 
 # Plotting the actual and the predicted distribution
 > result <- data.frame(price=c(tree_predict, tree_true),type=c(replicate(length(tree_predict),"Predicted"),replicate(length(tree_true),"Actual")))

 > ggplot(res, aes(x=price, colour=type)) + scale_x_continuous(labels = comma, limits = c(0, 2e+06)) + scale_y_continuous(labels = comma) +
    geom_density()
	