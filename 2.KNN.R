##Step 1-collecting data

##Step 2-exploring and preparing the data
# iris is a default data set in R. Import the data.
iris

#examine the structure of the iris data frame
str(iris)

# table of diagnosis
table(iris$Species)

# recode Species as a factor
iris$Species <- factor(iris$Species, levels = c("setosa", "versicolor","virginica"))

# table or proportions with more informative labels
round(prop.table(table(iris$Species)) * 100, digits = 1)

# summarize three numeric features
summary(iris[c("Sepal.Length", "Sepal.Width", "Petal.Length")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the iris data
iris_n <- as.data.frame(lapply(iris[1:4], normalize))

# confirm that normalization worked
summary(iris_n$Sepal.Length)

# create training and test data. We use set.seed()and sample() to randomize the data set.
set.seed(123)
train_sample<-sample(150,120)
str(train_sample)

iris_train <- iris_n[train_sample, ]
iris_test <- iris_n[-train_sample, ]

# create labels for training and test data

iris_train_labels <- iris[train_sample,5]
iris_test_labels <- iris[-train_sample,5]


## Step 3: Training a model on the data 

# load the "class" library
library(class)

iris_test_pred <- knn(train = iris_train, test = iris_test,
                      cl = iris_train_labels, k = 11)

head(iris_test)
head(iris_test_pred)

## Step 4: Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = iris_test_labels, y = iris_test_pred,
           prop.chisq = FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
iris_z <- as.data.frame(scale(iris[-5]))

# confirm that the transformation was applied correctly
summary(iris_z$Sepal.Length)

# create training and test datasets
iris_train <- iris_z[train_sample, ]
iris_test <- iris_z[-train_sample, ]

# re-classify test cases
iris_test_pred <- knn(train = iris_train, test = iris_test,
                      cl = iris_train_labels, k = 11)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = iris_test_labels, y = iris_test_pred,
           prop.chisq = FALSE)

# try several different values of k
iris_train <- iris_n[train_sample, ]
iris_test <- iris_n[-train_sample, ]

iris_test_pred <- knn(train = iris_train, test = iris_test, cl = iris_train_labels, k=1)
CrossTable(x = iris_test_labels, y = iris_test_pred, prop.chisq=FALSE)

iris_test_pred <- knn(train = iris_train, test = iris_test, cl = iris_train_labels, k=6)
CrossTable(x = iris_test_labels, y = iris_test_pred, prop.chisq=FALSE)

iris_test_pred <- knn(train = iris_train, test = iris_test, cl = iris_train_labels, k=20)
CrossTable(x = iris_test_labels, y = iris_test_pred, prop.chisq=FALSE)

iris_test_pred <- knn(train = iris_train, test = iris_test, cl = iris_train_labels, k=60)
CrossTable(x = iris_test_labels, y = iris_test_pred, prop.chisq=FALSE)





