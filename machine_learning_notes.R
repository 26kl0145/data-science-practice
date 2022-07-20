## load required libraries
library(dplyr)
library(ggplot2)

## Unsupervised learning: k-means clustering

iris_numerics <- select(iris, -Species) %>%
  scale()

iris_clusters <- kmeans(iris_numerics, centers = 3)
iris_clusters # our results

iris_clusters$cluster # vector designating a cluster for each row
iris$cluster <- iris_clusters$cluster
head(iris)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = as.factor(cluster)))

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species))

ggplot(iris, aes(x = cluster)) +
  geom_bar(aes(fill = Species))



## Try this on your own dataset! Use at least 2 numeric variables and make a plot
vgsales_numerics <- select(vgsales, Global_Sales, NA_Sales, EU_Sales) %>%
  scale()

vgsales_clusters <- kmeans(vgsales_numerics, centers = 10)
vgsales_clusters

vgsales$cluster <- vgsales_clusters$cluster

ggplot(vgsales, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point(aes(color = as.factor(cluster)))

ggplot(vgsales, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point(aes(color = as.factor(Genre)))


ggplot(vgsales, aes(x = cluster)) +
  geom_bar(aes(color = Genre))

ggplot(vgsales, aes(x = cluster)) +
  geom_bar(aes(color = Platform))






### Supervised modeling
# Visualizing data
ggplot(iris, aes( x = Petal.Length, y = Sepal.Length)) +
  geom_point()

ggplot(iris, aes( x = Petal.Length, y = Sepal.Width)) +
  geom_point()

ggplot(iris, aes( x = Petal.Length, y = Petal.Width)) +
  geom_point()

cor(iris$Petal.Length, iris$Sepal.Length) # gives correlation value
cor(iris$Petal.Length, iris$Sepal.Width)
cor(iris$Petal.Length, iris$Petal.Width)

# Choose features
# Petal.Width, Sepal.Length, Sepal.Width

# Split into training, test, and validation sets
greetings <- c(rep("hello", 5),rep("goodbye",3)) %>%
  sample(8, replace = F)
greetings

iris_len <-nrow(iris)

iris$label <-c(rep("training", ceiling(.6*iris_len)),
               rep("test", ceiling(.2*iris_len)),
               rep("validation", ceiling(.2*iris_len))) %>%
  sample(iris_len, replace = F)

head(iris)  
# training and test sets help train to make the model better
# the validation set is just some new variables for it to predict


### Choosing a model!
## When we use a model, we "train" it using the training set and
## "test" it using the testing set
iris_train <- filter(iris, label == "training")
iris_test <- filter(iris, label == "test")
iris_valid <- filter(iris, label == "validation")

## Linear model
# Takes the form of y = m1x1 + m2x2... + b
# Only takes and predicts numeric values

iris_lm <- lm(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width, data = iris_train) 
# the first variable (Petal.Length) is the y variable
# Petal.Width is x1 and Sepal.Length is x2
# The intercept is b
# m1 and m2 are the coefficients

iris_lm
summary(iris_lm) # you want the adjusted r-squared value to be as close to 1

# select out only the x-values we used (Petal.Width and Sepal.Length)
iris_lm_predictions <- select(iris_test, Petal.Width, Sepal.Length, Sepal.Width) %>%
  predict(object = iris_lm) # objects is the model that we just created

iris_test$lm_pred <- iris_lm_predictions

head(iris_test)


## Logistic model
# Fits data to a more flexible function, used when y is a binomial categorical variable
# Takes the form of y = f(x1,x2,...)
# x-values can be numeric or categorical

# create 2 categories
mean(iris$Petal.Length)
iris_train_glm <- iris_train %>%
  mutate(petal_length_cat = as.factor(ifelse(Petal.Length < 3.758, "short", "long")))
# ifelse takes a statement. if it returns true, then it returns the second argument. if it returns false, it returns the third
head(iris_train_glm)

iris_glm <- glm(petal_length_cat ~ Petal.Width + Sepal.Length + Sepal.Width + Species,
                data = iris_train_glm,
                family = binomial(link = "logit"))

summary(iris_glm)

iris_glm_preds <- iris_test %>%
  select(Petal.Width, Sepal.Length, Sepal.Width, Species) %>%
  predict(object = iris_glm)

iris_test$glm_pred <- iris_glm_preds

iris_test <- iris_test %>%
  mutate(petal_length_cat = as.factor(ifelse(Petal.Length < 3.758, "short", "long")))

head(iris_test)

# filter down to 2 categories
iris_train_2species <- filter(iris_train, Species %in% c("setosa", "virginica"))

# create the model
iris_glm <- glm(Species ~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length,
                data = iris_train_2species,
                family = binomial(link = "logit"))

summary(iris_glm)

# create test set with only 2 species
iris_test_2species <- iris_test %>%
  filter(Species %in% c("setosa", "viriginica"))

# make predictions based on model
iris_2species_preds <- iris_test_2species %>%
  select(-Species) %>%
  predict(object = iris_glm)

# add predictions to test set
iris_test_2species$glm_2spec_pred <- iris_2species_preds



## General boosting machine
# Uses decision trees instead of a formula
# Predicts categorical or numeric variables
# Less interpretable and more computationally intensive
install.packages("gbm")
library(gbm)

# create the model
# order for x-values doesn't matter as much here
iris_gbm <- gbm(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width + Species,
                data = iris, # iris_test was too small, make sure to use test for your data
                n.trees = 500)

summary(iris_gbm)

# select out only the x-values we used from test and predict
iris_gbm_preds <- iris_test %>%
  select(Petal.Width, Sepal.Length, Sepal.Width, Species) %>%
  predict(object = iris_gbm)

# save predictions back into test set
iris_test$gbm_pred <- iris_gbm_preds


## Evaluate performance of models
View(iris_test)

install.packages("Metrics")
library(Metrics)

# calculate rmse between predictions and true values
rmse(iris_test$Petal.Length, iris_test$lm_pred)
rmse(iris_test$Petal.Length, iris_test$gbm_pred) # wins! smaller error

# calculate mae between predictions and true values
mae(iris_test$Petal.Length, iris_test$lm_pred)
mae(iris_test$Petal.Length, iris_test$gbm_pred) # wins! smaller error


# Accuracy
iris_test <- iris_test %>%
  mutate(glm_petal_cat = ifelse(glm_pred < 0, "long", "short"))
View(iris_test)

true_vals <- sum(iris_test$glm_petal_cat == iris_test$petal_length_cat)
total_vals <- nrow(iris_test)

accuracy <- true_vals/total_vals
accuracy

# f1 score tells us about false positive and false negative rates
f1(iris_test$glm_petal_cat, iris_test$petal_length_cat)
