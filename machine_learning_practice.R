# load required libraries
library(dplyr)
library(ggplot2)

## Unsupervised learning: k-means clustering
View(iris)
iris_numerics <- select(iris, -Species) %>%
  scale()

iris_clusters <- kmeans(iris_numerics, centers = 3)
iris_clusters

iris_clusters$cluster
iris$cluster <- iris_clusters$cluster
head(iris)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = as.factor(cluster)))

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species))

ggplot(iris, aes(x = cluster)) +
  geom_bar(aes(fill = Species))

## Try this on your own dataset! Use at least 2 numeric variables & make a plot!
spotify_data <- read.csv("data/Spotify-2000.csv")
View(spotify_data)

spotify_data_numeric <- select(spotify_data, -c(Title, Artist, Top.Genre, Year, Length..Duration.)) %>%
  scale()



### Supervised modeling
# Visualizing data
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

cor(iris$Sepal.Length, iris$Sepal.Length) # gives correlation
cor(iris$Sepal.Length, iris$Sepal.Width)
cor(iris$Sepal.Length, iris$Sepal.Width)

# Choose features
# Petal.Width, Sepal.Length, Sepal.Width

# Split into training, test, and validation sets
greetings <- c(rep("hello", 5), rep("goodbye",3)) %>%
  sample(8, replace = T)
greetings

iris_len <- nrow(iris)

iris$label <- c(rep("training", ceiling(.6*iris_len)),
                rep("test", ceiling(.2*iris_len)),
                rep("validation", ceiling(.2*iris_len))) %>%
  sample(iris_len, replace = F)

head(iris)

### Choosing a model!
## When we use a model, we "train" it uring the the training set and
## "test" it using the testing set
iris_train <- filter(iris, label == "training")
iris_test <- filter(iris, label == "test")
iris_valid <- filter(iris, label == "validation")

## Linear model

iris_lm <- lm(Petal.Length ~ Petal.Width + Sepal.Length, data = iris_train)
iris_lm

# select out only the x values we use (Petal.Width and Sepal.Length)
iris_lm_predictions <- select(iris_test, Petal.Width, Sepal.Length, Sepal.Width) %>%
  predict(object = iris_lm) #object is the model that we just created

iris_train$lm_pred <- iris_lm_predictions

head(iris_train)

## Logistic model

mean(iris$Petal.Length)
iris_train_glm <- iris_train %>%
  mutate(petal_length_cat = as.factor(ifelse(Petal.Length <3.758, "short", "long")))
iris_train_glm

iris_glm <- glm(petal_length_cat ~ Petal.Width + Sepal.Length + Sepal.Width,
                data = iris_train_glm,
                family = binomial(link = "logit"))

summary(iris_glm)

iris_glm_preds <- iris_test %>%
  select(Petal.Width, Sepal.Length, Sepal.Width) %>%
  predict(object = iris_glm)

iris_test$glm_pred <- iris_glm_preds

iris_test <- iris_test %>%
  mutate(petal_length_cat = as.factor(ifelse(Petal.Length <3.758, "short", "long")))

head(iris_test)

# filter down to 2 categories
iris_train_2species <- filter(iris_train, Species %in% c("setosa", "virginica"))

# create the model
iris_glm <- glm(Species ~ Petal.Width + Sepal.Length + Sepal.Width,
                data = iris_train_2species,
                family = binomial(link = "logit"))

summary(iris_glm)

# make predictions based on modek
iris_test_2species <- iris_test %>%
  filter(Species %in% c("setosa", "viginica"))

# make predictions based on model
iris_2species_preds <- iris_test_2species %>%
  select(-Species) %>%
  predict(object = iris_glm)

# add predictions to test set
iris_test_2species_2spec_pred <- iris_2species_preds

### Generalized boosted regression modeling
install.packages("gbm")
library(gbm)

# create the model
iris_gbm <- gbm(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width + Species,
                data = iris, # iris_test was too small, make sure to use test for your data
                n.trees = 500)

# select out only the x values we used from test and predict
iris_gbm_preds <- iris_test %>%
  select(Petal.Width, Sepal.Length, Sepal.Width, Species) %>%
  predict(object = iris_gbm)

## Evalutate performance of models
View(iris_test)

install.packages("Metrics")
library(Metrics)

# calculate rmse between predictions and true values
rmse(iris_test$Sepal.Length, iris_test$lm_pred)
rmse(iris_test$Sepal.Length, iris_test$gbm_pred)

# calculate mae between predictions and true values

mae(iris_test$Sepal.Length, iris_test$lm_pred)
mae(iris_test$Sepal.Length, iris_test$gbm_pred)



# Accuracy
iris_test <- iris_test %>%
  mutate(glm_petal_cat = ifelse(glm_pred < 0, "long", "short"))
View(iris_test)

true_vals <- sum(iris_test$glm_petal_cat == iris_test$petal_length_cat)
total_vals <- nrow(iris_test)

