#library(devtools)

#options(devtools.install.args = c("–no-multiarch", "–no-test-load"))

#install.packages("C:/Users/Gabriel/Downloads/catboost-R-windows-x86_64-1.2.5.tgz", repos = NULL, type = "source", INSTALL_opts = c("–no-multiarch", "–no-test-load"))

#install.packages("caret")
#install.packages("titanic")
library(catboost)
library(caret)
library(titanic)

# load data
set.seed(1)
idx=sample(1:nrow(iris),nrow(iris)*.7)
train=iris[idx,]
test=iris[-idx,]
fit_control <- caret::trainControl(
  method = "cv", 
  number = 3, 
  search = "random",
  classProbs = TRUE
)
# set grid options
grid <- expand.grid(
  depth = c(4, 6, 8),
  learning_rate = 0.1,
  l2_leaf_reg = 0.1,
  rsm = 0.95,
  border_count = 64,
  iterations = 10
)
model <- caret::train(
  x = train[,-5], 
  y = train[,5],
  method = catboost.caret,
  metric = "Accuracy",
  maximize = TRUE,
  preProc = NULL,
  tuneGrid = grid, 
  tuneLength = 30, 
  trControl = fit_control
)

table(test$Species,predict(model,test))

# Pertama, data Titanic diubah menjadi format data frame:
data <- as.data.frame(as.matrix(titanic::titanic_train), stringsAsFactors=TRUE)

# Kemudian, nilai NA pada kolom Age diisi dengan nilai yang paling sering muncul di kolom tersebut:
age_levels <- levels(data$Age)
most_frequent_age <- which.max(table(data$Age))
data$Age[is.na(data$Age)] <- age_levels[most_frequent_age]

# Kolom yang tidak diperlukan untuk pelatihan model dihapus, dan data dibagi menjadi fitur (x) dan target (y):
drop_columns = c("PassengerId", "Survived", "Name", "Ticket", "Cabin")
x <- data[,!(names(data) %in% drop_columns)]
y <- data[,c("Survived")]

# Kontrol pelatihan untuk cross-validation diatur menggunakan caret:
fit_control <- caret::trainControl(
  method = "cv", 
  number = 3, 
  search = "random",
  classProbs = TRUE
)

# Parameter grid untuk model CatBoost ditentukan:
grid <- expand.grid(
  depth = c(4, 6, 8),
  learning_rate = 0.1,
  l2_leaf_reg = 0.1,
  rsm = 0.95,
  border_count = 64,
  iterations = 10
)

# Model CatBoost dilatih menggunakan caret dengan parameter grid yang telah ditentukan:
model <- caret::train(
  x = x, 
  y = as.factor(make.names(y)),
  method = catboost.caret,
  metric = "Accuracy",
  maximize = TRUE,
  preProc = NULL,
  tuneGrid = grid, 
  tuneLength = 30, 
  trControl = fit_control
)

print(model)

# variable importance
importance <- varImp(model, scale = FALSE)
print(importance)

plot(importance)