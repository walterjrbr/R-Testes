library(caret)


data(iris)
# rename the dataset
dataset = iris
print(dataset)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index = createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation = dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset = dataset[validation_index,]
dim(dataset)
sapply(dataset, class)
head(dataset)
levels(dataset$Species)

percentage = prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

summary(dataset)

control = trainControl(method="cv", number=10)
metric = "Accuracy"

#Avaliando os algoritmos

#  linear algorithms
set.seed(7)
fit.lda = train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
#  nonlinear algorithms
# CART
set.seed(7)
fit.cart = train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn = train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
#  advanced algorithms
# SVM
set.seed(7)
fit.svm = train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf = train(Species~., data=dataset, method="rf", metric=metric, trControl=control)


results = resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)


print(fit.lda)



predictions = predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
